# Performance investigation finding — SWB2 daily loop (Oct 2026)

**Status:** Investigation complete; one safe quick win identified and one
candidate optimization scoped for future work.
**Branch:** phenology_phase_1
**Benchmark:** `test/integration_tests/cs` (Central Sands, WI), 138,400 active
cells, 2 years daily (2012-01-01 .. 2013-12-31), `FLOW_ROUTING_METHOD NONE`.
**Toolchain:** gfortran 15.2 (conda-forge), pixi-managed build.

---

## Motivation

Investigate whether SWB2's runtime can be improved with low-risk changes —
specifically whether the original contiguous-memory (struct-of-arrays) design
had eroded over time into inefficient access patterns. Prompted in part by a
colleague's assertion that SWB is "too slow" relative to PRMS.

## Framing note (problem size vs. code efficiency)

SWB solves an inherently O(number of grid cells) problem. For the Central Sands
case that is ~1e8 cell-day computations (138,400 cells x 730 days). A lumped
HRU model solving the "same" area with a handful of HRUs does orders of
magnitude less work; it is not a like-for-like comparison. SWB's deliverable is
a spatially distributed recharge grid for MODFLOW — the resolution *is* the
product. Optimization here targets the **constant factor** on an unavoidable
O(n) cost, not the O(n) itself. A distributed grid model will never cost like a
lumped one, nor should it try.

---

## What was tested and found

### 1. Memory layout / architecture — NOT the problem
The struct-of-arrays layout (every state variable a contiguous 1D array indexed
by active cell) is intact and appropriate. The two-tier method design
(whole-array `array_method` vs. per-cell `index_method`) is deliberate; the
per-cell loop exists for D8 routing's ordering dependency. No "silly access
model" was found in the state container or the daily loop structure. The
per-cell science modules (FAO-56 ET, curve number, soil mass balance, solar)
are each only 1-3% of runtime individually — the arithmetic is cheap.

### 2. Compiler optimization level — NO win (regression)
Measured, same-machine, warm cache (Windows gfortran 15.2):

| Build | Flags | Daily-loop run time |
|-------|-------|---------------------|
| release | -O2, no -ffast-math, FPE traps | 3:56 (236 s) |
| release_fast | -O3 -funroll-loops, no -ffast-math | 4:36 (276 s) |

**-O3 is ~17% SLOWER than -O2** for this code (confirmed, 40 s swing; the -O3
binary is also ~7.6 MB vs ~6 MB, consistent with code bloat). Likely cause:
over-aggressive inlining/unrolling and poor vectorization choices on a branchy,
procedure-pointer-heavy per-cell loop. **Keep -O2.** (The long-standing
conservatism about aggressive optimization flags is vindicated here — for
performance reasons, not the floating-point-safety reason originally assumed.
Note: -O3 without -ffast-math preserves IEEE semantics; it was simply slower.)

### 3. FP-exception trapping (`-ffpe-trap=overflow,zero`) — small, safe win
Measured, same-machine (Hovenweep / Linux):

| Build | Daily-loop run time |
|-------|---------------------|
| release (with -ffpe-trap) | 6:12 (372 s) |
| release_notrap (no -ffpe-trap) | 5:44 (344 s) |

Dropping the FP-exception traps saves **~28 s, ~7.5%**. Real, numerics
unchanged, but modest.
**Recommendation (with a caveat — see "FPE trap vs. ingestion checks" below):**
consider keeping `-ffpe-trap` in `develop` and dropping it from the production
`release` build for the ~7.5% — BUT first audit what the trap currently catches
that code-level checks do not. Removing it is only "free" if nothing relies on
it to turn bad input into a clean abort. (Not applied — left as a maintainer
decision pending that audit.)

### 4. gprof flat-profile self-time is UNTRUSTWORTHY on this toolchain
The gprof flat profile attributed ~59% of self-time to libgfortran FP-environment
routines (`_gfortrani_set_fpu_except_flags`, `get_fpu_state`,
`support_fpu_rounding_mode`). This was **misleading**: the call graph showed
these as `<spontaneous>` with no caller — the known artifact of profiling a
`-pg` binary linked against non-instrumented libgfortran. The experiment in (3)
proved the real cost of the trap/FP-exception machinery is only ~7.5%, not 59%.

**Lesson:** on this platform, trust gprof's **call counts** (from `mcount`) and
its **call-graph children times**, but NOT the flat-profile self-time of
library leaf functions. Prefer before/after wall-clock experiments for
attribution. (gprof does not even produce time samples at all on Windows/MinGW;
profiling must be done on Linux.)

### Trustworthy structural facts from the profile
- Daily loop ~66 s decomposes as: `perform_daily_calculation` ~36 s (the
  per-cell science, spread thinly across many 1-3% routines),
  `get_weather_data` ~26 s (~40%!), `write_output` ~3 s.
- `get_weather_data` is called once per day (731x), not per cell. Its cost is
  dominated by resampling each day's Daymet weather onto the model grid.
- **`grid_getGridColNum` is called 304,203,208 times** (and `grid_getGridRowNum`
  likewise). This is a reliable call count, not a sampling artifact.

---

## Candidate optimization for future work: cache the resample index map

### The observation
Each timestep, new weather grids (precip, tmin, tmax) are resampled from their
native Daymet grid onto the model grid by `grid_GridToGrid_sgl`
(`src/grid.F90:2419`). For **every destination cell on every timestep**, it
calls `grid_GetGridColRowNum` -> `grid_GetGridColNum` / `grid_GetGridRowNum`
(`src/grid.F90:1998` / `:2031`), each of which computes a nearest-neighbor
source index via a floating-point division and `NINT`:

```fortran
iColumnNumber = NINT(real(pGrd%iNX, c_double) &
             * ( rX - pGrd%rX0 ) / (pGrd%rX1 - pGrd%rX0) + 0.5_c_double, c_int)
```

This source->destination (col,row) mapping is **invariant for as long as the
native grid geometry is unchanged**. SWB is commonly fed a *time series* of
NetCDF files. In principle a later file could have a different
extent/resolution/projection than an earlier one (e.g., 1981-1987 grids at
200x300, then 1988-2000 grids at 300x500). The current per-timestep
recomputation silently *adapts* to such a change by rebuilding the mapping every
step — which is likely why it looks the way it does (~304 million calls).

**Design decision (maintainer intent): varying grid geometry mid-run should be a
hard error, not a silently-supported mode.** Under that policy the mapping is
genuinely compute-once, and the cache's geometry check becomes the enforcement
mechanism: build the map from the first grid; on any later grid, assert its
geometry matches and `die` with a clear message otherwise. The cache and the
desired hard-error are therefore the *same* change.

#### What the ingestion path already guards (verified) vs. the gap
- `grid_CompletelyCover` (`grid.F90:1327`) — checks the native grid covers the
  model domain, but is invoked only *conditionally*
  (`if this%lRequireCompleteSpatialCoverage`) in `transform_grid_to_grid_sub`.
  Would catch a too-small new grid when that flag is set.
- `grid_CheckForPROJ4Error` (`grid.F90:1665`) — catches a *failed* projection
  operation, not a *changed* projection.
- Dimension sanity (`iNX>0`, allocation success) at grid creation.
- **GAP:** there is currently **no assertion that successive files in a time
  series share the same geometry.** A new grid of different extent that still
  covers the domain would be silently accommodated by the per-timestep remap and
  produce different resampling with no error. So the maintainer's "just error
  out" intent is NOT fully enforced today; the existing checks are partial and
  conditional (coverage + PROJ4-error), not a geometry-equality trap.

### Why the compiler cannot remove the recomputation
The compiler **cannot** hoist or eliminate these calls:
- The inputs (`pGrd%rX0`, `rX1`, `iNX`, and the per-cell `pGrdTo%rX/rY`) are
  runtime values reached through **pointers** (`type(GENERAL_GRID_T), pointer`).
  It cannot prove they are invariant across the timestep calls, nor that the
  source and destination grids do not alias.
- The invariance is a **domain fact**, recoverable only from how the model is
  used (and, per the decision above, something to be *enforced*), not from the
  code itself.
- `NINT` + floating-point division are genuine operations with observable
  results; nothing lets the optimizer treat them as redundant across calls it
  must assume may see changed memory.

This is exactly the class of redundancy that requires a human-introduced cache;
`-O2`/`-O3` cannot fix it.

### Proposed design (sketch — not yet implemented)
Compute the mapping once; enforce geometry constancy; gather thereafter.

1. Add to `DATA_CATALOG_ENTRY_T` (or the native-grid resample context):
   ```fortran
   integer (c_int), allocatable :: src_col(:,:)   ! (iNX_to, iNY_to)
   integer (c_int), allocatable :: src_row(:,:)
   logical,         allocatable :: src_valid(:,:) ! in-bounds mask
   logical                      :: cache_built = .false.
   type(GRID_GEOMETRY_KEY_T)    :: cached_native_geometry  ! nx,ny,x0,y0,x1,y1,proj
   ```
2. On the first resample for a dataset, compute `grid_GetGridColRowNum` once per
   destination cell, store the (col,row) + in-bounds flag, record the native
   geometry signature, set `cache_built`.
3. On every subsequent resample, compare the current native geometry to
   `cached_native_geometry`. If it differs -> **`die`** with a clear message
   (this is the maintainer-intended hard error). If it matches -> reuse cache.
4. With a valid cache, the per-timestep resample is a direct gather (no NINT, no
   float division, no per-cell index recomputation):
   ```fortran
   where (src_valid)
     pGrdTo%rData = pGrdFrom%rData_indexed_by(src_col, src_row)   ! conceptual
   end where
   ```
5. Reuse/extend existing grid machinery for the geometry signature and the
   coverage/PROJ4 checks rather than inventing a parallel validation path.

### Expected payoff — HONEST uncertainty
- The 304M call count is real, so there is genuine redundant work to remove.
- **Magnitude is unproven.** The discredited flat profile cannot be used to size
  it. Call-graph children time suggested the grid-lookup family is on the order
  of ~10% of the daily loop, possibly more if the `NINT`/FP-divide is a
  significant share of the (now-suspect) FP-environment time. Treat it as a
  "likely modest-to-moderate" win, not a 3x.
- **Verification plan:** prototype the cache behind a flag, run `time-cs`
  before/after on the same machine (do NOT rely on gprof self-time), and
  confirm output grids are bit-for-bit identical (the mapping is unchanged, so
  they must be). The wall-clock delta is the real answer.

### Risk / correctness notes
- Must reproduce the exact same nearest-neighbor selection and the same
  in-bounds `cycle` behavior as the current loop, so results are identical.
- Watch memory: two integer arrays at destination-grid size (138,400 cells ->
  ~1 MB each) are negligible.
- Confirm the cache is keyed correctly if multiple data-catalog entries share or
  differ in native grid geometry.

---

## Bigger lever (noted, not pursued here): parallelism

For an O(cells) model, the largest potential speedup is parallelism, not
micro-optimization. With `FLOW_ROUTING_METHOD NONE`, `sort_order` is the
identity sequence (`model_domain.F90:607`) and the per-cell loop in
`daily_calculation.F90` has **no inter-cell dependency** — it is embarrassingly
parallel. `grid_GridToGrid_sgl` even has a commented-out `!$OMP PARALLEL DO`.
An OpenMP parallel-do over active cells (guarded to the no-routing case, or with
a separate ordered path for D8) is the one change with genuine
multiples-of-speedup potential on a many-core machine like Hovenweep. It is a
larger, higher-risk change (thread safety of the per-cell method pointers, the
`update_previous_5_day_rainfall` shared state in curve number, etc.) and was
out of scope for this low-risk inquiry.

---

## Open design question (capture for later): mid-run grid geometry change

Deciding how SWB should behave when input grid geometry (extent / resolution /
projection) changes partway through a simulation is a **semantics/feature
question, not a performance question**, and is explicitly NOT being resolved
here. Captured so it is not lost.

Two legitimate, competing needs:
- **Safety:** most mid-run geometry changes are user mistakes (wrong data
  product, bad clip, drifted projection). Silently adapting would produce
  subtly wrong recharge feeding MODFLOW — the worst failure mode. Argues for a
  hard error.
- **Flexibility:** some bespoke datasets legitimately change geometry at a known
  point in time (switching data sources, stitching differently-gridded eras,
  a refined grid becoming available). Forbidding this outright prevents valid
  science. Argues for graceful continuation.

### Proposed resolution to revisit (maintainer's sketch)
A single elegant policy that serves both without a fail-vs-adapt dichotomy:

1. **Compute the source->destination mapping once** at the start of the
   simulation (the performance win).
2. **If the grid mapping parameters change**, trigger a **remap** (recompute the
   index map for the new geometry) — not a per-timestep recompute, just once at
   the change.
3. **If the remap fails** (e.g., the new grid cannot validly cover the model
   domain / produces an invalid mapping), **kill the simulation** with a clear
   message.
4. **Otherwise, log a message noting the geometry change and carry on.**

The kill criterion is *validity of the remap*, not *the fact that geometry
changed*. This keeps the common case fast (one computation), supports the
legitimate bespoke-change case (remap + continue + log), and still fails hard on
the genuinely broken case (invalid coverage). It also makes the geometry-change
event visible in the log rather than silent.

### Hard parts to work through when this is picked up
- **Per-cell state continuity under a changing extent.** If the model domain /
  active-cell set changes with a new grid, what happens to soil-moisture, snow,
  and other per-cell state in cells that enter or leave the domain? This is the
  real can of worms and goes well beyond resampling; it may be the reason to
  restrict "adapt" to geometry changes that preserve the model grid while only
  the *native* (input) grid changes.
- Defining the geometry signature precisely (nx, ny, x0, y0, x1, y1, projection
  string; tolerance on floating-point extents).
- Whether the policy should be user-selectable via a directive
  (e.g. `GRID_GEOMETRY_CHANGE_POLICY = ERROR | REMAP_OR_DIE | SILENT_ADAPT`)
  with a safe default of erroring / remap-or-die, versus hardcoded behavior.
- Interaction with the resample index-cache above: the cache's rebuild path IS
  the remap in step 2; the two features share one mechanism.

If pursued, this likely warrants its own `feature_consideration__*.md` since it
is a semantics decision, not an optimization.

---

## Build/benchmark scaffolding added during this investigation
(All reversible; no science source `.F90` changed.)
- `meson_options.txt` / `meson.build`: added gfortran build profiles
  `release_notrap` (-O2, no -ffpe-trap), `release_fast` (-O3, no -ffast-math),
  and `profile` (-pg at -O2). `-Doptimization` pinned explicitly in setup tasks
  to prevent meson option leakage between reconfigures.
- `pixi.toml`: `setup-notrap`, `setup-fast`, `setup-profile` tasks (Linux +
  win-64); `time-cs` (prints exe path/size/mtime for identity, relies on SWB2's
  built-in timer) and `profile-cs` (runs case + gprof) tasks.
- `test/integration_tests/cs/central_sands_swb2.ctl`: fixed weather filename
  templates from the old v3 names to `daymet_v4_daily_{prcp,tmax,tmin}_%y_cs.nc`
  to match the files on disk (the run was previously bailing out at the first
  missing weather file). This fix is arguably worth committing on its own.

## Executable-identity caution
Many stale `swb2` binaries exist on the dev systems. The `time-cs`/`profile-cs`
tasks print the exact binary's path, size, and mtime before running, and SWB2's
banner prints compile date + git hash. Always confirm the binary matches the
build just made before trusting a timing.

## Bottom line
The code is structurally sound; there is no single large "free" win. Keep -O2.
Drop `-ffpe-trap` from `release` for a safe ~7.5%. The resample index-cache is a
sensible, low-risk follow-up with likely modest-to-moderate payoff (verify by
measurement). The only path to a *large* speedup is parallelizing the
dependency-free per-cell loop — a bigger undertaking for a future effort.
