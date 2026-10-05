# Performance investigation — resume notes (continue on Hovenweep / Linux)

**Date started:** 2026-10-05 (Windows session)
**Reason to move to Hovenweep:** gprof on the Windows/MinGW gfortran toolchain
records call counts but **no time samples** (`no time propagated`, empty flat
profile) because the MinGW runtime does not deliver the `SIGPROF` profiling
timer interrupt. Linux delivers it, so gprof produces a full flat profile +
call graph there. All scaffolding below is already in place and portable.

---

## The benchmark

- Case: `test/integration_tests/cs` (Central Sands, WI), control file
  `central_sands_swb2.ctl`.
- Size: **138,400 active cells** (full 400 x 346 grid), **2 years daily**
  (2012-01-01 .. 2013-12-31).
- `FLOW_ROUTING_METHOD NONE` in the .ctl. Consequence: `sort_order` is the
  identity sequence (`model_domain.F90:607`), so the per-cell loop in
  `daily_calculation.F90` has **no inter-cell data dependency** — each cell's
  daily computation is independent. The per-cell `index_method` calls
  (`calc_runoff`, `calc_irrigation`, `calc_actual_et`,
  `calc_direct_net_infiltration`, `calc_maximum_net_infiltration`,
  `calc_direct_soil_moisture`, `storm_drain_capture_calculate`, and a cheap
  `calc_routing` -> `_none`) are ALL still active; only D8 ordering/`runon`
  coupling is inert.
- ~98.5% of wall-clock is the daily loop (init ~5-10 s, run ~4-6 min), i.e.
  **compute-bound, not I/O-bound**.

## Attributed timing results (warm cache, Windows, gfortran 15.2)

| Build | Flags | Run time | Total |
|-------|-------|----------|-------|
| release (baseline) | -O2, no -ffast-math, FPE traps | 3:56 (236 s) | 4:02 |
| release_fast | -O3 -funroll-loops, no -ffast-math, FPE traps | 4:36 (276 s) | 4:42 |

**Conclusion so far: -O3 is ~17% SLOWER than -O2 here** (confirmed, 40 s swing,
not jitter). Likely cause: inlining/unrolling code bloat + poor vectorization on
a branchy, procedure-pointer-heavy per-cell loop. **No quick win from compiler
flags; -O2 is the right setting.** The -O3 binary is also notably larger
(~7.6 MB vs ~6 MB), corroborating code bloat.
NOTE: an earlier 5:56 cold-cache run is NOT trustworthy for attribution and is
not used in any conclusion.

## Executable-identity hazard (important)

Several stale `swb2`/`swb2.exe` builds exist on the Windows system (bin/, dist/,
build/meson/, builddir/). The `time-cs` and `profile-cs` tasks now print the
exact exe path + size + mtime BEFORE running, and SWB2's banner prints its
compile date + git hash/branch. Always confirm the printed binary matches the
build you just made. Replicate this discipline on Hovenweep.

---

## Build/run scaffolding already added (portable; no .F90 source changed)

- `meson_options.txt`: `profile` option gained `release_fast` and `profile`
  choices (alongside `release`, `develop`, `static_analysis`).
- `meson.build` (gfortran branch):
  - `release_fast`: `-funroll-loops -ffpe-trap=overflow,zero -fbacktrace`
    (the -O3 level comes from `-Doptimization=3` in the setup task; NO -ffast-math).
  - `profile`: `-pg -ffpe-trap=overflow,zero -fbacktrace`, and `-pg` added to
    `link_args`. Profiled at -O2 so the hot-spot breakdown reflects realistic
    optimized code.
- `pixi.toml` generic `[tasks]` (used on linux-64):
  - `setup` -> release -O2; `setup-fast` -> release_fast -O3;
    `setup-profile` -> profile -pg at -O2; plus `build`.
  - `time-cs` -> runs cs case, prints exe identity, SWB2 self-times.
  - `profile-cs` -> runs cs case, then `gprof builddir/src/swb2 gmon.out >
    gprof_report.txt` and prints the top of the flat profile.
- `central_sands_swb2.ctl`: weather filename templates fixed from the old v3
  names to `daymet_v4_daily_{prcp,tmax,tmin}_%y_cs.nc` (3 lines) to match the
  files actually on disk. (Same filenames expected on Hovenweep.)

---

## EXACT NEXT STEPS on Hovenweep (Linux)

```sh
cd <repo>
pixi run setup-profile      # gfortran -pg at -O2
pixi run build
pixi run profile-cs         # runs cs case, writes + prints gprof_report.txt
```

Then read `test/integration_tests/cs/gprof_report.txt` — on Linux the flat
profile WILL be populated with self-time per function. Rank the top functions.

### Hypotheses to test against the profile (candidates for the hot spot)
1. **Per-cell procedure-pointer indirection** defeats inlining/vectorization.
   Because routing is NONE, the loop is dependency-free, so in principle the
   per-cell `index_method` work could be re-expressed as whole-array
   `array_method` passes (vectorizable, no indirect call per cell per day).
   This is the most promising structural lever IF indirection dominates.
2. **FAO-56 two-stage actual ET** (`actual_et__fao56__two_stage.F90`) — heavy
   per-cell arithmetic.
3. **Curve-number branching** (`update_curve_number_fn` in
   `runoff__curve_number.F90`) — data-dependent branches per cell per day.
4. **Mixed c_float/c_double conversions** in the daily loop expressions
   (state arrays mix precisions; conversions can block vectorization).

### Decision rule (user's "is it worth it?" bar)
- If the profile shows time is dominated by (1) indirection/loop structure ->
  a whole-array fast path for the no-routing case is a candidate real win
  (possibly multiplicative). Worth a prototype.
- If time is dominated by (2)/(3) irreducible arithmetic -> little cheap upside;
  likely NOT worth a major refactor to shave a bit off a minutes-long run.

### Also useful
- Baseline for comparison on Hovenweep: `pixi run setup && pixi run build &&
  pixi run time-cs` to get the Linux -O2 run time (Linux/CPU differ from the
  Windows numbers above; re-establish the baseline there).
- `gmon.out` call COUNTS are valid even on Windows; on Linux you get counts +
  time together.

## Open housekeeping (optional)
- The `release_fast`/`profile` meson profiles and the fast/profile pixi tasks
  are experiment scaffolding. Keep for now; can be pruned later if undesired.
- Consider whether the cs .ctl filename-template fix should be committed (it was
  a genuine data/.ctl drift, not just a benchmarking convenience).
