# swbstats2: Retire, Replace, or Overhaul?

**Date:** September 11, 2026
**Status:** Design analysis — decision pending
**Related:** `swb2_netcdf_cf_compliance.md` (Sections 4a/4b — the CF + GDAL task cascade
that prompted this), `project_scoping_and_design.md` (Section 6 — gridded uncertainty
post-processing; Section 7 — Option A/B and the mosaic workflow)
**Prompt:** "A heretical thought: let swbstats2 die. Rely more on xarray. Perhaps a
Rust-based replacement? Or overhaul swbstats2." Not heretical — it is the right question
to ask *before* sinking more Fortran CF-compliance work into a tool we might retire.

---

## 1. What swbstats2 Actually Does (inventory, source-verified)

Enumerated from `swbstats2.F90` (CLI) and `swbstats2_support.F90` (implementation),
read 2026-09-11. This is the surface any replacement must cover (or consciously drop):

| Capability | CLI flag(s) | Notes |
|-----------|-------------|-------|
| Temporal statistics | `--annual_statistics`, `--monthly_statistics`, `--daily_statistics` | mean, sum, min, max, variance over the period |
| Statistic set | (implicit) | `STATS_MEAN … STATS_VARIANCE` — mean/sum/min/max/variance |
| Sum handling | `--annualize_sums`, `--slice=` | annualize accumulated fluxes; extract a time slice |
| Unit conversion | `--report_in_meters` | inch/cm/mm → meters (from the variable `units`) |
| Volume output | `--report_as_volume` | multiplies by cell area; **scrapes the PROJ4 `+units=` for the length factor** (the fragile bit, see CF doc 4a) |
| Stress-period aggregation | `--stress_period_file=` | aggregate to MODFLOW stress periods |
| **Zonal statistics** | `--zone_grid=`, `--zone_period_file=` | per-zone stats over an integer zone grid |
| **Multi-zone statistics** | `--zone_grid=` + secondary | cross two zone grids |
| **Grid comparison** | `--comparison_grid=`, `--comparison_scale_factor=`, `--comparison_period_file=` | compare output against another grid |
| Output formats | `--{no_}netcdf_output`, `--{no_}arcgrid_output` | NetCDF and/or ESRI ASCII Arc Grid |

**Takeaway:** it is more than "compute a mean raster." The genuinely non-trivial pieces
are **zonal / multi-zonal statistics**, **grid comparison**, **stress-period
aggregation**, and **Arc Grid output**. A replacement that only does temporal
mean/sum grids would silently drop real functionality.

---

## 2. The Four Options

### Option 1 — Let it die; do everything in xarray
Post-process SWB2's raw per-variable NetCDFs directly with `xarray` (+ `rioxarray`,
`flox` for fast grouped/zonal reductions, `dask` for out-of-core).

| Pros | Cons |
|------|------|
| Temporal stats (`.resample`/`.groupby("time").mean/sum`) are one-liners | Zonal stats need `flox`/`xarray-groupby` over a zone raster — doable but you rebuild `calc_zonal_stats`/`calc_multizonal_stats` logic |
| Zonal reductions via `flox` are fast and idiomatic | Stress-period aggregation = custom binning; must re-implement |
| CRS/units handled by `rioxarray`/`pint` once SWB2 writes clean CF (the CF doc work) | Arc Grid export via `rioxarray.to_raster` — easy, but someone must want it |
| No Fortran build coupling; ships with the analysis stack we already use (`py313`) | Volume/area math must be reproduced (trivial with a proper CF grid) |
| Directly enables the **mosaic** workflow (Option A/B) in the same tool | A pile of small scripts can sprawl without discipline (tests, a thin package) |
| Uncertainty post-processing (per-cell mean/σ across the PEST++ ensemble) is native xarray | Loses a single self-contained redistributable binary |

**Fit here:** very high. The VA project *already* mandates xarray-based mosaicking and
ensemble uncertainty rasters. Most of what we need from swbstats2 (temporal stats,
volume, unit conversion, ensemble stats) is a thin xarray layer **once SWB2 emits clean
CF** — which is the same CF work already scoped. The CF fixes are the enabler either way.

### Option 2 — Rust-based replacement
A compiled `swbstats2`-like tool in Rust (`netcdf` crate, `ndarray`).

| Pros | Cons |
|------|------|
| Single fast static binary; no Python env at runtime | The post-processing is **I/O-bound** (NetCDF deflate + disk), so Rust buys little over xarray+dask — same argument as the mosaic-compositor analysis in the CF doc |
| Memory-safe, good for a long-lived operational tool | Rust geospatial ecosystem (zonal stats, CRS, Arc Grid) is far thinner than Python's; you reimplement a lot |
| Attractive if a dependency-free deliverable is required | New toolchain/skills + build/CI surface for the team |

**Fit here:** low for *this* project. Justified only if a self-contained,
Python-free operational binary is a hard requirement (it is not stated to be), or if a
specific step proves CPU-bound under profiling (unlikely for reductions). Keep in reserve.

### Option 3 — Overhaul swbstats2 in place (Fortran)
Keep it, fix the CF output (doc 4b), fix the PROJ4 unit-scrape, add `cell_methods`,
modernize per the SWB2 F2018/test-drive/CI plan.

| Pros | Cons |
|------|------|
| Preserves all existing features and user muscle memory | Invests more Fortran effort into post-processing, the part least suited to Fortran |
| Stays inside the existing build; no new stack | Every new stat/format is a Fortran change; slow iteration vs xarray |
| Lowest disruption | Does not advance the mosaic/ensemble workflows the project actually needs |

**Fit here:** medium. It is the "do nothing structural" path — safe, but it keeps
scientific post-processing in the language where it is most expensive to evolve.

### Option 4 — Hybrid (recommended): freeze swbstats2, grow a tested xarray package
Stop investing in swbstats2 beyond the minimum, and build a small, **tested** Python
package (`py313`) that owns post-processing going forward.

- Keep swbstats2 running as-is for now (do **not** delete). Apply only the *cheap* CF
  output fixes it inherits automatically from the shared writer, so its output is not a
  CF liability in the interim.
- Build `swb_postprocess/` (or similar): temporal stats, volume/units, stress-period
  aggregation, zonal/multizonal stats (via `flox`), grid comparison, Arc Grid export,
  **plus** the two things swbstats2 never did and the project needs: **multi-model NetCDF
  mosaicking** (Option A/B) and **PEST++ ensemble → per-cell uncertainty rasters**
  (design Section 6).
- Migrate feature-by-feature with parity tests (Section 3) against swbstats2 output.
  When parity holds and the mosaic/ensemble features land, swbstats2 becomes optional and
  can be retired without a flag-day.

---

## 3. How This Interacts with the CF / GDAL Cascade (the key linkage)

The CF doc (4b) lists swbstats2 tasks: inherit writer fixes, add `cell_methods`, fix the
PROJ4 unit-scrape, re-validate output. **The disposition choice reprices those tasks:**

- If we go **Option 1/4 (xarray owns post-processing)**, several swbstats2 tasks
  **evaporate**: no need to add `cell_methods` logic to swbstats2's writer or to
  carefully fix its volume math — xarray does volume/units/aggregation from clean CF. We
  still need swb2's *writer* CF fixes (they feed xarray), but swbstats2's own output path
  stops being something we extend.
- The **PROJ4 unit-scrape fix** is still worth doing *only* as long as swbstats2 is in
  the loop; under Option 4 it can be a minimal stopgap rather than a proper fix, because
  xarray reads the CRS from `crs_wkt`/CF attrs, not from a scraped PROJ4 string.
- The **GDAL migration** (CF doc 4a.3) is about swb2's *core* transforms
  (`grid.F90`, `data_catalog`), which are **independent of** the swbstats2 decision.
  Retiring swbstats2 does not remove the GDAL question, but it does remove swbstats2 from
  the list of things a GDAL migration would have to touch.

**Net linkage:** choosing xarray-forward (Option 4) *shrinks* the Fortran CF/GDAL task
cascade — it deletes the "extend swbstats2's CF output" work and downgrades the
unit-scrape fix to a stopgap. That is a point in favor of Option 4.

---

## 4. Recommendation

**Option 4 (freeze + grow a tested xarray package), with Rust held in reserve.**

Rationale:
1. Post-processing is I/O-bound and iterative — Python/xarray is the right tool; Rust's
   speed advantage is largely illusory here (same conclusion as the mosaic-compositor
   analysis).
2. The project's *new* needs — multi-model mosaicking and ensemble uncertainty rasters —
   are not in swbstats2 at all and are natural in xarray. Building them there means the
   new tool subsumes the old one rather than duplicating it.
3. It *reduces* the Fortran CF/GDAL cascade (Section 3) rather than adding to it.
4. It avoids a flag-day: swbstats2 keeps working until parity + new features justify
   retiring it.
5. It honors the project's stated stack (`py313`, xarray) and coding expectations.

Guardrails so "rely on xarray" does not become "a pile of nasty scripts":
- One installable package with Google-style docstrings, typing, and tests (per
  `expectations_for_kiro.md`).
- **Parity tests** against swbstats2 for every migrated feature (numeric equality within
  tolerance on shared fixtures) — this is the objective evidence that retiring is safe.
- Reuse the CF test harness (CF doc Section 6): the package's outputs must pass
  `cfchecker` too.

Revisit Rust only if a dependency-free operational binary becomes a hard requirement or
profiling shows a genuine CPU bottleneck.

---

## 5. Suggested Sequencing

1. **swb2 writer CF fixes** (CF doc 3–4) — enabler for everything; do first.
2. **Stand up `swb_postprocess/`** with the highest-value, easy pieces first: temporal
   stats + volume/units + ensemble per-cell mean/σ, each with parity tests vs swbstats2.
3. **Add mosaicking** (Option A/B) — the feature swbstats2 never had; de-risks the
   architecture decision (the two-tile experiment from the breadcrumbs lives here).
4. **Migrate zonal / multizonal / comparison / stress-period** with parity tests.
5. **Downgrade swbstats2 to maintenance** — apply only inherited CF fixes; skip the
   deeper swbstats2-specific CF/unit work the CF doc listed, since xarray now owns it.
6. **Retire swbstats2** once parity holds and the new features are in use — no flag-day.
7. **GDAL/PROJ migration** for swb2 core remains a *separate* decision (CF doc 4a),
   unaffected by this one except that swbstats2 drops off its task list.

---

## 6. Provenance

swbstats2 capability inventory read from `swbstats2.F90` (usage/CLI block, ~line 82–164;
argument parsing ~line 171+) and `swbstats2_support.F90` (`calc_zonal_stats` ~line 319,
`calc_multizonal_stats` ~line 389, `initialize_comparison_grid` ~line 708, Arc Grid
writers, `close_output_netcdf_files` ~line 1740+), on 2026-09-11. No source modified.
