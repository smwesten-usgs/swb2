# Phenology Phase 1 — Session Notes (2026-07-09)

## What Was Accomplished

### Repository Housekeeping (committed to main)
- Removed `docs/` (2,200+ files) from git tracking; deployed via GitHub Actions instead
- Added `.gitattributes` for cross-platform line endings
- Added `.github/workflows/docs.yml` (Doxygen → GitHub Pages, auto-deploys on push to main)
- Added `linux-64` platform to `pixi.toml`
- Set `core.autocrlf = false` globally
- Updated `plans_for_code_and_repo_improvement.md` and `changelog.md`

### Phenology Module (on `phenology_phase_1` branch)
- Created `src/phenology.F90` — new unified phenology provider
- Created `test/unit_tests/test_phenology.F90` — 27 passing tests
- Created `test/test_data/tables/phenology_test.txt` — test lookup table
- Registered in build system (`src/meson.build`, `test/unit_tests/meson.build`, `tester.F90`)

---

## Design Decisions Made

### Clean Break from `growing_season.F90`
- No backward compatibility / fallback path
- `growing_season.F90` will be deleted (not deprecated)
- Legacy parameter names (`First_day_of_growing_season`, `Planting_date`, `GDD_plant`) are not supported

### Unified Parameter Names
| Column | Type | Used by |
|---|---|---|
| `Growing_season_start_date` | mm/dd or integer DOY | DOY_BASED, FAO56_DATES (Phase 2) |
| `Growing_season_end_date` | mm/dd or integer DOY | DOY_BASED only |
| `Growing_season_start_GDD` | float (degree-days) | GDD_THRESHOLD, FAO56_GDD (Phase 3) |
| `Killing_frost_temperature` | float (temperature) | GDD_THRESHOLD, FAO56_GDD (Phase 3) |

**Rationale:** `Growing_season_start_date` replaces both `First_day_of_growing_season` and `Planting_date` — they are physically the same concept.

### Per-Landuse Method Selection (not global)
- Method determined from table contents at initialization, not from a control file directive
- `PHENOLOGY_METHOD_INDEX(:)` array records which method each landuse uses
- Rule: if DOY columns have data → DOY_BASED; if GDD columns have data → GDD_THRESHOLD; neither → PHENOLOGY_NONE
- This allows mixed-method domains (forests=DOY, row crops=GDD) in a single table

### Architecture: Pure Logic + Loop Dispatcher
- `phenology_update_doy_based` — **pure** subroutine, no module state access
- `phenology_update_gdd_threshold` — **pure** subroutine, takes `it_is_growing_season_in` for statefulness
- `phenology_update` — dispatcher, reads `PHENOLOGY_METHOD_INDEX(landuse_index)`, calls the appropriate pure subroutine via `select case`
- Model integration: `model_update_phenology` wraps a loop over all cells, calling `phenology_update` per cell
- **Not elemental** — the `select case` dispatch prevents vectorization anyway, and performance is negligible for this logic (the real work is ET/mass balance)

### Output Contract
Per cell, per day:
- `growth_fraction` (c_float) — 0.0 or 1.0 for Phase 1 methods; continuous in Phases 2-3
- `it_is_growing_season` (c_bool) — derived from growth_fraction > 0
- `growth_stage` (c_int) — DORMANT or MID for Phase 1; INI/DEV/MID/LATE in later phases

### PARAMS Infrastructure Findings
- Dictionary lookup is **case-insensitive** (`.strapprox.` operator)
- `get_parameters(sKey=..., fValues=...)` returns `NA_FLOAT` for per-row `<NA>` entries ✅
- `get_parameters(sKey=..., slValues=...)` returns full string list including literal `"<NA>"` strings ✅
- Multiple tables with the **same LU_Code column** merge cleanly (new columns added alongside existing)
- Tables with **different LU_Code counts or values** cause fatal warnings — test tables must share the same LU_Code set

---

## Current State of `phenology.F90`

```
Module-level arrays (populated by phenology_initialize):
  GROWING_SEASON_START_DOY(:)   — integer, -9999 for NODATA
  GROWING_SEASON_END_DOY(:)     — integer, -9999 for NODATA
  GROWING_SEASON_START_GDD(:)   — float, NA_FLOAT for NODATA
  KILLING_FROST_TEMP(:)         — float, NA_FLOAT for NODATA
  PHENOLOGY_METHOD_INDEX(:)     — integer enum per landuse

Public subroutines:
  phenology_initialize()               — reads PARAMS, populates arrays
  phenology_update(landuse_index, ...) — dispatches to correct method
  phenology_update_doy_based(...)      — pure, handles winter crop wrap
  phenology_update_gdd_threshold(...)  — pure, stateful (needs previous state)

Constants:
  GROWTH_STAGE_DORMANT=0, _INI=1, _DEV=2, _MID=3, _LATE=4
  PHENOLOGY_NONE=0, PHENOLOGY_DOY_BASED=1, PHENOLOGY_GDD_THRESHOLD=2,
  PHENOLOGY_FAO56_DATES=3, PHENOLOGY_FAO56_GDD=4
```

---

## Next Steps (Integration into Model)

### Immediate (Step B/C — wiring)
1. Add `model_update_phenology(this)` subroutine to `model_domain.F90`:
   - Matches `array_method` interface (takes `class(MODEL_DOMAIN_T)`)
   - Loops over cells, calls `phenology_update` per cell
   - For now, `growth_fraction` and `growth_stage` are local (discarded) — only `it_is_growing_season` is written back to `this%it_is_growing_season(indx)`
2. Change pointer default: `this%update_growing_season => model_update_phenology`
3. Change `model_initialize.F90` line 303: call `phenology_initialize()` instead of `MODEL%initialize_growing_season()`
4. Handle the FAO56 pointer swap at line 1267 (temporarily keep it, or disable it)
5. Remove `growing_season.F90` from `src/meson.build`

### Before removing `growing_season.F90`
- The FAO56 crop coefficient code currently swaps the growing season pointer to its own version (`model_update_growing_season_crop_coefficient_FAO56`). This needs to be addressed:
  - Option A: disable the swap, require FAO56 users to use `PHENOLOGY_METHOD FAO56_DATES` (Phase 2)
  - Option B: keep the swap temporarily, delete `growing_season.F90` but leave the FAO56 variant
  - **Recommendation:** Option B for now — the FAO56 path still works, we just remove the DOY/GDD legacy path

### After Integration Works
- Add `growth_fraction(:)` and `growth_stage(:)` arrays to `MODEL_DOMAIN_T`
- Write them in `model_update_phenology`
- Downstream consumers (interception, rooting depth) can start using them (Phase 4)

### Phase 2 (FAO56_DATES)
- Add `phenology_update_fao56_dates` — reads `Growing_season_start_date` + `L_ini/L_dev/L_mid/L_late`
- Produces continuous `growth_fraction` (0→1 ramp over DEV stage)
- Extract growth-stage-date logic from `crop_coefficients__fao56.F90`
- `crop_coefficients__fao56.F90` becomes a pure Kcb interpolator

### Phase 3 (FAO56_GDD)
- Same as Phase 2 but driven by thermal accumulation
- `Growing_season_start_GDD` + `GDD_ini/GDD_dev/GDD_mid/GDD_late`

---

## Files Modified This Session

### On `main` (pushed):
- `.gitattributes` (new)
- `.gitignore` (added docs/)
- `.github/workflows/docs.yml` (new)
- `pixi.toml` (linux-64 platform, linux docs task)
- `design/changelog.md`
- `design/plans_for_code_and_repo_improvement.md`
- `design/phenology_module_design.md` (unified param names)
- `design/phenology_implementation_plan.md` (clean break, unified names)

### On `phenology_phase_1` (pushed):
- `src/phenology.F90` (new)
- `src/meson.build` (added phenology.F90)
- `test/unit_tests/test_phenology.F90` (new, 27 tests)
- `test/unit_tests/meson.build` (added test_phenology.F90)
- `test/unit_tests/tester.F90` (added phenology suite)
- `test/test_data/tables/phenology_test.txt` (new)

---

## Key Reference: Model Wiring Points

```
model_domain.F90:191   — procedure pointer declaration (array_method)
model_domain.F90:366   — default assignment (=> model_update_growing_season)
model_domain.F90:1267  — FAO56 swap (=> model_update_growing_season_crop_coefficient_FAO56)
model_domain.F90:2562  — model_update_growing_season_crop_coefficient_FAO56 impl
model_domain.F90:2589  — model_update_growing_season impl (calls growing_season_update)
model_initialize.F90:303 — call MODEL%initialize_growing_season()
daily_calculation.F90:41 — call cells%update_growing_season()
```
