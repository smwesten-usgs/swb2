# Phenology Phase 2/3 — Session Notes (2026-07-10)

## What Was Accomplished

### Phase 1 Completion (model wiring)
- `model_update_phenology` and `model_initialize_phenology` added to `model_domain.F90`
- Default procedure pointers switched from `growing_season.F90` to `phenology.F90`
- `growing_season.F90` removed from build AND deleted from repository
- FAO56 growing season pointer swap removed (phenology handles all growing season logic now)

### Killing Frost Hard Latch
- `frost_killed_season` per-cell flag added to `MODEL_DOMAIN_T`
- Once frost kills the GDD-based growing season, it stays dormant until January 1 reset
- Eliminates November/December oscillation problem that was never solved in old code

### Phase 2/3: FAO56 Phenology Integration
- `phenology_update_fao56_dates` — date-based growth stages using days-since-planting
- `phenology_update_fao56_gdd` — GDD-based growth stages with frost latch
- `crop_coefficients_FAO56_interpolate_Kcb` — pure 5-case Kcb interpolator (replaces ~300 lines of old code)
- Per-landuse auto-detection: FAO56_DATES > FAO56_GDD > DOY_BASED > GDD_THRESHOLD > NONE
- `growth_stage(:)`, `stage_fraction(:)`, `growth_fraction(:)` arrays added to MODEL_DOMAIN_T
- Call order fixed in `daily_calculation.F90`: phenology before crop coefficients
- Full Central Sands FAO56 integration test runs correctly (Kcb peaks ~June 27 for May 1 planting)

### Dead Code Removal
- `src/growing_season.F90` deleted
- Old FAO56 growing season subroutines removed from `model_domain.F90`
- Old Kcb functions removed from `crop_coefficients__fao56.F90` (~300 lines)
- Public declarations cleaned up
- `crop_coefficients_FAO56_calculate_Kcb_Max` retained (still used by two-stage ET)

### Bug Fixes
- `exceptions.F90`: Array bounds overflow when >50 fatal warnings → segfault. Fixed by adding proper block-if guard around HINT_TEXT assignment.
- `-Wcompare-reals` warning in phenology.F90 (NA_FLOAT comparison)

### Integration Tests
- `test/integration_tests/cs_phenology/` — T-M mode phenology test (DOY, GDD, winter crop). All passing.
- `pixi run integration-test-phenology` task added
- Full CS FAO56 integration test runs to completion with correct behavior

### Data Migration
- `Irrigation_lookup_CDL.txt`: renamed `Planting_date` → `Growing_season_start_date`, old DOY columns → `deprecated_*`
- `Lookup__crop_coefficient_test.txt`: renamed `Planting_date` → `Growing_season_start_date`, `GDD_plant` → `Growing_season_start_GDD`

### Other
- `pixi run install` task (copies swb2.exe + swbstats2.exe to d:\bin)
- Phenology summary table written to logfile during initialization
- Legacy column name detection (fatal error when old names like `Planting_date` found)
- Design notes: test cleanup plan, incomplete weather data, leap year DOY fix, per-crop GDD base/max, PARAMS dependency injection

---

## Current Test Status

**180 tests, 4 failing** — all due to test isolation (shared global PARAMS between suites):

1. `init_method_index_doy_landuse` — expects FAO56_DATES but phenology table alone doesn't have L_* columns
2. `init_method_index_gdd_landuse` — expects FAO56_GDD but phenology table alone doesn't have GDD_ini etc.
3. `init_method_index_none_landuse` — cascade from above
4. `kcb_method_gdd_detected` — FAO56 initialize hasn't run yet when phenology tests execute first

---

## Breadcrumbs for Next Session

### Immediate: Fix the 4 failing tests

**Root cause:** The phenology test table (`phenology_test.txt`) doesn't include `L_ini`/`L_dev`/`L_mid`/`L_late` or `GDD_ini`/`GDD_dev`/`GDD_mid`/`GDD_late` columns. These only come from the crop coefficient test table. When phenology runs first (before fao56 suite loads the crop coeff table), the stage-length columns aren't in PARAMS.

**Quick fix (5 minutes):** Add the FAO56 stage length columns to `phenology_test.txt`:
```
LU_Code  ...  L_ini  L_dev  L_mid  L_late  GDD_ini  GDD_dev  GDD_mid  GDD_late
1  Corn  ...  20     50     40     30      <NA>     <NA>     <NA>     <NA>
12 Sweet Corn  ...  <NA>  <NA>  <NA>  <NA>  100  350  1000  2000
```

This makes the phenology test table fully self-contained — tests pass regardless of execution order.

**Proper fix (longer term):** Dependency injection for PARAMS (see design doc entry in `phenology_module_design.md`).

### Next priorities (after tests are green)

1. **Phenology log summary table** — verify it looks good in the logfile output
2. **Legacy column detection** — verify it fires correctly when old table is used
3. **FAO56_GDD integration test** — create a test case with GDD-based stage lengths to exercise that path end-to-end
4. **Phase 4 considerations** — continuous `growth_fraction` integration into interception, rooting depth
5. **Test cleanup** — delete `old_fruit_tests/`, rationalize regression tests

### Key files modified this session

```
src/phenology.F90                          — FAO56 methods, log table, legacy detection
src/crop_coefficients__fao56.F90           — interpolate_Kcb, dead code removed
src/model_domain.F90                       — wiring, new arrays, dead code removed
src/daily_calculation.F90                  — call order fix
src/exceptions.F90                         — bounds overflow fix
src/meson.build                            — growing_season.F90 removed
test/unit_tests/test_phenology.F90         — 37 tests (10 new FAO56)
test/unit_tests/test_fao56.F90             — rewritten Kcb tests
test/unit_tests/tester.F90                 — suite reorder
test/test_data/tables/phenology_test.txt   — aligned with crop coeff values
test/test_data/tables/Lookup__crop_coefficient_test.txt — unified column names
test/test_data/tables/Irrigation_lookup_CDL.txt — unified column names
test/test_data/tables/phenology_lookup.tsv — integration test table
test/integration_tests/cs_phenology/       — new integration test
pixi.toml                                  — install task, phenology integration test task
design/phenology_module_design.md          — multiple additions
design/plans_for_code_and_repo_improvement.md — weather data issue
design/test_cleanup_plan.md                — new
```

### Branch: `phenology_phase_1` (pushed)
