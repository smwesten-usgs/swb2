# Phenology Module Development Status

**Updated:** 2026-07-15

---

## Overall: Phases 0–3 complete. Infrastructure hardened. Ready for Phase 4.

---

## Phase 0 (Test Infrastructure) — ✅ Complete

- test-drive framework vendored, 199 tests across 11 suites
- FRUIT tests archived to `old_fruit_tests/`
- `pixi run test` runs all tests, individual suite/test selection via CLI

---

## Phase 1 (Scaffold + DOY/GDD Methods) — ✅ Complete

- `phenology.F90` created with full output contract (`growth_fraction`, `it_is_growing_season`, `growth_stage`, `stage_fraction`)
- `phenology_update_doy_based`: pure subroutine, handles winter crops (start > end)
- `phenology_update_gdd_threshold`: pure subroutine with frost hard latch
- `growing_season.F90` deleted from repo and build
- Wired into model: `model_update_phenology` replaces old pointer, called from `daily_calculation.F90`
- Per-landuse auto-detection from table contents (priority: FAO56_DATES > FAO56_GDD > DOY_BASED > GDD_THRESHOLD > NONE)
- Phenology summary table written to logfile at init

---

## Phase 2 (FAO56 Date-Based) — ✅ Complete

- `phenology_update_fao56_dates`: continuous `growth_fraction` ramp through INI/DEV/MID/LATE stages
- Year-wrap logic for winter crops
- `crop_coefficients__fao56.F90` refactored: old ~300 lines of growth-stage logic replaced by pure `crop_coefficients_FAO56_interpolate_Kcb`
- Old FAO56 growing season pointer swap eliminated
- `crop_coefficients_FAO56_interpolate_Kcb` is a pure function that takes `growth_stage` + `stage_fraction` (does NOT use `growth_fraction`)

---

## Phase 3 (FAO56 GDD-Based) — ✅ Complete

- `phenology_update_fao56_gdd`: same stage progression driven by thermal accumulation
- Frost hard latch prevents November/December season oscillation
- Full Central Sands FAO56 integration test confirms Kcb peak timing is correct
- Integration test `cs_phenology/` exercises DOY, GDD_THRESHOLD, and winter crop paths

---

## Infrastructure Hardening (this session, 2026-07-15) — ✅ Complete

- **`PARAMS_DICT` moved into `PARAMETERS_T` as instance member (`this%dict`)**
  - Each `PARAMETERS_T` instance now has its own isolated dictionary
  - Enables per-suite test isolation without global state pollution
  - Module-level `PARAMS_DICT` removed; dead imports cleaned from `model_domain.F90`, `runoff__curve_number.F90`
  - `model_initialize.F90`: local `PARAMS_LU_TABLE` replaced with global `PARAMS` (local dict would have been lost at scope exit)

- **Dependency injection for `phenology_initialize(params)` and `crop_coefficients_FAO56_initialize(params)`**
  - Both accept `type(PARAMETERS_T), intent(inout) :: params` argument
  - Production code passes global `PARAMS`; tests can pass isolated instances
  - Pattern established for incremental migration of other modules

- **Column name unification in `crop_coefficients__fao56.F90`**
  - `Planting_date` → reads `Growing_season_start_date`
  - `GDD_plant` → reads `Growing_season_start_GDD`

- **Legacy column detection** in `phenology_initialize` — fatal error with migration instructions when old names found

- **Test data consolidated**
  - `phenology_test.txt`: only phenology-unique columns (`Growing_season_end_date`, `Killing_frost_temperature`)
  - `Lookup__crop_coefficient_test.txt`: added `Growing_season_start_GDD` for Soybeans/Sunflower/Dbl Crop
  - `test_fixtures.F90`: uses global `PARAMS` directly (removed `TEST_PARAMS`)
  - All 199 tests passing, no execution-order dependency

---

## Phase 4 (Continuous `growth_fraction` Integration) — IN PROGRESS

**Goal:** Interception bucket module uses `growth_fraction` for smooth storage capacity
transitions instead of the binary `it_is_growing_season` switch.

| Step | Description | Files |
|------|-------------|-------|
| 4.1 | Define and test `growth_fraction` trajectory (19 tests) | `test_phenology.F90` ✅ |
| 4.2 | Interception (bucket): interpolate storage max with `growth_fraction` | `interception__bucket.F90` |
| 4.3 | Unit tests for interception with `growth_fraction` input | `test_interception.F90` (new) |
| 4.4 | Verify mass balance with continuous interception | integration tests |
| 4.5 | Consider: should curve number also use `growth_fraction`? (probably keep binary) | decision |

**Notes:**
- Rooting depth and plant height STAY keyed to Kcb (FAO-56 methodology). They already
  get a smooth signal from the Kcb curve itself, and this approach works regardless of
  how Kcb is derived (date-based, GDD-based, or future NDVI/LAI-based).
- `growth_fraction` is consumed ONLY by interception bucket (for now) — it needs a
  structural signal for canopy capacity that is independent of transpiration rate.
- `it_is_growing_season` can potentially be removed as stored model state — derivable
  from `growth_fraction > 0.0`
- Curve number stays binary (CN tables are defined for growing/dormant, not intermediate states)
- For DOY_BASED / GDD_THRESHOLD users (no FAO56 crop coefficients), the interception
  formula `nongrowing + growth_fraction * (growing - nongrowing)` produces identical
  results to the current binary switch (since growth_fraction is 0 or 1).
- When touching `interception__bucket.F90`, add `params` argument to its initialize routine and write unit tests

---

## Deferred Items (documented, not blocking)

| Item | Status | Notes |
|------|--------|-------|
| Leap year DOY correction | Deferred | 1-day shift after Feb 28 in leap years for mm/dd inputs. Store month+day and recompute DOY per year. |
| Per-crop GDD base/max temperatures | Implemented, untested | Already in `growing_degree_day.F90` and `growing_degree_day_baskerville_emin.F90`. Needs systematic unit tests to verify per-landuse values are read and applied correctly. |
| PARAMS dependency injection (remaining 20 modules) | Incremental | Pattern established. Migrate each module when adding unit tests for it. |
| `detect_legacy_column_names` verification | Low priority | Manually confirm it fires correctly with an old-style table. |
| FAO56_GDD dedicated integration test | Nice to have | CS FAO56 test exercises this path, but a focused test would be cleaner. |
| Delete `old_fruit_tests/` | Cleanup | Safe to remove once confident in test-drive coverage. |
| Phase 5 (Gridded LAI) | Future | See `phenology_implementation_plan.md` |

---

## Key Architecture Notes

### How Kcb Uses Phenology

`crop_coefficients_FAO56_interpolate_Kcb` does NOT use `growth_fraction`. It uses:
- `growth_stage` (enum: DORMANT/INI/DEV/MID/LATE) — selects the Kcb formula
- `stage_fraction` (0–1 position within current stage) — interpolates within stage

### What Uses Kcb Directly (unchanged)

- **Rooting depth** (`rooting_depth__FAO56.F90`): scales Zr using `(Kcb - Kcb_min) / (Kcb_max - Kcb_min)` — a smooth signal derived from the Kcb curve itself
- **Plant height** (`actual_et__fao56__two_stage.F90`): scales h using `(Kcb - Kcb_min) / (Kcb_mid - Kcb_min)`

These stay keyed to Kcb because: (a) the FAO-56 methodology defines them this way, (b) Kcb already provides a smooth progression regardless of derivation method (dates, GDD, or future NDVI/LAI), and (c) the Kcb-based proxy correctly handles late-season decline for rooting depth purposes (roots don't retract, but the `max(Zr_i, ...)` ratchet in the code already prevents that).

### What Uses `growth_fraction` (new)

- **Interception capacity** (`interception__bucket.F90`): needs a structural signal for canopy presence that is independent of transpiration rate. `growth_fraction` interpolates between nongrowing and growing storage maxima.

### growth_fraction Trajectory (FAO56 methods)

```
DORMANT:  0.0
INI:      0.0 → 0.1  (stage_fraction * 0.1)
DEV:      0.1 → 1.0  (0.1 + stage_fraction * 0.9)
MID:      1.0
LATE:     1.0         (structure remains intact during senescence)
DORMANT:  0.0         (abrupt drop: harvest / leaf-off / frost kill)
```

`growth_fraction` represents structural development — how much of the plant's
physical form (canopy, root system) is in place. Once fully grown, it stays at 1.0
until the season ends. It does NOT decline during senescence; physiological decline
is handled separately by the Kcb curve (via `growth_stage` + `stage_fraction`).

For DOY_BASED and GDD_THRESHOLD: strictly binary (0.0 or 1.0).

### `it_is_growing_season` — Can It Be Removed?

Potentially yes as stored state. It's derivable: `growth_fraction > 0.0`. Currently used by:
- Curve number (binary CN selection) — could test `growth_fraction > 0.0` directly
- Interception (binary switch) — being replaced by continuous `growth_fraction` in Phase 4
- NetCDF output variable — derive at output time
- Rooting depth / plant height — indirectly (they key off Kcb, which is only computed when growing)

Recommend: remove from `MODEL_DOMAIN_T` after Phase 4 interception change is validated. Derive locally where needed.
