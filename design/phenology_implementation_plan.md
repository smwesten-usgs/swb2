# Phenology Module: Phased Implementation Plan

**Date:** July 2026
**Companion doc:** `design/phenology_module_design.md`
**Guiding principle:** Explicit over auto-detected. Users declare what they want; the model does exactly that.

---

## Sequencing Overview

```
Phase 0: Test infrastructure (test-drive migration)
    │
    ▼
Phase 1: Phenology provider — scaffold and DOY/GDD methods
    │
    ▼
Phase 2: Migrate FAO56 date-based growth stages into phenology provider
    │
    ▼
Phase 3: GDD-based crop coefficient curves (FAO56_GDD phenology method)
    │
    ▼
Phase 4: Continuous growth_fraction integration (interception, rooting depth, etc.)
    │
    ▼
Phase 5: Gridded LAI / LAI-based Kcb (future)
```

Each phase produces a working, testable build. No phase leaves the code in a broken intermediate state.

---

## Phase 0: Test Infrastructure (test-drive Migration)

**Status: ✅ COMPLETE (2026-07-08)**

**Rationale:** Before making structural changes, establish a reliable automated test harness. The existing FRUIT tests are manually registered, lack individual-test execution, and use a discontinued framework. Converting to test-drive gives us:
- Individual test execution (can run one test in isolation)
- Meson-native test registration (`meson test` works out of the box)
- Better failure diagnostics (error messages propagate cleanly)
- A clean pattern for writing new phenology tests alongside the implementation

**What was done:**
- Vendored `testdrive.F90` directly (simpler than subproject; avoids DLL issues on Windows)
- Created `test_fixtures.F90` with tiered environment setup (DOY → FAO56_DATES → FAO56_GDD)
- Converted all 6 FRUIT test modules + added 4 new test modules (10 suites, 144 tests)
- Old FRUIT files moved to `test/unit_tests/old_fruit_tests/` (can be deleted once confident)
- Driver supports suite/test selection via CLI, ANSI-colored output, test count summary

**Definition of done:** ✅ `pixi run test` runs all tests, each can be run individually, all pass.

---

## Phase 1: Phenology Provider — Scaffold and Simple Methods

**Goal:** Create `phenology.F90` with the output contract and implement the two simplest methods (DOY_BASED, GDD_THRESHOLD). Wire it into the model as an alternative to `growing_season.F90`. Both code paths work; user selects via control file.

**New control file directive:**
```
PHENOLOGY_METHOD  DOY_BASED
```
or
```
PHENOLOGY_METHOD  GDD_THRESHOLD
```

`PHENOLOGY_METHOD` is **required**. If absent, the model emits a fatal error directing the user to add one. There is no fallback to `growing_season.F90` — that module is removed entirely.

**Unified parameter names (lookup table columns):**

| Column | Type | Method |
|---|---|---|
| `Growing_season_start_date` | mm/dd or integer DOY | DOY_BASED |
| `Growing_season_end_date` | mm/dd or integer DOY | DOY_BASED |
| `Growing_season_start_GDD` | float (degree-days) | GDD_THRESHOLD |
| `Killing_frost_temperature` | float (temperature) | GDD_THRESHOLD |

`Growing_season_start_date` accepts either `mm/dd` format (e.g., `03/17`, `4-01`) or integer day-of-year (e.g., `91`).

**Scope:**

| Step | Description | Files affected |
|------|-------------|---------------|
| 1.1 | Create `src/phenology.F90` with module skeleton: output type, public interface, method enum | `src/phenology.F90` (new) |
| 1.2 | Implement `phenology_initialize_doy_based()` — reads `Growing_season_start_date`, `Growing_season_end_date` from lookup table | `src/phenology.F90` |
| 1.3 | Implement `phenology_update_doy_based()` — sets growth_fraction (0 or 1), it_is_growing_season, growth_stage (DORMANT or MID). Handles winter-crop case (start DOY > end DOY). | `src/phenology.F90` |
| 1.4 | Implement `phenology_initialize_gdd_threshold()` — reads `Growing_season_start_GDD`, `Killing_frost_temperature` | `src/phenology.F90` |
| 1.5 | Implement `phenology_update_gdd_threshold()` — same output contract. GDD accumulates from DOY 1; growing season starts when GDD ≥ threshold; ends on killing frost. | `src/phenology.F90` |
| 1.6 | Add `PHENOLOGY_METHOD` directive parsing in `model_domain.F90` method-selection block | `src/model_domain.F90` |
| 1.7 | Replace procedure pointer `this%update_growing_season` with `this%update_phenology` | `src/model_domain.F90` |
| 1.8 | Wire new pointer into daily calculation (call `update_phenology`) | `src/daily_calculation.F90` |
| 1.9 | Remove `growing_season.F90` from source and build | `src/growing_season.F90` (delete), `src/meson.build` |
| 1.10 | Write unit tests for `phenology_update_doy_based` and `phenology_update_gdd_threshold` | `test/unit_tests/test_phenology.F90` (new) |
| 1.11 | Run existing FAO-56 tests — they still pass (crop coeff module unchanged in this phase) | — |

**Key design choice (clean break, no legacy support):** `growing_season.F90` is deleted. The control file must specify `PHENOLOGY_METHOD`. If absent, the model errors with: `"PHENOLOGY_METHOD is required. Valid options: DOY_BASED, GDD_THRESHOLD, FAO56_DATES, FAO56_GDD."` Legacy parameter names (`First_day_of_growing_season`, `Planting_date`, `GDD_plant`, etc.) are not recognized — users must update their lookup tables to use the unified names.

**Definition of done:** New phenology module produces correct `growth_fraction`, `it_is_growing_season`, and `growth_stage` for DOY and GDD cases. `growing_season.F90` is removed. New tests pass. Existing FAO-56 crop coefficient tests still pass (that module is untouched in this phase).

---

## Phase 2: FAO56 Date-Based Growth Stages → Phenology Provider

**Goal:** Extract the growth-stage-date logic from `crop_coefficients__fao56.F90` into the phenology provider. The crop coefficient module becomes a pure Kcb interpolator that receives growth_stage as input.

**New control file directive:**
```
PHENOLOGY_METHOD  FAO56_DATES
```

This replaces the implicit coupling where `CROP_COEFFICIENT FAO56` would override the growing season mechanism.

**Scope:**

| Step | Description | Files affected |
|------|-------------|---------------|
| 2.1 | Implement `phenology_initialize_fao56_dates()` — reads `Growing_season_start_date`, `L_ini`, `L_dev`, `L_mid`, `L_late`; computes GROWTH_STAGE_DATE array. Growing season end is implicit (start + sum of stage lengths). | `src/phenology.F90` |
| 2.2 | Implement `phenology_update_fao56_dates()` — determines current growth_stage from date comparison; computes growth_fraction; sets it_is_growing_season | `src/phenology.F90` |
| 2.3 | Implement `phenology_advance_planting_year()` — the year-rollover logic currently in `crop_coefficients_FAO56_update_growth_stage_dates` | `src/phenology.F90` |
| 2.4 | Refactor `crop_coefficients__fao56.F90`: remove `GROWTH_STAGE_DATE` ownership and stage-date calculation; receive growth_stage from phenology provider instead | `src/crop_coefficients__fao56.F90` |
| 2.5 | Refactor `crop_coefficients_FAO56_calculate()` to accept growth_stage + stage boundaries as arguments rather than computing them internally | `src/crop_coefficients__fao56.F90` |
| 2.6 | Remove the procedure pointer swap (`update_growing_season => model_update_growing_season_crop_coefficient_FAO56`) | `src/model_domain.F90` |
| 2.7 | Remove `crop_coefficients_FAO56_update_growing_season` subroutine (its job is now done by phenology provider) | `src/crop_coefficients__fao56.F90` |
| 2.8 | Update `model_domain.F90` method-selection: `CROP_COEFFICIENT FAO56` no longer touches the growing season pointer. Add validation: if `CROP_COEFFICIENT FAO56` is specified without `PHENOLOGY_METHOD FAO56_DATES` or `FAO56_GDD`, emit a clear error: "CROP_COEFFICIENT FAO56 requires PHENOLOGY_METHOD FAO56_DATES or FAO56_GDD" | `src/model_domain.F90` |
| 2.9 | Write unit tests: phenology_fao56_dates produces correct growth_stage for known dates; Kcb interpolation produces same values as before when given same growth_stage | `test/unit_tests/test_phenology.F90`, `test/unit_tests/test_FAO56_functions.F90` |
| 2.10 | Run integration test (cs or similar) comparing output before/after refactor | `test/integration_tests/` |

**Key architectural change:** `CROP_COEFFICIENT` and `PHENOLOGY_METHOD` become orthogonal. The control file now requires both:
```
PHENOLOGY_METHOD      FAO56_DATES
CROP_COEFFICIENT      FAO56
```

This is more explicit (two directives instead of one), but each directive does one thing and the interaction is obvious.

**Definition of done:** Identical numerical output for an existing FAO-56 test case. The growth-stage logic lives in `phenology.F90`. `crop_coefficients__fao56.F90` is smaller and only does Kcb math.

---

## Phase 3: GDD-Based Crop Coefficient Curves (FAO56_GDD)

**Goal:** Make GDD-based Kcb curves work reliably through the new phenology provider. This is the near-term feature you want to use.

**New control file directive:**
```
PHENOLOGY_METHOD  FAO56_GDD
```

**Scope:**

| Step | Description | Files affected |
|------|-------------|---------------|
| 3.1 | Implement `phenology_initialize_fao56_gdd()` — reads `Growing_season_start_GDD`, `GDD_ini`, `GDD_dev`, `GDD_mid`, `GDD_late`, `Killing_frost_temperature` from lookup table | `src/phenology.F90` |
| 3.2 | Implement `phenology_update_fao56_gdd()` — determines growth_stage from GDD thresholds; computes growth_fraction; sets it_is_growing_season | `src/phenology.F90` |
| 3.3 | Handle GDD reset: when does GDD accumulation restart? Options: (a) after killing frost; (b) on January 1; (c) when growth_fraction returns to 0. Document the choice. | `src/phenology.F90` |
| 3.4 | Ensure `crop_coefficients_FAO56_calculate` works with GDD-derived growth_stage (it should already from Phase 2 refactor — the Kcb interpolation is stage-based regardless of how stage was determined) | `src/crop_coefficients__fao56.F90` |
| 3.5 | Write unit tests: GDD accumulation → growth_stage transitions at correct thresholds; Kcb values match expected curve for known GDD sequences | `test/unit_tests/test_phenology.F90` |
| 3.6 | Write a focused integration test with a real weather timeseries and GDD-based phenology | `test/integration_tests/` |
| 3.7 | Update documentation: which lookup table columns are needed for GDD-based operation | docs |

**Key question to resolve:** What triggers the *start* of GDD accumulation each year? In the current code, `Growing_season_start_GDD` is a threshold (start counting from DOY 1, growth starts when GDD ≥ threshold). Is that the semantics you want, or should there be an explicit "base date" for GDD accumulation (e.g., January 1 in northern hemisphere, July 1 in southern)?

**Definition of done:** A test case runs with `PHENOLOGY_METHOD FAO56_GDD` + `CROP_COEFFICIENT FAO56`, producing a smooth Kcb curve driven by thermal accumulation. Tests verify correct stage transitions.

---

## Phase 4: Continuous growth_fraction Integration

**Goal:** Downstream consumers use growth_fraction (continuous 0–1) instead of the binary it_is_growing_season where appropriate.

**Scope:**

| Step | Description | Files affected |
|------|-------------|---------------|
| 4.1 | Modify interception__bucket: interpolate storage max: `max = nongrowing + growth_fraction * (growing - nongrowing)` | `src/interception__bucket.F90` |
| 4.2 | Modify interception__bucket: scale interception amount by growth_fraction (gradual canopy development) | `src/interception__bucket.F90` |
| 4.3 | Modify rooting_depth__FAO56: use growth_fraction for Zr interpolation (if not already using its own stage logic) | `src/rooting_depth__FAO56.F90` |
| 4.4 | Modify plant height calculation in actual_et__fao56__two_stage: use growth_fraction instead of Kcb-derived proxy | `src/actual_et__fao56__two_stage.F90` |
| 4.5 | Verify mass balance: continuous interception changes don't violate conservation | unit tests + integration tests |
| 4.6 | Add regression test comparing binary vs. continuous interception for a test domain | `test/unit_tests/` |
| 4.7 | Consider: should curve_number also use growth_fraction? (probably keep binary — CN tables are defined for growing/dormant, not for intermediate states) | decision doc |

**Key design note:** The binary `it_is_growing_season` remains available for consumers that need it (curve number). It's derived from growth_fraction: `it_is_growing_season = (growth_fraction > 0.0)`. Both signals are always present.

**Definition of done:** Interception, rooting depth, and plant height vary continuously over the growing season. Mass balance still closes. Regression tests document the behavioral difference.

---

## Phase 5: Gridded LAI and LAI-Based Kcb (Future)

**Goal:** Accept LAI timeseries grids; optionally derive Kcb from LAI.

**New control file directives:**
```
PHENOLOGY_METHOD      GRIDDED_LAI
LAI                   LAI_input_prefix_%%%%%.nc
CROP_COEFFICIENT      LAI_DERIVED
```

**Scope (sketch — details TBD):**

| Step | Description |
|------|-------------|
| 5.1 | Add `phenology_initialize_gridded_lai()` — register LAI as a DATA_CATALOG entry |
| 5.2 | Add `phenology_update_gridded_lai()` — read daily LAI; compute growth_fraction = LAI / LAI_max |
| 5.3 | Add `LAI_max`, `k_ext`, `S_leaf` to lookup table columns |
| 5.4 | Implement LAI-based fc: `fc = 1 - exp(-k_ext * LAI)` |
| 5.5 | Implement LAI-based interception: `storage_max = S_leaf * LAI` |
| 5.6 | Add `CROP_COEFFICIENT LAI_DERIVED` method: `Kcb = Kcb_full * fc` |
| 5.7 | Add `Kcb_full` to lookup table |
| 5.8 | Test with resampled MODIS LAI product |
| 5.9 | Document preprocessing requirements |

---

## Deprecation and Removal Timeline

| Item | Status |
|------|--------|
| `growing_season.F90` (module) | **Removed in Phase 1** — replaced by `phenology.F90` |
| FRUIT test framework | ✅ Removed (Phase 0 complete) |
| `fruit.F90`, `fruit_util.F90`, `fruit_driver.F90` | ✅ Moved to `old_fruit_tests/` |
| Implicit pointer swap for growing season | Removed in Phase 2 |
| Auto-detection of Kcb method from table contents | Removed in Phase 2 (replaced by explicit `PHENOLOGY_METHOD`) |
| `crop_coefficients_FAO56_update_growing_season` | Removed in Phase 2 |
| Legacy parameter names (`First_day_of_growing_season`, `Planting_date`, `GDD_plant`, etc.) | **Not supported** — unified names only |

---

## Control File Evolution

### Before (current):
```
CROP_COEFFICIENT  FAO56
```
(implicitly overrides growing season mechanism, implicitly auto-detects date vs. GDD vs. monthly from table contents)

### After (Phase 1+):
```
PHENOLOGY_METHOD      DOY_BASED
```
or
```
PHENOLOGY_METHOD      GDD_THRESHOLD
```
or
```
PHENOLOGY_METHOD      FAO56_DATES
CROP_COEFFICIENT      FAO56
```
or
```
PHENOLOGY_METHOD      FAO56_GDD
CROP_COEFFICIENT      FAO56
```

Each directive does exactly one thing. No implicit coupling. No auto-detection.

### Error messaging for common mistakes:
```
ERROR: CROP_COEFFICIENT FAO56 requires PHENOLOGY_METHOD to be FAO56_DATES,
       FAO56_GDD, or GRIDDED_LAI. Please add a PHENOLOGY_METHOD directive.

ERROR: PHENOLOGY_METHOD FAO56_DATES requires columns 'Growing_season_start_date',
       'L_ini', 'L_dev', 'L_mid', 'L_late' in the lookup table.
       Column 'L_dev' was not found.
```

---

## Testing Strategy Per Phase

| Phase | New tests |
|-------|-----------|
| 0 | ✅ All existing tests converted to test-drive (10 suites, 144 tests) |
| 1 | `test_phenology_doy_based`: known DOY → correct growth_fraction/stage; winter crop (start > end) case. `test_phenology_gdd_threshold`: known GDD sequence → correct on/off transitions; killing frost terminates. |
| 2 | `test_phenology_fao56_dates`: Growing_season_start_date + L_* → correct stage dates and growth_fraction. `test_kcb_interpolation_from_stage`: given growth_stage, Kcb values match FAO-56 expectations. Regression: full integration test comparing output before/after refactor. |
| 3 | `test_phenology_fao56_gdd`: GDD sequence → correct stage transitions. `test_kcb_gdd_vs_date`: for equivalent thermal time and calendar time, Kcb curves should be similar. |
| 4 | `test_continuous_interception`: growth_fraction ramp → smooth interception capacity transition. `test_mass_balance_continuous`: soil + interception + ET + recharge = precip (within tolerance). |
| 5 | `test_lai_to_growth_fraction`: known LAI series → correct normalization. `test_lai_derived_kcb`: LAI → Kcb matches expected values. |
