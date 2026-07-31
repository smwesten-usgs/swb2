# Phenology Session Notes — 2026-07-16

## What Was Accomplished

### Comprehensive Phenology Integration Test
- Created `test/integration_tests/cs_comprehensive_phenology/` exercising all 5 phenology methods (NONE, DOY_BASED, GDD_THRESHOLD, FAO56_DATES, FAO56_GDD) in a single run
- Single unified lookup table (`Landuse_lookup_comprehensive.tsv`) — no duplicate columns, no multi-table loading
- Python verification script checks growing_season transitions and interception_storage_max behavior (binary for DOY/GDD, smooth ramp for FAO56 methods)
- Added `pixi run integration-test-comprehensive` task

### Bugs Fixed
1. **Duplicate file re-reading** (`parameters.F90`): `munge_file()` re-processed already-munged lookup tables on second call. Fixed by making `munged_files` a persistent member of `PARAMETERS_T`.
2. **NA_FLOAT → integer overflow** (`phenology.F90`): L_ini/L_dev/L_mid/L_late columns with `<NA>` were cast to `-2147483648` instead of `NODATA_INT`, causing DOY_BASED land uses to be misclassified as FAO56_DATES. Fixed with `where()` guard.
3. **Brittle GDD base/max detection** (`growing_degree_day.F90`, `growing_degree_day_baskerville_emin.F90`): `gdd_base_l(1) > rTINYVAL` only checked first element. Fixed with `any()` + per-element fallback to defaults.
4. **Broken SIMPLE GDD method** (`growing_degree_day.F90`): Did not cap at Tupper per FAO-56. Eliminated entirely; `modified_growing_degree_day_calculate` (the correct FAO-56 formula) is now the sole implementation under the name `growing_degree_day_calculate`. Signature changed from `(gdd, tmean, order)` to `(gdd, tmin, tmax, order)`.

### GDD Unit Tests
- New test module: `test/unit_tests/test_growing_degree_day.F90` (12 tests)
- Covers: basic FAO-56 formula, capping at base and upper, per-landuse differentiation, Baskerville-Emin comparison, GDD reset

### FAO-56 (2025) Reference Data Captured
- `design/FAO56_2025_Table_6.10_Tbase_Tupper.tsv` — 114 crops
- `design/FAO56_2025_Table_6.11_GDD_stages.tsv` — 53 entries
- `design/FAO56_2025_Table_6.12_GDD_stages_supplemental.tsv` — 64 entries
- `design/FAO56_2025_Table_7.1_Kcb_values.tsv` — vegetable crops + herbs (~100 entries)
- `design/FAO56_2025_Table_7.2_Kcb_field_crops.tsv` — field crops (~62 entries)
- `design/FAO56_2025_Table_7.3_Kcb_trees_vines.tsv` — trees, vines, plantations (~150 entries)
- `design/FAO56_2025_reference_tables_notes.md` — parsing guide, unit conversions, CDL crosswalk

### Final Test Counts
- **221 unit tests** in 13 suites, all passing
- **3 integration tests** (phenology, interception, comprehensive), all passing

---

## Where to Resume Next Session

### Priority 1: Build a GDD-based FAO-56 Crop Coefficient Table for Central Sands

We now have everything needed to create a realistic "irrigation lookup table" (SWB terminology) that pairs:
- Per-crop **Tbase/Tupper** from Table 6.10 (converted to °F)
- Per-crop **GDD stage lengths** from Table 6.11 (converted to °F·d)
- Per-crop **Kcb values** from Table 7.2
- A reasonable **Growing_season_start_GDD** estimate per crop (not in FAO-56 — needs regional calibration or estimation from typical Central Sands planting dates)

The CDL crosswalk in `FAO56_2025_reference_tables_notes.md` has the key crops pre-mapped.

**Steps:**
1. Convert Table 6.10 Tbase/Tupper to °F for Central Sands crops
2. Convert Table 6.11 GDD stage lengths to °F degree-days
3. Estimate Growing_season_start_GDD for each crop (from historical planting dates → thermal time from Jan 1)
4. Assemble into a single TSV matching the SWB2 column format
5. Create a focused integration test: `CROP_COEFFICIENT_METHOD FAO56` + `FAO56_GDD` phenology for a subset of GDD-driven crops (all land uses must have Kcb values when FAO56 is active)
6. Verify Kcb curves in DUMP_VARIABLES output

### Priority 2: Consider Adding growth_fraction to DUMP_VARIABLES

Would make verification of FAO56 phenology much easier — currently we can only infer growth_fraction indirectly from interception_storage_max.

### Priority 3: Decide on Baskerville-Emin Future

FAO-56 (2025) references only the simple method (which is now our sole `growing_degree_day_calculate`). B-E remains available as a separate method but all GDD threshold values in the FAO-56 tables are calibrated against the simple method. Control file should default to `SIMPLE` for new projects.

---

## Files Modified This Session

```
src/parameters.F90                              — munged_files member added to PARAMETERS_T
src/model_initialize.F90                        — stale comment updated
src/phenology.F90                               — where() guard for L_* float→int conversion
src/growing_degree_day.F90                      — eliminated SIMPLE, merged MODIFIED as sole method, per-element base/max fix
src/growing_degree_day_baskerville_emin.F90     — per-element base/max fix
src/model_domain.F90                            — updated GDD wrapper and method selection

test/unit_tests/test_growing_degree_day.F90     — new (12 tests)
test/unit_tests/test_fao56.F90                  — updated GDD test to new signature
test/unit_tests/tester.F90                      — added growing_degree_day suite
test/unit_tests/meson.build                     — added source and suite

test/integration_tests/cs_comprehensive_phenology/   — new directory
  Landuse_lookup_comprehensive.tsv
  comprehensive_phenology_test.ctl
  verify_comprehensive_phenology.py
  README.md

design/FAO56_2025_Table_6.10_Tbase_Tupper.tsv
design/FAO56_2025_Table_6.11_GDD_stages.tsv
design/FAO56_2025_Table_6.12_GDD_stages_supplemental.tsv
design/FAO56_2025_Table_7.1_Kcb_values.tsv
design/FAO56_2025_Table_7.2_Kcb_field_crops.tsv
design/FAO56_2025_Table_7.3_Kcb_trees_vines.tsv
design/FAO56_2025_reference_tables_notes.md

pixi.toml                                       — added integration-test-comprehensive task
```

### Branch: `phenology_phase_1`
