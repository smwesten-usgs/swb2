# Comprehensive Phenology Integration Test

**Purpose:** Exercise all 5 phenology methods in a single run, verifying that each
produces correct `growing_season` transitions and `interception_storage_max` behavior.

## Setup

Uses the Central Sands grid and climate data (2012) with a single unified lookup
table containing all needed columns (CN, interception, phenology, GDD parameters).
No multi-table loading — one file, no duplicate columns.

`CROP_COEFFICIENT_METHOD NONE` and `SOIL_MOISTURE_METHOD THORNTHWAITE-MATHER` are
used to isolate phenology behavior from the FAO-56 crop coefficient machinery.

| Land Use | LU Code | Method | Key Parameters |
|----------|---------|--------|----------------|
| Barren | 131 | NONE | All phenology columns `<NA>` |
| Deciduous Forest | 141 | DOY_BASED | start=04/15, end=10/15 |
| Corn | 1 | GDD_THRESHOLD | GDD≥200, killing frost≤28°F |
| Soybeans | 5 | FAO56_DATES | start=05/20, L=20/30/60/25 days |
| Sweet Corn | 12 | FAO56_GDD | start_GDD=100, GDD=350/1000/1600/2000, frost≤28°F |

## Running

```bash
pixi run integration-test-comprehensive
```

Or manually:

```bash
cd test/integration_tests/cs_comprehensive_phenology
swb2 --data_dir=../../test_data/cs --weather_data_dir=../../test_data/cs \
     --output_dir=output --logfile_dir=logfile comprehensive_phenology_test.ctl
python verify_comprehensive_phenology.py output
```

## What is verified

1. **PHENOLOGY_NONE (Barren):** `growing_season=0` every day, `interception_storage_max`
   permanently at nongrowing value (0.01).

2. **DOY_BASED (Deciduous Forest):** Binary `growing_season` transition at DOY 105 and
   DOY 288. `interception_storage_max` snaps between 0.05 (dormant) and 0.20 (growing).

3. **GDD_THRESHOLD (Corn):** `growing_season` starts when GDD≥200 and temperature >28°F.
   Ends on killing frost (≤28°F) with a hard latch for the rest of the year.
   `interception_storage_max` snaps between 0.03 and 0.15.

4. **FAO56_DATES (Soybeans):** `growing_season=1` for the full stage duration
   (20+30+60+25=135 days from planting). `interception_storage_max` ramps smoothly
   from 0.03 toward 0.15 during INI and DEV stages (proof of continuous `growth_fraction`).

5. **FAO56_GDD (Sweet Corn):** GDD-driven stage transitions with frost termination.
   `interception_storage_max` ramps smoothly during early stages. Season terminates
   on killing frost or after all GDD stages are completed.

For FAO56 methods (tests 4 and 5), the verification script checks for **intermediate**
`interception_storage_max` values between the nongrowing and growing extremes — this
proves that `growth_fraction` produces a continuous ramp, not a binary snap.
