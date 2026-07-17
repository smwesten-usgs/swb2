# FAO56 GDD-Based Crop Coefficient Integration Test

## Purpose

This test exercises `CROP_COEFFICIENT_METHOD FAO-56` with `GROWING_DEGREE_DAY_METHOD SIMPLE`
(FAO56_GDD phenology) for **all land uses** in the Central Sands domain. The daily weather
data drives phenology through growing degree-day (GDD) accumulation, which in turn drives the
basal crop coefficient (Kcb) curve through four growth stages: initial, development, mid-season,
and late-season.

## Key Design Decisions

- **All vegetated land uses** use GDD-based Kcb (no monthly or DOY-based fallback).
  This lets the daily temperature data determine *when* growth begins and how fast it progresses.
- **Non-vegetated** land uses (barren, waste) have `Growing_season_start_GDD=9999` so they
  remain dormant all year with Kcb=0.05.
- **Open water / aquaculture** start immediately (`Start_GDD=0.1`) and maintain Kcb=1.05.
- **Forest phenology** uses Tbase=32°F with stage lengths estimated from Radtke et al. (2001)
  leaf phenology data and Ameriflux eddy covariance observations.
- **Grass/pasture/alfalfa** use the FAO-56 Table 6.12 "Grass Pasture" GDD stages.

## Data Sources

| Parameter | Source |
|-----------|--------|
| Tbase, Tupper | FAO-56 (2025) Table 6.10, converted to °F |
| GDD stage durations | FAO-56 (2025) Tables 6.11 & 6.12, converted to °F·d (×1.8) |
| Kcb values | FAO-56 (2025) Tables 7.1 & 7.2 |
| Growing_season_start_GDD | Estimated from typical Central Sands planting dates |
| Forest stages | Radtke et al. (2001), Tree Physiology 21(8):505–512 |
| Killing frost | 28°F (warm-season crops), 24°F (cool-season/forest), 20°F (evergreen) |

## Files

| File | Description |
|------|-------------|
| `fao56_gdd_test.ctl` | SWB2 control file |
| `Landuse_lookup_FAO56_GDD.tsv` | Lookup table (59 land uses, all GDD-based Kcb) |
| `verify_fao56_gdd.py` | Python verification script |
| `README.md` | This file |

## Running

```
pixi run integration-test-fao56-gdd
```

Or manually:
```
cd test/integration_tests/cs_fao56_gdd
rmdir /s /q output logfile 2>nul
mkdir output logfile
..\..\..\builddir\src\swb2.exe --data_dir=..\..\test_data\cs --weather_data_dir=..\..\test_data\cs --output_dir=output --logfile_dir=logfile fao56_gdd_test.ctl
python verify_fao56_gdd.py output
```

## Verification Tests

The Python script checks:

1. **Dormant Kcb**: Before growing season starts, Kcb = Kcb_min for all crops.
2. **Kcb curve shape**: Peak reaches Kcb_mid, no overshoot, visible ramp-up during
   development stage.
3. **Start timing**: Growing season starts within expected DOY windows for each crop
   (broad windows account for year-to-year weather variability).
4. **Differential timing**: Cool-season crops (Tbase=32°F) start earlier than
   warm-season crops (Tbase=50°F) — this is the key benefit of GDD-driven phenology.
5. **Summary statistics**: Reports start DOY, end DOY, peak Kcb, and final Kcb for
   each verification point.

## DUMP_VARIABLES Points

| Point | Crop | LU Code | Tbase | Why included |
|-------|------|---------|-------|--------------|
| 1 | Corn | 1 | 50°F | Classic warm-season annual |
| 2 | Potatoes | 43 | 35.6°F | Moderate Tbase, earlier start |
| 3 | Spring Wheat | 23 | 32°F | Cool-season, earliest start |
| 4 | Deciduous Forest | 141 | 32°F | Non-crop, leaf phenology |
| 5 | Grass/Pasture | 176 | 32°F | Perennial grass, low Kcb_mid |
| 6 | Sweet Corn | 12 | 50°F | Comparison with comprehensive test |
