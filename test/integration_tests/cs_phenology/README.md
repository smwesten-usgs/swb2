# Phenology Integration Test — Central Sands

## Purpose

Verifies that the new `phenology.F90` module correctly transitions between
growing and dormant seasons for both DOY-based and GDD-based methods.

Uses Thornthwaite-Mather soil moisture (no FAO-56) to exercise the phenology
module in isolation from the crop coefficient machinery.

## What It Tests

Three DUMP_VARIABLES points target cells with different land uses:

| Point | Land Use | LU Code | Phenology Method | Parameters |
|-------|----------|---------|------------------|------------|
| 1 | Deciduous Forest | 141 | DOY_BASED | start=04/15, end=10/15 |
| 2 | Corn | 1 | GDD_THRESHOLD | GDD=200, frost=28°F |
| 3 | Winter Wheat | 24 | DOY_BASED (winter) | start=10/01, end=05/30 |

## How to Run

```bash
cd test/integration_tests/cs_phenology

# Run SWB2
swb2 phenology_test.ctl

# Verify results (point at the directory containing dump CSVs)
python verify_phenology.py output/
```

## Expected Results

- **Deciduous Forest:** `it_is_growing_season` = 1 from DOY 106 to DOY 289 (inclusive), 0 otherwise
- **Corn:** `it_is_growing_season` transitions to 1 when GDD ≥ 200, transitions to 0 when tmean ≤ 28°F
- **Winter Wheat:** `it_is_growing_season` = 1 when DOY ≥ 275 OR DOY ≤ 151 (wraps year boundary)

## Dependencies

- Existing Central Sands test data: `test/test_data/cs/` (grids + Daymet NetCDFs)
- Existing CDL lookup table: `test/test_data/tables/Landuse_lookup_CDL.txt`
- Phenology supplement table: `phenology_lookup.txt` (in this directory)

## Notes

- DOY values account for 2012 being a leap year
- The phenology table is loaded as a supplementary table alongside the CDL table;
  columns merge by matching LU_Code values
- The verification script checks every single day, not just transitions
