# Interception Storage Integration Test

**Purpose:** Verify that `growth_fraction` correctly interpolates `interception_storage_max`
between nongrowing and growing values for different phenology methods.

## Setup

Uses the Central Sands grid and climate data (2012) with a modified lookup table
that has distinct growing/nongrowing interception values:

| Land Use | LU Code | Method | Growing | Nongrowing |
|----------|---------|--------|---------|------------|
| Deciduous Forest | 141 | DOY_BASED (04/15–10/15) | 0.15 | 0.03 |
| Corn | 1 | GDD_THRESHOLD (GDD=200, frost=28°F) | 0.12 | 0.02 |
| Winter Wheat | 24 | DOY_BASED winter crop (10/01–05/30) | 0.10 | 0.02 |

## Running

```bash
# Run SWB2
cd test/integration_tests/cs_interception
swb2 interception_test.ctl

# Verify results
python verify_interception.py output
```

## What is verified

1. **DOY_BASED:** `interception_storage_max` equals the growing value within the
   defined growing season and the nongrowing value outside it (binary transition).

2. **GDD_THRESHOLD:** Same binary behavior, keyed to GDD accumulation and frost.

3. **Range check:** `interception_storage_max` never exceeds the growing value or
   falls below the nongrowing value.

## Expected behavior with FAO56 methods (future)

When FAO56_DATES or FAO56_GDD phenology is active, `interception_storage_max` should
ramp smoothly from nongrowing toward growing during the INI/DEV stages, then hold
at the growing value through MID and LATE. This test currently exercises only the
binary DOY/GDD methods.
