# FAO-56 (2025) Reference Tables — Notes for Future Use

**Date captured:** 2026-07-16
**Source:** Allen, R.G., Pereira, L.S., Raes, D., and Smith, M., 2025, Crop evapotranspiration — Guidelines for computing crop water requirements: FAO Irrigation and Drainage Paper 56 (revised edition).

---

## Files in this directory

| File | Table | Full Title |
|------|-------|------------|
| `FAO56_2025_Table_6.10_Tbase_Tupper.tsv` | Table 6.10 | Base temperature (Tbase) and upper temperature (Tupper) for GDD calculation |
| `FAO56_2025_Table_6.11_GDD_stages.tsv` | Table 6.11 | Cumulative growing degree days required to complete each crop growth stage and the total crop season for non-stressed and well-managed vegetable, field, and woody fruit crops estimated using the base and upper temperature thresholds given in Table 6.10 |
| `FAO56_2025_Table_6.12_GDD_stages_supplemental.tsv` | Table 6.12 | Approximate ranges for cumulative growing degree days required to complete each crop growth stage and the total crop season for non-stressed and well-managed vegetable, field, and woody fruit crops for crops not found in Table 6.11. These values were derived from the original Table 11 of the 1998 FAO56 publication |
| `FAO56_2025_Table_7.1_Kcb_values.tsv` | Table 7.1 | Basal crop coefficients, Kcb, and ranges of maximum plant heights and of maximum fraction of ground cover for non-stressed, well-managed vegetable crops in subhumid climates (RHmin≈45%, u2≈2 m/s) for use with the FAO Penman-Monteith ETo equation |
| `FAO56_2025_Table_7.2_Kcb_field_crops.tsv` | Table 7.2 | Basal crop coefficients, Kcb, and ranges of maximum plant heights and of maximum fraction of ground cover for non-stressed, well-managed field crops in subhumid climates (RHmin≈45%, u2≈2 m/s) for use with the FAO Penman-Monteith ETo equation |
| `FAO56_2025_Table_7.3_Kcb_trees_vines.tsv` | Table 7.3 | Basal crop coefficients, Kcb, and ranges of maximum plant heights and of maximum fraction of ground cover for non-stressed, well-managed fruit trees and vines for use with the FAO Penman-Monteith ETo equation |

---

## Unit Conventions

### Temperature (Tables 6.10, 6.11, 6.12)
- **Tbase and Tupper** in Table 6.10 are in **°C**.
- **GDD stage lengths** in Tables 6.11 and 6.12 are in **°C degree-days**.
- SWB2 operates in **°F**. Conversions:
  - Temperature: `T_F = T_C × 1.8 + 32`
  - GDD (degree-days): `GDD_F = GDD_C × 1.8`
  - Example: Maize Tbase = 10°C = 50°F; GDD_ini = 200°C·d = 360°F·d

### Kcb (Tables 7.1, 7.2, 7.3)
- Kcb values are **dimensionless** (ratio of crop ET to reference ET).
- Values are for **subhumid climates** (RHmin≈45%, u2≈2 m/s). FAO-56 Equation 72 adjusts Kcb_mid and Kcb_end for other climates.
- `Kcb_ini` is universally 0.15 for annual crops (bare soil evaporation is handled separately by the Ke coefficient in the dual crop coefficient approach).

### Heights and ground cover (Tables 7.1, 7.2, 7.3)
- Heights in **meters**.
- `fc_max` is the maximum fraction of ground cover (0–1), used in the FAO-56 formula for computing the fraction of exposed and wetted soil (`few = 1 - fc`).

---

## Parsing Notes

### Table 6.11 vs 6.12
- Table 6.11 contains crops where GDD values were directly measured/calibrated.
- Table 6.12 contains crops where GDD values were back-calculated from the 1998 edition's day-length-based Table 11 using the Tbase/Tupper from Table 6.10. These are approximate.
- If a crop appears in both, prefer Table 6.11.

### Season types
- **"Short season" / "Long season"**: Represent variety or climate differences. Short season = early-maturing variety or shorter growing climate. Long season = full-season variety or longer climate.
- **"Min" / "Max"** (Table 6.12 only): Range bounds. Use midpoint for a reasonable default, or choose based on local conditions.
- **"Common"**: Single representative value.

### GDD stage columns (Tables 6.11, 6.12)
- `GDD_ini`: GDD accumulated during the initial (establishment) stage
- `GDD_dev`: GDD accumulated during the development (canopy growth) stage
- `GDD_mid`: GDD accumulated during the mid-season (full canopy) stage
- `GDD_late`: GDD accumulated during the late-season (senescence/harvest) stage
- `GDD_total`: Sum of all stages (should equal ini + dev + mid + late)
- These are **stage durations**, not cumulative thresholds. To get the GDD at the end of each stage: end_ini = GDD_ini; end_dev = GDD_ini + GDD_dev; etc.

### GDD_start (not in these tables)
- FAO-56 does not define a "GDD_start" threshold — it assumes the planting date is known and GDD accumulation begins from planting.
- In SWB2, `Growing_season_start_GDD` serves a different purpose: it's the cumulative GDD from January 1 at which the model assumes planting occurs. This is a model-specific parameter that must be calibrated or estimated regionally.

### Table 7.3 structure (trees/vines)
- Unlike annual crops, trees/vines don't have a single Kcb set. Their Kcb depends on:
  - **Age** (young vs. mature)
  - **Planting density** (plants per hectare)
  - **Training system** (vase, central leader, trellis, hedgerow, etc.)
  - **Fraction of ground cover** (fc)
- The `fc_mid` column in our TSV is a representative midpoint of the fc range given.
- For SWB2, tree/vine land uses would typically use a single representative density class (e.g., "Medium" for established orchards).

### Ranges in original tables
- Where the original table gives a range (e.g., height "0.30–0.45 m"), the TSV captures a midpoint.
- Where sub-entries exist (Year 1/Year 2, processing/fresh market), each is a separate row.

---

## Crosswalk to CDL Land Use Codes (Central Sands)

Key mappings for the crops most relevant to the Central Sands test domain:

| CDL Code | CDL Name | FAO-56 Crop | Tbase °C (°F) | Tupper °C (°F) | Kcb_mid | GDD_total °C (short/long) |
|----------|----------|-------------|---------------|----------------|---------|---------------------------|
| 1 | Corn | Maize, grain | 10 (50) | 32 (89.6) | 1.15 | 1420 / 1955 |
| 4 | Sorghum | Sorghum, grain | 10 (50) | 30 (86) | 1.00 | 1380 / 1520 |
| 5 | Soybeans | Soybean | 10 (50) | 40 (104) | 1.05 | 1660 / 1960 |
| 6 | Sunflower | Sunflower | 8 (46.4) | 30 (86) | 1.10 | 1790 / 1905 |
| 12 | Sweet Corn | Maize, sweet | 10 (50) | 32 (89.6) | 1.10 | 985 / 1485 |
| 21 | Barley | Barley | 0 (32) | 30 (86) | 1.05 | 1450 / 2325 |
| 23 | Spring Wheat | Wheat, common (spring) | 0 (32) | 35 (95) | 1.10 | 1620 / 2300 |
| 24 | Winter Wheat | Wheat, common (winter) | 0 (32) | 35 (95) | 1.10 | 1645 / 2105 |
| 28 | Oats | Oats | 0 (32) | 30 (86) | 1.00 | 1850 (common) |
| 36 | Alfalfa | Alfalfa (first cutting) | 0 (32) | 30 (86) | — | 850 / 940 |
| 43 | Potatoes | Potato | 2 (35.6) | 30 (86) | 1.05 | 1600 / 2260 |

---

## Known Limitations

1. **Multi-year crops** (sugar cane, asparagus, fruit trees): GDD totals may exceed one year's accumulation. SWB2's annual GDD reset would need to be disabled or the threshold interpreted differently for these crops.

2. **Perennial forages** (alfalfa, bermuda): These have multiple cutting cycles per year. The GDD values in Table 6.12 are per-cutting-cycle, not per-year. SWB2 doesn't currently model cutting cycles.

3. **Tree crops**: Table 7.3's density-dependent Kcb values don't map cleanly to SWB2's single-Kcb-per-land-use approach. A representative density class must be chosen.

4. **GDD_start**: These tables don't provide a "start GDD" threshold. For GDD_THRESHOLD phenology in SWB2, this must be estimated separately (e.g., from regional planting date statistics converted to thermal time from Jan 1).
