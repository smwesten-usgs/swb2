# Feature Consideration: Improve Min_vegetative_cover_fraction Defaults and Discoverability

**Date:** August 3, 2026  
**Status:** Proposed  
**Priority:** Medium — affects all FAO56_TWO_STAGE runs with forested land uses  
**Affects:** `src/actual_et__fao56__two_stage.F90`  

---

## Problem

The `Min_vegetative_cover_fraction` parameter (aliased as `Minimum_fraction_vegetative_cover` or `Min_fraction_covered_soil`) controls the minimum fraction of ground considered "covered" by vegetation. This directly limits `few` (fraction exposed and wetted soil), which limits bare soil evaporation (Ke × ET0).

**Current behavior when the column is absent from the lookup table:**
- A non-fatal warning is logged (easily missed in verbose logfiles)
- Default value of **0.05** is assigned to all land uses
- This means up to 95% of the soil surface is treated as "exposed" for evaporation
- For forested land uses, this produces unrealistically high bare soil evap in dormant months

**Impact discovered during the McDonalds Branch PEST++ example (Aug 2026):**
- With the 0.05 default, bare soil evaporation was 7–9 in/yr under a dense Pinelands forest
- This accounted for ~30% of total ET — far too high for a site with thick needle litter
- Assigning forest values of 0.85–0.90 reduced `few` from 0.95 to 0.10–0.15 during dormancy
- This is physically correct: forest floors have litter, understory, roots, and moss that prevent direct soil evaporation

---

## Why This Is a "Gotcha"

1. **The parameter is optional** — the model runs without it, producing reasonable-looking (but wrong) results
2. **The default is almost never appropriate** for vegetated land uses
3. **Three column name aliases** make it hard to discover in documentation
4. **The warning is non-fatal and buried** in the logfile among many other "missing data" notices
5. **The effect is large** — can shift annual ET by 5+ inches for forests
6. **No documentation** explicitly tells users they need this column for forested applications

---

## Proposed Improvements

### Short-term (low effort)

1. **Improve the warning message** when the column is absent:
   ```
   WARNING: No 'Min_vegetative_cover_fraction' column found in lookup table.
   Using default of 0.05 for ALL land uses. This allows up to 95% of the soil
   surface to evaporate as bare soil during dormancy. For forested land uses,
   this typically produces unrealistically high bare soil evaporation.
   
   RECOMMENDATION: Add a 'Min_vegetative_cover_fraction' column to your lookup
   table with values of 0.7-0.9 for forests, 0.3-0.5 for shrubs/crops, and
   0.05-0.1 for barren/developed land uses.
   ```

2. **Standardize to one column name:** `Min_vegetative_cover_fraction`. Keep the aliases for backwards compatibility but log a deprecation notice when old names are used.

3. **Add to the logfile summary table** (like the Phenology Method Summary) so users can see what values are in effect.

### Medium-term (moderate effort)

4. **Derive a better default from Kcb_min:** If no column is present, compute a per-land-use default as:
   ```fortran
   MIN_FRACTION_COVERED_SOIL(indx) = max(0.05, KCB_l(KCB_MIN, indx) / KCB_l(KCB_MID, indx))
   ```
   Rationale: if a land use has Kcb_min > 0, it transpires year-round, implying persistent ground cover. A forest with Kcb_min=0.40 and Kcb_mid=0.95 would get a default of 0.42 — much better than 0.05.

5. **Add a control file directive** for a global override:
   ```
   MIN_VEGETATIVE_COVER_FRACTION  0.80
   ```
   This would apply a single value to all land uses (useful for quick sensitivity tests).

### Long-term (larger effort)

6. **Tie to growth_fraction:** During the growing season, the actual fraction covered could be interpolated using `growth_fraction` from the phenology module:
   ```fortran
   fc = max(fc_from_kcb, MIN_FRACTION_COVERED_SOIL(indx) + growth_fraction * (1.0 - MIN_FRACTION_COVERED_SOIL(indx)))
   ```
   This would give a smooth transition from the litter-only minimum cover in winter to full canopy cover in summer, rather than relying solely on the Kcb-derived fc calculation.

---

## Typical Values by Land Use Type

| Land Use Type | Min_vegetative_cover_fraction | Rationale |
|---------------|:-----------------------------:|-----------|
| Open Water | 0.01 | No vegetation |
| Barren | 0.05 | Essentially bare ground |
| Developed/High | 0.10 | Mostly impervious |
| Developed/Low | 0.20–0.30 | Scattered vegetation |
| Crops (row) | 0.20–0.40 | Residue/mulch between rows |
| Shrubland | 0.40–0.60 | Partial ground cover, litter |
| Herbaceous Wetland | 0.40–0.60 | Dense herbaceous cover |
| Woody Wetland | 0.60–0.80 | Saturated surface, moss |
| Deciduous Forest | 0.80–0.90 | Thick leaf litter, understory |
| Evergreen Forest | 0.85–0.95 | Dense needle litter, year-round canopy |

---

## References

- FAO-56, Allen et al., Equation 76 — derivation of fc from Kcb
- The `few` calculation: `few = 1.0 - fc`, clipped to [0.05, 1.0]
- Source code: `actual_et__fao56__two_stage.F90`, function `calculate_fraction_exposed_and_wetted_soil_fc`
