# FAO-56 Two-Stage Implementation: Known Issues and Planned Fixes

**Date:** June 2026 (updated July 2026)  
**Source file:** `src/actual_et__fao56__two_stage.F90`  
**Comparison reference:** pyfao56 package (Thorp, 2022)

**Changelog:**
- July 2026: Added Issue 1a (irrigation module amplification), quantitative evidence from Michigan SWB model, git chronology of when issues were introduced, and clarified the interaction chain between variable rooting depth, TAW, Ks, and irrigation triggering.
- July 28, 2026: Revised impact assessment based on controlled 4-run factorial experiment (irrigation × rooting depth). Key finding: because `soil_storage_max` is never updated, the net infiltration threshold is identical regardless of rooting depth method — the bug affects Ks/ET/irrigation triggering but does NOT produce excess net infiltration. Original quantitative evidence was confounded by comparing heterogeneous cell populations rather than controlled runs.

---

## Issue 1: Soil moisture deficit computed against wrong reference (MEDIUM priority — revised down from HIGH)

**Current behavior:**

```fortran
soil_moisture_deficit = max(0.0, soil_storage_max - interim_soil_storage2)
```

`soil_storage_max` is the full-profile capacity (AWC × maximum rooting depth), which is a fixed value. TAW is computed from `current_rooting_depth * awc`, which grows over the season. Early in the season when roots are shallow, TAW is small but the deficit is measured against the full profile.

**Problem:** When `soil_moisture_deficit > TAW` (which happens whenever rooting depth is small relative to the full profile), `calculate_water_stress_coefficient_ks` returns Ks = 0, indicating full stress — even when the root zone itself might have adequate water.

**Impact (revised):** Affects the water stress coefficient (Ks) and therefore actual ET partitioning, but does NOT affect net infiltration (see Appendix). The fix is still important for physical correctness of ET estimates, irrigation reporting, and future implementations where `soil_storage_max` is made dynamic.

**FAO-56 intent (Eq. 84):** Dr (root zone depletion) should be bounded by TAW. The deficit is the amount of water missing *from the current root zone*, not from the entire soil column.

**pyfao56 approach:** Tracks Dr via daily water balance (`Dr = Dr_prev + ETa - P_eff - Irr + DP`), bounded by TAW. Dr can never exceed TAW by construction.

**Proposed fix:** Compute soil moisture deficit relative to the current TAW:

```fortran
! Option A: scale the deficit proportionally
soil_moisture_deficit = max(0.0, taw - (interim_soil_storage2 * taw / soil_storage_max))

! Option B: directly compute from root-zone water content
soil_moisture_deficit = min(max(0.0, taw - root_zone_water), taw)
```

Option B is cleaner but requires tracking root-zone water content separately from the full soil bucket. This may require a more significant refactor of how SWB2's soil storage interacts with the FAO-56 module.

---

## Issue 1a: Irrigation module amplifies the deficit/TAW mismatch (MEDIUM priority — revised down from HIGH)

**Related to:** Issue 1

**Current behavior (`irrigation.F90`, lines 537–541):**

```fortran
if ( total_available_water > 0.0_c_float ) then
  depletion_fraction = min( ( soil_storage_max - soil_storage ) / total_available_water, 1.0 )
else
  depletion_fraction = min( ( soil_storage_max - soil_storage ) / soil_storage_max, 1.0 )
endif
```

The irrigation trigger uses `total_available_water` (= TAW = `current_rooting_depth × AWC`) as the denominator, but `soil_storage_max - soil_storage` (deficit relative to FULL profile) as the numerator.

**Problem:** Early in the growing season when rooting depth is small:

1. TAW is small (e.g., 1 inch for a 0.1 m root zone on a sandy soil)
2. `soil_storage_max - soil_storage` can be much larger than TAW (deficit relative to full profile)
3. `depletion_fraction` is clipped to 1.0, meaning it **always exceeds MAD**
4. Irrigation triggers *every single day* the irrigation season is active
5. But `Ks = 0` (from Issue 1), so the crop can't transpire the irrigated water
6. The irrigation water fills `soil_storage` toward `soil_storage_max` (the full-profile capacity)
7. Next day: because the deficit is measured against the full profile and divided by the small TAW, depletion_fraction still ≥ 1.0 → irrigation triggers again (until `soil_storage` finally reaches `soil_storage_max`)

~~Original claim (incorrect): The irrigation water "passes through the soil column as net infiltration." This is wrong because `soil_storage_max` is the full-profile capacity, not TAW — the water is absorbed into the large fixed bucket.~~

This creates a daily irrigation cycle that persists until roots grow large enough for TAW to approach `soil_storage_max`. (Note: the irrigated water accumulates in `soil_storage` but does NOT overflow as net infiltration because `soil_storage_max` is the full-profile capacity — see revised impact below.)

**Furthermore**, the application amount (for the `APP_FIELD_CAPACITY` method) is:

```fortran
interim_irrigation_amount = max(0.0, soil_storage_max - soil_storage)
```

This refills to the full static `soil_storage_max`, not to the current TAW. So early-season applications can be large relative to what the root zone can hold.

**Impact on results (revised July 28, 2026):** The mechanism described above is real — irrigation triggers daily and Ks = 0 suppresses transpiration. However, controlled diagnostic runs (see Appendix: Diagnostic Run Results) demonstrate that this does **not** produce excess net infiltration in the current code. The reason: `soil_storage_max` is the full-profile capacity (`AWC × Zr_max`), and net infiltration only occurs when `soil_storage > soil_storage_max`. Because `soil_storage_max` is the same fixed value whether rooting depth is variable or constant, the irrigation water is absorbed into the large fixed-capacity bucket and does not overflow as recharge.

The actual impacts of Issues 1 and 1a are:
1. **Incorrect ET partitioning:** Ks = 0 early in the season suppresses transpiration when it should be occurring (albeit at reduced capacity with shallow roots).
2. **Excessive irrigation application:** The trigger fires daily and the application refills to the full profile, applying far more water than the crop's actual root zone can use. This water accumulates in the bucket.
3. **No excess net infiltration:** Because `soil_storage_max` is unchanged, the recharge threshold is identical in variable-RZ and constant-RZ runs. The diagnostic comparison shows Comparison E (interaction term) ≈ 0.000 in/yr for all cell categories.

The practical consequence is that the irrigation amounts reported by the model are unrealistically high early in the growing season, and the actual ET partitioning between transpiration and soil evaporation is wrong — but the net infiltration estimate is not inflated by this mechanism.

**Note:** The issues would produce excess net infiltration *if and only if* `soil_storage_max` were updated to track TAW (the proposed fix). Under the proposed fix, a smaller `soil_storage_max` early in the season would correctly cause the bucket to overflow sooner — but the irrigation trigger and Ks calculations would also be correct, preventing the artificial daily cycle. The fix is self-consistent; the current code is self-consistently wrong in a way that happens to not inflate recharge.

**Proposed fix:** Same as Issue 1 — if `soil_storage_max` is updated to track `current_rooting_depth × AWC`, then:
- Depletion fraction becomes `(TAW - root_zone_storage) / TAW`, bounded [0, 1] by construction
- Irrigation application fills to `TAW` (current root zone capacity), not the full profile
- Both the trigger and the amount scale appropriately with the crop's actual water needs

---

## Issue 2: Ks not initialized before first call to FAO-56 subroutine (LOW priority)

**Current behavior:** `Ks` is initialized to 0.0 (from array allocation defaults) and only updated when `calculate_actual_et_fao56_two_stage` is called. Before the growing season starts, Ks remains at 0.0 in the output.

**Expected behavior:** Ks = 1.0 when no crop is active (no stress possible without a crop to stress).

**Fix:** Initialize `Ks = 1.0` in model domain allocation, or set Ks = 1.0 at the beginning of the dormant/pre-planting period.

---

## Issue 3: Kcb_max uses hardcoded wind speed and humidity (LOW priority)

**Current behavior:**

```fortran
Kcb_max = crop_coefficients_FAO56_calculate_Kcb_Max(wind_speed_meters_per_sec=2., &
                                                     relative_humidity_min_pct=55., ...)
```

**FAO-56 intent (Eq. 72):** Kcb_max should use actual daily wind speed (u2) and minimum relative humidity. These are climate-dependent and affect the upper limit on Ke.

**Impact:** For humid climates (RHmin > 55%), the current code overestimates Kcb_max slightly. For arid climates with high winds, it underestimates. The effect on total ET is generally small.

**Proposed fix:** Accept u2 and RHmin as inputs from weather data, or compute from tmin/tmax if wind data is unavailable. This may require adding wind speed and humidity as optional SWB2 weather inputs.

---

## Issue 4: Evaporable layer deep percolation not explicit (MINOR)

**Current behavior:** When infiltration exceeds the evaporable water capacity (TEW), excess is implicitly discarded by clipping `evaporable_water_storage` to TEW:

```fortran
evaporable_water_storage = clip(evaporable_water_storage + infiltration, minval=0.0, maxval=TEW)
```

**FAO-56 (Eq. 79):** Explicitly computes DPe (deep percolation from the evaporation layer) as the excess when De would go negative:

```
DPe = max(0, (P - RO) + I/fw - De_prev)
```

**Impact:** Functionally equivalent for the water balance (excess water leaves the evaporation layer either way), but the explicit DPe would be useful for debugging and verification.

---

## Issue 5: REW/TEW values in lookup tables were incorrect (FIXED)

**Problem:** The original `IRR_lookup_MN_v3.txt` had REW and TEW values of ~0.05–0.10 inches (1.3–2.5 mm), which is far too small. These values effectively disabled the evaporation layer dynamics (Kr → 0 immediately, making Ke ≈ 0).

**Fix applied:** Created `IRR_lookup_MN_v3_corrected.txt` with FAO-56 Table 19 values appropriate for each hydrologic soil group (e.g., silt loam: REW=9mm, TEW=25mm).

---

## Git Chronology: When These Issues Were Introduced

The core issue has been present since the FAO-56 two-stage module was first written in March 2017 — over 9 years. The following timeline is derived from `git log` on the SWB2 repository.

| Date | Commit | Event |
|------|--------|-------|
| 2014-07-13 | `b5b5a17` | `soil_storage_max` first appears in `model_domain.F90` — set as a fixed value at initialization (`rooting_depth_max × AWC`) |
| 2015-05-22 | `acf16cf` | FAO-56 actual ET module first wired up to the soil moisture framework |
| **2017-03-03** | `31ef817` | **`actual_et__fao56__two_stage.F90` created.** TAW (`calculate_total_available_water`) implemented. The pattern of computing deficit against `soil_storage_max` while comparing to dynamic TAW was present from day one. |
| 2017-03-07 | `dfcde07` | Continued development — `soil_storage_max` used in deficit calculation |
| 2017-03-08 | `bef9f11` | `rooting_depth__FAO56.F90` first appears — variable rooting depth tied to Kcb progression |
| **2017-09-08** | `2fca655` | "Fix subtle departure from SWB v. 1.0 in irrigation algorithms" — `total_available_water` first used as denominator in irrigation depletion fraction (Issue 1a introduced) |
| 2018-06-12 | `088c068` | TAW further integrated into irrigation module |
| 2018-07-27 | `8250217` | `total_available_water` logic in irrigation formalized |
| 2021-01-06 | `968f194` | Major rework of evaporable water layer tracking — `soil_storage_max` usage refined but fundamental deficit calculation unchanged |
| 2021-01-25 | `114b497` | "table values work" — further refinements to FAO-56 two-stage integration |
| **2024-10-31** | `9d2a671` | Per-CDL `variable_rooting_depth` parameter added to irrigation lookup table (the `'varying'`/`'constant'` toggle). Before this, variable rooting depth was all-or-nothing at the model level. |
| 2024-12-04 | `f271846` | Fix: variable rooting depth should not decrease for Kcb > Kcb_mid |

### Summary

- **Issue 1** (deficit computed against `soil_storage_max` instead of TAW): present since **March 3, 2017**
- **Issue 1a** (irrigation depletion fraction uses TAW as denominator): present since **September 8, 2017**
- **Per-CDL variable/constant toggle**: added **October 31, 2024** (before this, the issue affected all CDL codes uniformly when FAO-56 rooting depth was active)

The issues were never caught because:
1. Early testing focused on getting the FAO-56 equations right in isolation, not on the interaction with the SWB2 soil storage framework
2. The irrigation module was developed separately and later wired to use TAW without reconsidering the deficit reference
3. The effect is subtle in non-irrigated cells (Ks = 0 early in season, but ET demand is also low then)
4. **The errors cancel for net infiltration:** because `soil_storage_max` is fixed at the full-profile value, excess irrigation water fills the large bucket rather than overflowing as recharge. The bug produces wrong Ks and wrong irrigation amounts, but the net infiltration output is unaffected — making the error invisible in the primary output that users inspect
5. The Ks=0 condition suppresses transpiration early in the season, but this period also has low reference ET, so the actual ET difference is small and easy to overlook

---

## References

- Allen, R.G., Pereira, L.S., Raes, D., and Smith, M., 1998, Crop evapotranspiration: FAO Irrigation and Drainage Paper 56, 300 p.
- Thorp, K.R., 2022, pyfao56: FAO-56 evapotranspiration in Python: SoftwareX 19, 101208.


---

## Quantitative Evidence: Michigan SWB Model (July 2026)

### Original Evidence (Confounded — July 2026)

The Michigan Lower Peninsula SWB model (1995–2025, 1 km CDL grid, Daymet forcing) uses `ROOTING_DEPTH_METHOD FAO56` with `SOIL_MOISTURE_METHOD FAO56_TWO_STAGE`. Monthly zonal statistics computed from the baseline model output appeared to show a large irrigation × variable rooting depth interaction:

| Category | Cell Count | Annual Total (in/yr) |
|---|---|---|
| Variable rooting depth, non-irrigated | 39,735 | 8.67 |
| Variable rooting depth, irrigated | 11,124 | **18.20** |
| Constant rooting depth, non-irrigated | 75,560 | 10.15 |
| Constant rooting depth, irrigated | 4,957 | 11.60 |

**However**, this comparison is confounded: the "variable" and "constant" groups contain different CDL codes with inherently different hydrologic properties (row crops vs. forest/grassland). The ~10 in/yr gap is largely explained by the different land-use compositions of the two groups, not by the rooting depth mechanism.

### Controlled Diagnostic Runs (July 28, 2026)

A 4-run factorial experiment was conducted to isolate the effect:
- Same grid, same lookup tables, same executable
- Only `ROOTING_DEPTH_METHOD` (FAO56 vs. NONE) and `IRRIGATION_METHOD` (FAO56 vs. NONE) varied

| Run | Domain Mean | Irrigated Cells | Non-Irrigated Cells |
|---|---|---|---|
| Irrigation + Variable RZ | 9.55 in/yr | 13.64 in/yr | 8.98 in/yr |
| No Irrigation + Variable RZ | 9.46 in/yr | 12.85 in/yr | 8.98 in/yr |
| Irrigation + Constant RZ | 9.53 in/yr | 13.57 in/yr | 8.96 in/yr |
| No Irrigation + Constant RZ | 9.44 in/yr | 12.80 in/yr | 8.96 in/yr |

**Key results:**
- **Comparison E (interaction term) ≈ 0.000 in/yr** for all cell categories including corn on HSG A soils
- Variable vs. constant rooting depth produces **identical** net infiltration (difference < 0.03 in/yr domain-wide)
- The irrigation effect is modest (+0.79 in/yr for irrigated cells) and **the same** under both rooting depth methods
- Corn on HSG A soils (irrigated): 19.92 in/yr under BOTH variable and constant rooting depth

### Why the Expected Signal Did Not Appear

The diagnostic runs confirmed that:
1. SWB2 correctly reads and applies the `ROOTING_DEPTH_METHOD` directive (log shows "DYNAMIC" vs. "STATIC" submodel selected)
2. The per-CDL `allow_variable_rooting_depth` flags are set only when `ROOTING_DEPTH_METHOD FAO56` is active
3. Despite this, `soil_storage_max` remains fixed (`AWC × Zr_max`) in all cases
4. Net infiltration = `max(0, soil_storage - soil_storage_max)` — since `soil_storage_max` is identical in all runs, the recharge threshold is unchanged

The variable rooting depth affects *internal* calculations (TAW, Ks, irrigation triggering) but NOT the net infiltration pathway. The "daily irrigation → recharge cycle" described in Issue 1a does not manifest as excess recharge because the irrigated water fills the large fixed-capacity bucket (`soil_storage_max = AWC × Zr_max`) rather than overflowing a small dynamic bucket.

### Revised Interpretation

- **The model's +1.57 in/yr positive bias is NOT attributable to Issues 1 and 1a.** The rooting depth × irrigation interaction has near-zero effect on net infiltration.
- Issues 1 and 1a still produce incorrect behavior: wrong Ks values, wrong irrigation triggering patterns, wrong actual ET partitioning. These are real bugs worth fixing for physical correctness.
- The positive bias relative to the recharge ensemble must have a different source (e.g., climate forcing differences, CN parameterization, interception, or differences between the local and Hovenweep-compiled executables).

### Recommended validation approach

~~Run the Michigan model with `ROOTING_DEPTH_METHOD STATIC` (all other parameters unchanged) and compare cell-by-cell. The difference isolates the effect of Issues 1 and 1a. Expected result: the variable-RZ run will show higher net infiltration primarily in irrigated agricultural cells on A/B soils during March–June.~~

**Completed July 28, 2026.** Result: no meaningful difference in net infiltration between variable-RZ and constant-RZ runs. The recommended validation confirmed that Issues 1/1a do not affect net infiltration under the current code architecture (see above).

---

## Proposed Refactor: Split Soil Storage into Root Zone and Sub-Root Zone

### Concept

The current SWB2 soil bucket is a single fixed-capacity store (`soil_storage_max = AWC × Zr_max`). The FAO-56 methodology requires that water stress be computed relative to the *current* root zone capacity, which grows over the season. The proposed refactor splits the soil into two compartments:

- **`root_zone_storage`** (0 to Zr_current) — the active layer where transpiration draws water and where Ks is computed.
- **`sub_root_zone_storage`** (Zr_current to Zr_max) — water stored below the current root depth, inaccessible to the crop until roots grow into it.

### Precedent

**AquaCrop (FAO)** uses this approach explicitly. When roots grow, soil below the previous root depth is "incorporated" into the active root zone at its current moisture content (typically assumed to be at field capacity). Reference: Raes, D., Steduto, P., Hsiao, T.C., and Fereres, E., 2009, AquaCrop—The FAO crop model to simulate yield response to water: Agronomy Journal, v. 101, no. 3, p. 426–437.

**pyfao56** tracks root zone depletion (Dr) bounded by TAW. When Zr grows, TAW increases and Dr stays the same, effectively improving Ks.

**SWAP (Wageningen)** uses a similar two-compartment approach in its macroscopic water balance mode.

### Implementation Sketch

```fortran
! --- State variables (per cell) ---
! root_zone_storage          : water in active root zone (inches)
! root_zone_storage_max      : capacity of active root zone = Zr_current * AWC
! sub_root_zone_storage      : water below current roots (inches)
! sub_root_zone_storage_max  : capacity below roots = (Zr_max - Zr_current) * AWC
! previous_rooting_depth     : Zr from the previous timestep

! --- Daily update when rooting depth increases ---
delta_Zr = current_rooting_depth - previous_rooting_depth

if (delta_Zr > 0) then
    ! New soil incorporated into root zone — assume at field capacity
    root_zone_storage_max = current_rooting_depth * awc
    root_zone_storage = root_zone_storage + delta_Zr * awc
    sub_root_zone_storage_max = (max_rooting_depth - current_rooting_depth) * awc
    sub_root_zone_storage = sub_root_zone_storage - delta_Zr * awc
endif

! --- Infiltration partitioning ---
root_zone_storage = root_zone_storage + infiltration

if (root_zone_storage > root_zone_storage_max) then
    excess = root_zone_storage - root_zone_storage_max
    root_zone_storage = root_zone_storage_max

    ! Excess goes to sub-root zone first, remainder becomes net infiltration
    sub_root_zone_space = sub_root_zone_storage_max - sub_root_zone_storage
    to_sub_root = min(excess, sub_root_zone_space)
    sub_root_zone_storage = sub_root_zone_storage + to_sub_root
    net_infiltration = excess - to_sub_root
endif

! --- Deficit and Ks computed against root zone only ---
soil_moisture_deficit = max(0.0, root_zone_storage_max - root_zone_storage)
Ks = calculate_water_stress_coefficient_ks(taw, raw, soil_moisture_deficit)
```

### Key Assumptions

1. **Newly incorporated soil is at field capacity.** This is the AquaCrop convention and the simplest defensible assumption. If the sub-root zone has been drained by deep percolation, a more conservative assumption would be to track its actual moisture content.

2. **Excess root-zone water fills the sub-root zone before becoming net infiltration.** This prevents immediate recharge when the shallow root zone overflows but deeper soil still has capacity.

3. **Net infiltration (recharge) only occurs when both zones are full.** This is consistent with SWB2's current recharge definition but adds the intermediate buffer of the sub-root zone.

### Migration Path

1. Add `root_zone_storage`, `sub_root_zone_storage`, and `previous_rooting_depth` as new state arrays in `MODEL_DOMAIN_T`.
2. Initialize: `root_zone_storage = soil_storage * (Zr_ini / Zr_max)`, `sub_root_zone_storage = soil_storage * (1 - Zr_ini/Zr_max)`.
3. Modify `calculate_actual_et_fao56_two_stage` to use `root_zone_storage` for the deficit/Ks calculation.
4. Update `mass_balance__soil` to handle the two-compartment bookkeeping.
5. Existing single-bucket behavior (Thornthwaite-Mather method) remains unchanged — the split only applies when `SOIL_MOISTURE_METHOD FAO56_TWO_STAGE` is active.


---

## Design Constraint: Consistent Soil Storage Interface Across Methods

### Background

SWB2 uses a strategy pattern where `calc_actual_et` is a procedure pointer set at initialization. All ET methods (Thornthwaite-Mather, exponential, FAO-56 two-stage) must work with the same state variables: `soil_storage`, `soil_storage_max`, and `net_infiltration`. The mass balance module enforces `net_infiltration = excess when soil_storage > soil_storage_max`.

The Thornthwaite-Mather and exponential methods are genuinely single-layer concepts — a fixed-capacity bucket is correct for them. The FAO-56 two-stage method was adapted to this same interface, which forced the deficit/Ks calculation to use `soil_storage_max` (fixed, full-profile capacity) rather than a dynamic TAW.

### Least-Invasive Alternative

Rather than introducing a second soil layer with different semantics, the FAO-56 method could simply make `soil_storage_max` dynamic:

- When `SOIL_MOISTURE_METHOD FAO56_TWO_STAGE` is active, `soil_storage_max` is updated daily to `current_rooting_depth * awc` (= TAW in FAO-56 terms).
- Net infiltration still fires when `soil_storage > soil_storage_max` — this is physically correct (water escaping the active root zone becomes recharge).
- Ks is computed against the current `soil_storage_max` (= TAW), which is now the correct reference.
- As roots grow, `soil_storage_max` increases. The new soil volume is assumed to be at field capacity (adding `delta_Zr * awc` to both storage and max simultaneously, so the deficit doesn't change).

This preserves a single bucket concept — the bucket just grows over time. No secondary storage variable needed. The mass balance module, output routines, and all other methods continue to work unchanged.

**Important implication (from diagnostic runs):** Making `soil_storage_max` dynamic will CHANGE the net infiltration output — specifically, it will produce more net infiltration early in the season (when the bucket is small and overflows more easily) and potentially less later (as the bucket grows and captures more water). The current code produces identical net infiltration regardless of rooting depth method because the fixed `soil_storage_max` acts as a large buffer. Making it dynamic removes that buffer and makes the recharge signal responsive to root growth. This is physically correct behavior, but it means the fix is not "neutral" with respect to model outputs — it will require re-evaluation of model calibration and comparison with independent recharge estimates.

### Key Principle

Changing the `SOIL_MOISTURE_METHOD` should not introduce wildly differing definitions of what "soil storage" means. All methods should be expressible as: *a bucket with a defined capacity, where excess above capacity becomes net infiltration*. The difference is only whether that capacity is fixed (T-M) or grows with root depth (FAO-56).


---

## Method Interaction Matrix: Rooting Depth × Soil Moisture

`soil_storage_max` is set once at initialization (`rooting_depth_max * awc`) and never updated. The FAO-56 rooting depth method grows `current_rooting_depth` over the season, but this only feeds into the `calculate_total_available_water` subroutine (computing TAW/RAW) — it does not update `soil_storage_max`.

| ROOTING_DEPTH_METHOD | SOIL_MOISTURE_METHOD | Behavior | Status |
|---|---|---|---|
| STATIC | THORNTHWAITE-MATHER | Fixed roots, fixed bucket. Consistent. | ✓ OK |
| STATIC | FAO56_TWO_STAGE | TAW = fixed = soil_storage_max. Deficit reference is correct (both are the same value). | ✓ OK |
| FAO56 | THORNTHWAITE-MATHER | Rooting depth grows but has **no effect** on T-M water balance. `soil_storage_max` is fixed; T-M doesn't use `current_rooting_depth`. Variable rooting depth is cosmetic only. | ⚠️ Misleading |
| FAO56 | FAO56_TWO_STAGE | TAW grows with roots (correct), but `soil_moisture_deficit` is computed against fixed `soil_storage_max`. Early season: deficit > TAW → Ks = 0 (incorrect stress). Irrigation depletion fraction also uses TAW as denominator → triggers daily when roots are small (Issue 1a). **However**, net infiltration is unaffected because it is gated by the same fixed `soil_storage_max`. | ❌ Bug (ET/irrigation partitioning only; net infiltration unaffected) |

### Root Cause

`model_update_rooting_depth_FAO56` updates `this%current_rooting_depth` but not `this%soil_storage_max`. The T-M and mass balance modules key off `soil_storage_max` for all capacity decisions. The FAO-56 two-stage module computes TAW from `current_rooting_depth * awc` but then computes the deficit from `soil_storage_max - soil_storage`.

**Critical implication (confirmed by diagnostic runs):** Because net infiltration is triggered by `soil_storage > soil_storage_max`, and `soil_storage_max` is fixed regardless of rooting depth method, the variable rooting depth bug produces zero difference in net infiltration. The bug is "self-limiting" — excess irrigation water fills the large fixed bucket rather than overflowing as recharge. The variable and constant rooting depth runs are identical with respect to net infiltration output.

### Recommendation

If `ROOTING_DEPTH_METHOD FAO56` is active, `soil_storage_max` should be updated daily to `current_rooting_depth * awc`. This makes the dynamic bucket approach work correctly for FAO56_TWO_STAGE while being harmless for T-M (since T-M with FAO56 rooting depth is a dubious combination anyway — consider emitting a warning if the user requests it).

**Implementation note:** The fix must be applied holistically. If `soil_storage_max` becomes dynamic (= TAW), then the irrigation trigger, Ks calculation, and recharge threshold all become consistent — irrigation only fires when the root zone is actually depleted, Ks reflects actual root-zone stress, and recharge fires when the root zone overflows. Without the fix, all three are independently wrong but the errors happen to cancel for the recharge pathway.


---

## Appendix: Diagnostic Comparison Run Results (July 28, 2026)

### Experimental Setup

Four runs on the Michigan Lower Peninsula grid (527×343 cells, 1 km, 1995–2025):

| Run | IRRIGATION_METHOD | ROOTING_DEPTH_METHOD |
|-----|-------------------|---------------------|
| 1 | FAO56 | FAO56 (dynamic) |
| 2 | NONE | FAO56 (dynamic) |
| 3 | FAO56 | NONE (static) |
| 4 | NONE | NONE (static) |

All four runs used identical input grids, lookup tables, and the same compiled SWB2 executable on the same workstation. Only the two control file directives differed.

### Results: Mean Annual Net Infiltration (in/yr)

| Zone | Run 1 (I+V) | Run 2 (NoI+V) | Run 3 (I+C) | Run 4 (NoI+C) |
|------|-------------|---------------|-------------|----------------|
| All active (128,605 cells) | 9.55 | 9.46 | 9.53 | 9.44 |
| Irrigated (15,960 cells) | 13.64 | 12.85 | 13.57 | 12.80 |
| Non-irrigated (112,645 cells) | 8.98 | 8.98 | 8.96 | 8.96 |
| Corn/HSG A/irrigated (3,893) | 19.92 | 19.58 | 19.92 | 19.58 |
| Corn/HSG A/non-irrigated (2,129) | 18.17 | 18.17 | 18.17 | 18.17 |

### Comparisons

| Comparison | Description | Domain Mean | Irrigated Cell Mean |
|---|---|---|---|
| A: Run 1 − Run 2 | Irrigation effect (variable RZ) | +0.098 | +0.791 |
| B: Run 3 − Run 4 | Irrigation effect (constant RZ) | +0.095 | +0.765 |
| C: Run 1 − Run 3 | Variable vs. constant RZ (with irr) | +0.017 | +0.025 |
| D: Run 2 − Run 4 | Variable vs. constant RZ (no irr) | +0.014 | +0.000 |
| **E: A − B** | **Interaction term (the "bug" signal)** | **+0.003** | **+0.025** |

### Interpretation

1. **Comparison E ≈ 0:** The variable rooting depth × irrigation interaction does not produce excess net infiltration. The hypothesized "daily irrigation → recharge cycle" does not manifest because `soil_storage_max` is unchanged.

2. **Comparison A ≈ B:** The irrigation effect on net infiltration (+0.79 in/yr for irrigated cells) is real and modest, but it is the **same** regardless of rooting depth method. This is legitimate irrigation-driven recharge from water applications that exceed the full-profile bucket capacity.

3. **Comparison C ≈ D ≈ 0:** Variable vs. constant rooting depth produces no meaningful difference in net infiltration for any cell category, confirming that `current_rooting_depth` does not affect the recharge pathway.

4. **Non-irrigated cells are identical across all 4 runs** (8.98 in/yr for Run 1/2, 8.96 for Run 3/4 — the 0.02 difference is from variable RZ affecting Ks → slightly different AET → slightly different soil_storage trajectory, but the recharge signal is negligible).

### Log Confirmation

- Run 1 log: `ROOTING_DEPTH_METHOD FAO56` → `==> DYNAMIC rooting depth submodel selected.`
  - Sets `allow_variable_rooting_depth` per CDL code from irrigation lookup table
- Run 3 log: `ROOTING_DEPTH_METHOD NONE` → `==> STATIC rooting depth submodel selected.`
  - Does NOT set `allow_variable_rooting_depth` flags (absent from debug log)
- Despite these different code paths being active, net infiltration output is identical

### Source

Scripts: `mi_swb_report_figures/python/preprocess_diagnostic_runs.py`, `plot_diagnostic_comparisons.py`, `plot_diagnostic_zonal_lines.py`, `summarize_diagnostic_runs.py`

Run outputs: `E:\projects\michigan_swb\PARALLEL_LOCAL_RUNS\{run_name}\output\`
