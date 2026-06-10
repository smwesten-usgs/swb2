# FAO-56 Two-Stage Implementation: Known Issues and Planned Fixes

**Date:** June 2026  
**Source file:** `src/actual_et__fao56__two_stage.F90`  
**Comparison reference:** pyfao56 package (Thorp, 2022)

---

## Issue 1: Soil moisture deficit computed against wrong reference (HIGH priority)

**Current behavior:**

```fortran
soil_moisture_deficit = max(0.0, soil_storage_max - interim_soil_storage2)
```

`soil_storage_max` is the full-profile capacity (AWC × maximum rooting depth), which is a fixed value. TAW is computed from `current_rooting_depth * awc`, which grows over the season. Early in the season when roots are shallow, TAW is small but the deficit is measured against the full profile.

**Problem:** When `soil_moisture_deficit > TAW` (which happens whenever rooting depth is small relative to the full profile), `calculate_water_stress_coefficient_ks` returns Ks = 0, indicating full stress — even when the root zone itself might have adequate water.

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

## References

- Allen, R.G., Pereira, L.S., Raes, D., and Smith, M., 1998, Crop evapotranspiration: FAO Irrigation and Drainage Paper 56, 300 p.
- Thorp, K.R., 2022, pyfao56: FAO-56 evapotranspiration in Python: SoftwareX 19, 101208.


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
| FAO56 | FAO56_TWO_STAGE | TAW grows with roots (correct), but `soil_moisture_deficit` is computed against fixed `soil_storage_max`. Early season: deficit > TAW → Ks = 0 (incorrect stress). | ❌ Bug |

### Root Cause

`model_update_rooting_depth_FAO56` updates `this%current_rooting_depth` but not `this%soil_storage_max`. The T-M and mass balance modules key off `soil_storage_max` for all capacity decisions. The FAO-56 two-stage module computes TAW from `current_rooting_depth * awc` but then computes the deficit from `soil_storage_max - soil_storage`.

### Recommendation

If `ROOTING_DEPTH_METHOD FAO56` is active, `soil_storage_max` should be updated daily to `current_rooting_depth * awc`. This makes the dynamic bucket approach work correctly for FAO56_TWO_STAGE while being harmless for T-M (since T-M with FAO56 rooting depth is a dubious combination anyway — consider emitting a warning if the user requests it).
