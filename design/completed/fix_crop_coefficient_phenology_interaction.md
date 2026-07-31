# Fix: Crop Coefficient Module Should Defer to Phenology Module

**Date:** July 30, 2026  
**Status:** Proposed  
**Affects:** `src/crop_coefficients__fao56.F90`  
**Depends on:** `src/phenology.F90` (phenology_phase_1 branch)

---

## Problem

The crop coefficient module (`crop_coefficients__fao56.F90`) and the phenology module (`phenology.F90`) both independently read the same lookup table columns and both independently determine which phenology/Kcb method applies to each land-use code. They don't communicate, and their validation logic differs:

- **`phenology.F90`** (called first via `initialize_ancillary_values` → `MODEL%initialize_growing_season`) uses `> NA_FLOAT` checks, handles missing values gracefully, and assigns `PHENOLOGY_METHOD_INDEX` per land use.

- **`crop_coefficients__fao56.F90`** (called later via `initialize_methods_sub` → `init_crop_coefficient`) re-reads the same columns, applies its own `>= 0.0` checks, and fatally errors if none of the three Kcb methods (monthly, GDD, DOY-based) can be identified for any land use.

When using a comprehensive lookup table with mixed phenology methods (some rows GDD-based, some rows with no vegetation), the crop coefficient module's independent validation fails because:

1. It doesn't recognize `PHENOLOGY_NONE` as a valid state (no fallback for bare/water land uses)
2. Its threshold check (`>= 0.0`) differs from the phenology module's (`> NA_FLOAT`)
3. It re-reads columns that the phenology module has already correctly parsed

---

## Call Sequence (Current)

```
model_initialize.F90:
  line 193: call initialize_ancillary_values()
              → call MODEL%initialize_growing_season()     [phenology.F90 runs here]
                  → phenology_initialize(PARAMS)
                  → assigns PHENOLOGY_METHOD_INDEX per LU   ← CORRECT

  line 196: call MODEL%initialize_methods()
              → call this%init_crop_coefficient             [crop_coefficients__fao56.F90 runs here]
                  → crop_coefficients_FAO56_initialize(PARAMS)
                  → re-reads GDD/L/Kcb columns
                  → own auto-detection loop
                  → FATAL if no method found                ← FAILS
```

---

## Proposed Fix

Make `crop_coefficients__fao56.F90` defer to the phenology module's `PHENOLOGY_METHOD_INDEX` rather than re-detecting the method. The crop coefficient module should only be responsible for:

1. Reading `KCB_ini`, `KCB_mid`, `KCB_end`, `KCB_min` (and monthly Kcb if present)
2. Setting `KCB_METHOD` based on what the phenology module already determined
3. Interpolating Kcb values during the daily loop

It should NOT independently validate or determine growth timing.

### Code Change

In `crop_coefficients_FAO56_initialize`, replace the auto-detection loop (approximately lines 345–375) with:

```fortran
use phenology, only : PHENOLOGY_METHOD_INDEX, PHENOLOGY_FAO56_GDD,   &
                      PHENOLOGY_FAO56_DATES, PHENOLOGY_NONE,          &
                      PHENOLOGY_DOY_BASED, PHENOLOGY_GDD_THRESHOLD,   &
                      GROWING_SEASON_START_GDD, GDD_INI, GDD_DEV,     &
                      GDD_MID, GDD_LATE

! ...

! Assign KCB_METHOD based on phenology module's per-LU method determination
do iIndex = lbound(KCB_METHOD, 1), ubound(KCB_METHOD, 1)

  select case ( PHENOLOGY_METHOD_INDEX(iIndex) )

    case ( PHENOLOGY_FAO56_GDD )
      ! Phenology module determined this LU uses GDD-based stages
      ! Populate GROWTH_STAGE_GDD from phenology module's arrays
      GROWTH_STAGE_GDD( GDD_PLANT, iIndex ) = GROWING_SEASON_START_GDD(iIndex)
      GROWTH_STAGE_GDD( GDD_INI,   iIndex ) = GDD_INI(iIndex)
      GROWTH_STAGE_GDD( GDD_DEV,   iIndex ) = GDD_DEV(iIndex)
      GROWTH_STAGE_GDD( GDD_MID,   iIndex ) = GDD_MID(iIndex)
      GROWTH_STAGE_GDD( GDD_LATE,  iIndex ) = GDD_LATE(iIndex)

      if ( all( KCB_l( KCB_INI:KCB_MIN, iIndex ) > 0.0_c_float ) ) then
        KCB_METHOD( iIndex ) = KCB_METHOD_GDD
      else
        call warn("FAO56_GDD phenology active for landuse "              &
          //asCharacter(LANDUSE_CODE(iIndex))                            &
          //" but KCB values are missing or zero.", lFatal=TRUE)
      endif

    case ( PHENOLOGY_FAO56_DATES )
      ! Phenology module determined this LU uses date-based stages
      if ( all( KCB_l( KCB_INI:KCB_MIN, iIndex ) > 0.0_c_float ) ) then
        KCB_METHOD( iIndex ) = KCB_METHOD_FAO56
      else
        call warn("FAO56_DATES phenology active for landuse "            &
          //asCharacter(LANDUSE_CODE(iIndex))                            &
          //" but KCB values are missing or zero.", lFatal=TRUE)
      endif

    case ( PHENOLOGY_DOY_BASED, PHENOLOGY_GDD_THRESHOLD )
      ! Simple binary growing season — check for monthly Kcb first, then staged
      if ( all( KCB_l( JAN:DEC, iIndex ) > 0.0_c_float ) ) then
        KCB_METHOD( iIndex ) = KCB_METHOD_MONTHLY_VALUES
        KCB_l( KCB_MIN, iIndex ) = minval( KCB_l(JAN:DEC, iIndex) )
        KCB_l( KCB_MID, iIndex ) = maxval( KCB_l(JAN:DEC, iIndex) )
      else if ( all( KCB_l( KCB_INI:KCB_MIN, iIndex ) > 0.0_c_float ) ) then
        KCB_METHOD( iIndex ) = KCB_METHOD_FAO56
      else
        ! No Kcb data — default to Kcb=1.0 (no reduction from reference ET)
        KCB_METHOD( iIndex ) = KCB_METHOD_NONE
      endif

    case ( PHENOLOGY_NONE )
      ! No vegetation (water, barren, impervious) — no crop coefficient needed
      ! Set minimal values so downstream code doesn't crash
      KCB_METHOD( iIndex ) = KCB_METHOD_NONE
      KCB_l( KCB_INI:KCB_MIN, iIndex ) = 0.0_c_float

    case default
      call warn("Unrecognized phenology method for landuse "             &
        //asCharacter(LANDUSE_CODE(iIndex)), lFatal=TRUE)

  end select

end do
```

### New Constant Needed

Add `KCB_METHOD_NONE` to the enum:

```fortran
enum, bind(c)
  enumerator :: KCB_METHOD_NONE = 0, KCB_METHOD_GDD = 1, &
                KCB_METHOD_MONTHLY_VALUES, KCB_METHOD_FAO56
end enum
```

### Handle KCB_METHOD_NONE in the Daily Interpolation

In `crop_coefficients_FAO56_interpolate_Kcb` (or wherever Kcb is applied daily), add:

```fortran
case ( KCB_METHOD_NONE )
  ! No crop coefficient — return Kcb_min or 0
  Kcb = KCB_l( KCB_MIN, landuse_index )
```

---

## Additional Cleanup: Remove Redundant Column Reads

Once the crop coefficient module defers to phenology, it no longer needs to read the GDD or L_* columns. These lines can be removed from `crop_coefficients_FAO56_initialize`:

```fortran
! REMOVE — phenology module handles these:
call params%get_parameters( sKey="Growing_season_start_date", slValues=SL_PLANTING_DATE )
call params%get_parameters( sKey="L_ini", fValues=L_ini_l)
call params%get_parameters( sKey="L_dev", fValues=L_dev_l)
call params%get_parameters( sKey="L_mid", fValues=L_mid_l)
call params%get_parameters( sKey="L_late", fValues=L_late_l)
call params%get_parameters( sKey="L_fallow", fValues=L_fallow_l)
call params%get_parameters( sKey="Growing_season_start_GDD", fValues=GDD_plant_l)
call params%get_parameters( sKey="GDD_ini", fValues=GDD_ini_l)
call params%get_parameters( sKey="GDD_dev", fValues=GDD_dev_l)
call params%get_parameters( sKey="GDD_mid", fValues=GDD_mid_l)
call params%get_parameters( sKey="GDD_late", fValues=GDD_late_l)
```

The crop coefficient module still needs:
```fortran
! KEEP — Kcb values are the crop coefficient module's responsibility:
call params%get_parameters( sKey="Kcb_ini", fValues=KCB_ini_l)
call params%get_parameters( sKey="Kcb_mid", fValues=KCB_mid_l)
call params%get_parameters( sKey="Kcb_end", fValues=KCB_end_l)
call params%get_parameters( sKey="Kcb_min", fValues=KCB_min_l)
call params%get_parameters( sKey="Kcb_Jan", fValues=KCB_jan )
! ... (monthly Kcb values)
```

---

## Also Remove: The Planting Date Loop

The entire `SL_PLANTING_DATE` loop (lines ~249–310) that computes `GROWTH_STAGE_DATE` from planting dates and L_* values is no longer needed. The phenology module handles growth stage progression internally. The crop coefficient module only needs to know: "what KCB_METHOD is active?" and "what are the Kcb values?" The growth stage and growth fraction come from the phenology module at runtime.

If the crop coefficient module's `crop_coefficients_FAO56_interpolate_Kcb` function currently uses `GROWTH_STAGE_DATE` to determine where on the Kcb curve the current day falls, it needs to be refactored to instead receive `growth_stage` and `growth_fraction` from the phenology module's daily output.

---

## Interaction Diagram (Proposed)

```
INITIALIZATION:
  phenology_initialize(PARAMS)
    → reads Growing_season_start_date, Growing_season_start_GDD,
      Killing_frost_temperature, L_*, GDD_* columns
    → assigns PHENOLOGY_METHOD_INDEX per LU
    → stores GDD thresholds / DOY boundaries internally

  crop_coefficients_FAO56_initialize(PARAMS)
    → reads Kcb_ini, Kcb_mid, Kcb_end, Kcb_min, Kcb_Jan..Dec
    → reads PHENOLOGY_METHOD_INDEX from phenology module
    → assigns KCB_METHOD per LU (defers to phenology for timing)

DAILY LOOP:
  phenology_update(tmin, tmax, gdd_accumulated)
    → updates growth_stage, growth_fraction, it_is_growing_season per cell

  crop_coefficients_FAO56_interpolate_Kcb(growth_stage, growth_fraction)
    → interpolates Kcb based on position in growth cycle
    → returns Kcb for use by actual_et module
```

---

## Testing

After implementing, the comprehensive phenology test case (`test/integration_tests/cs_comprehensive_phenology/`) should pass — it exercises all 5 phenology methods in a single run with a mixed table. Additionally, the McDonalds Branch example (this repo) provides a real-world test with all-GDD land uses plus the two-stage ET module active.

---

## Summary of Required Changes

| File | Change |
|------|--------|
| `src/crop_coefficients__fao56.F90` | Replace auto-detection loop with `select case (PHENOLOGY_METHOD_INDEX)`. Remove redundant column reads. Remove planting date loop. Add `KCB_METHOD_NONE`. |
| `src/crop_coefficients__fao56.F90` | Add `use phenology, only: ...` to access phenology module's public data. |
| `src/crop_coefficients__fao56.F90` | Update `interpolate_Kcb` to receive growth state from phenology module rather than computing it internally from `GROWTH_STAGE_DATE`. |
| `src/phenology.F90` | Ensure `PHENOLOGY_METHOD_INDEX`, `GROWING_SEASON_START_GDD`, `GDD_INI`, `GDD_DEV`, `GDD_MID`, `GDD_LATE` are `public`. (They may already be.) |
