# Assessment: Mixed Precision and iso_c_binding Usage in SWB2

**Date:** June 2026  
**Context:** Developer question — is the mixed `c_float`/`c_double` usage causing the min/max problems, and is it worth changing?

---

## Short Answer

**Yes, the mixed precision is the direct cause of the min/max errors.** But it's a **design choice that makes scientific sense** — it just needs a few explicit conversions at the boundaries where `c_float` and `c_double` interact.

**No, do not change to all-double or all-float.** The current approach is rational. Fix the ~18 boundary expressions and move on.

---

## How the Mixed Precision Works Today

The codebase has a clear (and defensible) precision policy:

### State variables stored as `c_float` (single precision)

These are quantities where extra precision doesn't help — the input data is inherently imprecise:

- `gross_precip`, `rainfall`, `snowfall`, `tmin`, `tmax` — gridded climate inputs (at best ~0.1mm accuracy)
- `interception_storage`, `snow_storage` — storage pools driven by rough empirical parameters
- `runoff`, `infiltration`, `net_infiltration` — fluxes that depend on coarse curve numbers
- `soil_storage_max`, `awc`, `curve_num_adj` — static properties from GIS layers
- `crop_coefficient_kcb`, `pervious_fraction` — tabular lookup values

### State variables stored as `c_double` (double precision)

These are quantities involved in subtractive cancellation or cumulative error:

- `reference_et0` — used in subtractions (actual_et = min(et0, storage))
- `soil_storage` — updated daily by addition/subtraction over years (cumulative)
- `actual_et_interception`, `actual_et_soil`, `actual_et_impervious` — differences between similar quantities
- `surface_storage` — same rationale as soil_storage
- `soil_moisture_deficit` — explicitly tracking a deficit (near-zero subtractive quantity)
- `adjusted_depletion_fraction_p` — sensitive fraction in FAO-56 ET calculation
- FAO-56 two-stage coefficients (`Ks`, `Kr`, `Ke`) — coefficients between 0 and 1 in sensitive calculations

### The logic

Inputs and coarse physical quantities → `c_float` (saves memory on large grids, matches input precision)  
Accumulating/subtractive quantities → `c_double` (prevents drift over long simulations)

**This is a perfectly valid scientific computing strategy.** Climate/hydrology models routinely use mixed precision for exactly this reason. MODFLOW uses single precision for heads but double for budgets, for example.

---

## Where the Pain Points Are

The problems occur at the ~18 locations where a `c_double` variable interacts with a `c_float` variable inside `min` or `max`:

```fortran
! mass_balance__interception.F90, line 37:
real (c_double), intent(in)   :: reference_et0       ! double
real (c_float), intent(inout) :: interception_storage ! float

actual_et_interception = min( reference_et0, interception_storage )
!                             ^^^^^^^^^^^    ^^^^^^^^^^^^^^^^^^^^^
!                             c_double       c_float  → ERROR under -std=f2018
```

The Fortran 2018 standard says: "all arguments to MIN/MAX must have the same type and kind." The old `-std=gnu` mode silently promoted the narrower argument. Under strict mode, it's either a fatal error (gfortran) or a warning (ifx).

---

## The Fix: Explicit Conversion at Boundaries

The fix is NOT to change the type system. It's to add explicit `real(..., kind)` at the 18 interaction points:

```fortran
! Option A: promote float to double (preserves precision of the double):
actual_et_interception = min( reference_et0, real(interception_storage, c_double) )

! Option B: demote double to float (acceptable when the result is stored in float):
interception_storage = max( 0.0_c_float, real(interception_storage - actual_et_interception, c_float) )
```

**Which to choose?** Look at what the *result* is assigned to:
- If the result goes into a `c_double` variable → promote the `c_float` argument up
- If the result goes into a `c_float` variable → it's acceptable to do the comparison in float

In `mass_balance__interception.F90`:
```fortran
! actual_et_interception is c_double, so promote interception_storage up:
actual_et_interception = min( reference_et0, real(interception_storage, c_double) )

! interception_storage is c_float, so keep the max in float:
interception_storage = real(max( 0.0_c_double, real(interception_storage, c_double) - actual_et_interception ), c_float)
! OR simply:
interception_storage = max( 0.0_c_float, interception_storage - real(actual_et_interception, c_float) )
```

---

## Assessment: iso_c_binding Types vs Native Fortran Types

### Does the code actually need iso_c_binding?

**Yes, unambiguously.** The code has ~35 `bind(c)` interfaces to the NetCDF C library and 1 to PROJ4. Data arrays declared with `c_float`/`c_double` are passed directly to `nc_put_var_float`/`nc_put_var_double`. Using native Fortran kinds (`real(4)`, `real(8)`) would be formally non-portable — on any platform where `c_float /= kind(1.0)` or `c_double /= kind(1.0d0)`, the C interop would break.

In practice, `c_float == 4` and `c_double == 8` on every platform you'll ever target. But:

1. The code is correct and portable as-is
2. Changing ~5000 declarations to `real(sp)`/`real(dp)` gains nothing
3. It would break all the C interop type-matching

### Could you define aliases?

You could add:
```fortran
integer, parameter :: sp = c_float    ! single precision (same as c_float)
integer, parameter :: dp = c_double   ! double precision (same as c_double)
```

This is cosmetic sugar. It gains readability for non-C-interop modules but adds confusion about whether `sp` == `c_float` (it does). Not recommended unless you find the `c_float`/`c_double` names distracting.

---

## Assessment: Should You Switch to All-Double?

| Consideration | All-double | Keep mixed |
|--------------|-----------|------------|
| Memory (100M cell grid) | 2× for all float arrays (~60 arrays × 2× = significant) | Current |
| Cache performance | Worse (2× data through cache for same work) | Current |
| NetCDF output file size | Larger (unless you write as float anyway) | Current |
| Numerical correctness | Overkill for precip/temp, helpful for storage | Targeted doubles where needed |
| Compilation errors | Zero min/max issues (everything same kind) | 18 explicit conversions needed |
| Scientific defensibility | "Wasteful but safe" | "Appropriate precision where it matters" |

**Verdict:** The current approach is correct engineering. The 18 conversion sites are a one-time 1-hour fix. Switching to all-double would:
- Double memory usage for the grid state (matters for large domains)
- Waste cache lines on quantities that have 3-4 significant digits of physical meaning
- Require changing every NetCDF write call that currently uses `nc_put_var_float`
- NOT make the code more correct (you can't add precision to 0.1mm rain gauge data)

---

## Should You Switch to All-Float?

**No.** The variables that are `c_double` were chosen deliberately:

- `soil_storage` accumulates ±changes daily for 30+ year simulations. After 10,000+ additions/subtractions, single-precision drift becomes measurable (~1mm over a decade).
- `reference_et0` is subtracted from storage values — if both are near 3.5mm and differ by 0.01mm, single precision can't resolve the difference.
- The FAO-56 two-stage method has depletion fractions and stress coefficients that multiply by each other in chains — precision loss compounds.

---

## Recommendations

1. **Keep the current mixed-precision design.** It's scientifically sound.

2. **Fix the 18 min/max boundary points** with explicit `real(..., kind)` conversions. This is 1 hour of work and makes both compilers happy. See `design/static_analysis_remediation_plan.md`, Phase 1a.

3. **Do NOT mass-refactor types.** The ROI is negative — huge effort, increased memory, no correctness gain.

4. **Keep `iso_c_binding` types.** They serve a real purpose (C interop) and are the most portable way to express kinds in Fortran. They happen to also be used everywhere else for consistency, which is fine.

5. **Consider adding a brief comment** at the top of `model_domain.F90` documenting the precision policy:
   ```fortran
   ! Precision policy:
   !   c_float  — inputs, static properties, non-cumulative fluxes
   !   c_double — cumulative storages, ET quantities involved in subtractions, sensitive coefficients
   ```

---

## The 18 Fixes (Quick Reference)

All are `min`/`max` calls mixing `c_float` and `c_double`. The fix for each is one of:
- `min(dbl_var, real(flt_var, c_double))` — when result is assigned to double
- `max(real(dbl_var, c_float), flt_var)` — when result is assigned to float
- `max(0.0_c_double, dbl_expression)` — when the literal's kind was wrong

After fixing these, both `gfortran -std=f2018` and `ifx /stand:f18` will be satisfied.
