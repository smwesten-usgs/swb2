# Feature Consideration: Unit-Agnostic Operation (Metric/Imperial Output)

**Date:** May 2026  
**Status:** Proposed  
**Priority:** Medium — significant user-facing improvement, moderate implementation effort  

---

## Motivation

SWB2 currently operates internally in Imperial units (inches for length, °F for temperature). Input data in metric units (mm, °C, K) is converted to Imperial on ingestion. Output is always in inches/°F. This creates friction for international users and for coupling with metric-native models.

The most immediately useful improvement: **allow output in user-selected units (mm or inches, °C or °F)** without changing the internal calculation engine.

A more ambitious goal: allow the internal engine to operate in either unit system, eliminating unnecessary conversions and making the code clearer about what units each equation expects.

---

## Current Architecture: How Units Flow Through the Code

```
INPUT (any units)
  │
  ├─ _UNITS_CELSIUS    → scale=F_PER_C, add_offset=32  (C→F)
  ├─ _UNITS_KELVIN     → sub_offset=273.15, scale=F_PER_C, add_offset=32  (K→F)
  ├─ _UNITS_MILLIMETERS → scale=1/25.4  (mm→inches)
  │
  ▼
INTERNAL STATE (always inches, °F)
  │
  ├─ snowfall threshold: FREEZING = 32.0°F
  ├─ snowmelt: converts F→C internally, result in inches
  ├─ ET (Hargreaves): converts F→C, calculates in mm, converts back to inches
  ├─ ET (Jensen-Haise): checks T≤32°F, calculates in mm, divides by 25.4
  ├─ Curve Number: Smax = (1000/CN) - 10 [inches]
  ├─ CFGI: converts F→C, snow depth inches→cm
  ├─ FAO-56 depletion: converts inches→mm for the 5mm reference
  │
  ▼
OUTPUT (always inches, °F — no conversion applied)
  │
  └─ OUTSPECS hardcodes "inches" / "degrees_fahrenheit" as unit labels
```

---

## Catalog of Unit-Dependent Code

### Tier 1: Output-Only Changes (Low Risk)

These changes add output-time conversion without touching the calculation engine.

| File | What to Change | Effort |
|------|---------------|--------|
| `output.F90` | Add `output_length_scale` and `output_temp_scale`/`output_temp_offset` factors. Apply to arrays before writing. Update OUTSPECS unit strings dynamically. | 4-6 hours |
| `model_initialize.F90` | Parse new control file directive: `OUTPUT_UNITS METRIC` or `OUTPUT_UNITS IMPERIAL` | 1 hour |
| `constants_and_conversions.F90` | Add global `OUTPUT_LENGTH_UNIT` and `OUTPUT_TEMP_UNIT` enum/variables | 1 hour |

### Tier 2: Input Conversion Changes (Medium Risk)

If the goal is to run internally in metric, the ingestion logic must be reversed.

| File | What to Change | Effort |
|------|---------------|--------|
| `model_initialize.F90` (lines 1192-1209) | Make the conversion direction configurable: if internal=metric, don't convert mm→in or C→F. If input is already in the target internal unit, set scale=1. | 3-4 hours |
| `data_catalog_entry.F90` | The `dUserScaleFactor`, `dUserAddOffset`, `dUserSubOffset` mechanism is already general. No structural change needed — just different values passed in. | 0 hours |

### Tier 3: Process Module Changes (High Risk — Core Science)

These modules contain unit-specific constants embedded in the physics equations.

| Module | File | Unit Dependencies | Change Required | Effort |
|--------|------|-------------------|-----------------|--------|
| **snowfall__original** | `snowfall__original.F90` | `FREEZING = 32.0` (°F) | Replace with configurable freezing point parameter | 1-2 hours |
| **snowmelt__original** | `snowmelt__original.F90` | Already has `imperial_units` logical! Dual paths exist. | Wire the flag to a global unit setting | 1 hour |
| **et__hargreaves_samani** | `et__hargreaves_samani.F90` | `F_to_C(rTavg)`, `mm_to_in()` on result | If internal=metric: skip F_to_C (already °C), skip mm_to_in (already mm) | 2-3 hours |
| **et__jensen_haise** | `et__jensen_haise.F90` | `rFREEZING` (32°F), `F_to_K()`, `/MM_PER_IN` | Conditional: if metric, use 0°C threshold, skip K conversion, skip mm→in | 2-3 hours |
| **actual_et__fao56** | `actual_et__fao56.F90` | `in_to_mm(reference_et0)` for depletion fraction | If internal=metric: reference_et0 is already in mm, skip conversion | 1 hour |
| **actual_et__fao56__two_stage** | `actual_et__fao56__two_stage.F90` | Same `in_to_mm()` pattern | Same fix | 1 hour |
| **runoff__curve_number** | `runoff__curve_number.F90` | `Smax = (1000/CN) - 10` [inches]; AMC thresholds in inches | **Most complex.** Options: (a) convert inflow mm→in before CN calc, or (b) use metric CN formula: `Smax = (25400/CN) - 254` [mm]. AMC thresholds × 25.4. | 3-4 hours |
| **continuous_frozen_ground_index** | `continuous_frozen_ground_index.F90` | `F_to_C()`, `fCM_PER_INCH = 2.54` | If internal=metric: T is already °C, snow depth already in mm (÷10 for cm) | 2 hours |
| **growing_degree_day** | `growing_degree_day.F90` | GDD base/max from lookup tables (assumed °F) | If internal=metric: tables must be in °C. Add documentation or auto-convert. | 2 hours |
| **awc__depth_integrated** | `awc__depth_integrated.F90` | Rooting depth in feet × 12 = inches; soil horizons in inches | If internal=metric: rooting depth in meters, horizons in mm or cm. Lookup table format changes. | 3-4 hours |
| **crop_coefficients__fao56** | `crop_coefficients__fao56.F90` | Uses `in_to_mm()` | Same pattern as actual_et modules | 1 hour |

### Tier 4: Lookup Table / User Data Implications

| Concern | Impact |
|---------|--------|
| Curve Number AMC thresholds | Currently hardcoded in inches (1.40, 0.50, 2.10, 1.10). Must be parameterized or auto-scaled. |
| GDD base/max temperatures | User lookup tables specify these. If running in °C, tables must be in °C. |
| Rooting depth tables | Currently in feet. If metric, must be in meters. |
| Soil horizon depths | Currently in inches. If metric, must be in mm or cm. |
| Interception storage max | Default 0.1 inches. Must be parameterized. |
| `swbstats2` post-processor | Already has `LENGTH_UNIT_T` with `conversion_factor_from_inches` — designed for this! |

---

## Recommended Implementation Strategy

### Option A: Output-Only Conversion (Minimal, Safest)

**Keep internal calculations in inches/°F. Apply a conversion factor at output time only.**

This is the approach `swbstats2` already takes with its `LENGTH_UNIT_T`. Extend it to the main model:

```fortran
! In output.F90, before writing each array:
if (OUTPUT_LENGTH_UNIT == UNIT_MILLIMETERS) then
  cells%array_output = values * MM_PER_IN
else
  cells%array_output = values
endif
```

For temperature outputs:
```fortran
if (OUTPUT_TEMP_UNIT == UNIT_CELSIUS) then
  cells%array_output = (values - 32.0) * C_PER_F
else
  cells%array_output = values
endif
```

**Pros:** Zero risk to calculation correctness. Minimal code changes (~1 file). Addresses the most common user request.  
**Cons:** Internal state remains in Imperial. Doesn't eliminate the conceptual confusion of "what units is this variable in?"  
**Effort:** 1-2 days.

### Option B: Configurable Internal Units (Ambitious, Higher Risk)

**Add a global `INTERNAL_UNITS` setting. Process modules check this to select the correct formula variant or skip unnecessary conversions.**

Implementation pattern:
```fortran
! In constants_and_conversions.F90:
integer(c_int), public :: INTERNAL_LENGTH_UNIT = UNIT_INCHES  ! default
integer(c_int), public :: INTERNAL_TEMP_UNIT = UNIT_FAHRENHEIT  ! default

! In snowfall__original.F90:
real(c_float) :: freezing_threshold
if (INTERNAL_TEMP_UNIT == UNIT_FAHRENHEIT) then
  freezing_threshold = 32.0_c_float
else
  freezing_threshold = 0.0_c_float
endif
```

**Pros:** Cleaner long-term architecture. Eliminates unnecessary F→C→F round-trips in ET modules. Makes the code self-documenting about units.  
**Cons:** Touches ~12 process modules. Risk of subtle bugs if one module doesn't get updated. Lookup tables must match the internal unit system. Requires extensive regression testing.  
**Effort:** 2-3 weeks.

### Option C: Hybrid (Recommended)

1. **Phase 1 (immediate):** Implement output-only conversion (Option A). This gives users mm/°C output with zero risk to calculations.
2. **Phase 2 (later):** Refactor process modules to accept a unit-system parameter, following the pattern already established in `snowmelt__original.F90`. Do this module-by-module with regression tests after each.

---

## Effort Estimate Summary

| Scope | Files Modified | Effort | Risk |
|-------|---------------|--------|------|
| **Output-only conversion** | 3 files (`output.F90`, `model_initialize.F90`, `constants_and_conversions.F90`) | **1-2 days** | Very Low |
| **+ Input flexibility** (skip conversion if already in target units) | +1 file (`model_initialize.F90`) | **+0.5 days** | Low |
| **+ Full internal metric option** | +12 process module files | **+2-3 weeks** | Medium-High |
| **+ Lookup table format support** (metric tables) | Documentation + validation logic | **+3-5 days** | Medium |
| **Regression testing** (for full internal metric) | New test cases comparing metric vs imperial runs | **+3-5 days** | — |

### Total for Recommended Hybrid Approach:
- **Phase 1 (output conversion):** 1-2 days → immediate user value
- **Phase 2 (full internal metric):** 3-4 weeks including testing → long-term architectural improvement

---

## Control File Syntax (Proposed)

```
# Phase 1: Output units only
OUTPUT_LENGTH_UNITS  MILLIMETERS    # or INCHES (default)
OUTPUT_TEMPERATURE_UNITS  CELSIUS   # or FAHRENHEIT (default)

# Phase 2: Internal calculation units (future)
INTERNAL_LENGTH_UNITS  MILLIMETERS  # or INCHES (default)
INTERNAL_TEMPERATURE_UNITS  CELSIUS # or FAHRENHEIT (default)
```

---

## Key Observations

1. **`snowmelt__original.F90` already has the pattern** — it accepts an `imperial_units` logical and has dual code paths. This proves the concept works and provides a template for other modules.

2. **The mass balance modules are unit-agnostic** — `mass_balance__soil.F90`, `mass_balance__snow.F90`, etc. just add/subtract values. They don't care about units. This means the core water balance engine needs no changes.

3. **The curve number is the hardest module** — the SCS formula constants (1000, 10) are inherently in inches. The metric equivalent (25400, 254) is well-known but requires careful validation.

4. **ET modules already do internal conversions** — Hargreaves and Jensen-Haise convert F→C and mm→in internally. If running in metric, these conversions become no-ops, which is actually simpler and slightly faster.

5. **`swbstats2` already has the output conversion infrastructure** — the `LENGTH_UNIT_T` type with `conversion_factor_from_inches` shows this was anticipated.

---

## Risks and Mitigations

| Risk | Mitigation |
|------|-----------|
| Subtle unit mismatch in one module | Regression test: run same scenario in both unit systems, compare results (should be identical within floating-point tolerance) |
| User confusion about lookup table units | Add `LOOKUP_TABLE_UNITS` directive; validate/convert on read |
| GDD output meaningless if mixing °F and °C | GDD output units must track temperature units (degree-day-F vs degree-day-C) |
| Breaking existing control files | Default remains Imperial; metric is opt-in |
| Performance impact of runtime unit checks | Negligible — one branch per module per timestep, not per cell |

---

## Relationship to Other Planned Changes

- **CI pipeline (Phase 4 of improvement plan):** Essential before attempting Phase 2. Need automated regression tests to catch unit bugs.
- **test-drive migration:** Unit tests for each process module should include metric-mode variants.
- **PROJ modernization:** Orthogonal — coordinate system units (meters vs feet) are separate from the water-balance length units.
