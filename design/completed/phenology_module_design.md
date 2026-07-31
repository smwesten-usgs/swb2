# Unified Phenology Module Design

**Date:** July 2026
**Status:** Proposed
**Affects:** `growing_season.F90`, `crop_coefficients__fao56.F90`, `interception__bucket.F90`, `actual_et__fao56__two_stage.F90`, `rooting_depth__FAO56.F90`, `runoff__curve_number.F90`

---

## Motivation

SWB2 currently has three independent mechanisms that each answer the question "is vegetation actively growing right now?" They live in different modules, use different input columns, and compete for control of a single boolean flag (`it_is_growing_season`). This creates cognitive dissonance for users and makes the model's behavior opaque when multiple related parameters are present in lookup tables.

The problem intensifies as we add GDD-based crop coefficient curves and (eventually) gridded LAI inputs. Each new mechanism adds another "growing season start" concept with its own naming conventions and interaction rules.

---

## Problem Statement

### Current Architecture

Three modules independently define when growth is active:

| Module | Trigger mechanism | Key input columns |
|--------|-------------------|-------------------|
| `growing_season.F90` | DOY threshold or GDD accumulation / killing frost | `First_day_of_growing_season`, `Last_day_of_growing_season`, `GDD_first_day_of_growing_season`, `Killing_frost_temperature` |
| `crop_coefficients__fao56.F90` | Position on Kcb curve (date-based) | `Planting_date`, `L_ini`, `L_dev`, `L_mid`, `L_late`, `L_fallow` |
| `crop_coefficients__fao56.F90` | Position on Kcb curve (GDD-based) | `GDD_plant`, `GDD_ini`, `GDD_dev`, `GDD_mid`, `GDD_late` |

When `CROP_COEFFICIENT FAO56` is specified, a procedure pointer swap silently replaces the growing season logic: `update_growing_season` is redirected from `model_update_growing_season` (which calls `growing_season.F90`) to `model_update_growing_season_crop_coefficient_FAO56` (which sets `it_is_growing_season = Kcb > Kcb_min`).

### Consequences

1. **Silent parameter ignoring.** Users may carefully parameterize `First_day_of_growing_season` columns, but when FAO-56 crop coefficients are active, those columns are never read. No warning is emitted.

2. **Naming confusion.** `Planting_date` and `First_day_of_growing_season` represent the same physical event (onset of growth) but use different names, live in different modules, and may carry different values.

3. **Single boolean is insufficient.** Downstream consumers (interception, curve number, ET, rooting depth, plant height) have varying needs — some need a binary signal, others need continuous growth progression.

4. **Extensibility is poor.** Adding GDD-based Kcb curves or gridded LAI inputs requires threading yet another "growing season" mechanism through the same tangled pointer-swap system.

5. **Mixed-method tables are ambiguous.** A single lookup table may contain columns for DOY-based, date-based, and GDD-based definitions simultaneously. The model silently picks one based on completeness heuristics in `crop_coefficients_FAO56_initialize`, with no user-facing explanation.



---

## Proposed Architecture: Unified Phenology Provider

### Core Concept

Replace the competing mechanisms with a single **phenology provider module** (`phenology.F90`) that:

1. Owns all growth-timing logic (DOY, GDD, date-based, gridded)
2. Produces a **standardized output contract** consumed by all downstream modules
3. Reports at initialization which method and columns are active per land use
4. Eliminates the procedure-pointer swap for growing season control

### Output Contract

The phenology provider emits the following per-cell, per-day state:

```fortran
! --- Phenology provider output (per cell, updated daily) ---
real(c_float)    :: growth_fraction          ! 0.0 (dormant) to 1.0 (full maturity)
logical(c_bool)  :: it_is_growing_season     ! growth_fraction > 0 (or > small threshold)
integer(c_int)   :: growth_stage             ! enum: DORMANT, INI, DEV, MID, LATE, FALLOW
real(c_float)    :: leaf_area_index          ! when available; derived or direct
```

All consumers read from these fields. No consumer independently determines growth timing.

### Growth Fraction Semantics

The `growth_fraction` value follows a standardized trajectory through the growing season:

```
growth_fraction
    1.0 ─────────────────────────────────── ┐
        /                                 \  │ MID stage
       /  DEV                       LATE   \ │
      /                                     \│
    0.0 ───┐         INI            ┌────── 0.0
           │                        │
        DORMANT                  FALLOW/DORMANT
```

- **DORMANT/FALLOW:** growth_fraction = 0.0
- **INI:** growth_fraction = small positive value (e.g., 0.05–0.10) representing emergence
- **DEV:** growth_fraction ramps linearly from INI value to 1.0
- **MID:** growth_fraction = 1.0
- **LATE:** growth_fraction declines from 1.0 toward 0.0 (or toward a senescent minimum)

For the simple DOY-based case, growth_fraction snaps between 0.0 and 1.0 at the defined boundaries (preserving current binary behavior).

### Phenology Methods

```
PHENOLOGY_METHOD:

  DOY_BASED            → Growing_season_start_date + Growing_season_end_date (binary on/off)
  GDD_THRESHOLD        → Growing_season_start_GDD + Killing_frost_temperature (binary on/off)
  FAO56_DATES          → Growing_season_start_date + L_ini + L_dev + L_mid + L_late
  FAO56_GDD            → Growing_season_start_GDD + GDD_ini + GDD_dev + GDD_mid + GDD_late + Killing_frost_temperature
  GRIDDED_LAI          → Time-varying LAI grid → normalized to growth_fraction
  GRIDDED_PHENOLOGY    → Preprocessed growth_fraction grid (0–1), read directly
```

Method selection is **explicit only**, via control file directive. There is no auto-detection.

### Explicit Method Selection (No Auto-Detection)

The user must declare the phenology method in the control file:

```
PHENOLOGY_METHOD  FAO56_GDD
```

Valid options: `DOY_BASED`, `GDD_THRESHOLD`, `FAO56_DATES`, `FAO56_GDD`, `GRIDDED_LAI`, `GRIDDED_PHENOLOGY`.

**Rationale for no auto-detection:** SWB2's historical approach of "take what you need from the table" is flexible but opaque. Auto-detection requires the user to understand the priority rules to predict what the model will do. Explicit declaration eliminates this: the user says what they want, the model does exactly that, and if required columns are missing the model errors with a clear message.

**Error messaging:** When `PHENOLOGY_METHOD` is specified, the model validates that the required columns are present:

```
ERROR: PHENOLOGY_METHOD FAO56_DATES requires columns 'Planting_date', 'L_ini',
       'L_dev', 'L_mid', 'L_late' in the lookup table. Column 'L_dev' was not found.
```

If extra columns are present that are not used by the selected method, the model logs a notice:

```
INFO: Columns 'First_day_of_growing_season', 'Last_day_of_growing_season' are present
      in the lookup table but are not used by PHENOLOGY_METHOD FAO56_GDD. These columns
      will be ignored.
```

This gives the user full awareness without requiring them to memorize priority rules.



---

## Module Hierarchy and Consumer Integration

### New Module Structure

```
phenology.F90  (unified provider — NEW, growing_season.F90 is DELETED)
  ├── phenology_doy_based()           ← simple binary on/off from DOY
  ├── phenology_gdd_threshold()       ← GDD accumulation / killing frost
  ├── phenology_fao56_dates()         ← growth stage logic (extracted from crop_coefficients__fao56.F90)
  ├── phenology_fao56_gdd()           ← GDD stage logic (extracted from crop_coefficients__fao56.F90)
  ├── phenology_gridded_lai()         ← FUTURE: reads LAI grids via DATA_CATALOG
  └── phenology_gridded_fraction()    ← FUTURE: reads preprocessed growth_fraction grids

crop_coefficients__fao56.F90  (REFACTORED: pure Kcb interpolation only)
  ← receives growth_stage, growth_fraction FROM phenology provider
  ← no longer owns growth timing logic
  ← retains: Kcb interpolation, Kcb_max calculation, monthly Kcb lookup

interception__bucket.F90  (MODIFIED: uses growth_fraction)
  ← receives growth_fraction (continuous) or it_is_growing_season (binary)
  ← can optionally interpolate storage max between growing/nongrowing extremes

actual_et__fao56__two_stage.F90  (MODIFIED: receives state from phenology)
  ← receives growth_fraction for plant height scaling
  ← receives it_is_growing_season for gating transpiration

rooting_depth__FAO56.F90  (MODIFIED: receives growth_fraction)
  ← uses growth_fraction to interpolate Zr_min → Zr_max

runoff__curve_number.F90  (MINIMAL CHANGE)
  ← continues to use it_is_growing_season (binary) as today
```

### How Consumers Use the Phenology Output

| Consumer | Uses | How |
|----------|------|-----|
| Interception (bucket) | `growth_fraction` | Interpolate storage max: `max = nongrowing + growth_fraction * (growing - nongrowing)` |
| Interception (bucket, simple) | `it_is_growing_season` | Binary switch (current behavior, backwards-compatible option) |
| Curve number | `it_is_growing_season` | Binary: growing-season CN vs. dormant CN (unchanged) |
| Kcb interpolation | `growth_stage` + stage boundaries | Position-based lookup on the Kcb curve |
| Rooting depth | `growth_fraction` | `Zr = Zr_min + growth_fraction * (Zr_max - Zr_min)` |
| Plant height | `growth_fraction` | `h = h_min + growth_fraction * (h_max - h_min)` |
| FAO-56 two-stage ET | `it_is_growing_season`, `Kcb` | Gates transpiration; Kcb comes from crop coeff module |
| `few` (exposed soil fraction) | `growth_fraction` or `LAI` | Could replace Kcb-derived proxy with direct LAI-based `fc` |

### Interaction with Existing Procedure Pointer System

The current pointer swap (`update_growing_season => model_update_growing_season_crop_coefficient_FAO56`) is **eliminated**. Instead:

- A single procedure pointer `update_phenology` replaces `update_growing_season`
- It is set once at initialization based on the `PHENOLOGY_METHOD` control file directive
- It always produces the full output contract (growth_fraction, it_is_growing_season, growth_stage)
- `CROP_COEFFICIENT FAO56` no longer implies or overrides the phenology method — it only controls Kcb interpolation

This means `CROP_COEFFICIENT` and `PHENOLOGY_METHOD` become orthogonal directives:

| PHENOLOGY_METHOD | CROP_COEFFICIENT | Behavior |
|---|---|---|
| DOY_BASED | NONE | Basic T-M mode. Binary growing season. Kcb = 1.0 |
| DOY_BASED | FAO56 | Kcb curve active, but growing season for interception/CN uses DOY. Kcb stages must be defined independently. |
| FAO56_DATES | FAO56 | Kcb curve and growing season both driven by planting_date + stages. Fully consistent. (Most common FAO-56 configuration.) |
| FAO56_GDD | FAO56 | Same as above but GDD-driven. |
| GRIDDED_LAI | FAO56 | LAI provides growth_fraction; Kcb interpolated from that. |
| GRIDDED_LAI | LAI_DERIVED | Future: Kcb computed directly from LAI (see "LAI as Basis for Crop Coefficients" below). |



---

## Breaking Changes: Analysis and Justification

### What Breaks

1. **The implicit pointer swap is removed.** Specifying `CROP_COEFFICIENT FAO56` no longer automatically overrides the growing season mechanism. Users must now explicitly specify a phenology method via `PHENOLOGY_METHOD`.

2. **`Growing_season_start` / `Growing_season_end` may no longer be required.** Currently `growing_season_initialize` may error if these columns are missing. Under the new design, they're only needed if `PHENOLOGY_METHOD DOY_BASED` (or `GDD_THRESHOLD`) is active.

3. **Interception timing may shift slightly.** For users who had both `First_day_of_growing_season = 04/15` and `Planting_date = 05/01`, the old behavior was: interception switches on 05/01 (because FAO-56 overrode the growing season). Under the new design, the user explicitly chooses: `PHENOLOGY_METHOD FAO56_DATES` means interception keys off the planting date (same as old behavior). `PHENOLOGY_METHOD DOY_BASED` with `CROP_COEFFICIENT FAO56` would mean interception switches on 04/15 while Kcb starts on 05/01 — a deliberate user choice rather than an accident.

4. **Control file requires a new directive.** Adding `PHENOLOGY_METHOD` as a required control file directive. Existing control files will error with a clear message telling the user what to add.

5. **`growing_season.F90` is deprecated/removed.** Its logic moves into `phenology.F90`. Any external code that `use`s `growing_season` would need updating — but since SWB2 is self-contained, this only affects the internal build.

### What Doesn't Break

- **Lookup table column names remain the same.** `Planting_date`, `L_ini`, `First_day_of_growing_season`, etc. are all still recognized.
- **Simple T-M runs with DOY-based growing season require one new line.** Adding `PHENOLOGY_METHOD DOY_BASED` to the control file. Behavior is otherwise identical.
- **FAO-56 runs with complete date-based or GDD-based columns require one new line.** Adding `PHENOLOGY_METHOD FAO56_DATES` (or `FAO56_GDD`) to the control file. Behavior is otherwise identical to today.
- **Output variables are unchanged.** `it_is_growing_season` remains in NetCDF output.
- **Monthly Kcb values continue to work.** They're a special case where growth_stage isn't meaningful; growth_fraction can be derived from the monthly Kcb values if needed.

### Justification for Breaking

The breaks are justified because:

1. **The current behavior is silently incorrect for some configurations.** The pointer swap means users can't have independent interception timing and Kcb timing even if they want to.
2. **Silent parameter ignoring violates the principle of least surprise.** A user who parameterizes columns that are then ignored is worse off than a user who gets an explicit error or warning.
3. **The migration is low-effort.** Existing users add one line (`PHENOLOGY_METHOD ...`) to their control file. No table changes required. The error message tells them exactly what to add.
4. **Version boundary is appropriate.** A v2.5 or v3.0 release can document these changes clearly.

### Migration Guide for Existing Users

| Current setup | Action needed |
|---|---|
| T-M with DOY growing season, no crop coefficients | Add `PHENOLOGY_METHOD DOY_BASED`. Rename `First_day_of_growing_season` → `Growing_season_start_date`, `Last_day_of_growing_season` → `Growing_season_end_date` in lookup table. |
| FAO-56 with `Planting_date` + `L_*` columns | Add `PHENOLOGY_METHOD FAO56_DATES`. Rename `Planting_date` → `Growing_season_start_date`. Remove `L_fallow` (end is implicit from sum of stage lengths). |
| FAO-56 with GDD columns (`GDD_plant`, etc.) | Add `PHENOLOGY_METHOD FAO56_GDD`. Rename `GDD_plant` → `Growing_season_start_GDD`. |
| Mixed table with both DOY columns and FAO-56 columns | Choose the method you want. Remove or ignore the extra columns — the model will tell you they're unused. |

### Unified Parameter Names (replaces all legacy synonyms)

The following parameter names are the **only** recognized column headers for the phenology module. Legacy synonyms (`First_day_of_growing_season`, `Planting_date`, `GDD_plant`, `GDD_first_day_of_growing_season`, etc.) are no longer supported.

| Column | Type | Methods that use it |
|---|---|---|
| `Growing_season_start_date` | mm/dd string or integer DOY | DOY_BASED, FAO56_DATES |
| `Growing_season_end_date` | mm/dd string or integer DOY | DOY_BASED only |
| `Growing_season_start_GDD` | float (degree-days) | GDD_THRESHOLD, FAO56_GDD |
| `Killing_frost_temperature` | float (temperature units matching input) | GDD_THRESHOLD, FAO56_GDD |
| `L_ini` | integer (days) | FAO56_DATES |
| `L_dev` | integer (days) | FAO56_DATES |
| `L_mid` | integer (days) | FAO56_DATES |
| `L_late` | integer (days) | FAO56_DATES |
| `GDD_ini` | float (degree-days) | FAO56_GDD |
| `GDD_dev` | float (degree-days) | FAO56_GDD |
| `GDD_mid` | float (degree-days) | FAO56_GDD |
| `GDD_late` | float (degree-days) | FAO56_GDD |

**Design rationale:**
- `Growing_season_start_date` replaces both `First_day_of_growing_season` and `Planting_date` — they are physically the same concept (when vegetation becomes active).
- `Growing_season_end_date` replaces `Last_day_of_growing_season` — only needed for DOY_BASED where there is no stage-length information.
- `Growing_season_start_GDD` replaces both `GDD_first_day_of_growing_season` and `GDD_plant` — the thermal sum at which growth initiates.
- `L_fallow` is removed. The growing season end under FAO56_DATES is implicitly `Growing_season_start_date + L_ini + L_dev + L_mid + L_late`. The fallow period is simply the remainder of the year.
- `Growing_season_start_date` accepts either mm/dd format (e.g., `03/17`, `3-15`) or integer DOY (e.g., `91`).



---

## LAI Applications: What Leaf Area Index Enables

### Overview

Leaf Area Index (LAI, m² leaf / m² ground) is the single most informative variable for characterizing vegetation state from a water-balance perspective. If LAI is available — either from remote sensing products (MODIS, Sentinel-2, Landsat), land surface model output, or crop models — it can drive or replace several parameterizations that currently depend on empirical lookup tables.

### Direct Applications of LAI in SWB2

#### 1. Canopy Cover Fraction (fc)

**Current approach:** Derived from Kcb via an empirical power law (Eq. 76, FAO-56):

```fortran
fc = ((Kcb - Kcb_min) / (Kcb_mid - Kcb_min)) ^ (1 + 0.5*h)
```

**LAI-based approach:** Beer's Law extinction:

```fortran
fc = 1.0 - exp(-k_ext * LAI)
```

Where `k_ext` is the canopy light extinction coefficient (typically 0.4–0.7 for crops, 0.3–0.5 for forests). This is physically based and well-established in the literature.

**Benefit:** Removes the circular dependency where fc depends on Kcb which depends on growth stage. LAI provides fc directly from observable vegetation state.

#### 2. Fraction of Exposed and Wetted Soil (few)

**Current approach:** `few = 1 - fc` (with fc derived from Kcb as above).

**LAI-based approach:** `few = exp(-k_ext * LAI)` — the complement of fc.

**Benefit:** Same as above. Direct, physically interpretable.

#### 3. Interception Capacity

**Current approach:** Binary switch between `interception_storage_max_growing` and `interception_storage_max_nongrowing`.

**LAI-based approach:** Interception storage capacity scales with LAI:

```fortran
interception_storage_max = S_leaf * LAI
```

Where `S_leaf` is the specific leaf storage capacity (typically 0.1–0.3 mm per unit LAI for broadleaves, less for needleleaves). This provides a continuous, physically-based interception capacity that tracks seasonal vegetation development.

**Benefit:** Eliminates the abrupt step function. A forest leafing out in spring gradually increases interception capacity rather than jumping from bare to full on a single day.

#### 4. Growth Fraction Derivation

**Current approach:** growth_fraction computed from position on date-based or GDD-based curve.

**LAI-based approach:** `growth_fraction = LAI / LAI_max` where LAI_max is from lookup table.

**Benefit:** Directly observable; accounts for year-to-year variability in phenology (early/late springs, drought stress). No assumption that growth follows the same calendar or GDD trajectory every year.

#### 5. Rooting Depth

**Current approach:** Rooting depth scales with time or growth stage (linear ramp from Zr_min to Zr_max over the growing season).

**LAI-based approach:** Root growth tracks shoot growth. `Zr = Zr_min + (LAI / LAI_max) * (Zr_max - Zr_min)`.

**Benefit:** Root development responds to actual above-ground growth conditions rather than a fixed schedule. In a late spring, roots develop later because LAI develops later.

#### 6. Plant Height

**Current approach:** Scaled from Kcb relative position (in the two-stage module).

**LAI-based approach:** `h = h_min + (LAI / LAI_max) * (h_max - h_min)` or a lookup curve.

**Benefit:** Consistent with the rest of the LAI-driven parameterizations.

### Data Sources for LAI

| Source | Resolution | Temporal | Latency | Notes |
|--------|-----------|----------|---------|-------|
| MODIS MCD15A2H | 500m, 8-day | 2000–present | ~2 weeks | Widely used, gap-filled products available |
| Sentinel-2 LAI (Copernicus) | 10–20m, ~5 days | 2015–present | days | High resolution, good for field-scale |
| Landsat (derived) | 30m, 16 days | 1984–present | days–weeks | Long record, empirical LAI derivation |
| NLDAS/GLDAS | ~12km, daily | varies | varies | Land surface model output, pre-processed |
| Crop models (DSSAT, AquaCrop) | field-scale, daily | simulated | N/A | For coupled modeling studies |

For SWB2's typical grid resolution (100m–1km), MODIS and Sentinel-2 are the most practical sources. A preprocessing step would resample LAI to the SWB2 model grid and interpolate temporally (8-day → daily).



---

## Future Vision: LAI as the Basis for Crop Coefficients

### Concept

Rather than using FAO-56's empirical piecewise-linear Kcb curve (defined by L_ini, L_dev, L_mid, L_late and three Kcb values), derive crop coefficients **directly from LAI**. This eliminates the growth stage machinery entirely for land uses where LAI data is available.

### Physical Basis

The basal crop coefficient Kcb represents the ratio of crop transpiration to reference ET under non-stressed conditions. Transpiration scales with the transpiring leaf area:

```
Kcb = Kcb_full * (1 - exp(-k_t * LAI))
```

Where:
- `Kcb_full` is the maximum crop coefficient at full cover (similar to Kcb_mid in FAO-56)
- `k_t` is a transpiration extinction coefficient (typically 0.5–0.7)
- The exponential form captures the diminishing return of additional leaf layers

This relationship is well-established in the literature:
- Allen and Pereira (2009) "Estimating crop coefficients from fraction of ground cover and height" (Agricultural Water Management)
- Er-Raki et al. (2007) "Combining FAO-56 model and ground-based remote sensing to estimate water consumptions of wheat crops" (Agricultural Water Management)

### What This Replaces

| Current FAO-56 requirement | LAI-based replacement |
|---|---|
| `Planting_date` | Not needed — LAI timeseries implicitly defines season start |
| `L_ini`, `L_dev`, `L_mid`, `L_late`, `L_fallow` | Not needed — LAI timeseries defines the growth trajectory |
| `Kcb_ini`, `Kcb_mid`, `Kcb_end` | Replaced by `Kcb_full` + `k_t` (2 parameters vs. 3+5=8) |
| Growth stage date tracking | Not needed — LAI provides continuous state directly |
| GDD accumulation (for GDD-based Kcb) | Not needed if LAI timeseries is available |
| `GDD_plant`, `GDD_ini`, `GDD_dev`, `GDD_mid`, `GDD_late` | Not needed |

**Net reduction:** From 8–13 empirical parameters per land use down to 2–3 (`Kcb_full`, `k_t`, optionally `LAI_max`).

### The Two-Component Form

Following the FAO-56 dual crop coefficient approach but with LAI-based parameterization:

```fortran
! --- Transpiration component ---
fc = 1.0 - exp(-k_ext * LAI)          ! canopy cover fraction
Kcb = Kcb_full * fc                    ! simplified: Kcb proportional to cover

! --- Evaporation component (unchanged from current two-stage) ---
few = 1.0 - fc                         ! exposed soil fraction
Ke = Kr * (Kcb_max - Kcb) * few       ! surface evaporation coefficient (Eq. 71)

! --- Combined ---
ETc = (Kcb * Ks + Ke) * ET0           ! actual crop ET
```

The evaporation component (the two-stage module's surface energy balance for bare soil) remains largely unchanged — it just receives `few` from LAI rather than from the Kcb-derived proxy.

### When This Makes Sense vs. When It Doesn't

**LAI-based Kcb is appropriate for:**
- Large-domain studies where detailed crop parameterization isn't feasible
- Retrospective analyses where remote-sensing LAI products are available
- Forested areas where the FAO-56 "crop" concept doesn't really apply
- Mixed land uses where tabulated growth stages don't match actual conditions
- Climate-change studies where phenology shifts and fixed dates are inappropriate

**Traditional FAO-56 Kcb curves remain appropriate for:**
- Irrigation scheduling applications where future Kcb must be predicted
- Single-field studies with well-characterized crops
- Prospective/planning scenarios without historical LAI data
- Situations where daily LAI data isn't available at adequate resolution

### Hybrid Approach: GDD-Driven LAI Model

A middle path that doesn't require external LAI grids:

```fortran
! Simple GDD-driven LAI model (per land use)
if (growth_stage == DORMANT .or. growth_stage == FALLOW) then
    LAI = LAI_min
elseif (growth_stage == INI) then
    LAI = LAI_min + (LAI_max - LAI_min) * 0.05   ! emergence
elseif (growth_stage == DEV) then
    LAI = LAI_min + (LAI_max - LAI_min) * dev_fraction  ! ramp up
elseif (growth_stage == MID) then
    LAI = LAI_max
elseif (growth_stage == LATE) then
    LAI = LAI_max * (1.0 - late_fraction * senescence_rate)  ! decline
endif
```

This still uses the growth stage machinery (from GDD or dates) but produces LAI as an intermediate variable. The benefit: **all downstream parameterizations (fc, few, interception, Kcb) derive from a single physical quantity** rather than from independent empirical lookups.

Lookup table columns for this approach:
```
LU_Code  LAI_min  LAI_max  k_ext  Kcb_full  S_leaf  GDD_plant  GDD_ini  GDD_dev  GDD_mid  GDD_late
```

### Relationship to the Phenology Provider

Under this design, the phenology provider and LAI-based Kcb interact as:

```
Phenology provider (owns growth timing)
    │
    ├── Produces: growth_fraction, growth_stage, it_is_growing_season
    │
    ├── IF GRIDDED_LAI method:
    │       LAI comes from grids → growth_fraction = LAI/LAI_max
    │       Kcb = f(LAI) directly — no piecewise curve needed
    │
    ├── IF FAO56_GDD or FAO56_DATES method:
    │       growth_stage computed from GDD or dates
    │       LAI = f(growth_stage, LAI_max) — internal LAI model
    │       Kcb = f(LAI) — same formula, LAI just has different source
    │
    └── IF DOY_BASED method (simple mode):
            growth_fraction = 0 or 1 (binary)
            LAI = LAI_min or LAI_max (snap)
            Kcb from traditional lookup or = Kcb_full * fc
```

The key architectural insight: **LAI becomes the universal intermediate variable between growth state and water-balance parameters.** Whether LAI comes from remote sensing, a GDD-driven model, or a simple binary lookup, the downstream formulas are the same.



---

## Implementation Roadmap

### Phase 1: Unified Phenology Provider (Near-Term — Next Month)

**Goal:** Consolidate growing season logic; enable GDD-based Kcb curves reliably.

1. Create `phenology.F90` module with the output contract (growth_fraction, it_is_growing_season, growth_stage)
2. Implement `phenology_doy_based` and `phenology_gdd_threshold` (migrate from `growing_season.F90`)
3. Implement `phenology_fao56_dates` and `phenology_fao56_gdd` (extract from `crop_coefficients__fao56.F90`)
4. Add auto-detection logic with logging
5. Refactor `crop_coefficients__fao56.F90` to receive growth_stage as input (pure Kcb interpolation)
6. Remove the procedure pointer swap for growing season
7. Update interception to use growth_fraction (optionally continuous, with binary as fallback)
8. Deprecate `growing_season.F90` (or redirect to phenology.F90 with a wrapper for build compatibility)
9. Add unit tests for each phenology method
10. Update user-facing documentation with method selection guidance

**Estimated effort:** Medium. The logic already exists; this is primarily reorganization and interface cleanup.

### Phase 2: Continuous Growth Fraction Integration (Near-Term)

**Goal:** Downstream consumers benefit from continuous growth_fraction rather than binary signal.

1. Modify interception to optionally interpolate storage max with growth_fraction
2. Modify rooting_depth__FAO56 to use growth_fraction directly (rather than reimplementing stage logic)
3. Modify plant_height calculation to use growth_fraction
4. Verify mass balance with continuous interception changes
5. Add regression tests comparing old (binary) vs. new (continuous) behavior

**Estimated effort:** Low. These are straightforward formula changes once Phase 1 provides growth_fraction.

### Phase 3: Gridded LAI Support (Medium-Term)

**Goal:** Accept preprocessed LAI grids and derive growth_fraction, fc, interception from them.

1. Add `phenology_gridded_lai` method to the provider
2. Define DATA_CATALOG entry type for time-varying LAI grids
3. Add `LAI_max`, `k_ext` to lookup table (per land use)
4. Implement normalization: growth_fraction = LAI / LAI_max
5. Implement LAI-based fc: `fc = 1 - exp(-k_ext * LAI)`
6. Implement LAI-based interception: `storage_max = S_leaf * LAI`
7. Test with MODIS MCD15A2H product resampled to a test domain
8. Document preprocessing requirements for LAI inputs

**Estimated effort:** Medium. The DATA_CATALOG infrastructure already handles time-varying grids; the main work is the new formulas and testing.

### Phase 4: LAI-Based Crop Coefficients (Longer-Term)

**Goal:** Option to derive Kcb directly from LAI without piecewise growth stage curves.

1. Add `CROP_COEFFICIENT LAI_DERIVED` as a method option
2. Implement `Kcb = Kcb_full * (1 - exp(-k_t * LAI))`
3. Add `Kcb_full`, `k_t` to lookup table
4. Implement hybrid GDD→LAI→Kcb pathway (internal LAI model)
5. Validate against pyfao56 and/or field data
6. Document when LAI-based vs. traditional Kcb is appropriate

**Estimated effort:** Low-Medium (the formula is simple; validation is the real work).

---

## Design Decisions to Resolve Before Implementation

1. **~~Explicit vs. auto-detected phenology method.~~** RESOLVED: Explicit only. No auto-detection. `PHENOLOGY_METHOD` is required in the control file. This breaks backwards compatibility (existing control files need one new line) but eliminates the cognitive burden of understanding auto-detection rules.

2. **What happens to the monthly Kcb special case?** Monthly Kcb values (`Kcb_Jan`...`Kcb_Dec`) don't map cleanly to growth stages. Options: (a) treat as a separate Kcb method that doesn't use the phenology provider for Kcb interpolation; (b) derive growth_fraction from the monthly Kcb values (growth_fraction = (Kcb_month - Kcb_min) / (Kcb_max - Kcb_min)). *Recommendation:* Option (b) — it gives interception and other consumers a meaningful growth_fraction without requiring additional columns. However, this should be paired with a simple phenology method (DOY_BASED or GDD_THRESHOLD) to drive interception timing; the monthly Kcb values are then purely a crop coefficient interpolation mechanism.

3. **Should DOY_BASED produce a smooth transition or hard step?** Could introduce a short transition ramp (e.g., 7-day linear ramp at start/end) to avoid the discontinuity. *Recommendation:* Default to hard step (preserving current behavior); add optional `transition_days` parameter for users who want smoothing.

4. **How does the killing frost / GDD hybrid work under the new design?** Currently, growing season can start on GDD threshold and end on killing frost. This is `GDD_THRESHOLD` in the new design — it produces binary on/off without growth stages. Should it also produce a growth_fraction trajectory? *Recommendation:* For GDD_THRESHOLD, growth_fraction = 0 or 1 (binary). If users want continuous growth_fraction with GDD-based timing, they should use `FAO56_GDD` which provides full stage progression.

5. **Control file directive naming.** ~~`PHENOLOGY_METHOD` vs. `GROWING_SEASON_METHOD` vs. `VEGETATION_DEVELOPMENT_METHOD`.~~ RESOLVED: `PHENOLOGY_METHOD` — it's concise, technically correct, and distinct from the current `GROWING_SEASON` concept (signaling to users that something has changed).

---

## References

- Allen, R.G., and Pereira, L.S., 2009, Estimating crop coefficients from fraction of ground cover and height: Irrigation Science, v. 27, no. 1, p. 17–34.
- Allen, R.G., Pereira, L.S., Raes, D., and Smith, M., 1998, Crop evapotranspiration: FAO Irrigation and Drainage Paper 56, 300 p.
- Er-Raki, S., Chehbouni, A., Guemouria, N., Duchemin, B., Ezzahar, J., and Hadria, R., 2007, Combining FAO-56 model and ground-based remote sensing to estimate water consumptions of wheat crops in a semi-arid region: Agricultural Water Management, v. 87, no. 1, p. 41–54.
- Raes, D., Steduto, P., Hsiao, T.C., and Fereres, E., 2009, AquaCrop—The FAO crop model to simulate yield response to water: Agronomy Journal, v. 101, no. 3, p. 426–437.
- Thorp, K.R., 2022, pyfao56: FAO-56 evapotranspiration in Python: SoftwareX 19, 101208.


---

## Design Decision: Leap Year Handling for mm/dd → DOY Conversion

**Date:** 2026-07-10
**Status:** Deferred (implement when next touching phenology_initialize)
**Affects:** `phenology.F90`, `datetime.F90` (`mmdd2doy`)

**Problem:** `mmdd2doy` converts "04/15" to DOY using a hardcoded non-leap year (1999). In leap years, DOY 105 lands on April 14, not April 15. This is a 1-day error after February in leap years.

**Design principle:** Respect the user's intent based on input format:
- `"04/15"` (mm/dd) → user means "April 15th" in any year. Convert to correct DOY for the current simulation year.
- `105` (integer) → user means "day 105" regardless of year. Use as-is. This is the PEST++-friendly path.

**Implementation plan:**
1. In `phenology_initialize`, store both `month` + `day` (for mm/dd inputs) and a flag indicating the input was a raw integer DOY
2. Add `phenology_update_doy_for_year(current_year)` — called once at the start of each simulation year
3. For mm/dd entries: recompute `GROWING_SEASON_START_DOY` using actual year (trivial: `julian_day(year, month, day) - julian_day(year, 1, 1) + 1`)
4. For integer DOY entries: leave unchanged

**Impact:** 1 day shift in leap years for mm/dd-specified dates. Matches user intent. No impact on integer-DOY users or PEST++ workflows.

**Current behavior (preserved until fix):** All inputs converted to DOY using non-leap 1999 reference. Off by 1 day after Feb 28 in leap years. Integration test expects this behavior.


---

## Enhancement: Per-Crop GDD Base and Maximum Temperatures

**Date:** 2026-07-10
**Status:** Planned (priority when FAO56 phenology phases begin)
**Affects:** `growing_degree_day.F90`, `growing_degree_day_baskerville_emin.F90`, lookup tables

**Current state:** Both GDD calculation routines use hardcoded defaults:
- `GDD_base_temperature = 50°F`
- `GDD_max_temperature = 86°F`

It's unclear whether per-landuse override was ever wired up (investigate).

**Motivation:** FAO-56 (2025 edition, Tables 6.10–6.12) defines base and max temperatures on a per-crop basis. For example:
- Corn: GDD_base=50°F, GDD_max=89.6°F
- Spring wheat: GDD_base=32°F, GDD_max=77°F
- Soybeans: GDD_base=50°F, GDD_max=86°F

Using crop-specific values significantly affects when GDD thresholds are reached, which directly impacts the phenology module's growing season start date.

**Implementation sketch:**
1. Add `GDD_base_temperature` and `GDD_max_temperature` columns to lookup tables (optional, with fallback to current defaults)
2. Modify `growing_degree_day_calculate` and the Baskerville-Emin variant to accept per-cell base/max
3. Wire through `model_calculate_GDD` → pass `this%landuse_index` to look up per-landuse values

**Related question:** Is the Baskerville-Emin method important? It uses a sine-curve approximation for the diurnal temperature cycle to improve GDD estimation when daily min/max straddle the base temperature. It's more accurate than the simple (Tmax+Tmin)/2 - Tbase approach, but adds complexity. Worth evaluating whether users actually benefit from it vs. the simpler method, especially given that climate input data (Daymet, gridMET) already have their own temperature averaging assumptions baked in.

**Priority:** Address alongside FAO56 phenology (Phase 3 — GDD-based crop coefficient curves).


---

## Wishlist: Detect and Error on Legacy Column Names

**Date:** 2026-07-10
**Status:** Wishlist
**Affects:** `phenology.F90` (phenology_initialize)

**Problem:** When users have tables with legacy column names (`Planting_date`, `First_day_of_growing_season`, `GDD_plant`, `GDD_first_day_of_growing_season`), the new phenology module silently doesn't find them. The result: all land uses get `PHENOLOGY_NONE` and `growth_stage = DORMANT` always. With FAO56 crop coefficients active, this means Kcb = Kcb_min permanently — a subtle and confusing failure.

**Desired behavior:** At the end of `phenology_initialize`, scan PARAMS for known legacy column names. If found, emit a fatal error with a clear migration message:

```
ERROR: Legacy column name 'Planting_date' detected in lookup table.
       The phenology module requires unified column names.
       Please rename: 'Planting_date' → 'Growing_season_start_date'

       Full mapping:
         Planting_date                    → Growing_season_start_date
         First_day_of_growing_season      → Growing_season_start_date
         Last_day_of_growing_season       → Growing_season_end_date
         GDD_plant                        → Growing_season_start_GDD
         GDD_first_day_of_growing_season  → Growing_season_start_GDD
```

**Implementation:** Simple — after the main parameter reads, call `PARAMS%get_parameters` with `lFatal=FALSE` for each legacy name. If any returns data, emit the fatal error with the rename instructions.


---

## Design Decision: PARAMS Dependency Injection for Test Isolation

**Date:** 2026-07-10
**Status:** Planned (implement incrementally as modules are touched)
**Affects:** All modules that call `PARAMS%get_parameters(...)`, test infrastructure

**Problem:** Unit test suites share a single global `PARAMS` instance. When multiple test suites load different lookup tables, earlier suites pollute PARAMS for later suites. This causes:
- Test execution order dependency
- Fragile tests that break when table column names overlap
- "Relaxed" test assertions that don't actually test anything specific

**Root cause:** `phenology_initialize()`, `crop_coefficients_FAO56_initialize()`, and other init routines all reach directly for the module-level global `PARAMS` instance. Tests cannot provide an isolated parameter set.

**Solution: Dependency injection.** Each initialize routine accepts an optional (or required) `PARAMETERS_T` argument:

```fortran
! Production code:
subroutine phenology_initialize(params)
  type(PARAMETERS_T), intent(inout) :: params
  call params%get_parameters(sKey="Growing_season_start_date", ...)
end subroutine

! In model_initialize.F90:
call phenology_initialize(PARAMS)  ! pass global

! In test:
type(PARAMETERS_T) :: my_params
call my_params%add_file("phenology_test.txt")
call my_params%munge_file()
call phenology_initialize(my_params)  ! isolated, no contamination
```

**Benefits:**
- Complete test isolation — each suite controls exactly what data it sees
- No execution order dependency
- Tests become precise and deterministic
- No need for a "clear PARAMS" hack
- Production code unchanged (just pass the global instance)

**Migration path:**
1. When next modifying a module's initialize routine, add `type(PARAMETERS_T), intent(inout) :: params` as the first argument
2. Replace internal `PARAMS%get_parameters(...)` with `params%get_parameters(...)`
3. Update callers in model_initialize.F90 to pass `PARAMS`
4. Update test callers to pass a local instance

**Interim workaround (current):** Test suite execution order in `tester.F90` is arranged so that suites with isolated data needs run first (phenology before fao56). This works but is fragile.
