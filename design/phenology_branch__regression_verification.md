# Phenology Branch — Regression Verification (date-based behavior preservation)

**Date:** September 9, 2026
**Status:** OPEN — verification not yet run
**Affects:** `src/crop_coefficients__fao56.F90`, `src/phenology.F90`, `src/growing_season.F90`
**Branch under test:** `phenology_phase_1`
**Origin:** root-cause investigation of a base-vs-local net-infiltration mismatch in the
Michigan SWB project. Narrative/decision record:
`E:\projects\michigan_swb\breadcrumbs_phenology_mismatch_root_cause_sep9.md`.

---

## Motivation

The design intent of the phenology rewrite (`growing_season.F90` +
`crop_coefficients__fao56.F90` → unified `phenology.F90`, see
`design/completed/phenology_module_design.md`) was that runs using **date-based**
growing-season definitions should be **behavior-preserving** relative to the older code:
an equivalent lookup table should produce equivalent results.

A Michigan SWB comparison appeared to violate this: an SWB 2.3.3 run and an SWB 2.4.1
`phenology_phase_1` run of the "same" model differed in net infiltration at ~46.7M cells
(max |diff| 4.117 in). That prompted the question — is the phenology migration a
regression, or is the difference from something else?

## What the investigation established

The two Michigan builds differ by the linear commit range `8a41497d..64fb4164`
(base `v2.3.3-rc0-28`, local `v2.3.5-rc0-58`). Within that range, the ONLY commit that
materially changes the **date-based** water balance (aside from the phenology migration
itself) is:

- **`af937bfe`** — "fix: correct the FAO-56 crop coefficient date calculations" (PR #50,
  on `main`, inherited by `phenology_phase_1`). Changes:
  1. planting-date DOY→date **off-by-one** correction
     (old: `SIM_DT%start + DOY`; new: `Jan1 + (DOY - 1.0)`)
  2. replace `%setYear()` in-place drift with fresh date construction each year
  3. stage-boundary comparisons `>` → `>=` in `interpolate_Kcb`
  4. `prob_runoff_enhancement` UL/LL argument-order fix

Three ET commits initially suspected (`13eeb25c`, `33bc6553`, `76f6dfb8`) all PRE-DATE
the base build — `76f6dfb8` is literally tagged `v2.3.3-rc0` — so they are in BOTH builds
and irrelevant to the diff.

### Direct evidence: `af937bfe` produces a uniform one-day Kcb-curve shift

The Michigan DEBUG logfiles emit the assumed Kcb curve tables. Comparing the base
(2.3.3, heading "## Updating Kcb Date Values ##") vs local (2.4.1, heading
"## Crop Kcb Curve Summary ##") initialization tables:

| LU  | 2.3.3 base planting (doy) | 2.4.1 local planting (doy) |
|-----|---------------------------|----------------------------|
| 43  | 1995-04-03 (93)           | 1995-04-02 (92)            |
| 207 | 1995-04-03 (93)           | 1995-04-02 (92)            |

Every date in the newer run is exactly **one day earlier**, and the offset propagates
uniformly through all stage boundaries (boundaries = planting + cumulative L-lengths).
This is the expected fingerprint of the off-by-one fix (1) plus the `>`→`>=` change (3).
Importantly, the tables show **no gross reshaping** — planting DOYs (modulo the one-day
fix), stage lengths, and curve shape are preserved. This is *encouraging* evidence that
the migration is close to behavior-preserving, but it is NOT proof, because `af937bfe`
and the migration are entangled in the Michigan comparison.

## The open question

Is the phenology **migration** (commits `85c8c60e → f34c6b73 → c88ff025 → 717fb8a9 →
e3e1d86f → 64fb4164`) behavior-preserving for date-based landuses **once the `af937bfe`
date fix is held constant**? The Michigan comparison cannot answer this because it varies
both the date fix and the migration at once.

## Proposed verification: controlled A/B

Hold `af937bfe` constant; vary only the migration. Same lookup table, DATE-based growing
season, single small tile, a few years, identical weather.

- **Build A (pre-migration, date-fix present):** commit **`0e02a90c`** — the last commit
  before the phenology work begins (parent of the first phenology commit `85c8c60e`).
  Verified that `af937bfe` is already an ancestor of `0e02a90c`, so Build A has the date
  fix but none of the migration.
- **Build B (post-migration):** commit **`64fb4164`** — phenology branch tip.

Run both on an identical date-based configuration and compare net infiltration (and,
ideally, daily Kcb / actual_et for a single cell).

**Interpretation:**
- A ≈ B to tolerance → migration IS behavior-preserving; the entire Michigan date-path
  difference is attributable to `af937bfe` (+ version-independent effects). Close this
  doc; move to `design/completed/`.
- A and B diverge on the date path → genuine phenology-migration regression. Re-open the
  candidate mechanisms from `design/completed/phenology_module_design.md`
  (auto-detection → explicit method selection changing per-LU Kcb method) and
  `design/completed/fix_crop_coefficient_phenology_interaction.md`
  (KCB_METHOD assignment / removal of the planting-date growth-stage loop).

A good high-signal, low-cost diagnostic is a **single-cell daily trace** (log
growing-season start, growth_stage, Kcb, actual_et per day for a couple of cells) rather
than another whole-grid diff.

## Notes / caveats to carry forward

- GDD-based landuses use a separate code path not shown in the date-table logs; the
  frost-latch fix (`design/completed/phenology_conditional_frost_latch__2026_08_03.md`)
  affects those and would be a separate verification if GDD landuses are in scope.
- The FAO-56 two-stage issues (`design/fao56_two_stage_implementation_issues.md`) affect
  Ks/ET/irrigation triggering but NOT net infiltration (`soil_storage_max` never updated;
  factorial interaction term ≈ 0). So they are not a confound for an NI-based A/B.
