# Changelog

---

## 2026-07-02

### Added: Doxygen documentation generation via pixi

- Added `doxygen` and `graphviz` to `pixi.toml` dependencies (conda-forge)
- Added `pixi run docs` task that cleans `docs/` and regenerates HTML documentation
- Updated doxygen-awesome-css from ~v2.1 to v2.4.2 for Doxygen 1.17 compatibility
- Created custom `doc_src/header.html` to load doxygen-awesome JS extensions (dark mode toggle, code fragment copy buttons, paragraph link anchors, tabs)
- Removed stale `custom-modified-doxygen-awesome.css` (full-copy override replaced by upstream v2.4.2)

### Fixed: Doxygen configuration for doxygen-awesome v2.4.2

- `HTML_HEADER` → points to new `doc_src/header.html`
- `HTML_EXTRA_STYLESHEET` → now references only `doxygen-awesome.css` (no custom override)
- `HTML_COPY_CLIPBOARD = NO` (required; doxygen-awesome provides its own copy button)
- `HTML_COLORSTYLE = LIGHT` with `HUE=220, SAT=100, GAMMA=80` — lets doxygen-awesome CSS fully control light/dark theming
- `PAGE_OUTLINE_PANEL = YES` — uses Doxygen 1.14+ native page outline instead of JS interactive TOC
- Removed `$mermaid` placeholder from header (not recognized by Doxygen 1.17.0, rendered as literal text)
- jQuery loaded from CDN (required by doxygen-awesome JS extensions; Doxygen 1.17 dropped bundled jQuery)

---

## 2026-06-25

### Fixed: Implicit narrowing conversions (52 of 89 instances)

Added explicit `real(..., c_float)`, `int(..., c_int)`, or `int(..., c_short)` wrappers to make intentional precision reductions explicit. Files completed:

- `datetime.F90` (18) — `c_int` → `c_short` member assignments
- `simulation_datetime.F90` (5) — `c_double` date arithmetic → `c_int`
- `mass_balance__soil.F90` (3) — `c_double` storage → `c_float` delta
- `crop_coefficients__fao56.F90` (5) — `c_double` interpolation → `c_float` Kcb
- `model_domain.F90` (4) — coordinates and date arithmetic
- `model_initialize.F90` (4) — grid calculations
- `data_catalog_entry.F90` (4) — scale/offset operations
- `actual_et__fao56.F90` (2) — ET calculations
- `actual_et__fao56__two_stage.F90` (2) — bare soil evap
- Others (5) — 1 each in meteorological_calculations, et__zone_values, irrigation, growing_degree_day_baskerville_emin, model_iterate_multiple_simulations

### Completed: Unused variables — ALL Fortran instances eliminated

All 289 unused variable warnings resolved (automated script + manual multi-var edits).

### Status: 540 → 124 gfortran warnings remaining

Remaining: 37 conversions (grid.F90, netcdf4_support.F90, test files), 31 unused functions, 13 maybe-uninitialized, 11 unused module values, 10 stack-to-static, 10 float comparisons, and small items.

---

## 2026-06-24

### Fixed: Mixed-kind min/max intrinsic errors (18 instances)

All 18 instances of mixed `c_float`/`c_double` arguments in `min`/`max` calls have been resolved. These were fatal errors under `gfortran -std=f2018` and warnings (#7374) under `ifx /stand:f18`.

| File | Instances | Fix approach |
|------|-----------|-------------|
| `mass_balance__interception.F90` | 2 | Promote float to double (result is double); do subtraction in double, demote |
| `mass_balance__impervious_surface.F90` | 2 | Do max in double, demote; promote float to double for min |
| `mass_balance__soil.F90` | 2 | Promote infiltration to double; do arithmetic in double, demote |
| `actual_et__fao56.F90` | 1 | Promote crop_etc to double |
| `actual_et__fao56__two_stage.F90` | 3 | Promote floats to double for consistent comparisons |
| `daily_calculation.F90` | 1 | Use `0.0_c_double`, wrap result in `real(..., c_float)` |
| `et__hargreaves_samani.F90` | 1 | Changed `rZERO` to `dZERO` |
| `irrigation.F90` | 4 | Use `1.0_c_double`, do arithmetic in double, wrap results |
| `model_domain.F90` | 2 | Change literals to `c_double` kind |

### Fixed: Impure function warnings (64 eliminated)

Marked 5 string-comparison functions in `fstring.F90` as `pure`:
- `char_to_uppercase_fn`
- `char_to_lowercase_fn`
- `is_string2_present_in_string1_case_insensitive_fn`
- `is_string2_present_in_string1_case_sensitive_fn`
- `is_char_equal_to_char_case_insensitive_fn`

This eliminated 64 of 69 `-Wfunction-elimination` warnings. The remaining 5 involve `fstring_list.F90` methods that require a deeper refactor.

---

## 2026-06-23

### Added: `static_analysis` build profile

New meson build option (`-Dprofile=static_analysis`) that enables pedantic compiler warnings for both gfortran and ifx without stopping compilation:

- gfortran: `-std=f2018 -Wall -Wextra -Wimplicit-interface -pedantic`
- ifx: `/warn:all /stand:f18 /Qdiag-error-limit:0`

### Refactored: Meson build system

- **DRY source lists:** Eliminated duplicated 55-file source list. Single `swb_common_src` compiled into `swb_library`; executables link the library.
- **Explicit profile option:** Added `profile` combo option (release/develop/static_analysis) replacing the optimization-level hack.
- **Removed Cray HPC hardcoded paths** from `src/meson.build`.
- **Fixed duplicate** `conf_data.set('SWB_MAJOR_VERSION_STRING', ...)`.

### Added: Build scripts for static analysis

- `build_swb__gfortran_windows__static_analysis.cmd`
- `build_swb__gfortran_windows__static_analysis.sh`

### Added: Design documentation

- `design/static_analysis_remediation_plan.md` — full plan to achieve warning-free builds
- `design/build_revamp__pixi_and_simplified_meson.md` — future pixi-based build proposal
- `design/assessment__mixed_precision_and_iso_c_binding.md` — analysis of type system design
