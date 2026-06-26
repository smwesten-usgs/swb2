# Changelog — June 2026

## June 26, 2026 — Static Analysis Complete

Eliminated all actionable compiler warnings from both gfortran and Intel ifx under Fortran 2018 standard conformance with maximum warning levels.

### Summary

- **Starting state:** 540 gfortran warnings, 487 ifx diagnostics (code compiled but with extensive noise)
- **Ending state:** 0 actionable warnings on both compilers

### Changes Made

**Deleted dead code:**
- Removed 12 stale/buggy unused private functions (debug helpers, superseded lookups, buggy string utilities)
- Removed all 16 unused module-level variable declarations and orphaned `use` imports
- Previously removed 289 unused local variables (June 24–25)

**Fixed real bugs (3):**
- `solar_calculations.F90`: wrong variable in expression (`dAlpha = HALFPI - dAlpha` → `dAlpha = HALFPI - dTheta_z`)
- `swbstats2_support.F90` (×2): typo left Kahan summation compensator uninitialized

**Fixed maybe-uninitialized (13):**
- Initialized pointer results (`=> null()`), scalar results (sentinel values), and string variables
- All via executable statements (not at declaration, to avoid implicit `SAVE`)

**Fixed float comparisons (10):**
- NODATA sentinel checks changed from `==` to `<=` (valid data is always above sentinel)
- Sign-change detection changed from `sign() /=` to product test

**Fixed stack-to-static (10):**
- Local `ASCII_FILE_T` variables changed to `allocatable` + `allocate()`

**Fixed impure function warnings (5):**
- Marked `is_file_open_fn`, `return_count_of_matching_strings_fn`, `retrieve_value_from_list_at_index_fn`, and `is_char_equal_to_char_case_sensitive_fn` as `pure`

**Fixed Fortran 2018 conformance:**
- INQUIRE specifier variables changed from `logical(c_bool)` to default `logical` (16 instances)
- Character truncation: widened local variable from 256 to 512 chars
- Argument aliasing: changed `intent(out)` to `intent(inout)` for in-place operations
- Lines >132 characters wrapped (6 instances)

**Fixed build system:**
- Removed manual `/MT` flags from meson.build; CRT linkage now controlled via `b_vscrt` option
- Build scripts updated: `-Db_vscrt=mt` (release), `-Db_vscrt=mtd` (debug/static_analysis)

### Retained by Design (not bugs)

- 20 private unused functions: intentional reserve API (NetCDF abstraction, spatial analysis library, zone aggregation feature). Documented in `design/unused_functions_analysis.md`.
- 83 ifx remarks (#7712): unused dummy arguments inherent to Fortran OOP type-bound procedure signatures.

---

## June 25, 2026

- Eliminated all 89 `-Wconversion` (implicit narrowing) warnings
- Eliminated all remaining Fortran unused variable warnings (289 → 0)

## June 24, 2026

- Fixed 18 mixed-kind min/max errors (fatal under gfortran `-std=f2018`)
- Marked 64 functions as `pure` to eliminate impure-function-elimination warnings
- Fixed 11 implicit conversion warnings
- Removed 271 unused variables in bulk pass

## June 23, 2026

- Created `static_analysis` build profile in Meson
- Initial gfortran static analysis build: 540 warnings (build failed due to min/max errors)
- Initial ifx static analysis build: 487 diagnostics
- Created remediation plan
