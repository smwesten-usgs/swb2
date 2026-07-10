# Test Directory Cleanup and Modernization

**Date:** July 2026
**Status:** Planned
**Related:** Phenology Phase 1 integration, test-drive migration (Phase 0 complete)

---

## Motivation

The `test/` directory has accumulated tests across several eras of SWB development. Some are outdated (reference removed modules, use legacy control file directives), some overlap in purpose, and some lack clear documentation of what they verify. As the phenology module replaces `growing_season.F90` and control file conventions change, the test suite needs to be rationalized.

---

## Current State

```
test/
├── unit_tests/           — 11 suites, 171 tests (test-drive, current)
│   └── old_fruit_tests/  — deprecated FRUIT tests (can be deleted)
├── integration_tests/
│   └── cs/               — Central Sands full model run (FAO-56 + irrigation)
├── regression_tests/
│   ├── cs/swb1/, cs/swb2/  — Central Sands SWB1 vs SWB2 comparison
│   ├── or/swb1/, or/swb2/  — Oregon vegetation test
│   └── sar/swb1/, sar/swb2/ — Sardinia test
├── feature_tests/
│   └── ak/              — Alaska (Beaver Creek) feature test
├── bugfix_tests/
│   ├── 02_segfault_...  — AWC grid missing
│   └── 11_segfault_...  — invalid dump coordinates
└── test_data/            — shared grids, tables, climate files
```

---

## Tasks

### 1. Audit existing tests
- [ ] Inventory each test: what it runs, what it verifies, what modules/methods it exercises
- [ ] Identify which tests reference `growing_season.F90` behavior or legacy directives (`GROWING_SEASON DOY1 DOY2 TRUE`)
- [ ] Identify which tests require FAO-56 crop coefficients (and thus the old pointer swap)
- [ ] Flag tests that duplicate coverage

### 2. Remove dead tests
- [ ] Delete `test/unit_tests/old_fruit_tests/` (superseded by test-drive conversion)
- [ ] Remove or archive any regression test that compares against SWB1 behavior we no longer support

### 3. Update existing tests for the new phenology approach
- [ ] Update lookup tables to use unified parameter names (`Growing_season_start_date`, etc.)
- [ ] Remove the vestigial `GROWING_SEASON 133 268 TRUE` directive from all control files
- [ ] Where FAO-56 is used, will eventually need `PHENOLOGY_METHOD FAO56_DATES` (Phase 2)

### 4. Add new integration tests
- [ ] **Phenology smoke test (T-M mode):** simple run proving DOY and GDD transitions work (this session's immediate task)
- [ ] **Phenology regression test:** compare `it_is_growing_season` output from the new module against saved reference output from the old `growing_season.F90` for the same inputs
- [ ] **FAO-56 regression test (Phase 2):** once FAO56 phenology is migrated, verify Kcb curves match

### 5. Standardize test structure
- [ ] Each test directory gets a README explaining purpose, expected behavior, and how to run
- [ ] Control files should be self-documenting (comments explaining what's being tested)
- [ ] Consider: should integration/regression tests be runnable via `pixi run` tasks?

---

## Priority

- **Immediate:** Create the phenology smoke test (DOY + GDD, T-M mode)
- **Soon:** Delete `old_fruit_tests/`, remove dead `GROWING_SEASON` directives
- **Phase 2:** Update FAO-56 tests when phenology absorbs that logic
- **Ongoing:** Keep test coverage aligned with code changes
