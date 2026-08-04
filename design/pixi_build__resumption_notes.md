# Pixi Build Implementation — Final Status

**Date:** July 2, 2026  
**Branch:** `pixi_build`  
**Status:** ✅ Complete — ready for code review

---

## What Works Today

- `pixi install` downloads gfortran 15.2, meson, ninja, netcdf, hdf5, zlib
- `pixi run setup` configures meson (passes CONDA_PREFIX/Library as netcdf_root)
- `pixi run setup-dev` configures with runtime checks (develop profile)
- `pixi run setup-sa` configures with maximum warnings (static_analysis profile)
- `pixi run build` compiles swb2.exe, swbstats2.exe, swbtest.exe
- `pixi run test` runs FRUIT unit tests from correct working directory
- `pixi run clean` removes builddir (Windows-safe)
- SSL through USGS MITM proxy works via SSL_CERT_FILE env var pointing to
  combined Mozilla + DOI cert bundle
- All compiler flag suppressions removed; code compiles clean with `-std=f2018`
  and no `-fallow-argument-mismatch`
- All three build profiles (release, develop, static_analysis) validated
- `pixi.lock` committed for reproducibility

## Key Files Modified (relative to main)

| File | Changes |
|------|---------|
| `pixi.toml` | Created — dependencies, tasks, Windows overrides |
| `pixi.lock` | Created — locked dependency versions |
| `meson.build` | Added `fortran_std=f2018` to default_options; removed all `-Wno-*` suppressions, `-fall-intrinsics`, `-std=gnu`, `-g`; removed netcdf_root from top-level |
| `src/meson.build` | Replaced pkg-config with `find_library()` approach; netcdf_root with platform defaults moved here |
| `meson_options.txt` | Unchanged |
| `.gitignore` | Added `.pixi/`, `builddir/`, quickstart doc |
| `design/developer_quickstart__pixi.md` | Full build instructions with USGS SSL setup |
| `design/feature_consideration__adopt_pixi_for_library_management.md` | Updated to reflect adoption, lessons learned, future work |

## Static Analysis Results (July 2, 2026 — gfortran 15.2)

| Warning Category | Count | Actionable? |
|-----------------|------:|:-----------:|
| `-Wunused-function` (reserve API) | 20 | No |
| `-Wunused-dummy-argument` (interface conformance) | 16 | No |
| `-Wmaybe-uninitialized` (false positive on allocatable) | 5 | No |
| **Total Fortran warnings** | **41** | **None** |
| C warnings (bundled proj4, third-party) | ~40 | No |

## Problems Encountered & Solutions

1. **Conda-forge netcdf.pc is broken on Windows** — Solution: bypassed
   pkg-config entirely, use `cc.find_library()` with explicit dirs.

2. **CONDA_PREFIX not expanding in meson** — Solution: wrap in `cmd /c` for
   Windows tasks.

3. **pixi `tls-root-certs = "system"` doesn't work** — rustls can't read
   Windows cert store for USGS CA. Solution: `SSL_CERT_FILE` env var pointing
   to concatenated Mozilla bundle + DOI root CA. Must be set as permanent user
   environment variable (not per-session).

4. **First build extremely slow** — endpoint security scanning unsigned pixi
   binaries. Subsequent runs are fast. Documented in quickstart.

5. **`pixi run clean` fails with `if` keyword** — pixi's shell parser rejects
   Windows `if` statements. Solution: `cmd /c "rmdir /s /q builddir 2>nul || exit /b 0"`

6. **`pixi run test` wrong path** — `cwd` setting means relative path must
   account for being in `test/unit_tests/`. Fixed to `..\\..\\builddir\\...`

7. **osx-arm64 platform fails** — SSL error fetching repodata + different
   gfortran package names. Solution: restricted to `win-64` only for now.

8. **`-Wno-unused-dummy-argument` ineffective** — gfortran 15.2 doesn't
   suppress when `-Wextra` also active. Documented as non-actionable.

## Architecture Decisions

- **No pkg-config for netcdf/hdf5** — broken upstream, not worth fighting.
  `find_library()` is simpler and works everywhere.
- **`fortran_std=f2018` globally** — all profiles use the same standard.
  No more `-fall-intrinsics` or `-std=gnu`.
- **No warning suppressions in release/develop** — after static analysis cleanup,
  any new warnings are real issues to fix.
- **Dynamic linking for pixi builds** — conda-forge provides MSVC .lib import
  libraries; gfortran links to DLLs at runtime. Static linking remains possible
  via MSYS2 (gfortran) or Intel ifx (same ABI as MSVC).
- **SSL via SSL_CERT_FILE** — more reliable than pixi's `tls-root-certs = "system"`
  which doesn't work with rustls on Windows.
- **Win-64 only** — multi-platform requires network access to all platform
  repodata; USGS proxy blocks osx-arm64. Re-add when CI runs outside USGS.
- **`pixi.lock` committed** — reproducibility for reviewer and future builds.
- **`pkg-config` removed from dependencies** — not used; build uses find_library.

## Future Work (Post-Code-Review)

| Task | Priority | Notes |
|------|----------|-------|
| GitHub Actions CI using pixi | High | `prefix-dev/setup-pixi` action; no USGS proxy issues on runners |
| Re-add linux-64 platform | High | Works on CI runners; test with `gfortran >= 13` |
| Re-add osx-arm64 platform | Medium | Needs `gfortran_osx-arm64` platform-specific dep |
| Add Intel ifx CI job | Medium | Separate from pixi; use Intel oneAPI containers |
| File conda-forge issue for broken netcdf.pc | Low | Document the Libs.private CMake leak |
| Add `pixi run dist` task | Low | Package exe + DLLs for end-user distribution |
| Add `pixi run lint` task | Low | After fprettify adoption |
