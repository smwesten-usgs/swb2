# Feature Consideration: Adopt Pixi for Library and Build Management

**Date:** May 2026 (proposed) → July 2, 2026 (adopted)  
**Status:** ✅ ADOPTED — working on Windows with gfortran 15.2 via conda-forge  
**Branch:** `pixi_build`

---

## Summary of Outcome

Pixi has been successfully adopted for SWB2's build workflow on Windows. The
original proposal's promise — "from clean checkout to working executable in
three commands" — has been realized. The effort estimate of ~1 day was roughly
accurate once platform quirks were resolved.

---

## What Was Implemented

### `pixi.toml` (final form)

```toml
[workspace]
name = "swb2"
channels = ["conda-forge"]
platforms = ["win-64"]
version = "2.3.5"

[dependencies]
meson = ">=1.6.0"
ninja = "*"
libnetcdf = "*"
hdf5 = "*"
zlib = "*"
gfortran = ">=13"
gcc = ">=13"
gxx = ">=13"

[tasks]
setup = { cmd = "meson setup builddir --wipe -Dprofile=release -Dnetcdf_root=$CONDA_PREFIX" }
setup-dev = { cmd = "meson setup builddir --wipe -Dprofile=develop -Doptimization=0 -Dnetcdf_root=$CONDA_PREFIX" }
setup-sa = { cmd = "meson setup builddir --wipe -Dprofile=static_analysis -Dnetcdf_root=$CONDA_PREFIX" }
build = "meson compile -C builddir"
test = { cmd = "../../builddir/test/unit_tests/swbtest", cwd = "test/unit_tests" }
clean = "rm -rf builddir"

[target.win-64.tasks]
setup = { cmd = "cmd /c meson setup builddir --wipe -Dprofile=release -Dnetcdf_root=%CONDA_PREFIX%/Library" }
setup-dev = { cmd = "cmd /c meson setup builddir --wipe -Dprofile=develop -Doptimization=0 -Dnetcdf_root=%CONDA_PREFIX%/Library" }
setup-sa = { cmd = "cmd /c meson setup builddir --wipe -Dprofile=static_analysis -Dnetcdf_root=%CONDA_PREFIX%/Library" }
clean = { cmd = "cmd /c \"rmdir /s /q builddir 2>nul || exit /b 0\"" }
test = { cmd = "cmd /c ..\\..\\builddir\\test\\unit_tests\\swbtest.exe", cwd = "test/unit_tests" }
```

### Key Architecture Decisions

1. **No pkg-config** — conda-forge's `netcdf.pc` on Windows is broken (leaks
   CMake target names like `-lHDF5::HDF5` into `Libs.private`). We use
   `cc.find_library()` with explicit dirs in `src/meson.build` instead.

2. **`fortran_std=f2018` globally** — set in `meson.build` `default_options`.
   No more `-fall-intrinsics` or `-std=gnu`.

3. **Dynamic linking** — conda-forge provides MSVC-built DLLs. gfortran links
   to them at runtime. Static linking remains possible via MSYS2 for release
   distribution.

4. **`pixi.lock` committed** — guarantees reproducible environments for
   reviewer and future developers.

5. **Win-64 only for now** — cross-platform (`linux-64`, `osx-arm64`) removed
   because the solver hits SSL errors fetching platform repodata from behind
   USGS MITM proxy, and gfortran package naming differs across platforms.
   Can be re-added when CI is set up outside USGS network.

---

## Problems Encountered & Solutions

| Problem | Root Cause | Solution |
|---------|-----------|----------|
| SSL errors during `pixi install` | USGS TLS-intercepting proxy; pixi's rustls doesn't trust Windows cert store | `SSL_CERT_FILE` env var pointing to combined Mozilla + DOI cert bundle |
| `netcdf.pc` broken on Windows | conda-forge packaging bug: leaked CMake target names in Libs.private | Bypassed pkg-config entirely; use `find_library()` with explicit dirs |
| `%CONDA_PREFIX%` not expanding | pixi's shell doesn't expand Windows env vars without `cmd /c` | Wrapped Windows tasks in `cmd /c` |
| `pixi run clean` fails | pixi's shell parser rejects `if` keyword | Used `rmdir /s /q builddir 2>nul \|\| exit /b 0` |
| `pixi run test` wrong path | `cwd` changes directory before running command; relative path was wrong | Fixed path to `..\\..\\builddir\\test\\unit_tests\\swbtest.exe` |
| osx-arm64 solver failure | SSL cert issue fetching repodata + different gfortran package names | Restricted to `win-64` platform only for now |
| First build extremely slow (~5 min) | Endpoint security scanning unsigned pixi/gfortran binaries | Subsequent runs fast; documented in quickstart |
| `-Wno-unused-dummy-argument` ineffective | gfortran 15.2 doesn't suppress when `-Wextra` also active | Documented as non-actionable; warnings are interface conformance |

---

## What Changed vs. Original Proposal

| Proposed | Actual |
|----------|--------|
| Use `dependency()` (pkg-config) for netcdf/hdf5 | Used `find_library()` — pkg-config is broken upstream |
| `pkg-config` as a pixi dependency | Removed — not used |
| `PKG_CONFIG_PATH` env var in tasks | Not needed — find_library takes explicit dirs |
| Cross-platform (win/linux/mac) | Win-64 only for now |
| `gfortran = "13.*"` | `gfortran = ">=13"` — currently resolves to 15.2 |

---

## Current Compiler: gfortran 15.2 (conda-forge)

Pixi resolves `gfortran >= 13` to gfortran 15.2.0 from conda-forge. This is a
very recent compiler with excellent F2018 support. Notable behavior:

- Full F2018 standard conformance checking works (`-std=f2018`)
- `-Wno-unused-dummy-argument` does NOT suppress warnings when `-Wextra` is active
  (appears to be a flag-ordering quirk in this version)
- No `-fallow-argument-mismatch` needed — all argument mismatches have been fixed
- No `-fall-intrinsics` needed — all non-standard intrinsics removed

---

## Future Work: Compiler Matrix & CI

These are post-code-review tasks, suitable for when CI is set up on
code.usgs.gov or GitHub Actions.

### Additional compilers to consider

| Compiler | Value-add | Availability via conda-forge | Notes |
|----------|-----------|------------------------------|-------|
| **Intel ifx** | Complementary diagnostics (unreachable code, precision) | ❌ Not on conda-forge | Requires separate oneAPI install or Intel's CI containers |
| **flang-new (LLVM 18+)** | Different warning set, LLVM-based analysis | ⚠️ Available as `flang` but F2018 support incomplete | Worth monitoring; not ready for production Fortran in mid-2026 |
| **NAG Fortran** | Gold standard semantic analysis | ❌ Commercial license | If USGS has access, highest-value diagnostic tool for Fortran |
| **LFortran** | Open-source, AST-level analysis | ❌ Too immature | Cannot compile SWB2 today; interesting for future |

### Recommended CI approach

```yaml
# GitHub Actions example
strategy:
  matrix:
    include:
      - os: windows-latest
        profile: release
      - os: windows-latest
        profile: static_analysis
      - os: ubuntu-latest
        profile: release
      # - os: ubuntu-latest  # Future: ifx via Intel containers
      #   compiler: ifx

steps:
  - uses: actions/checkout@v4
  - uses: prefix-dev/setup-pixi@v0.9.5
  - run: pixi run setup    # or setup-sa for static_analysis
  - run: pixi run build
  - run: pixi run test
```

### Platform expansion

To re-add `linux-64` and `osx-arm64` to `pixi.toml`:

1. The SSL issue won't exist in CI (no USGS MITM proxy on GitHub/GitLab runners)
2. gfortran package naming: `gfortran` works on linux-64 and win-64; on
   osx-arm64, conda-forge uses `gfortran_osx-arm64` (cross-compiler). May need
   platform-specific dependency declarations:
   ```toml
   [target.osx-arm64.dependencies]
   gfortran_osx-arm64 = ">=13"
   ```
3. The `[tasks]` (non-Windows) section already has correct Unix syntax

### Other future pixi tasks

| Task | Description | Priority |
|------|-------------|----------|
| `pixi run lint` | Run fprettify for formatting checks | After formatter adoption |
| `pixi run docs` | Generate Doxygen documentation | After docs setup |
| `pixi run dist` | Package exe + DLLs for distribution | For release workflow |
| `pixi run integration-test` | Run full model test cases | After test infrastructure |

---

## References

- Pixi documentation: https://pixi.sh/latest/
- MODFLOW6 pixi usage: https://github.com/MODFLOW-USGS/modflow6 (`pixi.toml`)
- conda-forge gfortran: https://anaconda.org/conda-forge/gfortran
- conda-forge netcdf issue (broken .pc): not yet filed upstream

---

## Lessons Learned

1. **Don't trust pkg-config on conda-forge Windows.** The `.pc` files leak
   CMake target names into link flags. `find_library()` is simpler and works.

2. **pixi's task shell parser is limited.** It doesn't understand Windows `if`
   statements, `for` loops, or other cmd.exe constructs. Wrap complex logic in
   `cmd /c "..."` with careful quoting, or keep it to simple single commands.

3. **`pixi clean` ≠ `pixi run clean`.** `pixi clean` removes the entire pixi
   environment (1+ GB). `pixi run clean` runs your custom clean task (removes
   builddir). Name your task carefully in docs to avoid confusion.

4. **SSL behind USGS proxy requires manual cert bundle.** pixi uses rustls
   which doesn't read the Windows certificate store. The `SSL_CERT_FILE` env
   var pointing to a combined Mozilla + DOI bundle is the only reliable fix.
   Must be set as a *permanent* user environment variable, not per-session.

5. **First run is slow due to endpoint security.** CrowdStrike/Defender scans
   new unsigned binaries extensively on first encounter. Document this for
   users so they don't think the build is broken.

6. **Start with one platform.** Multi-platform pixi.toml is appealing but the
   solver must be able to reach all platform repodata. On restricted networks,
   stick to the platform you're actually building on.
