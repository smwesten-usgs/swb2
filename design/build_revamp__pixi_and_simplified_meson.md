# Build Process Revamp: pixi + Simplified Meson

**Date:** June 2026  
**Status:** In progress — `pixi.toml` and pkg-config detection created; needs testing  
**Goal:** Eliminate hardcoded paths, platform hacks, and manual dependency installation. One command to build on any platform.

---

## Next Steps (Week of June 30, 2026)

### Step 1: Install pixi (if not already installed)

```powershell
# From PowerShell (no admin required):
irm get.pixi.sh/install.ps1 | iex
```

Or from MSYS2: `curl -fsSL https://pixi.sh/install.sh | bash`

Verify: `pixi --version`

### Step 2: Run `pixi install` from the repo root

```
cd E:\projects\swb_development\git\swb2
pixi install
```

**What to watch for:**
- If `gfortran` package name fails, try replacing `gfortran = ">=13"` in `pixi.toml` with `gfortran_win-64 = "*"` or `m2w64-gcc-fortran = "*"` — conda-forge naming varies.
- If `libnetcdf` doesn't pull in HDF5 automatically, you already have `hdf5` listed explicitly so it should be fine.
- This step downloads ~200–500 MB into `.pixi/` on first run.

### Step 3: Test the build

```
pixi run setup
pixi run build
```

**If setup fails** with "netcdf not found via pkg-config":
- Check that `.pixi/envs/default/Library/lib/pkgconfig/netcdf.pc` exists (Windows) or `.pixi/envs/default/lib/pkgconfig/netcdf.pc` (Linux/Mac)
- If the `.pc` file exists but isn't found, the `PKG_CONFIG_PATH` in `pixi.toml` may need adjusting. Run `pixi run -- pkg-config --list-all | grep netcdf` to debug.
- If it falls through to the `netcdf_root` fallback and that fails too, something is wrong with the pixi environment.

**If compile fails** with linker errors (undefined symbols):
- This likely means ABI mismatch: conda-forge's netCDF was built with MSVC, but gfortran uses MinGW. The fix is to ensure dynamic linking (`.dll.a` import libraries). Check if `.pixi/envs/default/Library/lib/netcdf.lib` or `libnetcdf.dll.a` exists.
- If static-only MSVC `.lib` files are provided, you may need to add `static: false` to the `dependency()` calls, or stick with MSYS2's MinGW packages for the actual link libraries while using pixi for everything else.

### Step 4: Test the executable

```
cd test\unit_tests
copy ..\..\build\meson\builddir\test\unit_tests\swbtest.exe .
swbtest.exe
```

### Step 5: If it all works — clean up

- Add `.pixi/` to `.gitignore`
- Add `pixi.lock` to `.gitignore` (or commit it for reproducibility — your call)
- Update README with the pixi build instructions
- Existing MSYS2 build scripts remain as-is (fallback path still works)

### Step 6: If gfortran-from-pixi doesn't work on Windows

Fall back to the **hybrid approach**:
- Remove `gfortran`, `gcc`, `gxx` from `pixi.toml` (keep just `meson`, `ninja`, `pkg-config`, `libnetcdf`, `hdf5`, `zlib`)
- Use your system gfortran (MSYS2/ucrt64) as the compiler
- Use pixi only for library management and pkg-config
- Build command becomes: `pixi run -- meson setup builddir` (pixi provides PKG_CONFIG_PATH, system provides gfortran)

This still eliminates the "where is netCDF installed?" problem while keeping your known-good compiler.

---

## Current Pain Points

| Problem | Where | Impact |
|---------|-------|--------|
| Hardcoded `netcdf_root` per platform | `meson.build`, all build scripts | Every new developer guesses paths |
| Hardcoded Cray HPC paths in `find_library` | `src/meson.build` | Only works on one specific cluster |
| `fix_linker__ifx_windows.exe` hack | Build scripts | Meson misidentifies `xilink.exe` vs `link.exe` for Intel |
| NetCDF auto-discovery only for ifx scripts | `build_swb__ifx_windows.cmd` | gfortran scripts hardcode paths |
| Manual compiler installation | Developer docs | 30+ min setup per platform |
| Platform-specific build scripts (6 files) | `build/meson/` | Maintenance burden, drift between scripts |
| `/MDd` vs `/MT` conflict | `meson.build` | 76 warnings from CRT mismatch |
| No reproducible environment | — | "Works on my machine" problems |

---

## Target Architecture

```
Developer runs: pixi run build
                    │
                    ▼
        pixi.toml defines:
        ├── compilers (gfortran OR ifx via conda-forge)
        ├── libraries (netcdf, hdf5, zlib)
        ├── build tools (meson, ninja, pkg-config)
        └── environment variables (PKG_CONFIG_PATH, etc.)
                    │
                    ▼
        meson.build uses:
        ├── dependency('netcdf')  ← found via pkg-config automatically
        ├── dependency('hdf5')    ← same
        └── No hardcoded paths anywhere
```

---

## pixi.toml

```toml
[workspace]
name = "swb2"
channels = ["conda-forge"]
platforms = ["win-64", "linux-64", "osx-arm64"]
version = "2.3.5"

[dependencies]
meson = ">=1.6.0"
ninja = "*"
pkg-config = "*"

# =================================================================
# Feature environments: choose your compiler
# =================================================================

[feature.gcc.dependencies]
gfortran = ">=13"
gcc = ">=13"
gxx = ">=13"
libnetcdf = "*"
hdf5 = "*"
zlib = "*"

[feature.intel.dependencies]
intel-fortran-rt = "*"
libnetcdf = "*"
hdf5 = "*"
zlib = "*"

# =================================================================
# Tasks
# =================================================================

[feature.gcc.tasks]
setup = { cmd = "meson setup builddir --wipe", env = { FC = "gfortran", CC = "gcc", CXX = "g++", PKG_CONFIG_PATH = "$CONDA_PREFIX/lib/pkgconfig" } }
setup-dev = { cmd = "meson setup builddir --wipe -Dprofile=develop -Doptimization=0", env = { FC = "gfortran", CC = "gcc", CXX = "g++", PKG_CONFIG_PATH = "$CONDA_PREFIX/lib/pkgconfig" } }
setup-sa = { cmd = "meson setup builddir --wipe -Dprofile=static_analysis", env = { FC = "gfortran", CC = "gcc", CXX = "g++", PKG_CONFIG_PATH = "$CONDA_PREFIX/lib/pkgconfig" } }

[feature.intel.tasks]
setup = { cmd = "meson setup builddir --wipe", env = { FC = "ifx", CC = "icx", CXX = "icx", PKG_CONFIG_PATH = "$CONDA_PREFIX/lib/pkgconfig" } }
setup-dev = { cmd = "meson setup builddir --wipe -Dprofile=develop -Doptimization=0", env = { FC = "ifx", CC = "icx", CXX = "icx", PKG_CONFIG_PATH = "$CONDA_PREFIX/lib/pkgconfig" } }
setup-sa = { cmd = "meson setup builddir --wipe -Dprofile=static_analysis", env = { FC = "ifx", CC = "icx", CXX = "icx", PKG_CONFIG_PATH = "$CONDA_PREFIX/lib/pkgconfig" } }

[tasks]
build = "meson compile -C builddir"
test = "meson test -C builddir"
clean = { cmd = "rm -rf builddir" }
sa-report = { cmd = "meson compile -C builddir 2> static_analysis_warnings.txt", depends-on = ["setup-sa"] }

# Windows overrides for PKG_CONFIG_PATH location
[target.win-64.feature.gcc.tasks]
setup = { cmd = "meson setup builddir --wipe", env = { FC = "gfortran", CC = "gcc", CXX = "g++", PKG_CONFIG_PATH = "%CONDA_PREFIX%\\Library\\lib\\pkgconfig" } }
setup-dev = { cmd = "meson setup builddir --wipe -Dprofile=develop -Doptimization=0", env = { FC = "gfortran", CC = "gcc", CXX = "g++", PKG_CONFIG_PATH = "%CONDA_PREFIX%\\Library\\lib\\pkgconfig" } }
setup-sa = { cmd = "meson setup builddir --wipe -Dprofile=static_analysis", env = { FC = "gfortran", CC = "gcc", CXX = "g++", PKG_CONFIG_PATH = "%CONDA_PREFIX%\\Library\\lib\\pkgconfig" } }

[environments]
gcc = ["gcc"]
intel = ["intel"]
```

**Note:** The exact conda-forge package names for Intel compilers may vary (`intel-fortran-rt`, `intel-cmplr-lic-rt`, etc.). This needs verification. If Intel compilers are not available on conda-forge for all platforms, the `intel` environment would only be available on platforms where they exist, or Intel is installed system-wide and pixi only provides the libraries.

---

## Simplified meson.build (dependency detection section)

Replace the entire netcdf_root / hardcoded path block with:

```meson
# =================================================================
# Dependencies: prefer pkg-config, fall back to manual search
# =================================================================

netcdf_dep = dependency('netcdf', required: false)
hdf5_dep = dependency('hdf5', required: false)
zlib_dep = dependency('zlib', required: false)

if netcdf_dep.found() and hdf5_dep.found()
  swb_deps = [netcdf_dep, hdf5_dep, zlib_dep]
  message('Dependencies found via pkg-config')
else
  # Fallback for users without pixi/pkg-config
  netcdf_root = get_option('netcdf_root')
  if netcdf_root == ''
    if system == 'windows'
      netcdf_root = 'C:/Program Files/netCDF 4.9.3'
    elif system == 'darwin'
      netcdf_root = '/opt/homebrew/opt/netcdf'
    elif system == 'linux'
      netcdf_root = '/usr/local'
    else
      error('NetCDF not found. Use pixi, or specify -Dnetcdf_root=/path/to/netcdf')
    endif
  endif
  message('Using manual netcdf_root: ' + netcdf_root)
  zlib = cc.find_library('zlib', dirs: [netcdf_root / 'lib'], required: false)
  hdf5 = cc.find_library('hdf5', dirs: [netcdf_root / 'lib'], required: true)
  hdf5_hl = cc.find_library('hdf5_hl', dirs: [netcdf_root / 'lib'], required: true)
  netcdf = cc.find_library('netcdf', dirs: [netcdf_root / 'lib'], required: true)
  swb_deps = [zlib, hdf5, hdf5_hl, netcdf]
endif
```

This means:
- With pixi: `dependency()` finds everything via pkg-config. Zero configuration needed.
- Without pixi: falls back to the current behavior (hardcoded defaults or `-Dnetcdf_root=`).
- Cray HPC paths: gone. Use `PKG_CONFIG_PATH` or `-Dnetcdf_root=` on HPC systems.

---

## What Gets Eliminated

| Current artifact | Disposition |
|------------------|-------------|
| `build_swb__gfortran_windows.cmd` | Replaced by `pixi run -e gcc setup && pixi run build` |
| `build_swb__gfortran_windows.sh` | Same |
| `build_swb__gfortran_mac.sh` | Same |
| `build_swb__ifx_windows.cmd` | Replaced by `pixi run -e intel setup && pixi run build` |
| `build_swb__ifx_Hovenweep_Linux.sh` | Replaced by pixi or keep as HPC-specific (module loads) |
| `build_swb__ifx_windows__static_analysis.cmd` | Replaced by `pixi run -e intel sa-report` |
| `build_swb__gfortran_windows__static_analysis.cmd` | Replaced by `pixi run -e gcc sa-report` |
| `fix_linker__ifx_windows.exe` | Eliminated (see below) |
| `fix_linker__ifx_windows.cpp` | Eliminated |
| Hardcoded Cray HPC paths in `src/meson.build` | Eliminated |
| `netcdf_root` platform defaults | Kept as fallback only |

### The fix_linker hack

The `xilink.exe` → `link.exe` substitution in `build.ninja` is needed because meson's Intel compiler detection on Windows incorrectly selects the LLVM linker driver (`xilink`) instead of the Microsoft linker (`link`). This is a known meson bug.

**Possible resolutions:**
1. **Upgrade meson** — Check if meson ≥1.7 fixes this. The issue was reported and there are patches in meson's git history.
2. **Use meson's `--native-file`** to explicitly specify the linker:
   ```ini
   # intel-windows.ini
   [binaries]
   fortran = 'ifx'
   c = 'icx'
   cpp = 'icx'
   c_ld = 'link'
   fortran_ld = 'link'
   ```
   Then: `meson setup builddir --native-file=intel-windows.ini`
3. **pixi environment** — If the pixi Intel environment correctly sets PATH so that `link.exe` is found before `xilink.exe`, the problem may disappear.

The native file approach (option 2) is the cleanest — it's built into meson and doesn't require post-processing.

---

## Developer Experience: Before vs After

### Before (current)

```bash
# Windows + Intel:
# 1. Install oneAPI (2GB download, 20 min)
# 2. Install NetCDF 4.9.3 from Unidata website
# 3. Open oneAPI command prompt
# 4. Navigate to build/meson/
# 5. Run build_swb__ifx_windows.cmd
# 6. Hope the netcdf path matches

# Windows + gfortran:
# 1. Install MSYS2
# 2. pacman -S mingw-w64-ucrt-x86_64-gcc-fortran
# 3. Install NetCDF (from Unidata? from MSYS2? Unclear)
# 4. Figure out where libraries ended up
# 5. Edit build script paths
# 6. Run build_swb__gfortran_windows.cmd

# Linux:
# 1. apt install gfortran libnetcdf-dev (easy)
# 2. Run build script

# HPC:
# 1. module load the right versions in the right order
# 2. Run build script
```

### After (pixi)

```bash
# Any platform, any compiler:
pixi install                    # one-time: downloads everything
pixi run -e gcc setup           # configure (gfortran)
pixi run build                  # compile
pixi run test                   # test

# Or for Intel:
pixi run -e intel setup
pixi run build

# Static analysis:
pixi run -e gcc sa-report       # gfortran warnings → file
pixi run -e intel sa-report     # ifx warnings → file
```

Same commands on Windows, Linux, macOS. No path editing, no manual downloads, no platform-specific scripts.

---

## HPC Considerations

HPC systems (Hovenweep, Denali, etc.) typically don't allow pixi/conda and require `module load`. For these systems, keep a minimal build script:

```bash
#!/bin/bash
# HPC build script — uses system modules, not pixi
module purge
module load cray-netcdf cray-hdf5 intel-oneapi meson ninja

export FC=ifx CC=icx CXX=icx
export PKG_CONFIG_PATH=$CRAY_NETCDF_PREFIX/lib/pkgconfig:$CRAY_HDF5_PREFIX/lib/pkgconfig

meson setup builddir ../..
cd builddir && meson compile
```

The key insight: even on HPC, use `PKG_CONFIG_PATH` rather than hardcoded paths in `meson.build`. The Cray modules set `*_PREFIX` environment variables; expose them to pkg-config and the same meson.build works everywhere.

---

## CI Integration

```yaml
# .github/workflows/ci.yml
name: CI
on: [push, pull_request]

jobs:
  build:
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
        compiler: [gcc, intel]
    runs-on: ${{ matrix.os }}
    steps:
      - uses: actions/checkout@v4
      - uses: prefix-dev/setup-pixi@v0.9.5
      - run: pixi run -e ${{ matrix.compiler }} setup
      - run: pixi run build
      - run: pixi run test

  static-analysis:
    runs-on: ubuntu-latest
    strategy:
      matrix:
        compiler: [gcc, intel]
    steps:
      - uses: actions/checkout@v4
      - uses: prefix-dev/setup-pixi@v0.9.5
      - run: pixi run -e ${{ matrix.compiler }} sa-report
      - run: |
          if [ -s static_analysis_warnings.txt ]; then
            echo "::warning::Static analysis produced warnings"
            cat static_analysis_warnings.txt
            exit 1
          fi
```

---

## Migration Path

| Step | What | Breaks existing workflow? |
|------|------|--------------------------|
| 1 | Add `pixi.toml` to repo root | No — additive |
| 2 | Add pkg-config detection to `meson.build` (before fallback) | No — existing `-Dnetcdf_root` still works |
| 3 | Remove Cray HPC hardcoded paths from `src/meson.build` | Yes for Hovenweep script — update it to use `PKG_CONFIG_PATH` |
| 4 | Add native file for Intel Windows linker fix | No — optional, scripts still work |
| 5 | Test pixi workflow on all platforms | No |
| 6 | Update `design/developer_quickstart.md` | No |
| 7 | Delete platform-specific build scripts | Yes — but pixi replaces them |
| 8 | Delete `fix_linker__ifx_windows.*` | Yes — after native file or meson upgrade resolves it |

Steps 1–2 are additive and risk-free. Steps 3–8 can be done incrementally.

---

## What's Still Needed for Release Builds

pixi is for **development and CI**. For distributing `swb2.exe` to end users:

| Approach | How | Output |
|----------|-----|--------|
| gfortran + ship DLLs | Copy DLLs from pixi env alongside `.exe` | `swb2.exe` + 4-5 DLLs |
| Intel static | `pixi run -e intel setup-static` + `/MT` + static libs | Single `swb2.exe` |
| MSYS2 static (gfortran) | Separate MSYS2 build for release only | Single `swb2.exe` |

A `pixi run release` task could automate the DLL-copying approach:

```toml
[tasks]
release = { cmd = "meson compile -C builddir && python scripts/package_release.py builddir" }
```

Where `package_release.py` copies the executable + required DLLs into a `dist/` directory.

---

## Effort Estimate

| Task | Effort |
|------|--------|
| Create and test `pixi.toml` | 2–3 hours |
| Add pkg-config detection to `meson.build` | 1 hour |
| Create Intel native file (linker fix) | 30 min |
| Test on Windows (gfortran + Intel) | 2–3 hours |
| Test on Linux/macOS | 1 hour |
| Update HPC script to use `PKG_CONFIG_PATH` | 30 min |
| Update developer docs | 1 hour |
| Remove old scripts + hack (after verification) | 30 min |
| **Total** | **~1.5 days** |

---

## Summary

The revamped build process:

1. **pixi** manages compilers, libraries, and tools — same command everywhere
2. **pkg-config** provides library paths to meson — no hardcoded locations
3. **Meson native files** handle linker quirks — no post-build hacks
4. **Fallback path** preserved for users who can't/won't use pixi
5. **One set of commands** replaces 6 platform-specific scripts

The result: `pixi install && pixi run build` from a clean checkout on any supported platform.
