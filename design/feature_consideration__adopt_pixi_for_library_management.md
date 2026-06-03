# Feature Consideration: Adopt Pixi for Library and Build Management

**Date:** May 2026  
**Status:** Proposed (high-ROI, likely the single biggest build-friction reduction)  

---

## The Problem

SWB2's current build requires users to manually install NetCDF (with HDF5 and zlib) and tell meson where to find it. On Windows this is a major pain point:

- NetCDF must be downloaded and installed manually
- The user must know the exact installation path and pass `-Dnetcdf_root=...`
- HDF5 and zlib versions must be compatible with the installed NetCDF
- Platform-specific defaults are hardcoded in `meson.build`
- pkg-config doesn't work on native Windows without a package manager
- Static linking requires all libraries to be built with the same compiler/ABI

The current `meson.build` has ~30 lines of platform-specific library detection code to cope with this.

---

## What Pixi Is

[Pixi](https://pixi.sh) is a cross-platform package manager and task runner built on conda-forge. It:

- Installs pre-built libraries (netCDF, HDF5, zlib, PROJ, compilers) into a local `.pixi/` directory
- Provides `pkg-config` and correctly-configured `.pc` files
- Sets environment variables (`PKG_CONFIG_PATH`, `PATH`, etc.) when running tasks
- Works identically on Windows, Linux, and macOS
- Locks dependency versions for reproducibility (`pixi.lock`)
- Requires no system-level installation — just a single binary

MODFLOW6 uses pixi for exactly this purpose. Their key insight: the `setup` task sets `PKG_CONFIG_PATH = "$CONDA_PREFIX/lib/pkgconfig"`, which makes meson's `dependency()` calls find everything automatically.

---

## How It Works on Windows (The Key Question)

### Why pkg-config normally fails on Windows

In a normal Windows development setup, pkg-config is either not installed, can't find `.pc` files (no standard location), or the paths inside `.pc` files use Unix-style paths.

### How pixi makes it work

1. `pixi install` downloads pre-built Windows packages from conda-forge
2. Libraries go into `.pixi/envs/default/Library/lib/`, headers into `.pixi/envs/default/Library/include/`
3. Pixi also installs `pkg-config` itself as a conda package
4. `.pc` files are placed in `.pixi/envs/default/Library/lib/pkgconfig/` with correct Windows paths
5. When you run `pixi run setup`, the `PKG_CONFIG_PATH` environment variable points to that directory
6. Meson's `dependency('netcdf')` calls pkg-config, which finds the `.pc` file, which has correct paths

Everything is self-consistent because pixi controls the entire environment.

### ABI caveat

Conda-forge Windows packages are typically built with MSVC. With gfortran (MinGW):
- **Dynamic linking works** (DLL linking is cross-ABI compatible)
- **Static linking does not** (can't link MSVC `.lib` files with MinGW's linker)

For development and CI, dynamic linking via pixi is the path of least resistance. For distribution, you'd either ship DLLs alongside the executable or build a static release using MSYS2's MinGW packages.

---

## What the Migration Looks Like

### New file: `pixi.toml`

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
libnetcdf = "*"       # pulls in hdf5, zlib automatically
gfortran = "13.*"
gcc = "13.*"

[tasks]
setup = { cmd = "meson setup builddir --prefix=$PIXI_PROJECT_ROOT", env = { PKG_CONFIG_PATH = "$CONDA_PREFIX/lib/pkgconfig" } }
build = "meson compile -C builddir"
test = "meson test --verbose --no-rebuild -C builddir"
clean = "rm -rf builddir"

[target.win-64.tasks]
setup = { cmd = "meson setup builddir --prefix=%PIXI_PROJECT_ROOT%", env = { PKG_CONFIG_PATH = "%CONDA_PREFIX%/Library/lib/pkgconfig" } }
```

Note: conda-forge on Windows puts libraries under `Library/` rather than directly in the prefix, hence the platform-specific task override.

### Simplified `meson.build`

```meson
# Try pkg-config first (works inside pixi, works on Linux with system packages)
netcdf_dep = dependency('netcdf', required: false)
hdf5_dep = dependency('hdf5', required: false)

if netcdf_dep.found() and hdf5_dep.found()
  # pkg-config found everything — done
  all_deps = [netcdf_dep, hdf5_dep]
else
  # Fallback: manual search (for users not using pixi)
  netcdf_root = get_option('netcdf_root')
  if netcdf_root == ''
    error('NetCDF not found via pkg-config. Please specify -Dnetcdf_root=/path/to/netcdf')
  endif
  zlib = cc.find_library('zlib', dirs: [netcdf_root / 'lib'], required: false)
  hdf5 = cc.find_library('hdf5', dirs: [netcdf_root / 'lib'], required: true)
  hdf5_hl = cc.find_library('hdf5_hl', dirs: [netcdf_root / 'lib'], required: true)
  netcdf = cc.find_library('netcdf', dirs: [netcdf_root / 'lib'], required: true)
  all_deps = [zlib, hdf5, hdf5_hl, netcdf]
endif
```

This preserves backward compatibility: users without pixi can still pass `-Dnetcdf_root=...`.

---

## Developer Experience Comparison

### Current (manual)

```bash
# 1. Download and install NetCDF 4.9.3 from Unidata (Windows installer)
# 2. Hope HDF5 and zlib versions are compatible
# 3. Figure out where it installed
# 4. Configure:
meson setup builddir -Dnetcdf_root="C:/Program Files/netCDF 4.9.3"
meson compile -C builddir
```

### With pixi

```bash
# One-time setup (downloads ~200MB: netcdf, hdf5, zlib, compilers, pkg-config)
pixi install

# Build (works identically on Windows, Linux, macOS)
pixi run setup
pixi run build
pixi run test
```

---

## What Pixi Solves

| Pain Point | How Pixi Fixes It |
|-----------|-------------------|
| "Where is netCDF installed?" | It's in `.pixi/envs/default/Library/` (Win) or `.pixi/envs/default/` (Linux/Mac) |
| "Which HDF5 version is compatible?" | Conda-forge's solver ensures mutual compatibility |
| "pkg-config can't find .pc files" | Pixi sets `PKG_CONFIG_PATH` in the task environment |
| "I need to install netCDF manually" | `pixi install` does it |
| "It works on my machine but not CI" | CI runs `pixi install` + `pixi run build` — identical environment |
| "Windows vs Linux paths" | Pixi abstracts this; `.pc` files have correct platform paths |
| "Static vs dynamic linking?" | Conda-forge provides both; choose in meson via `static: true/false` |

---

## How MODFLOW6 Does It

MODFLOW6's `pixi.toml` declares `netCDF4` (Python bindings) in its main dependencies and `libnetcdf` + `netcdf-fortran` in the `gcc-extended-build` feature. Their setup task:

```toml
setup = { cmd = "meson setup --prefix=$(pwd) --libdir=bin --bindir=bin", env = { PKG_CONFIG_PATH = "$CONDA_PREFIX/lib/pkgconfig" } }
```

They also have a helper script (`gcc-extended-build-update-pc-files.py`) that patches `.pc` files when needed — but this is for their extended parallel build with PETSc, not for basic netCDF usage.

Note: MODFLOW6's *standard* build doesn't use netCDF (it's optional for them). Their Windows netCDF path still has some manual elements for Intel compiler builds. SWB2 would actually have a cleaner story here since netCDF is always required.

---

## CI Integration

With pixi, the GitHub Actions CI becomes trivial:

```yaml
steps:
  - uses: actions/checkout@v4
  - uses: prefix-dev/setup-pixi@v0.9.5
    with:
      pixi-version: v0.41.4
  - run: pixi run setup
  - run: pixi run build
  - run: pixi run test
```

No manual compiler installation, no manual netCDF installation, no platform-specific library paths. Works on `ubuntu-latest`, `windows-latest`, and `macos-latest` with the same workflow.

---

## Distribution Considerations

For distributing `swb2.exe` to end users (who won't have pixi):

- **Dynamic linking approach:** Ship `netcdf.dll`, `hdf5.dll`, `hdf5_hl.dll`, `zlib.dll` alongside `swb2.exe`. These can be copied from the pixi environment during a release build step.
- **Static linking approach:** Use MSYS2's MinGW packages (`pacman -S mingw-w64-x86_64-netcdf`) for the release build, which provides MinGW-ABI-compatible static libraries.
- **Hybrid:** Use pixi for development/CI (fast iteration), MSYS2 for release builds (static executable).

---

## Effort Estimate

| Task | Effort |
|------|--------|
| Create `pixi.toml` with dependencies and tasks | 1-2 hours |
| Simplify `meson.build` netCDF detection (pkg-config first, fallback second) | 2-3 hours |
| Test on Windows (pixi install + build) | 2-4 hours |
| Test on Linux/macOS | 1 hour |
| Update developer docs | 1 hour |
| **Total** | **~1 day** |

---

## Recommendation

This is probably the single highest-ROI change in the entire improvement plan. It eliminates the #1 source of build friction on Windows, makes CI trivial to set up, and provides a reproducible development environment across all platforms. It should be done early (Phase 2) since it makes everything else easier — including the PROJ modernization, test-drive adoption, and CI pipeline setup.
