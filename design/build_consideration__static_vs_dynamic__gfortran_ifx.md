# Build Consideration: Static vs Dynamic Linking (gfortran and Intel ifx)

**Date:** May 2026  
**Context:** SWB2 dependencies are netCDF, HDF5, zlib, and (future) PROJ + sqlite3  

---

## The Three Options

| Option | What it means | DLLs to ship |
|--------|--------------|-------------|
| Fully static | Single .exe, no DLLs | None |
| Static Fortran + Dynamic C libs | Fortran runtime baked in, C libs as DLLs | 4-5 DLLs |
| Fully dynamic | Everything as DLLs | Many DLLs including Fortran runtime |

---

## With gfortran (MinGW, installed via mamba/conda)

### Fully static: Possible but painful

You need **every** C library (netCDF, HDF5, zlib, PROJ, sqlite3) compiled as static `.a` archives with the MinGW toolchain. The dependency chain:

```
swb2.exe
  ├── libnetcdf.a (needs libhdf5.a, libz.a, libcurl.a maybe)
  │     ├── libhdf5.a (needs libz.a, libsz.a maybe)
  │     │     └── libz.a
  │     └── libz.a
  ├── libproj.a (needs libsqlite3.a, libtiff.a maybe)
  │     └── libsqlite3.a
  ├── libgfortran.a (from -static-libgfortran)
  ├── libquadmath.a
  └── libgcc.a
```

**Where to get MinGW-compatible static libs:**

| Source | Pros | Cons |
|--------|------|------|
| MSYS2 (`pacman -S mingw-w64-x86_64-netcdf mingw-w64-x86_64-proj`) | Pre-built, correct ABI, includes static libs | Separate from pixi; MSYS2 is its own ecosystem |
| conda-forge `libnetcdf` | Convenient if already using pixi | Built with MSVC — **static linking won't work** with MinGW |
| Build from source | Full control | Enormous effort (netCDF → HDF5 → zlib → PROJ → sqlite3) |

**Verdict:** MSYS2 is the only realistic path for a fully static gfortran build on Windows. You'd use MSYS2 for release builds and pixi for development.

The meson invocation:
```bash
# Inside MSYS2 MinGW64 shell
meson setup builddir_static \
  -Ddefault_library=static \
  --prefer-static \
  -Dnetcdf_root=/mingw64
meson compile -C builddir_static
```

With link args: `-static -static-libgfortran -static-libgcc -static-libquadmath`

This is essentially what SWB2 does today (minus PROJ as an external dep). It works, it's just annoying to set up.

### Static Fortran + Dynamic C libs: The pragmatic middle ground

```
swb2.exe (statically links gfortran runtime)
  ├── netcdf.dll
  ├── hdf5.dll
  ├── hdf5_hl.dll
  ├── zlib1.dll (or zlib.dll)
  └── proj.dll (+ proj.db)
```

This is what you'd get naturally from pixi/conda-forge with gfortran:
- Fortran runtime is statically linked (`-static-libgfortran -static-libgcc`)
- C libraries are dynamically linked (conda-forge provides DLLs)
- The `.exe` has no dependency on MinGW runtime DLLs
- Users just need the DLLs in the same directory as the executable

**This is the recommended approach for gfortran.** The DLLs total ~5-10MB and eliminate all ABI headaches.

---

## With Intel Fortran (ifx, oneAPI)

### Fully static: Easier than gfortran

Intel Fortran on Windows uses the MSVC ABI. This means:
- Conda-forge's MSVC-built `netcdf.lib`, `hdf5.lib`, etc. are **directly compatible**
- You can statically link everything because it's all the same ABI
- Intel provides `/MT` (static CRT) which eliminates even the MSVC runtime DLL dependency

```
swb2.exe (fully self-contained)
  ├── netcdf.lib (static)
  ├── hdf5.lib (static)
  ├── hdf5_hl.lib (static)
  ├── zlib.lib (static)
  ├── proj.lib (static)
  ├── sqlite3.lib (static)
  └── Intel Fortran runtime (static via /MT)
```

**Where to get MSVC-compatible static libs:**

| Source | Works? |
|--------|--------|
| conda-forge (via pixi) | Yes — provides both `.lib` (static) and `.dll` (dynamic) |
| vcpkg (`vcpkg install netcdf-c hdf5 zlib proj`) | Yes — designed for exactly this |
| Unidata pre-built NetCDF installer | Partially — provides DLLs and import libs, not always static libs |

The meson invocation:
```bash
meson setup builddir -Ddefault_library=static
meson compile -C builddir
```

With Intel flags: `/MT` (already in the current SWB2 `meson.build`)

**Verdict:** Fully static with Intel on Windows is the cleanest path to a single-file distributable. This is likely why MODFLOW6 uses Intel for their Windows release builds.

---

## Summary Table

| Scenario | Compiler | Linking | DLLs to ship | Difficulty | Recommended? |
|----------|----------|---------|-------------|-----------|--------------|
| Dev/CI (any platform) | gfortran | Dynamic C libs | N/A (not distributing) | Easy (pixi) | ✅ Yes |
| Release (Windows) | gfortran | Static Fortran + Dynamic C | 4-5 DLLs (~10MB) | Easy | ✅ Yes |
| Release (Windows) | gfortran | Fully static | None | Hard (MSYS2 only) | ⚠️ If you must |
| Release (Windows) | Intel ifx | Fully static | None | Medium (vcpkg or conda) | ✅ Yes |
| Release (Linux) | gfortran | Fully static | None | Easy (system libs have static versions) | ✅ Yes |
| Release (macOS) | gfortran | Dynamic | Bundled in .app or Homebrew | Medium | ✅ Yes |

---

## Recommendation

**Ship DLLs with gfortran builds. Use Intel for fully-static release builds if you need a single .exe.**

The reasoning:

1. For development and CI, pixi + gfortran + dynamic linking is frictionless
2. For distribution to users, 4-5 DLLs alongside the .exe is completely normal — every GIS tool does this (QGIS ships hundreds of DLLs)
3. If you truly need a single portable .exe (e.g., for users who just want to drop it in a directory), do the release build with Intel ifx + static linking
4. The fully-static gfortran path via MSYS2 works but is a parallel build environment to maintain — it's only worth it if you can't use Intel

---

## How MODFLOW6 Handles This

MODFLOW6 does exactly this split:
- gfortran for development and CI
- Intel for official releases
- Their CI has a `build-static` job that verifies the gfortran static build has no dynamic dependencies (using `ldd` on Linux and `ntldd` on Windows) — but that's for builds **without** netCDF
- Their netCDF builds are always dynamic

---

## Practical Workflow for SWB2

```
Development (daily work):
  pixi + gfortran + dynamic linking
  Fast iteration, no library hassles

CI (GitHub Actions):
  pixi + gfortran + dynamic linking
  Same as development, tests pass/fail quickly

Release build (distributable):
  Option A: gfortran + ship DLLs (easy, ~10MB extra)
  Option B: Intel ifx + fully static (single .exe, requires oneAPI license)
  Option C: MSYS2 gfortran + fully static (no DLLs, harder to set up)
```

For the `proj.db` runtime file (if adopting modern PROJ): it must always be shipped alongside the executable regardless of static/dynamic linking, since it's a data file, not a library.
