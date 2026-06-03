# Feature Consideration: Adopt Modern PROJ Library

**Date:** May 2026  
**Status:** Proposed (Phase 3+ task, after CI is in place)  

---

## Current State

SWB2 bundles a static snapshot of PROJ4 source code (~168 C files in `src/proj4/`). This was done to keep the build simple by avoiding the sqlite3 dependency that modern PROJ requires.

### Current Usage (Very Narrow)

1. **One C wrapper function** (`src/proj4/custom_swb_functions.c`, ~50 lines) — `pj_init_and_transform()` takes two PROJ4 strings, transforms an array of x/y coordinates, frees resources.

2. **One Fortran call site** (`data_catalog_entry.F90:2166`) — transforms 4 corner points when an input grid's CRS differs from the model grid's CRS.

3. **One Fortran string parser** (`proj4_support.F90`) — parses PROJ4 strings into CF-convention attribute name/value pairs for NetCDF output metadata. This is pure string manipulation and does not call the PROJ library.

4. **160 references to "proj4"** across 11 files — mostly the `sPROJ4_string` member variable name on grid types, not library calls.

---

## Benefits of Modern PROJ (≥6.x / 9.x)

| Benefit | Impact for SWB2 |
|---------|----------------|
| WKT2 support | Users could define CRS via WKT strings (standard, unambiguous) |
| EPSG database | Users could specify `EPSG:26915` instead of full PROJ4 strings |
| Datum transformations | Proper pipeline-based transforms (PROJ4 strings can't express these) |
| Accuracy improvements | Better algorithms for some projections |
| Active maintenance | Security fixes, bug fixes |
| Thread safety | Modern PROJ is thread-safe (old PROJ4 has global state) |
| Repo cleanup | Remove 168 unmaintained C files from the repository |

---

## Costs and Risks

| Cost | Severity | Notes |
|------|----------|-------|
| sqlite3 dependency | Medium | PROJ ≥6 requires sqlite3 for the EPSG database. Trivial on Linux/macOS (system library). One more lib on Windows. |
| Runtime data files | Medium | PROJ needs `proj.db` (and optionally grid shift files) at runtime. Must be distributed with the executable or found via `PROJ_DATA` env var. |
| Windows linking complexity | Medium-High | Static linking PROJ on Windows with gfortran is non-trivial. Options: (a) build from source with MinGW, (b) link dynamically against pre-built DLL, (c) use conda-forge package. |
| API change | Low | Old `pj_init_plus`/`pj_transform` API is deprecated but still works via compatibility layer. Clean migration uses new `proj_create`/`proj_trans` API. |
| Build time | Low (improvement) | Remove 168 C files, replace with a `find_library` or pkg-config call. |
| Backward compatibility | None | Modern PROJ still accepts PROJ4 strings as input to `proj_create()`. No user-facing breakage. |

---

## The Interface Change

### Current C wrapper (`custom_swb_functions.c`):

```c
#include <projects.h>

int pj_init_and_transform(const char *from_projection, const char *to_projection,
    const char *caller_name, int caller_linenum, long point_count, 
    double *x, double *y) {
    
    projPJ pj_from = pj_init_plus(from_projection);
    projPJ pj_to = pj_init_plus(to_projection);
    int p = pj_transform(pj_from, pj_to, point_count, 1, x, y, NULL);
    pj_free(pj_to);
    pj_free(pj_from);
    return p;
}
```

### Modern equivalent:

```c
#include <proj.h>

int pj_init_and_transform(const char *from_crs, const char *to_crs,
    const char *caller_name, int caller_linenum, long point_count,
    double *x, double *y) {

    PJ_CONTEXT *ctx = proj_context_create();
    PJ *P = proj_create_crs_to_crs(ctx, from_crs, to_crs, NULL);
    if (!P) {
        fprintf(stderr, "PROJ ERROR: Cannot create transform from '%s' to '%s'\n"
                "  Called by: %s, line %d\n", from_crs, to_crs, caller_name, caller_linenum);
        proj_context_destroy(ctx);
        return -1;
    }

    PJ *P_norm = proj_normalize_for_visualization(ctx, P);
    proj_destroy(P);
    P = P_norm;

    for (long i = 0; i < point_count; i++) {
        PJ_COORD c = proj_coord(x[i], y[i], 0, 0);
        PJ_COORD result = proj_trans(P, PJ_FWD, c);
        x[i] = result.xy.x;
        y[i] = result.xy.y;
    }

    proj_destroy(P);
    proj_context_destroy(ctx);
    return 0;
}
```

The key win: `from_crs` and `to_crs` can now be PROJ4 strings, WKT, WKT2, or `EPSG:XXXX` — the same function handles all formats. The Fortran interface (`bind(c)` declaration in `grid.F90`) does not change at all.

---

## Linking Strategy on Windows

Three realistic options:

1. **Dynamic linking** (easiest): Ship `proj.dll` + `proj.db` alongside `swb2.exe`. Use `cc.find_library('proj')` in meson. This is what most Windows GIS software does.

2. **Static linking with conda-forge**: Use conda-forge's PROJ package (includes static libs). This is what MODFLOW6 does for its dependencies via pixi/conda.

3. **Keep bundled PROJ4 as fallback**: Make modern PROJ optional — if found, use it; if not, fall back to the bundled PROJ4. Lowest-risk migration path.

---

## Migration Plan

1. Add `proj` as an external dependency in `meson.build` (via pkg-config or `find_library`)
2. Rewrite `custom_swb_functions.c` to use the modern `proj.h` API (~50 lines)
3. Update `proj4_support.F90` to also accept WKT strings (detect by checking if input starts with `PROJCRS[` or `GEOGCRS[` or `EPSG:`)
4. Rename `sPROJ4_string` → `sCRS_string` across the codebase (or alias it)
5. Keep the old PROJ4 string parsing as a fallback path in `proj4_support.F90`
6. On Windows: distribute `proj.dll` + `proj.db` or use conda/pixi for the dependency
7. Remove `src/proj4/` directory (168 files)

---

## User-Facing Change

Control files could specify CRS as any of:

```
# Existing PROJ4 string (continues to work unchanged)
PROJECTION_DEFINITION  +proj=aea +lat_1=29.5 +lat_2=45.5 +datum=NAD83 +units=m

# New: EPSG code
PROJECTION_DEFINITION  EPSG:5070

# New: WKT2 (for unambiguous CRS definition)
PROJECTION_DEFINITION  WKT2:PROJCRS["NAD83 / Conus Albers", ...]
```

Existing PROJ4 strings continue to work with zero changes to user input files.

---

## Effort Estimate

| Task | Effort |
|------|--------|
| Meson build changes | 1 hour |
| Rewrite C wrapper | 1 hour |
| Update `proj4_support.F90` for WKT detection | 2-4 hours |
| Rename `sPROJ4_string` → `sCRS_string` (160 refs, 11 files) | 2 hours |
| Windows linking/packaging | 4-8 hours (most of the work) |
| Testing across platforms | 4 hours |
| **Total** | **~2-3 days** |

---

## Recommendation

**Do it, but after CI is in place (Phase 3+).** The interface surface is tiny, the migration is mechanically simple, and WKT/EPSG support is a genuine user-facing improvement. The bundled PROJ4 code is unmaintained dead weight. The sqlite3/data-file dependency is the only real complication, and it's manageable — especially if pixi/conda is adopted for dependency management (as proposed in the main improvement plan).


---

## Appendix: What Does "Managing the sqlite3 Dependency" Entail?

> **Prompt:** "You say that a dependency would become sqlite, but that it is manageable. Can you elaborate on what that (sqlite management) entails?"

The sqlite3 dependency is indirect — you never call sqlite3 yourself. PROJ uses it internally to query `proj.db`, which is an sqlite3 database containing the EPSG registry (coordinate system definitions, datum shift parameters, etc.). Here's what "managing" it actually means on each platform:

### Linux

Nothing to do. `libsqlite3` is a system library on every distro. When you install PROJ via package manager (`apt install libproj-dev`), sqlite3 comes along automatically. Your meson `dependency('proj')` just works.

### macOS

Same story. `brew install proj` pulls sqlite3 as a dependency automatically. No action needed.

### Windows (where it gets annoying)

This is the only platform where it matters, and there are a few sub-scenarios:

**If you link PROJ dynamically (recommended):**
- You ship `proj.dll` with your executable. That DLL has sqlite3 compiled *into* it already (PROJ's Windows builds typically statically link sqlite3 into the PROJ DLL). So you don't ship a separate `sqlite3.dll`.
- You *do* need to ship `proj.db` (~8MB file) and either place it next to the executable or set `PROJ_DATA` environment variable to point to it.
- Pre-built PROJ DLLs are available from conda-forge, OSGeo4W, or vcpkg.

**If you link PROJ statically (harder):**
- You need `libproj.a` which was itself built with sqlite3 statically linked in. This means you need a MinGW-compatible static build of PROJ — which means building PROJ from source with CMake under MSYS2/MinGW, or getting it from conda-forge's mingw packages.
- You still need `proj.db` at runtime.

**If you use conda-forge / pixi (cleanest):**
- `pixi add proj` in your `pixi.toml` gives you headers, libraries, and `proj.db` all in the conda environment. Meson finds it via pkg-config. This is the approach MODFLOW6 uses for its dependencies and sidesteps all manual library management.

### The `proj.db` Runtime Requirement

This is the part that's slightly different from the current setup (where the bundled PROJ4 has no runtime data files). With modern PROJ:

1. PROJ looks for `proj.db` in a search path: `PROJ_DATA` env var → compiled-in default path → relative to the library location.
2. If `proj.db` isn't found, `proj_create_crs_to_crs()` will fail when given EPSG codes or WKT — but it will still succeed with raw PROJ4 strings (those don't need the database).
3. For distribution, you'd either:
   - Bundle `proj.db` next to `swb2.exe` and set the search path in the C wrapper via `proj_context_set_search_paths()`
   - Or document that users need PROJ installed (e.g., via OSGeo4W or conda)

### Bottom Line

"Managing sqlite3" really means: **on Windows, make sure your PROJ library was built with sqlite3 included, and ship `proj.db` alongside the executable.** On Linux/macOS it's invisible. If you adopt pixi/conda for dependency management (as in the main improvement plan), it's invisible on all platforms — the package manager handles everything.
