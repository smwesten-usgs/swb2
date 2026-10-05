# Feature Consideration: Reading and Writing GeoTIFFs via GDAL

**Date:** May 2026  
**Status:** Proposed (Phase 3+, after PROJ modernization)  

---

## Motivation

SWB2 currently reads Arc ASCII grids and NetCDF files. GeoTIFF is the dominant raster format in the GIS world — supporting it would reduce friction for users who work with standard GIS tools (QGIS, ArcGIS, R, Python/rasterio).

---

## Options Evaluated

### Option A: Thin C Wrapper Around GDAL (Recommended)

Write a small C file (50-100 lines) exposing `read_geotiff()` and `write_geotiff()` using the GDAL C API. Call from Fortran via `iso_c_binding` — same pattern as the current PROJ4 wrapper.

| Aspect | Detail |
|--------|--------|
| Dependency | GDAL (~30MB DLL on Windows; depends on PROJ, libtiff, libgeotiff, sqlite3, curl) |
| Effort | ~3-4 days |
| Formats gained | GeoTIFF + any other GDAL-supported format (GRIB, COG, etc.) |
| Availability | conda-forge/pixi on all platforms |

**Example C wrapper:**
```c
#include "gdal.h"
#include "cpl_conv.h"

int read_geotiff_float(const char *filename, float *data,
                       int *ncols, int *nrows, double *geotransform,
                       char *proj_wkt, int proj_wkt_len) {
    GDALAllRegister();
    GDALDatasetH ds = GDALOpen(filename, GA_ReadOnly);
    if (!ds) return -1;

    *ncols = GDALGetRasterXSize(ds);
    *nrows = GDALGetRasterYSize(ds);
    GDALGetGeoTransform(ds, geotransform);
    strncpy(proj_wkt, GDALGetProjectionRef(ds), proj_wkt_len);

    GDALRasterBandH band = GDALGetRasterBand(ds, 1);
    GDALRasterIO(band, GF_Read, 0, 0, *ncols, *nrows,
                 data, *ncols, *nrows, GDT_Float32, 0, 0);

    GDALClose(ds);
    return 0;
}
```

### Option B: FortranGIS (ARPA-SIMC/fortrangis)

A Fortran `iso_c_binding` interface to GDAL, Shapelib, and PROJ.

| Aspect | Detail |
|--------|--------|
| Status | Last release v2.5 (2016), commits through 2024. Single maintainer, 51 stars. LGPL-3.0. |
| Effort | ~1-2 days to integrate |
| Pros | Already written; covers GDAL + PROJ + Shapelib |
| Cons | Uncertain maintenance; autotools build (not meson); overkill for SWB2's needs |

### Option C: Direct libtiff + libgeotiff Bindings (No GDAL)

Write `iso_c_binding` interfaces directly to libtiff and libgeotiff.

| Aspect | Detail |
|--------|--------|
| Effort | ~1 week |
| Pros | Smaller dependency than GDAL |
| Cons | Must handle TIFF strip/tile layout, compression, data types manually; only reads GeoTIFF (no other formats); more code to maintain |

### Option D: Shell Out to GDAL Command-Line Tools

Use `gdal_translate` to convert GeoTIFF ↔ Arc ASCII as a preprocessing step.

| Aspect | Detail |
|--------|--------|
| Effort | Zero code changes |
| Pros | No new dependencies |
| Cons | Not integrated; slow for large files; requires user to run extra steps |

---

## Recommendation

**Option A (thin GDAL C wrapper)** is the right choice:

1. **PROJ comes bundled** — if adopting GDAL, you get coordinate system handling and GeoTIFF in one dependency rather than two
2. **Tiny interface surface** — SWB2 needs maybe 4 C functions: `open_raster`, `read_band_float`, `write_geotiff_float`, `close_raster`
3. **GDAL gives you everything** — compression (LZW, DEFLATE), tiling, Cloud-Optimized GeoTIFF (COG), overviews, any projection via WKT/EPSG, nodata handling
4. **Same linking story as PROJ** — dynamic linking on Windows, ship `gdal.dll`, available via conda-forge/pixi
5. **FortranGIS is risky** — single maintainer, last release 2016. A 50-line C wrapper is less code than integrating their library.

---

## Integration Points in SWB2

### New files:
- `src/gdal_c_interface.c` (~100 lines: read_geotiff, write_geotiff, get_raster_info)
- `src/geotiff_support.F90` (~200 lines: Fortran module with `bind(c)` interfaces + higher-level `read_grid_from_geotiff` returning a `GENERAL_GRID_T`)

### Modified files:
- `src/data_catalog_entry.F90` — add GeoTIFF as a recognized input format
- `src/output.F90` — optionally write GeoTIFF output
- `src/meson.build` — add GDAL dependency, new source files

### Dispatch integration:

```fortran
select case (this%iFileType)
  case (FILETYPE_ARC_GRID)
    call read_arc_ascii_grid(...)
  case (FILETYPE_NETCDF)
    call read_netcdf_grid(...)
  case (FILETYPE_GEOTIFF)      ! NEW
    call read_geotiff_grid(...)
end select
```

---

## Dependency Consolidation Opportunity

If GDAL is adopted, it could potentially **subsume** the separate PROJ and netCDF dependencies for I/O:
- GDAL reads/writes NetCDF files natively
- GDAL includes PROJ for all coordinate transformations
- GDAL reads Arc ASCII grids too

For *input*, everything could route through GDAL. The specialized NetCDF *output* (with CF-convention metadata, time dimensions, etc.) would likely remain as direct netCDF C API calls since GDAL's NetCDF writer is less flexible for scientific data.

This is a longer-term architectural consideration — not something to do in the initial GeoTIFF implementation.

---

## Caveats: Easy to Underestimate, Not Show-Stoppers (added 2026-09-11)

None of these blocks GDAL adoption, but **missing any one produces subtle, hard-to-debug
problems** rather than a clean failure. They arise because adopting GDAL is also the
natural vehicle for retiring the bundled PROJ4 (see the "Relationship" section and
`feature_consideration__adopt_modern_PROJ_library.md`), and the SWB2↔PROJ seam has
legacy assumptions baked into the *call sites*, not just the wrapper. Verified by tracing
the transform code on 2026-09-11 (`grid.F90` `grid_Transform` ~line 1537;
`data_catalog_entry.F90` direct call ~line 2107).

The transform call surface is genuinely small — **one C wrapper (`pj_init_and_transform`)
called from two Fortran sites** (`grid_Transform`, and one direct corner-point call) plus
the pure-string parser in `proj4_support.F90`. But watch these three:

1. **Radians vs. degrees (silent 57× error).** Legacy PROJ4 expects *geographic*
   coordinates in **radians**, so `grid_Transform` and the direct call site manually
   multiply by `DEGREES_TO_RADIANS` before the transform and `RADIANS_TO_DEGREES` after.
   **Modern PROJ / GDAL work in degrees** (with `proj_normalize_for_visualization`, or
   GDAL's `OCTNewCoordinateTransformation`). If the migration reuses the wrapper but
   leaves these conversion blocks in place, lat/lon transforms are wrong by a factor of
   ~57.3 — and it will **not** crash, just produce garbage coordinates. The conversion
   blocks must be **removed at both call sites** as part of the swap.

2. **CRS-type detection by string-sniffing breaks on WKT/EPSG.** The radians logic keys
   off `csFromPROJ4 .containssimilar. "latlon"/"lonlat"/…`, and `swbstats2` keys its
   volume-unit factor off `.containssimilar. "units=m"/"us-ft"/"ft"` in the PROJ4 text.
   **Those tokens do not exist in a WKT2 or `EPSG:XXXX` string** — exactly the new input
   formats this upgrade enables. So any code that sniffs the CRS string must be replaced
   with a real PROJ/GDAL query (`proj_get_type`, coordinate-system axis/unit query), or
   the moment a user supplies `EPSG:5070` the geographic-vs-projected and unit logic
   misfires. This couples the GeoTIFF/PROJ upgrade to a small cleanup in **both**
   `grid.F90` and `swbstats2` (see the CF-compliance note in the VA project design for
   the swbstats2 side).

3. **Error-return semantics differ.** `grid_CheckForPROJ4Error` decodes the old
   `pj_transform` integer-code contract. Modern `proj_trans` reports per-coordinate
   `HUGE_VAL`/errno, and GDAL uses `CPLGetLastErrorNo`/return flags. This routine needs
   reworking, not a pass-through — otherwise transform failures may go undetected (points
   silently set to `HUGE_VAL` and propagated).

**Additional GeoTIFF-specific gotchas** (distinct from the PROJ seam):

- **GeoTransform vs. cell-center coordinates.** GDAL's 6-element geotransform is
  **pixel-corner** origin (top-left), and Y resolution is **negative** (north-up). SWB2's
  grids reason in cell terms; mixing corner vs. center by half a cell, or dropping the
  Y-sign, shifts everything by one pixel or flips the raster. Convert deliberately.
- **NoData translation.** GeoTIFF NoData must be mapped to/from SWB2's internal
  nodata/`_FillValue` convention (and the AWC<0 active-cell masking convention this
  project relies on) — a silent mismatch turns masked cells into real zeros or vice versa.
- **Data-type and band assumptions.** The example wrapper reads band 1 as `Float32`;
  real inputs may be integer (land use), multi-band, or tiled/compressed. Assert the band
  count / type rather than assuming.
- **`proj.db` at runtime.** Same asterisk as the PROJ doc: GDAL (which bundles PROJ)
  still needs `proj.db` findable at runtime (`PROJ_DATA`/search path), or EPSG/WKT
  resolution fails while raw PROJ4 strings still work — a confusing partial failure.

## Effort Estimate

| Task | Effort |
|------|--------|
| Write C wrapper (read + write + info) | 4 hours |
| Write Fortran `geotiff_support.F90` module | 4 hours |
| Integrate into `data_catalog_entry.F90` (read path) | 4 hours |
| Integrate into `output.F90` (write path) | 4-8 hours |
| Add GDAL to meson.build / pixi.toml | 1 hour |
| Testing | 4 hours |
| **Total** | **~3-4 days** |

Assumes pixi is already adopted (GDAL is trivial via `pixi add libgdal`). Without pixi, finding GDAL on Windows adds another half-day.

**The ~3-4 days covers GeoTIFF read/write only.** If this same GDAL adoption is used to
*retire the bundled PROJ4* (the natural bundling — see "Relationship" below), add
**~1-2 days** for the three CRS-seam caveats above (removing the radians conversions,
replacing CRS-string sniffing with PROJ/GDAL queries in `grid.F90` **and** `swbstats2`,
and reworking `grid_CheckForPROJ4Error`). These are cheap individually but must all be
done together — a partial job is where the "interesting problems" hide. Combined realistic
estimate for GeoTIFF **+** PROJ retirement: **~5-6 days**, most of the risk in Windows
packaging and cross-platform transform testing, not in code volume.

---

## Linking / Distribution

Same story as PROJ (see `build_consideration__static_vs_dynamic__gfortran_ifx.md`):
- **Development/CI:** Dynamic linking via pixi (`gdal.dll` in conda environment)
- **Release (gfortran):** Ship `gdal.dll` alongside `swb2.exe` (~30MB)
- **Release (Intel ifx):** Static link against MSVC-built GDAL `.lib`

Note: If GDAL is adopted, the separate `proj.dll` is no longer needed — GDAL includes PROJ internally. The `proj.db` file is still required at runtime.

---

## Relationship to Other Planned Changes

| Dependency | Status if GDAL adopted |
|-----------|----------------------|
| Bundled PROJ4 (`src/proj4/`) | Can be removed — GDAL provides PROJ |
| External PROJ library | Not needed separately — GDAL links it |
| netCDF (for input) | Could optionally route through GDAL |
| netCDF (for output) | Keep direct netCDF C API (more control over CF metadata) |
| libtiff / libgeotiff | Not needed separately — GDAL includes them |

This suggests a natural ordering: adopt GDAL (which gives you PROJ + GeoTIFF), then remove the bundled PROJ4 code. Two birds, one dependency.
