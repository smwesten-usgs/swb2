# Dependency Restraint: GDAL, PROJ, GeoTIFF, and the Temptation to Gild

**Date:** September 11, 2026
**Status:** Design counterweight / decision guardrail — deliberately opinionated
**Companion to:** `feature_consideration__adopt_modern_PROJ_library.md`,
`feature_consideration__reading_writing_geotiffs_gdal.md`,
`plans_for_code_and_repo_improvement.md` (CI is Phase 3+),
and the VA project note `virginia_swb/design/swb2_netcdf_cf_compliance.md`.

> **Purpose (read this when tempted to add bells and whistles):** the two feature docs
> above recommend "adopt modern PROJ" and "adopt GDAL." They are sound *if* the features
> are needed. This note is the deliberate counterweight — the argument for **restraint** —
> so the decision file holds both sides. Pull it out when the urge to add superfluous
> capability strikes. There are better places to spend the time (starting with **CI for
> SWB2**).

---

## 1. The Core Argument: You Already Feel a Lighter Version of This Pain

netCDF + HDF5 are already the sharp edge of the SWB2 Windows/gfortran build. **GDAL is
that same pain, larger and deeper.** Everything annoying about the current stack, GDAL
amplifies — and it does so to gain capability this project does not strictly require.

The decision should therefore start from *need*, not from "it would be nice to read
GeoTIFFs."

---

## 2. "Is GDAL That Large?" — Yes, and the Depth Is Worse Than the Size

- Raw size is ~11–14 MB for `libgdal-core` (conda-forge) — but that undersells it. The
  honest metric is the **transitive dependency tree**: GDAL links (directly or via
  drivers) PROJ, GEOS, libtiff, libgeotiff, sqlite3, libpng, libjpeg, zlib, curl — **and
  its own copy of netCDF and HDF5.**
- **The trap:** adopting GDAL puts a *second* netCDF/HDF5 in the process alongside
  SWB2's direct netCDF-C/HDF5. Two builds of the same libraries → ABI mismatches,
  duplicate symbols, "which HDF5 loaded" heisenbugs — the exact failure class already
  causing pain, now doubled. GDAL does **not** replace the netCDF headache; it adds a
  parallel one.
- Modern GDAL splits into `libgdal-core` + per-driver subpackages, which *can* prune the
  tree — but only with ongoing discipline about not pulling the netCDF/HDF4/etc. driver
  packages. That discipline is itself recurring maintenance.
- Runtime data: GDAL wants `GDAL_DATA` **and** (via PROJ) `proj.db`/`PROJ_DATA` findable
  at runtime — two data-file trees to ship and diagnose, not just a DLL.
- Long-project risk: GDAL releases often and deprecates aggressively. Over a 2-year
  pinned-environment project, a fast-moving heavy dependency is a liability. By contrast
  the bundled PROJ4 is *dead but stable* — its lack of maintenance means it never
  surprises the build. "Dead weight" cuts both ways.

---

## 3. The Benefits This Project Needs Do NOT Require GDAL

| Want | Needs GDAL? | Lighter route |
|------|:-----------:|---------------|
| CF-correct NetCDF output (de-risks the mosaic workflow) | **No** | Fix writer *attributes* (pure netCDF-C, already a dependency). The GDAL doc itself says keep CF output on direct netCDF calls. |
| Authoritative CRS / `crs_wkt` / EPSG / WKT | **No** | **PROJ**, not GDAL — one library + `proj.db`, an order of magnitude lighter. Or generate WKT offline (Section 5). |
| GeoTIFF read/write | Only path that truly benefits | **libtiff + libgeotiff** (Section 4), or `gdal_translate` as an *external* preprocessing step. |

The GeoTIFF convenience is the *only* thing that genuinely wants GDAL — and SWB2 already
reads Arc ASCII + NetCDF, and this project's inputs (gNATSGO, CDL, SSEBop, daily weather)
can be delivered/converted in those formats outside the model. Adopting GDAL's entire
tree to avoid a preprocessing step is a lopsided trade.

---

## 4. Lighter GeoTIFF Options (if GeoTIFF is ever actually wanted)

Ranked lightest-sane to heaviest:

1. **libtiff + libgeotiff via a thin C wrapper** — the real "GeoTIFF without GDAL"
   answer. Two small, decades-stable C libs; no second netCDF/HDF5. SWB2's need
   (single-band float rasters on a regular grid) is the *easy* path through libtiff
   (`TIFFReadEncodedStrip`/scanline), ~150-line wrapper, same `iso_c_binding` pattern as
   the current PROJ4 wrapper. The prior doc's "~1 week" is overstated for this narrow
   need. Caveat: libgeotiff's CRS geokeys re-raise the PROJ/WKT question (it can defer to
   PROJ), so it isn't entirely CRS-free.
2. **Full GDAL** — only if native multi-format raster I/O (COG, GRIB, etc.) becomes a
   hard, recurring requirement.

### 4a. DO NOT write a TIFF library from scratch (or adopt a native-Fortran toy)

Recorded explicitly because the thought was raised (and rightly made the fingers quiver):

- **Uncompressed single-band float GeoTIFF** is ~a day. But real inputs (CDL, gNATSGO)
  arrive **LZW/DEFLATE-compressed, tiled, possibly BigTIFF, with predictors** — and now
  you're reimplementing libtiff, badly, forever, with no community to find your edge-case
  bugs. Classic false economy: trading a well-tested external dependency for an
  under-tested internal one *you* maintain.
- No mature, maintained, pure-Fortran GeoTIFF library exists that handles compression +
  tiling + BigTIFF + geokeys. The options are "adopt a toy" or "become the maintainer."
- Herculean effort, negligible tangible gain. If scope were constrained to
  "uncompressed strip single-band only," real inputs still wouldn't honor it → back to
  `gdal_translate` anyway → you didn't need in-model GeoTIFF.

**Verdict:** if GeoTIFF is wanted, libtiff+libgeotiff. From-scratch/native-Fortran is a
maintenance liability dressed as minimalism.

---

## 5. Resolving the Real Bind: "PROJ keeps growing" vs "PROJ4 strings court disaster"

Both claims are true and point in *opposite* directions. They are reconciled by
recognizing that **WKT is a format, not a library.**

- **PROJ4 strings really do court disaster** — not fashion, substance. `+datum=NAD83`
  cannot fully specify a datum realization (NAD83(2011) vs NAD83(CSRS)…) and silently
  implies zero `towgs84`. For a project that co-registers SWB recharge with a MODFLOW
  grid and observation coordinates, that sub-meter datum ambiguity is exactly the silent
  error that surfaces years later as an unexplained offset. **WKT2 / EPSG codes remove
  it.** Migrating *away from PROJ4-string-as-the-CRS-carrier* is well-justified on its
  own merits (this is the CF doc's `crs_wkt` recommendation).
- **But wanting WKT does not force adopting ever-growing modern PROJ at runtime.** You can
  **generate `crs_wkt` offline with `pyproj`** for the handful of CRSs SWB2 actually
  supports (realistically EPSG:5070 + a few UTM / state-plane zones) and write it into the
  CF output. The control file could accept an EPSG code that SWB2 maps to a stored WKT
  string plus the PROJ4 params it already parses. **Zero new linked C dependency; the
  datum-ambiguity risk is gone.**
- This works because SWB2's supported-CRS set is small and known — it is a soil-water
  balance model on projected grids, not a general reprojection tool. Only if it had to
  accept *arbitrary* user CRS and datum-transform between them would real PROJ be
  required. That is not SWB2's job.

---

## 6. Recommendation (ranked by dependency weight, for THIS project's needs)

1. **Lightest / recommended:** Keep bundled PROJ4 for transforms; **generate `crs_wkt`
   offline via pyproj** for supported CRSs and write it in CF output; retire
   PROJ4-string-as-primary-carrier (per the CF doc). Zero new linked dependencies; kills
   the datum-ambiguity risk; ships now. Add **libtiff+libgeotiff** later *only if*
   GeoTIFF is genuinely needed.
2. **Middle:** Adopt **PROJ-only** (not GDAL) if the model itself must parse EPSG/WKT at
   runtime and do datum-aware transforms. One library + `proj.db`.
3. **Heaviest / avoid unless GeoTIFF-heavy:** full GDAL — only if native multi-format
   raster I/O becomes a hard, recurring requirement.
4. **Don't:** from-scratch / native-Fortran TIFF. False economy.

The non-obvious, high-value move is **#1**: it dissolves the WKT-vs-PROJ-weight dilemma —
WKT correctness is a *data* problem solvable with an offline pyproj step, not a
*dependency* problem requiring an ever-growing linked library.

**And do not adopt GDAL "because it bundles PROJ, two birds one stone."** That framing
hides the other eight birds you then have to feed — including a redundant copy of the
netCDF/HDF5 already causing pain.

---

## 7. Where the Time Actually Belongs

Every hour spent gilding I/O with GDAL or a hand-rolled TIFF parser is an hour not spent
on the improvements with real leverage, per `plans_for_code_and_repo_improvement.md`:

- **CI for SWB2** (GitHub Actions: lint → build → smoke → full; multi-compiler,
  multi-OS) — catches regressions across the whole codebase, permanently.
- **test-drive migration** off legacy FRUIT; pytest integration tests.
- **F2018 standard enforcement**, fprettify, codespell.
- **The CF-output fixes** (the VA project's `swb2_netcdf_cf_compliance.md`) — which
  deliver the mosaic/GW-handoff win with *no* new dependency.

CRS/format bells and whistles are discretionary; CI and correct CF output are
foundational. Spend there first.

---

## 8. Provenance

Dependency-tree and size characterization from conda-forge `libgdal` / `libgdal-core`
package metadata (checked 2026-09-11) and the two prior SWB2 feature docs. PROJ transform
call surface verified by source trace (see `swb2_netcdf_cf_compliance.md` Section 4a.3).
This note records the counterargument to those docs' "adopt" recommendations; it does not
override them — it ensures the decision is made with the restraint case in view.
