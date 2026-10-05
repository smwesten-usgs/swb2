# SWB2 NetCDF Output: CF-Compliance Audit, Projection Modernization, and Test Plan

**Date:** September 11, 2026
**Status:** Design / punch-list — arose from the "separate models + stitch the NetCDFs"
architecture rabbit hole (see `project_scoping_and_design.md` Section 7 and
`breadcrumbs_and_next_steps.md` Section 5)
**Scope:** The NetCDF *output* writer in the SWB2 source tree
(`E:\projects\swb_development\git\swb2\src`), primarily
`netcdf4_support.F90` (subroutines `nf_set_standard_attributes`,
`nf_set_global_attributes`) and its `swbstats2` twin (`swbstats2_support.F90`).
**Not** the Virginia project code — this is an upstream-SWB2 improvement list that the
VA project depends on for clean grid mosaicking.

---

## 1. Why This Matters to the Virginia Project

The VA design weighs running **separate SWB2 models per basin** and stitching their
NetCDF recharge outputs into one seamless grid (Option B), versus **one masked master
grid** (Option A / Section 2.2 active-cell masking). Either way, the deliverable to the
groundwater-modeling team is a mosaicked, georeferenced NetCDF.

Clean, standards-correct CF metadata is the thing that makes that mosaicking work with
off-the-shelf tools (`cdo collgrid`, `xarray.combine_by_coords`, GDAL) instead of a
bespoke compositor. Two of the defects below (a dangling `coordinates = "lat lon"`
reference and missing `axis` attributes) are exactly the kind of thing that makes CDO
refuse a file or makes a reader misidentify the X/Y dimensions. So tightening CF
compliance is not cosmetic — it directly de-risks the architecture we choose.

**Separate finding (out of scope for fixing, but recorded):** the merge itself is
I/O-bound, not CPU-bound. A compiled (Fortran/C/Rust) compositor buys little over a
streaming `xarray`+`dask` merge or `cdo collgrid`, because the cost is NetCDF
decompress/recompress + disk throughput, shared by all options. The decisive upstream
choice is to **carve every sub-model grid from one master grid spec** (shared EPSG:5070
origin, 250 m cell size, integer row/col offsets) so tiles coincide exactly and merging
is placement, not resampling. That belongs in the architecture doc; it is noted here
only because it motivated this audit.

---

## 2. What SWB2 Already Does Correctly (verified in source)

SWB2 is genuinely close to CF — closer than a first glance suggests. Confirmed present
and correct:

- Global `Conventions = "CF-1.6"`, plus `source`, `history`, `executable_version`.
- `time`: `units = "days since <origin> 00:00:00"`, `calendar = "standard"`,
  `long_name = "time"`, and optional `bounds = "time_bnds"`.
- `x` / `y`: `units` (from the PROJ4 string), `long_name = "x/y coordinate of
  projection"`, `standard_name = "projection_x_coordinate"` /
  `"projection_y_coordinate"`. These are the exact CF names.
- Data variable (`NC_Z`): `units`, `_FillValue`, `valid_min` / `valid_max` /
  `valid_range`, and `grid_mapping = "crs"`.
- A `crs` grid-mapping variable built from the PROJ4 string, carrying
  `grid_mapping_name`, `datum`, `spheroid`, `standard_parallel`, and projection
  parameters.
- NetCDF-4 with deflate compression.

The bones are CF. The items below are what keep it at "CF-ish."

---

## 3. CF-Compliance Defect List (concrete, source-cited)

Line numbers refer to `netcdf4_support.F90` as read on 2026-09-11; treat as approximate
anchors, not exact addresses.

### 3.1 BUG — dangling / misused `coordinates` attribute on the data variable (highest priority)

- In the branch **with** `valid_min/valid_max` (~line 3860+), the data variable is
  given both `coordinates = "crs"` **and** `grid_mapping = "crs"`. `"crs"` is a
  grid-mapping variable, **not** an auxiliary coordinate variable, so it must not appear
  in `coordinates`. It is redundant with `grid_mapping` and technically wrong.
- In the branch **without** `valid_min/valid_max` (~line 3915+), the data variable is
  given `coordinates = "lat lon"` **unconditionally** — even when `lLatLon` is false and
  no `lat`/`lon` variables are written. This is a **dangling reference to non-existent
  variables**, a hard CF violation that strict checkers flag and some readers (incl.
  CDO) may reject.

**Fix:** Only emit `coordinates = "lat lon"` when `lat`/`lon` variables are actually
written (`lLatLon == .true.`). Otherwise omit `coordinates` entirely. Associate the CRS
via the **extended `grid_mapping` syntax** instead (see 4.2): `grid_mapping = "crs: x y"`.

### 3.2 `time` variable missing `standard_name = "time"` and `axis = "T"`

Sets `long_name`, `units`, `calendar` but no `standard_name` or `axis`. CF wants
`standard_name = "time"`; `axis = "T"` is recommended.

### 3.3 `x` / `y` variables missing `axis = "X"` / `axis = "Y"`

They carry correct `units`/`long_name`/`standard_name` but no `axis`. CF strongly
recommends `axis` on projection coordinate variables so generic tools identify them
unambiguously. **This is one of the two items that most affects mosaicking tools.**

### 3.4 Data variable has no `long_name` (and no documented `standard_name`)

`NC_Z` gets `units` but no `long_name`. `net_infiltration` has no CF standard name
(fine — many hydrologic fluxes don't), but CF then wants at least a `long_name`. Add a
descriptive `long_name` per output variable; optionally document a local
(non-standard) `standard_name` convention.

### 3.5 `Conventions = "CF-1.6"` is stale

Bump to the target version (Section 5) once the above are fixed — and only claim the
version actually validated against.

### 3.6 `crs` uses non-standard `proj4_string`; lacks `crs_wkt` and the named-CRS attributes

CF has **no** `proj4_string` attribute at any version — this is the core "CF-ish" piece.
The modern, portable encoding (CF ≥ 1.7) is `crs_wkt` (OGC WKT2) plus the named-CRS
attributes. See Section 4. **Note:** `proj4_string` cannot simply be deleted — it is
read and used by `swbstats2` (see **Section 4a**). The fix is to *add* `crs_wkt` + CF
named attributes and make them the primary CRS carrier, while **retaining**
`proj4_string` (documented as a non-CF extra) until `swbstats2` is changed to stop
scraping it.

### 3.7 `swbstats2` output path likely inherits the same gaps + needs `cell_methods`

`swbstats2_support.F90` writes `valid_min/max` the same way (~line 1749+), so it likely
shares the `coordinates`/`axis`/`standard_name` defects. Additionally, any
**time-aggregated** variable it writes (monthly/annual **sums** or **means**) should
carry `cell_methods` — e.g. `cell_methods = "time: sum"` for accumulated recharge,
`"time: mean"` for an averaged flux — so the temporal aggregation is CF-correct.

---

## 4. Projection Encoding: Modernize to CF ≥ 1.7 (`crs_wkt` + named CRS)

### 4.1 The change

Replace reliance on the non-standard `proj4_string` with the standards-based encoding
introduced in **CF-1.7 (Aug 2017)** and refined through **1.9**:

1. **Keep** the single-property grid-mapping attributes SWB2 already emits
   (`grid_mapping_name`, `standard_parallel`, `longitude_of_central_meridian`,
   `latitude_of_projection_origin`, `false_easting`, `false_northing`,
   `semi_major_axis`, `inverse_flattening`, …). CF §5.6.1 is explicit that `crs_wkt`
   *supplements* rather than replaces these; describe the CRS "as thoroughly as
   possible" with both.
2. **Add `crs_wkt`** — a single OGC WKT2 string that fully specifies the CRS. This is
   the format GDAL / rasterio / QGIS / xarray read directly, so the files become
   unambiguously georeferenced without anyone parsing PROJ4.
3. **Add the named-CRS attributes** (CF-1.7): `projected_crs_name`,
   `geographic_crs_name`, `horizontal_datum_name`, `reference_ellipsoid_name`,
   `prime_meridian_name`. Per CF, these must be defined as a set (if any is defined the
   related ones should be too), and they materially improve WKT interoperability.
4. **Retain `proj4_string`, but demote it from primary CRS carrier** to a documented
   non-CF extra. It **cannot be removed yet** because `swbstats2` reads and string-scrapes
   it (Section 4a). Removal only becomes safe after the `swbstats2` unit fix (Section
   4a.4, step 2) and, ideally, a GDAL/PROJ migration (Section 4a.3).

### 4.2 Use the extended `grid_mapping` binding

CF-1.7 added the extended syntax that explicitly binds coordinates to the CRS:

```
<data_var>:grid_mapping = "crs: x y" ;
```

This is the clean replacement for the misused `coordinates = "crs"` (3.1): it associates
`x` and `y` with the `crs` grid mapping directly, and lets `coordinates` be reserved for
genuine auxiliary coordinates (`lat lon`) only when those are written.

### 4.3 Target attribute set for the VA grid (EPSG:5070, CONUS Albers)

```
int crs ;
    crs:grid_mapping_name = "albers_conical_equal_area" ;
    crs:standard_parallel = 29.5, 45.5 ;
    crs:longitude_of_central_meridian = -96.0 ;
    crs:latitude_of_projection_origin = 23.0 ;
    crs:false_easting = 0.0 ;
    crs:false_northing = 0.0 ;
    crs:semi_major_axis = 6378137.0 ;
    crs:inverse_flattening = 298.257222101 ;      // GRS 1980
    crs:longitude_of_prime_meridian = 0.0 ;
    crs:projected_crs_name = "NAD83 / Conus Albers" ;
    crs:geographic_crs_name = "NAD83" ;
    crs:horizontal_datum_name = "North_American_Datum_1983" ;
    crs:reference_ellipsoid_name = "GRS 1980" ;
    crs:prime_meridian_name = "Greenwich" ;
    crs:crs_wkt = "<OGC WKT2 string for EPSG:5070>" ;

double x(x) ;
    x:standard_name = "projection_x_coordinate" ;
    x:long_name = "x coordinate of projection" ;
    x:units = "m" ;
    x:axis = "X" ;

double y(y) ;
    y:standard_name = "projection_y_coordinate" ;
    y:long_name = "y coordinate of projection" ;
    y:units = "m" ;
    y:axis = "Y" ;

double time(time) ;
    time:standard_name = "time" ;
    time:long_name = "time" ;
    time:units = "days since 1981-01-01 00:00:00" ;
    time:calendar = "standard" ;    // see 4.5 on proleptic_gregorian
    time:axis = "T" ;

float net_infiltration(time, y, x) ;
    net_infiltration:long_name = "net infiltration at base of root zone" ;
    net_infiltration:units = "<e.g. mm d-1>" ;
    net_infiltration:_FillValue = <fill> ;
    net_infiltration:grid_mapping = "crs: x y" ;
```

The WKT2 string and the single-property values should be generated from the authoritative
EPSG definition, not hand-typed. `pyproj.CRS.from_epsg(5070).to_cf()` returns the full CF
grid-mapping attribute dict (including the `*_name` attributes), and `.to_wkt()` returns
the WKT2 — a useful cross-check even if the Fortran writer generates them itself.
(Verify the exact standard-parallel / origin values against the EPSG registry before
use; the values above are the standard EPSG:5070 definition and should be confirmed.)

### 4.4 Which CF version to declare

- Latest **released** version is **CF-1.13 (17 Dec 2025)**. **1.14 is still draft** — do
  not target a draft.
- Recommended declared value: **`Conventions = "CF-1.11"`** (or 1.8 at minimum). 1.11
  has broad, mature tool support today; everything we need (`crs_wkt`, named-CRS attrs,
  extended `grid_mapping`, `axis`) exists from 1.7–1.9. Targeting 1.11 gets the modern
  projection encoding without depending on very new features.
- Note 1.13's changes if adopting it: calendar/leap-second overhaul (see 4.5) and
  clarified anomaly-data conventions. Neither is required for SWB2 output; declare 1.13
  only if we validate against it.
- **Only claim the version we actually validate against** (Section 6 makes that
  automatic).

### 4.5 Calendar note (CF-1.13)

CF-1.13 clarified the meaning of the `standard` calendar and **withdrew** 1.12's
`units_metadata = "leap_seconds: ..."`. For model output that ignores leap seconds (SWB2
daily balance), CF-1.13 nudges toward `calendar = "proleptic_gregorian"` for an
unambiguous model timeline. `standard` remains acceptable. Low priority; flagged for
awareness.

---

## 4a. IMPORTANT — `proj4_string` is load-bearing; do NOT simply retire it

An earlier draft of this note said to "retire `proj4_string`." **That is wrong as
stated** — verification of the source shows `proj4_string` is *consumed*, not just
written, and removing it would break `swbstats2`.

### 4a.1 Where it is read and used (source-verified 2026-09-11)

- **`swbstats2.F90` (~line 353–356):** reads the `crs` variable's attributes and does
  `name_list%which("proj4_string")` → `swbstats%target_proj4_string`. It reads the
  attribute **by that exact name**.
- **`swbstats2.F90` (~line 433–445, `report_as_volume` path):** string-matches the
  stored PROJ4 text — `target_proj4_string .containssimilar. "units=m"` /
  `"units=us-ft"` / `"units=ft"` — to set the length-conversion factor for volume
  output (for MODFLOW hand-off), and **`call die(...)`** if it finds no units token in
  the PROJ4 string.

So `swbstats2 --report_as_volume` (a MODFLOW-relevant path for this very project)
**depends on `proj4_string` being present and containing a `+units=` token.** Deleting
the attribute, or dropping the `+units=` token from it, is a breaking change.

### 4a.2 How PROJ4 threads through the whole code (context for the GDAL decision)

`proj4_string` is one visible symptom of a deeper coupling. The model carries a PROJ4
string end to end:

- `model_domain.F90`, `model_initialize.F90`: `MODEL%PROJ4_string` is set from the
  control file and is the authoritative CRS for the run.
- `grid.F90`: a C interface to the **bundled PROJ4 library** (`src/proj4/`) does the
  actual coordinate transforms (`grid_Transform`, e.g. projecting to lon/lat with
  `+proj=lonlat +ellps=GRS80 +datum=WGS84`).
- `data_catalog*.F90`: source/target PROJ4 strings drive input-grid reprojection.
- `proj4_support.F90`: `create_attributes_from_proj4_string` **parses the PROJ4 string
  by hand** (a `select case` over `+proj`, `+datum`, `+ellps`, `+lat_1/2`, `+units`, …)
  to synthesize the CF `crs` attributes — with a **limited, hard-coded** lookup
  (only aea/aeqd/tmerc/merc/cea/lcc/utm; only GRS80/WGS84/clrk66/sphere ellipsoids;
  else emits `"unknown"`).

Two consequences matter here:
1. The CF `crs` attributes SWB2 writes today are only as good as this hand parser. Any
   projection/ellipsoid outside the hard-coded lists silently becomes `"unknown"`.
2. `swbstats2` round-trips through the **raw PROJ4 string**, not through the parsed CF
   attributes — so the PROJ4 string is currently the *real* CRS carrier, and the CF
   attributes are secondary. This is the reverse of where CF wants us (CF attributes +
   `crs_wkt` primary).

### 4a.3 The GDAL-vs-bundled-PROJ4 decision — with a VERIFIED call-surface trace

There are **two prior SWB2 design analyses** (both May 2026) that already scoped this,
and they conclude it is **not onerous**:
`swb2/design/feature_consideration__adopt_modern_PROJ_library.md` (~2–3 day estimate;
"do it, after CI") and `swb2/design/feature_consideration__reading_writing_geotiffs_gdal.md`
(thin GDAL C wrapper; GDAL bundles PROJ, so one dependency yields PROJ + GeoTIFF and lets
`src/proj4/` be deleted). An earlier draft of *this* note called GDAL "a heavy new
dependency / genuine architecture change" — **that was too pessimistic and is corrected
here.**

**Verified transform call surface (traced 2026-09-11, not estimated):** every entry into
the bundled PROJ4 library goes through **one** C function, `pj_init_and_transform`
(declared once in `grid.F90`), reached from exactly **two** Fortran sites:

- `grid.F90` `grid_Transform` (the wrapper) — called by `data_catalog_entry.F90:942,948`
  and `model_initialize.F90:1829`.
- `data_catalog_entry.F90:2107` — one *direct* call (transforms 4 corner points for the
  cookie-cut extent) that bypasses `grid_Transform`.

Plus the pure-string CF-attribute parser in `proj4_support.F90` (no library calls). So
the coupling is genuinely small — **the "limited number of places" instinct is correct.**

**But three items make it a *careful* change, not a mechanical drop-in** (these are the
things the optimistic May-2026 estimate underplays, surfaced by reading the actual code):

1. **Radians handling is business logic in the call sites, not just in the wrapper.**
   `grid_Transform` (and the direct site at :2107) manually multiply geographic coords by
   `DEGREES_TO_RADIANS` before the PROJ4 call and `RADIANS_TO_DEGREES` after, because
   legacy PROJ4 works in radians. **Modern PROJ (via `proj_normalize_for_visualization`)
   works in degrees.** These conversion blocks must be **removed in both sites** or
   coordinates come out silently wrong by 57× — a correctness trap, not a crash.
2. **CRS-type detection is fragile string-sniffing, duplicated.** Both the radians logic
   (`csFromPROJ4 .containssimilar. "latlon"`) and `swbstats2`'s unit-scrape
   (`.containssimilar. "units=m"`) detect CRS properties by substring-matching the PROJ4
   text. Those tokens **do not exist in WKT/EPSG input** — the very formats the upgrade
   adds — so a proper migration must replace this with real PROJ queries
   (`proj_get_type`, map-unit query), or WKT/EPSG inputs break the radians and unit paths.
3. **Error semantics change.** `grid_CheckForPROJ4Error` decodes the old `pj_transform`
   integer-code contract; `proj_trans` reports per-coordinate `HUGE_VAL`/errno. This
   routine needs rework, not pass-through.

**Corrected trade-off summary:**

- **Adopt GDAL / modern PROJ ≥ 6:**
  - *Pro:* authoritative CRS handling — parse EPSG/WKT2 directly, emit correct `crs_wkt`
    + all CF named attributes for **any** CRS (kills the `"unknown"` fallback);
    datum-aware, thread-safe transforms; removes 168 unmaintained bundled C files.
  - *Cost (revised):* on Windows it is essentially **one DLL + one data file** — ship
    `proj.dll` (sqlite3 is compiled into it, so no separate `sqlite3.dll`) **plus
    `proj.db` (~8 MB)** found via `PROJ_DATA`/search path. With pixi/conda this is
    invisible. **This is the "just one *.dll" recollection — accurate, with the `proj.db`
    asterisk.** GDAL instead of PROJ-only adds GeoTIFF but a bigger (~30 MB) DLL; PROJ-only
    is the lighter option if GeoTIFF isn't wanted.
  - *Effort (revised):* **~3–5 days**, not weeks — tiny call surface (above), but budget
    for the radians removal, the CRS-type-detection cleanup, and the error-handling rework
    (items 1–3), plus Windows packaging (the real time sink) and cross-platform testing.
    Prior doc said ~2–3 days; the extra 1–2 days is the correctness cleanup it omitted.

- **Keep bundled PROJ4, just fix the CF output (Section 4):**
  - *Pro:* localized, low risk; ships now.
  - *Con:* the hand parser stays limited; `crs_wkt` must be generated another way (a small
    lookup for the CRSs SWB2 actually supports, or an offline `pyproj` step) — workable
    but not authoritative.

**Important:** GDAL/PROJ adoption does **not** touch the NetCDF *output* writer — the GDAL
design doc explicitly recommends keeping CF NetCDF output on the direct netCDF C API "for
more control over CF metadata." So the CF writer fixes (Sections 3–4) and the GDAL/PROJ
migration are genuinely **independent** work streams; neither blocks the other.

### 4a.4 Recommendation on sequencing

**Decouple the two upgrades, but design the CF change so it does not fight a future
GDAL/PROJ migration.** Concretely:

1. **Now (CF fixes, Section 3–4), keep `proj4_string`** as a retained (documented)
   attribute *in addition to* `crs_wkt` and the CF named attributes. Change the earlier
   guidance from "retire" to **"keep, but demote from primary CRS carrier."** This keeps
   `swbstats2` working unchanged.
2. **Fix the actual `swbstats2` fragility regardless of GDAL:** the volume-unit factor
   should be derived from the CF `x`/`y` `units` attribute (or the `crs` map-units),
   **not** from string-scraping the PROJ4 text. This removes the hard `die()` dependency
   on a `+units=` token and is a small, safe change that survives either path.
3. **Treat GDAL/PROJ adoption as a separate, later decision** (its own design note),
   sequenced with the broader SWB2 modernization plan (F2018, test-drive, CI). If/when
   adopted, GDAL/PROJ becomes the authoritative source for `crs_wkt` + CF attributes and
   the hand parser in `proj4_support.F90` can be retired — at which point `proj4_string`
   can *finally* be dropped, once `swbstats2` no longer scrapes it (step 2 having already
   removed that need).

**Net:** the CF upgrade and the GDAL upgrade are complementary but should not be
bundled. Do the CF fixes now in a way that is GDAL-ready (CF attrs + `crs_wkt` primary,
`proj4_string` retained), fix the `swbstats2` unit-scraping so nothing hard-depends on
the PROJ4 text, and leave the PROJ4→GDAL/PROJ swap as a scoped follow-on. The order that
avoids rework: **(a) `swbstats2` unit fix → (b) CF attribute/`crs_wkt` fixes →
(c) optional GDAL/PROJ migration → (d) only then drop `proj4_string`.**

---

## 4b. Task cascade: what the CF and GDAL upgrades imply for `swbstats2` (and beyond)

These upgrades are **not** confined to the `swb2` writer. `swbstats2` both *reads* SWB2
NetCDF output and *writes* its own aggregated NetCDF output (mean/variance/annual/monthly
grids, verified in `swbstats2_support.F90` `close_output_netcdf_files` ~line 1740+, which
reuses the same `netcdf4_support` machinery). So each decision fans out into concrete
`swbstats2` tasks. Enumerated so the scope is honest:

### 4b.1 Tasks triggered by the CF upgrade (independent of GDAL)

| Area | Task | Notes |
|------|------|-------|
| swbstats2 **output** | Inherits every writer fix in Sections 3–4 automatically (it calls the same `nf_set_standard_attributes`) — but must be **re-validated**, not assumed | Same `axis`/`standard_name`/`coordinates`/`crs_wkt` fixes apply to its output files |
| swbstats2 **output** | Add `cell_methods` to aggregated variables: `"time: mean"`, `"time: sum"`, `"time: variance"`, `"time: minimum/maximum"` matching each statistic it computes | CF-correctness of temporal aggregation (3.7); this is *new* attribute logic specific to swbstats2, not shared with swb2 |
| swbstats2 **input** | The `proj4_string` **read** (`swbstats2.F90` ~line 353) keeps working as long as swb2 retains the attribute — no change needed short-term | Coupled to the "retain proj4_string" decision (4a) |
| swbstats2 **input** | **Fix the volume-unit scraping** (~line 433): derive the length-conversion factor from the CF `x`/`y` `units` (or `crs` map units), not from `.containssimilar. "units=m"` on the PROJ4 text; remove the hard `die()` on a missing `+units=` token | Safe, small, and prerequisite to *ever* dropping `proj4_string` |
| both | Extend the test harness (Section 6) to run `cfchecker` on **swbstats2 output** too, and to assert `cell_methods` per statistic | swbstats2 output is a deliverable to the GW team — it must be CF-valid, not just swb2's raw output |

### 4b.2 Additional tasks triggered *only if* GDAL/PROJ is adopted

| Area | Task | Notes |
|------|------|-------|
| swb2 core | Replace bundled `src/proj4/` C calls in `grid.F90` (`grid_Transform`) and `data_catalog*.F90` reprojection with GDAL/PROJ | The real work; test surface across coordinate transforms |
| swb2 core | Replace the hand parser in `proj4_support.F90` — let PROJ/GDAL emit `crs_wkt` + CF named attrs authoritatively for **any** CRS (kills the `"unknown"` fallback) | This is the payoff: correct CF CRS for arbitrary projections |
| swbstats2 | Once transforms go through GDAL/PROJ **and** the unit fix (4b.1) is in, retire the `proj4_string` **read** in favor of `crs_wkt`/CF attrs; then swb2 can stop writing `proj4_string` at all | Final removal of the load-bearing dependency |
| build | GDAL/PROJ in the Meson build on Windows/Linux/macOS; update `developer_quickstart.md`; verify static-link/redistributable size | New heavy dependency; coordinate with the F2018/CI modernization plan |
| tests | CRS round-trip tests now cover arbitrary EPSG inputs, not just the hard-coded subset | Broader coverage becomes possible |

### 4b.3 Bottom line on scope

- **CF-only path (no GDAL):** touches `netcdf4_support.F90` (writer), `swbstats2_support.F90`
  (`cell_methods` + inherited fixes), `swbstats2.F90` (unit-scrape fix), and the test
  harness. Moderate, low-risk, ships independently. `proj4_string` stays.
- **CF + GDAL path:** all of the above **plus** `grid.F90`, `data_catalog*.F90`,
  `model_initialize.F90`, `proj4_support.F90`, and the build system — a substantially
  larger effort that only then lets `proj4_string` be removed.

The two are **complementary but separable**; bundling them multiplies the test surface.
Recommended order remains: **swbstats2 unit fix → CF attribute/`crs_wkt` fixes (swb2 +
swbstats2) → optional GDAL/PROJ migration → drop `proj4_string`.** Each step is
independently shippable and testable.

---

## 5. Summary Fix List (priority order)

| # | Fix | Effort | Why |
|---|-----|--------|-----|
| 1 | Correct/remove the dangling & misused `coordinates` attribute (3.1); adopt `grid_mapping = "crs: x y"` (4.2) | Low | Hard CF violation; breaks mosaicking tools |
| 2 | Add `axis = "X"/"Y"/"T"` to `x`/`y`/`time` (3.2, 3.3) | Low | Tool identification of dims; mosaicking |
| 3 | Add `crs_wkt` + named-CRS attributes as primary CRS carrier; **retain** `proj4_string` for now (3.6, 4, 4a) | Low–Med | Unambiguous georeferencing; the core "CF-ish" fix. Do NOT delete `proj4_string` — swbstats2 uses it |
| 4 | Add `standard_name = "time"` (3.2) and per-variable `long_name` (3.4) | Low | Basic CF descriptiveness |
| 5 | Bump `Conventions` to target version (3.5, 4.4) | Trivial | Truth-in-labeling (do last, after validation) |
| 6 | Apply the same fixes + `cell_methods` in the `swbstats2` path; fix its volume-unit scraping (3.7, 4b) | Med | swbstats2 *reads* swb2 output and *writes* its own — full cascade in Section 4b |
| 7 | (Conditional) GDAL/modern-PROJ migration — **small call surface** (1 C wrapper, 2 call sites) but a *careful* change (radians removal, CRS-type detection, error handling); enables dropping `proj4_string` (4a.3, 4b.2) | ~3–5 days | Separate, later decision; do NOT bundle with the CF fixes. Windows = one DLL + `proj.db`. See swb2 prior docs |

All are pure attribute-definition changes — no algorithmic impact, low risk, and mostly
localized to `nf_set_standard_attributes` and its `swbstats2` twin. Recommend doing them
as small, separately reviewable commits with the test harness (Section 6) proving
before/after.

---

## 6. Test Plan: catch "goofy stuff" during development

Goal: an automated, repeatable check that any SWB2 (or swbstats2) NetCDF is CF-valid and
mosaics cleanly — run on tiny fixtures during development so regressions and edge cases
are caught immediately. All tooling is installable into the existing mamba env `py313`
(conda-forge); **do not install into base**.

### 6.1 Tools (all real, all conda-forge / pip installable)

- **`cfchecker`** — yes, it's a thing. The official CF-checker (Python package
  `cfchecker`, maintained by NCAS-CMS / CEDA) validates a file against a **specified CF
  version** and reports ERROR/WARN per variable. Install:
  `mamba install -p <py313> -c conda-forge cfchecker`. Invoke:
  `cfchecks -v 1.11 <file.nc>` (pin the version so we validate against our target).
- **`compliance-checker`** (IOOS) — optional second opinion; wraps cfchecker with extra
  suites. `mamba install -p <py313> -c conda-forge compliance-checker`.
- **CDO** — `mamba install -p <py313> -c conda-forge cdo`. Used for structural sanity
  (`cdo sinfo`, `cdo griddes`) and, critically, the **mosaic test** (`cdo collgrid`).
- **NCO** — `mamba install -p <py313> -c conda-forge nco`. `ncdump -h`, `ncks`,
  attribute inspection; `ncrcat` for time-concatenation edge cases.
- **`xarray` + `rioxarray` + `dask`** — already the project's analysis stack; used for
  the coordinate-coincidence and mass-preservation checks and as the reference mosaic
  path to compare against `cdo collgrid`.

### 6.2 Test fixtures (tiny, fast, committed)

Generate small synthetic NetCDFs (a handful of cells, a few time steps) that mimic SWB2
output structure — cheap to produce in Python and fast to check. Cover:

- **A single clean tile** — the happy path.
- **Two adjacent tiles carved from one master grid spec** — the mosaic case (shared
  origin/cell size/CRS, integer offsets, no overlap).
- **Edge cases deliberately included:** a `_FillValue`-only masked region (active-cell
  masking analog); the `lLatLon = false` case (must NOT emit `coordinates = "lat lon"`);
  a time axis crossing a year boundary; a variable that is a temporal **sum** vs a
  **mean** (checks `cell_methods`); a tile touching the master-grid edge.

Where possible, also run the real `swb2.exe` on two tiny adjacent extents to produce
*genuine* SWB2 output for the mosaic test (this doubles as the "two-tile concatenation"
de-risking experiment the breadcrumbs call for).

### 6.3 Assertions (the actual tests)

1. **CF validity:** `cfchecks -v 1.11 <file>` returns **zero ERRORs** (warnings triaged).
   Wrap in a pytest that parses the checker output and fails on any ERROR.
2. **Required attributes present & correct:** parse `ncdump -h` (or open with xarray)
   and assert: global `Conventions` == target; `x`/`y` have `axis`,
   `standard_name`, `units`; `time` has `standard_name`, `axis`, `calendar`, `units`;
   data var has `long_name`, `grid_mapping = "crs: x y"`, `_FillValue`; `crs` has
   `crs_wkt` and the named-CRS attrs; `proj4_string` **is present** (retained for
   swbstats2) and its `+units=` token agrees with the `x`/`y` `units`; **no** dangling
   `coordinates`.
3. **CRS round-trips:** `rioxarray`/`pyproj` reads the CRS from the file and it equals
   EPSG:5070 (`CRS.from_user_input(ds.rio.crs) == CRS.from_epsg(5070)`).
4. **Grid alignment:** the two tiles share cell size and CRS, and their `x`/`y`
   coordinates fall on the master-grid lattice (differences are integer multiples of the
   cell size, within tolerance).
5. **Mosaic — two independent paths agree:**
   - `cdo collgrid tile1.nc tile2.nc merged_cdo.nc` succeeds with no error/warning.
   - `xarray.combine_by_coords([tile1, tile2])` produces the same shape/extent.
   - Assert no gaps and no overlaps at the seam; assert **mass preservation** (sum over
     the merged grid equals the sum over the tiles, within float tolerance) since this is
     a recharge product.
6. **Time concatenation edge case:** `ncrcat` / `xarray` concat along time preserves
   monotonic, gap-free daily stamps across a year boundary.

### 6.4 Harness

- Put tests under an SWB2-repo `tests/` (or a small `swb2_cf_tests/` package) as
  **pytest** cases, each asserting one of the above. Keep fixtures tiny so the suite runs
  in seconds.
- Run in `py313`:
  `mamba run -p C:\Users\smwesten\.local\share\mamba\envs\py313 python -m pytest`.
- Wire into CI / the build so a CF regression fails the build. Because assertion #1 pins
  the CF version, "which version are we compliant with" becomes an automatically verified
  fact rather than a claim in an attribute.

### 6.5 A note on where these tests live

The CF fixes are upstream SWB2 changes, so the ideal home for the writer tests is the
**swb2 repo** (`E:\projects\swb_development\git\swb2`). The **mosaic/alignment** tests are
equally useful to the VA project's output pipeline and can be mirrored/reused here for the
Option A/B stitching workflow.

---

## 7. Provenance of This Audit

- SWB2 source read on 2026-09-11: `netcdf4_support.F90`
  (`nf_set_standard_attributes`, `nf_set_global_attributes`, `nf_set_standard_variables`),
  cross-referenced with `swbstats2_support.F90` and `proj4_support.F90`.
- CF specification: `https://cfconventions.org/cf-conventions/cf-conventions.html`
  (document build footer `1.12.0-rc7`, revision history current through **1.13** with a
  **1.14-draft**), Section 5.6 / 5.6.1 (grid mappings, CRS WKT) and Appendix F / Appendix
  A (grid-mapping attributes), plus the Revision History appendix for the per-version
  changes summarized in Section 4.
- PROJ call-surface trace (2026-09-11): `grid.F90` (`pj_init_and_transform` interface,
  `grid_Transform` ~line 1537), `data_catalog_entry.F90` (call sites ~942/948 and the
  direct call ~2107), `model_initialize.F90` (~1829), `proj4_support.F90` (hand parser).
  Confirmed one C wrapper + two Fortran call sites + the string parser.
- Prior SWB2 GDAL/PROJ analyses cross-referenced (both May 2026):
  `swb2/design/feature_consideration__adopt_modern_PROJ_library.md` and
  `swb2/design/feature_consideration__reading_writing_geotiffs_gdal.md`. Section 4a.3's
  cost/effort was revised to reconcile with these (and to correct an earlier overstatement
  in this note), while adding the radians / CRS-type-detection / error-handling caveats
  found by reading the transform call sites.
- No SWB2 source was modified in producing this document; it is a read-only audit plus a
  proposed fix + test plan.
