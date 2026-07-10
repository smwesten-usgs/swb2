"""Verify phenology transitions in SWB2 DUMP_VARIABLES output.

This script parses the CSV files produced by the DUMP_VARIABLES directive
and verifies that the `it_is_growing_season` flag transitions at the
expected times for DOY-based and GDD-based phenology methods.

Expected dump files (numbered by order of DUMP_VARIABLES in control file):
  dump_001.csv — Deciduous Forest (LU 141): DOY_BASED, start=04/15(105), end=10/15(288)
  dump_002.csv — Corn (LU 1): GDD_THRESHOLD, GDD_start=200.0, killing_frost=28.0°F
  dump_003.csv — Winter Wheat (LU 24): DOY_BASED winter crop, start=10/01(274), end=05/30(150)

Usage:
    python verify_phenology.py <output_directory>

    If <output_directory> is not provided, looks for dump files in ./output/
"""

from pathlib import Path
import sys
import csv


# Column indices in the DUMP_VARIABLES CSV output
# Integer columns (0-based):
#   0: date string (YYYY-MM-DD)
#   1: month, 2: day, 3: year (redundant)
#   4: landuse_code
#   5: landuse_index
#   6: soil_group
#   7: num_upslope_connections
#   8: sum_upslope_cells
#   9: sort_index
#  10: cell_index
#  11: target_index
#  12: it_is_growing_season (0 or 1)
# Float columns follow (13+):
#  13: awc
#  ...
#  49: gdd  (index from the write statement in model_domain.F90)

COL_DATE = 0
COL_MONTH = 1
COL_DAY = 2
COL_YEAR = 3
COL_LU_CODE = 4
COL_GROWING_SEASON = 12
# GDD is the 37th value in the float block (starting at col 13)
# From the write statement: awc(1), lat(2), ref_et(3), actual_et(4), cn_adj(5),
# gross_precip(6), inflow(7), runon(8), runoff(9), outflow(10), infiltration(11),
# snowfall(12), pot_snowmelt(13), snowmelt(14), interception(15), rainfall(16),
# net_rainfall(17), monthly_gross_precip(18), monthly_runoff(19),
# interception_storage(20), tmax(21), tmin(22), tmean(23), snow_storage(24),
# soil_storage(25), soil_storage_max(26), evap_water_storage(27),
# evap_water_deficit(28), delta_soil_storage(29), soil_moisture_deficit(30),
# surface_storage(31), surface_storage_excess(32), surface_storage_max(33),
# net_infiltration(34), rejected_net_infil(35), fog(36), irrigation(37),
# gdd(38), ...
COL_GDD = 13 + 37  # 0-based: column 50
COL_TMEAN = 13 + 10  # tmean is 23rd float = index 13+10=23... let me recount

# Actually let me be more careful. The float columns start at index 13.
# From the Fortran write statement order:
#  13: awc
#  14: latitude
#  15: reference_ET0
#  16: actual_ET
#  17: curve_num_adj
#  18: gross_precip
#  19: inflow
#  20: runon
#  21: runoff
#  22: outflow
#  23: infiltration
#  24: snowfall
#  25: potential_snowmelt
#  26: snowmelt
#  27: interception
#  28: rainfall
#  29: net_rainfall
#  30: monthly_gross_precip
#  31: monthly_runoff
#  32: interception_storage
#  33: tmax
#  34: tmin
#  35: tmean
#  36: snow_storage
#  37: soil_storage
#  38: soil_storage_max
#  39: evaporable_water_storage
#  40: evaporable_water_deficit
#  41: delta_soil_storage
#  42: soil_moisture_deficit
#  43: surface_storage
#  44: surface_storage_excess
#  45: surface_storage_max
#  46: net_infiltration
#  47: rejected_net_infiltration
#  48: fog
#  49: irrigation
#  50: gdd
#  51: runoff_outside
#  52: pervious_fraction
#  53: storm_drain_capture
#  54: canopy_cover_fraction
#  55: crop_coefficient_kcb
#  56: continuous_frozen_ground_index
#  57: rooting_depth_max
#  58: current_rooting_depth
#  59: current_plant_height
#  60: actual_et_soil
#  61: readily_available_water_raw
#  62: total_available_water_taw
#  63: plant_stress_coef_ks
#  64: evap_reduction_coef_kr
#  65: surf_evap_coef_ke
#  66: fraction_exposed_and_wetted_soil
#  67: actual_et_impervious
#  68: actual_et_interception
#  69: adjusted_depletion_fraction_p
#  70: crop_etc
#  71: bare_soil_evap
#  72: direct_net_infiltration
#  73: direct_soil_moisture
#  74-79: previous_5_day_rain(1:6)

COL_GDD = 50
COL_TMEAN = 35


def parse_dump_csv(filepath: Path) -> list[dict]:
    """Parse a DUMP_VARIABLES CSV file into a list of row dictionaries.

    Returns a list of dicts with keys: date, month, day, year, doy,
    lu_code, it_is_growing_season, gdd, tmean.
    """
    rows = []
    with open(filepath, "r") as f:
        reader = csv.reader(f)
        for line in reader:
            if not line or line[0].strip().startswith("#"):
                continue
            # Strip whitespace from all fields
            fields = [f.strip() for f in line]
            if len(fields) < 75:
                continue  # skip malformed lines

            # Skip header rows (first field should be a date like "2012-01-01")
            date_str = fields[COL_DATE]
            if not date_str[:4].isdigit():
                continue

            month = int(fields[COL_MONTH])
            day = int(fields[COL_DAY])
            year = int(fields[COL_YEAR])
            lu_code = int(fields[COL_LU_CODE])
            growing_season = int(fields[COL_GROWING_SEASON])
            gdd = float(fields[COL_GDD])
            tmean = float(fields[COL_TMEAN])

            # Calculate DOY from month/day (approximate, handles leap year 2012)
            from datetime import date
            doy = date(year, month, day).timetuple().tm_yday

            rows.append({
                "date": date_str,
                "month": month,
                "day": day,
                "year": year,
                "doy": doy,
                "lu_code": lu_code,
                "it_is_growing_season": growing_season,
                "gdd": gdd,
                "tmean": tmean,
            })
    return rows


def verify_doy_based(
    rows: list[dict],
    label: str,
    expected_lu_code: int,
    start_doy: int,
    end_doy: int,
) -> list[str]:
    """Verify DOY-based phenology transitions.

    For normal season (start < end): growing on [start, end], dormant otherwise.
    For winter crop (start > end): growing on [start, 365] ∪ [1, end], dormant otherwise.
    """
    errors = []

    if not rows:
        errors.append(f"{label}: No data rows found")
        return errors

    # Verify land use code
    actual_lu = rows[0]["lu_code"]
    if actual_lu != expected_lu_code:
        errors.append(
            f"{label}: Expected LU code {expected_lu_code}, got {actual_lu}"
        )
        return errors

    winter_crop = start_doy > end_doy

    for row in rows:
        doy = row["doy"]
        gs = row["it_is_growing_season"]

        if winter_crop:
            expected_growing = (doy >= start_doy) or (doy <= end_doy)
        else:
            expected_growing = (doy >= start_doy) and (doy <= end_doy)

        expected_gs = 1 if expected_growing else 0

        if gs != expected_gs:
            errors.append(
                f"{label}: DOY {doy} ({row['date']}): "
                f"expected it_is_growing_season={expected_gs}, got {gs}"
            )

    return errors


def verify_gdd_threshold(
    rows: list[dict],
    label: str,
    expected_lu_code: int,
    gdd_threshold: float,
    killing_frost_temp: float,
) -> list[str]:
    """Verify GDD-based phenology transitions.

    Growing season starts when GDD >= threshold AND temp > frost (while dormant).
    Growing season ends when tmean <= killing_frost_temp (while growing).
    Once frost kills the season, it stays dormant for the rest of the calendar year
    (hard latch — resets at year boundary).
    """
    errors = []

    if not rows:
        errors.append(f"{label}: No data rows found")
        return errors

    # Verify land use code
    actual_lu = rows[0]["lu_code"]
    if actual_lu != expected_lu_code:
        errors.append(
            f"{label}: Expected LU code {expected_lu_code}, got {actual_lu}"
        )
        return errors

    # Walk through the time series and verify state transitions
    prev_gs = 0  # assume dormant at start of year
    frost_killed = False
    current_year = rows[0]["year"] if rows else None

    for i, row in enumerate(rows):
        gs = row["it_is_growing_season"]
        gdd = row["gdd"]
        tmean = row["tmean"]

        # Reset frost latch at year boundary
        if row["year"] != current_year:
            frost_killed = False
            current_year = row["year"]

        # Determine expected state using the same logic as the Fortran code
        if frost_killed:
            # Hard latch: stay dormant for rest of year
            expected_gs = 0
        elif prev_gs == 1:
            # Was growing — check for killing frost
            if tmean <= killing_frost_temp:
                expected_gs = 0
                frost_killed = True
            else:
                expected_gs = 1
        else:
            # Was dormant, no prior frost — check GDD threshold AND temp
            if gdd >= gdd_threshold and tmean > killing_frost_temp:
                expected_gs = 1
            else:
                expected_gs = 0

        if gs != expected_gs:
            errors.append(
                f"{label}: DOY {row['doy']} ({row['date']}): "
                f"expected it_is_growing_season={expected_gs}, got {gs} "
                f"(GDD={gdd:.1f}, tmean={tmean:.1f}, frost_killed={frost_killed})"
            )

        prev_gs = gs

    return errors


def find_dump_files(output_dir: Path) -> list[Path]:
    """Find dump CSV files in the output directory.

    SWB2 names them: SWB2_variable_values__col_NNN__row_NNN__x_XXXXXX__y_YYYYYY.csv
    """
    found = sorted(output_dir.glob("SWB2_variable_values__*.csv"))
    if not found:
        # Fallback to any CSV
        found = sorted(output_dir.glob("*.csv"))
    return found


def find_dump_by_coordinates(dump_files: list[Path], x: int, y: int) -> Path | None:
    """Find the dump file matching specific coordinates.

    The filename contains x and y values: ...x_553927__y_459454.csv
    """
    for f in dump_files:
        if f"x_{x}" in f.name and f"y_{y}" in f.name:
            return f
    return None


def main() -> int:
    """Run phenology verification checks."""
    if len(sys.argv) > 1:
        output_dir = Path(sys.argv[1])
    else:
        output_dir = Path("output")

    if not output_dir.exists():
        print(f"ERROR: Output directory '{output_dir}' not found.")
        print("Run SWB2 with phenology_test.ctl first, then point this script")
        print("at the directory containing the dump CSV files.")
        return 1

    dump_files = find_dump_files(output_dir)

    if len(dump_files) < 3:
        print(f"ERROR: Expected at least 3 dump files, found {len(dump_files)} in {output_dir}")
        print(f"  Found: {[f.name for f in dump_files]}")
        return 1

    print(f"Found {len(dump_files)} dump files in {output_dir}")

    # Locate specific dump files by coordinates from the control file
    # Point 1: Deciduous Forest — x=553927, y=459454
    # Point 2: Corn — x=557946, y=434408
    # Point 3: Winter Wheat — x=568129, y=458340
    file_forest = find_dump_by_coordinates(dump_files, 553927, 459454)
    file_corn = find_dump_by_coordinates(dump_files, 557946, 434408)
    file_wheat = find_dump_by_coordinates(dump_files, 568129, 458340)

    missing = []
    if file_forest is None:
        missing.append("Deciduous Forest (x=553927, y=459454)")
    if file_corn is None:
        missing.append("Corn (x=557946, y=434408)")
    if file_wheat is None:
        missing.append("Winter Wheat (x=568129, y=458340)")

    if missing:
        print(f"ERROR: Could not find dump files for: {missing}")
        print(f"  Available files: {[f.name for f in dump_files]}")
        return 1

    print(f"  Forest: {file_forest.name}")
    print(f"  Corn:   {file_corn.name}")
    print(f"  Wheat:  {file_wheat.name}")

    # Parse dump files
    rows_forest = parse_dump_csv(file_forest)
    rows_corn = parse_dump_csv(file_corn)
    rows_wheat = parse_dump_csv(file_wheat)

    print(f"\nParsed rows: forest={len(rows_forest)}, corn={len(rows_corn)}, wheat={len(rows_wheat)}")

    all_errors: list[str] = []

    # Test 1: Deciduous Forest (LU 141) — DOY_BASED
    # mmdd2doy uses non-leap-year (1999) as reference, so:
    # 04/15 = DOY 105, 10/15 = DOY 288 (regardless of actual year)
    # In leap years, this means the season starts on 04/14 and ends on 10/14
    # — this matches the old growing_season.F90 behavior.
    print("\n--- Test 1: Deciduous Forest (LU 141) — DOY_BASED ---")
    print(f"  Expected: growing DOY 105–288 (04/15–10/15, non-leap DOY via mmdd2doy)")
    errors = verify_doy_based(
        rows_forest,
        label="Deciduous Forest",
        expected_lu_code=141,
        start_doy=105,  # 04/15 in non-leap year (mmdd2doy reference)
        end_doy=288,    # 10/15 in non-leap year (mmdd2doy reference)
    )
    if errors:
        print(f"  FAILED: {len(errors)} error(s)")
        for e in errors[:10]:
            print(f"    {e}")
        if len(errors) > 10:
            print(f"    ... and {len(errors) - 10} more")
    else:
        print("  PASSED")
    all_errors.extend(errors)

    # Test 2: Corn (LU 1) — GDD_THRESHOLD
    print("\n--- Test 2: Corn (LU 1) — GDD_THRESHOLD ---")
    print(f"  Expected: growing when GDD >= 200 until tmean <= 28°F")
    errors = verify_gdd_threshold(
        rows_corn,
        label="Corn",
        expected_lu_code=1,
        gdd_threshold=200.0,
        killing_frost_temp=28.0,
    )
    if errors:
        print(f"  FAILED: {len(errors)} error(s)")
        for e in errors[:10]:
            print(f"    {e}")
        if len(errors) > 10:
            print(f"    ... and {len(errors) - 10} more")
    else:
        print("  PASSED")
    all_errors.extend(errors)

    # Test 3: Winter Wheat (LU 24) — DOY_BASED winter crop
    # mmdd2doy: 10/01 = DOY 274, 05/30 = DOY 150 (non-leap reference)
    print("\n--- Test 3: Winter Wheat (LU 24) — DOY_BASED winter crop ---")
    print(f"  Expected: growing DOY >= 274 OR DOY <= 150 (10/01–05/30 wrap, non-leap DOY)")
    errors = verify_doy_based(
        rows_wheat,
        label="Winter Wheat",
        expected_lu_code=24,
        start_doy=274,  # 10/01 in non-leap year (mmdd2doy reference)
        end_doy=150,    # 05/30 in non-leap year (mmdd2doy reference)
    )
    if errors:
        print(f"  FAILED: {len(errors)} error(s)")
        for e in errors[:10]:
            print(f"    {e}")
        if len(errors) > 10:
            print(f"    ... and {len(errors) - 10} more")
    else:
        print("  PASSED")
    all_errors.extend(errors)

    # Summary
    print(f"\n{'='*60}")
    if all_errors:
        print(f"FAILED: {len(all_errors)} total error(s) across all tests")
        return 1
    else:
        print("ALL TESTS PASSED — phenology transitions verified")
        return 0


if __name__ == "__main__":
    sys.exit(main())
