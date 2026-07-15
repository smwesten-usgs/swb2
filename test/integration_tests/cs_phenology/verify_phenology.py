"""Verify phenology transitions in SWB2 DUMP_VARIABLES output.

This script parses the CSV files produced by the DUMP_VARIABLES directive
and verifies that the `it_is_growing_season` flag transitions at the
expected times for DOY-based and GDD-based phenology methods.

Expected dump files (identified by coordinates in filename):
  Forest (x=553927, y=459454) — Deciduous Forest (LU 141): DOY_BASED
  Corn   (x=557946, y=434408) — Corn (LU 1): GDD_THRESHOLD
  Wheat  (x=568129, y=458340) — Winter Wheat (LU 24): DOY_BASED winter crop

Usage:
    python verify_phenology.py <output_directory>

    If <output_directory> is not provided, looks for dump files in ./output/
"""

from pathlib import Path
import sys
import csv
from datetime import date


def parse_dump_csv(filepath: Path) -> list[dict]:
    """Parse a DUMP_VARIABLES CSV file into a list of row dictionaries.

    Reads column headers from the first non-comment line, then parses
    data rows by column name. Returns a list of dicts with keys:
    date, month, day, year, doy, lu_code, it_is_growing_season, gdd, tmean.
    """
    rows = []
    headers = None

    with open(filepath, "r") as f:
        reader = csv.reader(f)
        for line in reader:
            if not line:
                continue

            # Strip whitespace from all fields
            fields = [field.strip() for field in line]

            # Skip comment lines
            if fields[0].startswith("#"):
                continue

            # First non-comment line with non-numeric first field is the header
            if headers is None and not fields[0][:4].isdigit():
                headers = [h.lower() for h in fields]
                continue

            # If we still don't have headers, skip
            if headers is None:
                continue

            # Skip lines that don't start with a date
            if not fields[0][:4].isdigit():
                continue

            # Build a dict from header names
            row_dict = {}
            for i, h in enumerate(headers):
                if i < len(fields):
                    row_dict[h] = fields[i]

            # Extract the fields we need by column name
            try:
                date_str = row_dict.get("date", fields[0])
                month = int(row_dict["month"])
                day = int(row_dict["day"])
                year = int(row_dict["year"])
                lu_code = int(row_dict["landuse_code"])
                growing_season = int(row_dict["growing_season"])
                gdd = float(row_dict["gdd"])
                tmean = float(row_dict["tmean"])
            except (KeyError, ValueError) as e:
                print(f"  WARNING: Could not parse row in {filepath.name}: {e}")
                print(f"    Available columns: {list(row_dict.keys())[:20]}...")
                continue

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

    if headers is None:
        print(f"  WARNING: No header row found in {filepath.name}")

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
    For winter crop (start > end): growing on [start, 365/366] ∪ [1, end].
    """
    errors = []

    if not rows:
        errors.append(f"{label}: No data rows found")
        return errors

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

    Growing season starts when GDD >= threshold AND temp > frost.
    Growing season ends when tmean <= killing_frost_temp.
    Once frost kills the season, it stays dormant for the rest of the year.
    """
    errors = []

    if not rows:
        errors.append(f"{label}: No data rows found")
        return errors

    actual_lu = rows[0]["lu_code"]
    if actual_lu != expected_lu_code:
        errors.append(
            f"{label}: Expected LU code {expected_lu_code}, got {actual_lu}"
        )
        return errors

    prev_gs = 0
    frost_killed = False
    current_year = rows[0]["year"] if rows else None

    for row in rows:
        gs = row["it_is_growing_season"]
        gdd = row["gdd"]
        tmean = row["tmean"]

        # Reset frost latch at year boundary
        if row["year"] != current_year:
            frost_killed = False
            current_year = row["year"]

        # Determine expected state
        if frost_killed:
            expected_gs = 0
        elif prev_gs == 1:
            if tmean <= killing_frost_temp:
                expected_gs = 0
                frost_killed = True
            else:
                expected_gs = 1
        else:
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
    """Find dump CSV files in the output directory."""
    found = sorted(output_dir.glob("SWB2_variable_values__*.csv"))
    if not found:
        found = sorted(output_dir.glob("*.csv"))
    return found


def find_dump_by_coordinates(dump_files: list[Path], x: int, y: int) -> Path | None:
    """Find the dump file matching specific coordinates."""
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

    # Locate dump files by coordinates
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
    print("\n--- Test 1: Deciduous Forest (LU 141) — DOY_BASED ---")
    print(f"  Expected: growing DOY 105–288 (04/15–10/15, non-leap DOY via mmdd2doy)")
    errors = verify_doy_based(
        rows_forest,
        label="Deciduous Forest",
        expected_lu_code=141,
        start_doy=105,
        end_doy=288,
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
    print("\n--- Test 3: Winter Wheat (LU 24) — DOY_BASED winter crop ---")
    print(f"  Expected: growing DOY >= 274 OR DOY <= 150 (10/01–05/30 wrap, non-leap DOY)")
    errors = verify_doy_based(
        rows_wheat,
        label="Winter Wheat",
        expected_lu_code=24,
        start_doy=274,
        end_doy=150,
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
