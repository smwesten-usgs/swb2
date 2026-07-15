"""Verify interception_storage_max transitions in SWB2 DUMP_VARIABLES output.

This script verifies that growth_fraction correctly interpolates
interception_storage_max between nongrowing and growing values based
on the active phenology method.

Expected behavior:
  - DOY_BASED: storage_max = nongrowing when dormant, growing when active (binary)
  - GDD_THRESHOLD: same binary behavior keyed to GDD/frost
  - FAO56_DATES/GDD: smooth ramp during INI/DEV, holds at growing through MID/LATE

Usage:
    python verify_interception.py [output_directory]
"""

from pathlib import Path
import sys
import csv
from datetime import date


# Interception values from the test lookup table
INTERCEPTION = {
    141: {"growing": 0.15, "nongrowing": 0.03, "label": "Deciduous Forest", "method": "DOY_BASED"},
    1:   {"growing": 0.12, "nongrowing": 0.02, "label": "Corn", "method": "GDD_THRESHOLD"},
    24:  {"growing": 0.10, "nongrowing": 0.02, "label": "Winter Wheat", "method": "DOY_BASED_WINTER"},
}

# Tolerance for floating point comparison
TOL = 1.0e-4


def parse_dump_csv(filepath: Path) -> list[dict]:
    """Parse a DUMP_VARIABLES CSV file using column headers."""
    rows = []
    headers = None

    with open(filepath, "r") as f:
        reader = csv.reader(f)
        for line in reader:
            if not line:
                continue
            fields = [field.strip() for field in line]
            if fields[0].startswith("#"):
                continue
            if headers is None and not fields[0][:4].isdigit():
                headers = [h.lower() for h in fields]
                continue
            if headers is None:
                continue
            if not fields[0][:4].isdigit():
                continue

            row_dict = {}
            for i, h in enumerate(headers):
                if i < len(fields):
                    row_dict[h] = fields[i]

            try:
                month = int(row_dict["month"])
                day = int(row_dict["day"])
                year = int(row_dict["year"])
                rows.append({
                    "date": row_dict.get("date", ""),
                    "doy": date(year, month, day).timetuple().tm_yday,
                    "lu_code": int(row_dict["landuse_code"]),
                    "growing_season": int(row_dict["growing_season"]),
                    "interception_storage_max": float(row_dict["interception_storage_max"]),
                    "gdd": float(row_dict["gdd"]),
                    "tmean": float(row_dict["tmean"]),
                })
            except (KeyError, ValueError) as e:
                print(f"  WARNING: Could not parse row: {e}")
                print(f"    Available columns: {list(row_dict.keys())[:15]}...")
                continue

    return rows


def verify_interception_doy_based(
    rows: list[dict],
    lu_code: int,
    start_doy: int,
    end_doy: int,
) -> list[str]:
    """Verify interception_storage_max transitions for DOY_BASED phenology.

    During growing season: storage_max should equal growing value.
    During dormant: storage_max should equal nongrowing value.
    """
    errors = []
    info = INTERCEPTION[lu_code]
    growing_val = info["growing"]
    nongrowing_val = info["nongrowing"]
    label = info["label"]
    winter_crop = start_doy > end_doy

    for row in rows:
        doy = row["doy"]
        storage_max = row["interception_storage_max"]

        if winter_crop:
            is_growing = (doy >= start_doy) or (doy <= end_doy)
        else:
            is_growing = (doy >= start_doy) and (doy <= end_doy)

        expected = growing_val if is_growing else nongrowing_val

        if abs(storage_max - expected) > TOL:
            errors.append(
                f"{label}: DOY {doy} ({row['date']}): "
                f"interception_storage_max={storage_max:.4f}, "
                f"expected={expected:.4f} "
                f"({'growing' if is_growing else 'dormant'})"
            )

    return errors


def verify_interception_gdd_threshold(
    rows: list[dict],
    lu_code: int,
    gdd_threshold: float,
    killing_frost_temp: float,
) -> list[str]:
    """Verify interception_storage_max transitions for GDD_THRESHOLD phenology.

    Uses the growing_season flag from the model output to determine expected state,
    then verifies storage_max matches.
    """
    errors = []
    info = INTERCEPTION[lu_code]
    growing_val = info["growing"]
    nongrowing_val = info["nongrowing"]
    label = info["label"]

    for row in rows:
        storage_max = row["interception_storage_max"]
        is_growing = row["growing_season"] == 1

        expected = growing_val if is_growing else nongrowing_val

        if abs(storage_max - expected) > TOL:
            errors.append(
                f"{label}: DOY {row['doy']} ({row['date']}): "
                f"interception_storage_max={storage_max:.4f}, "
                f"expected={expected:.4f} "
                f"(growing_season={row['growing_season']}, "
                f"GDD={row['gdd']:.1f})"
            )

    return errors


def verify_storage_max_range(rows: list[dict], lu_code: int) -> list[str]:
    """Verify that interception_storage_max stays within [nongrowing, growing] bounds."""
    errors = []
    info = INTERCEPTION[lu_code]
    growing_val = info["growing"]
    nongrowing_val = info["nongrowing"]
    label = info["label"]
    lo = min(nongrowing_val, growing_val)
    hi = max(nongrowing_val, growing_val)

    for row in rows:
        storage_max = row["interception_storage_max"]
        if storage_max < lo - TOL or storage_max > hi + TOL:
            errors.append(
                f"{label}: DOY {row['doy']} ({row['date']}): "
                f"interception_storage_max={storage_max:.4f} "
                f"outside valid range [{lo:.4f}, {hi:.4f}]"
            )

    return errors


def find_dump_by_coordinates(dump_files: list[Path], x: int, y: int) -> Path | None:
    """Find the dump file matching specific coordinates."""
    for f in dump_files:
        if f"x_{x}" in f.name and f"y_{y}" in f.name:
            return f
    return None


def main() -> int:
    """Run interception verification checks."""
    if len(sys.argv) > 1:
        output_dir = Path(sys.argv[1])
    else:
        output_dir = Path("output")

    if not output_dir.exists():
        print(f"ERROR: Output directory '{output_dir}' not found.")
        return 1

    dump_files = sorted(output_dir.glob("SWB2_variable_values__*.csv"))

    if len(dump_files) < 3:
        print(f"ERROR: Expected at least 3 dump files, found {len(dump_files)}")
        return 1

    print(f"Found {len(dump_files)} dump files in {output_dir}")

    file_forest = find_dump_by_coordinates(dump_files, 553927, 459454)
    file_corn = find_dump_by_coordinates(dump_files, 557946, 434408)
    file_wheat = find_dump_by_coordinates(dump_files, 568129, 458340)

    if not all([file_forest, file_corn, file_wheat]):
        print("ERROR: Could not locate all expected dump files by coordinates")
        return 1

    print(f"  Forest: {file_forest.name}")
    print(f"  Corn:   {file_corn.name}")
    print(f"  Wheat:  {file_wheat.name}")

    rows_forest = parse_dump_csv(file_forest)
    rows_corn = parse_dump_csv(file_corn)
    rows_wheat = parse_dump_csv(file_wheat)

    print(f"\nParsed rows: forest={len(rows_forest)}, corn={len(rows_corn)}, wheat={len(rows_wheat)}")

    all_errors: list[str] = []

    # Test 1: Deciduous Forest — DOY_BASED, start=105, end=288
    print("\n--- Test 1: Deciduous Forest (LU 141) — DOY_BASED ---")
    print(f"  Expected: storage_max = 0.15 (growing DOY 105-288), 0.03 (dormant)")
    errors = verify_interception_doy_based(rows_forest, lu_code=141, start_doy=105, end_doy=288)
    errors += verify_storage_max_range(rows_forest, lu_code=141)
    if errors:
        print(f"  FAILED: {len(errors)} error(s)")
        for e in errors[:5]:
            print(f"    {e}")
        if len(errors) > 5:
            print(f"    ... and {len(errors) - 5} more")
    else:
        print("  PASSED")
    all_errors.extend(errors)

    # Test 2: Corn — GDD_THRESHOLD, threshold=200, frost=28
    print("\n--- Test 2: Corn (LU 1) — GDD_THRESHOLD ---")
    print(f"  Expected: storage_max = 0.12 (growing), 0.02 (dormant)")
    errors = verify_interception_gdd_threshold(rows_corn, lu_code=1,
                                               gdd_threshold=200.0,
                                               killing_frost_temp=28.0)
    errors += verify_storage_max_range(rows_corn, lu_code=1)
    if errors:
        print(f"  FAILED: {len(errors)} error(s)")
        for e in errors[:5]:
            print(f"    {e}")
        if len(errors) > 5:
            print(f"    ... and {len(errors) - 5} more")
    else:
        print("  PASSED")
    all_errors.extend(errors)

    # Test 3: Winter Wheat — DOY_BASED winter crop, start=274, end=150
    print("\n--- Test 3: Winter Wheat (LU 24) — DOY_BASED winter crop ---")
    print(f"  Expected: storage_max = 0.10 (growing DOY>=274 or DOY<=150), 0.02 (dormant)")
    errors = verify_interception_doy_based(rows_wheat, lu_code=24, start_doy=274, end_doy=150)
    errors += verify_storage_max_range(rows_wheat, lu_code=24)
    if errors:
        print(f"  FAILED: {len(errors)} error(s)")
        for e in errors[:5]:
            print(f"    {e}")
        if len(errors) > 5:
            print(f"    ... and {len(errors) - 5} more")
    else:
        print("  PASSED")
    all_errors.extend(errors)

    # Summary
    print(f"\n{'='*60}")
    if all_errors:
        print(f"FAILED: {len(all_errors)} total error(s)")
        return 1
    else:
        print("ALL TESTS PASSED — interception_storage_max transitions verified")
        return 0


if __name__ == "__main__":
    sys.exit(main())
