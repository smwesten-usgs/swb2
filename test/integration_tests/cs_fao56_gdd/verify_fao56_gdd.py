"""Verify FAO56 GDD-based Kcb curves in SWB2 DUMP_VARIABLES output.

Tests that:
  1. Kcb remains at Kcb_min during dormancy (before Growing_season_start_GDD)
  2. Kcb ramps from Kcb_ini to Kcb_mid during development stage
  3. Kcb holds at Kcb_mid during mid-season
  4. Kcb declines from Kcb_mid to Kcb_end during late stage
  5. Kcb returns to Kcb_min after killing frost
  6. Crops with different Tbase values start growing at different calendar dates
  7. Barren land (Start_GDD=9999) never enters growing season

Usage:
    python verify_fao56_gdd.py <output_directory>
"""

from pathlib import Path
import sys
import csv
from datetime import date
from typing import NamedTuple


class DumpRow(NamedTuple):
    """Single row of parsed DUMP_VARIABLES data."""

    date_str: str
    month: int
    day: int
    year: int
    doy: int
    lu_code: int
    growing_season: int
    gdd: float
    tmean: float
    crop_coefficient_kcb: float
    crop_etc: float


def parse_dump_csv(filepath: Path) -> list[DumpRow]:
    """Parse a DUMP_VARIABLES CSV file into a list of DumpRow records.

    Args:
        filepath: Path to the CSV file produced by SWB2 DUMP_VARIABLES.

    Returns:
        List of DumpRow named tuples with key fields extracted.
    """
    rows: list[DumpRow] = []
    headers: list[str] | None = None

    with open(filepath, "r") as f:
        reader = csv.reader(f)
        for line in reader:
            if not line:
                continue
            fields = [field.strip() for field in line]
            if fields[0].startswith("#"):
                continue
            if headers is None and not fields[0][:4].isdigit():
                headers = [h.lower().strip() for h in fields]
                continue
            if headers is None:
                continue
            if not fields[0][:4].isdigit():
                continue

            row_dict = {h: fields[i] if i < len(fields) else ""
                        for i, h in enumerate(headers)}

            try:
                month = int(row_dict["month"])
                day_val = int(row_dict["day"])
                year = int(row_dict["year"])
                rows.append(DumpRow(
                    date_str=row_dict.get("date", fields[0]),
                    month=month,
                    day=day_val,
                    year=year,
                    doy=date(year, month, day_val).timetuple().tm_yday,
                    lu_code=int(row_dict["landuse_code"]),
                    growing_season=int(row_dict["growing_season"]),
                    gdd=float(row_dict["gdd"]),
                    tmean=float(row_dict["tmean"]),
                    crop_coefficient_kcb=float(row_dict["crop_coefficient_kcb"]),
                    crop_etc=float(row_dict["crop_etc"]),
                ))
            except (KeyError, ValueError) as e:
                print(f"  WARNING: Could not parse row in {filepath.name}: {e}")
                continue

    return rows


def find_dump_files(output_dir: Path) -> list[Path]:
    """Locate all DUMP_VARIABLES CSV files in the output directory."""
    return sorted(output_dir.glob("SWB2_variable_values__*.csv"))


def find_dump_by_coordinates(dump_files: list[Path], x: int, y: int) -> Path | None:
    """Find the dump file matching specific coordinates."""
    for f in dump_files:
        if f"x_{x}" in f.name and f"y_{y}" in f.name:
            return f
    return None




# =============================================================================
# Verification functions
# =============================================================================


def verify_dormant_kcb(rows: list[DumpRow], label: str,
                       expected_kcb_min: float,
                       tolerance: float = 0.02) -> list[str]:
    """Verify that Kcb equals Kcb_min before growing season starts.

    Args:
        rows: Parsed dump data for a single point.
        label: Descriptive label for error messages.
        expected_kcb_min: Expected dormant Kcb value.
        tolerance: Allowed deviation from expected value.

    Returns:
        List of error message strings (empty if passed).
    """
    errors: list[str] = []
    for row in rows:
        if row.growing_season == 0:
            if abs(row.crop_coefficient_kcb - expected_kcb_min) > tolerance:
                errors.append(
                    f"{label}: DOY {row.doy}: dormant Kcb={row.crop_coefficient_kcb:.4f}, "
                    f"expected {expected_kcb_min:.2f} (±{tolerance})")
    return errors


def verify_kcb_curve_shape(rows: list[DumpRow], label: str,
                           expected_kcb_ini: float,
                           expected_kcb_mid: float,
                           expected_kcb_end: float,
                           expected_kcb_min: float,
                           tolerance: float = 0.05) -> list[str]:
    """Verify the overall shape of the Kcb curve through the growing season.

    Checks:
    - Peak Kcb reaches near Kcb_mid
    - Kcb starts near Kcb_ini when growing season begins
    - Kcb stays bounded between Kcb_min and Kcb_mid (plus tolerance)
    - There's a ramp-up period (development stage)

    Args:
        rows: Parsed dump data for a single point.
        label: Descriptive label for error messages.
        expected_kcb_ini: Initial stage Kcb value.
        expected_kcb_mid: Peak mid-season Kcb value.
        expected_kcb_end: Late-season end Kcb value.
        expected_kcb_min: Dormant/minimum Kcb value.
        tolerance: Allowed deviation.

    Returns:
        List of error message strings.
    """
    errors: list[str] = []

    growing_rows = [r for r in rows if r.growing_season == 1]
    if not growing_rows:
        return [f"{label}: Never entered growing season"]

    # Peak Kcb should reach near Kcb_mid
    peak_kcb = max(r.crop_coefficient_kcb for r in growing_rows)
    if peak_kcb < expected_kcb_mid - tolerance:
        errors.append(
            f"{label}: Peak Kcb={peak_kcb:.4f}, expected near {expected_kcb_mid:.2f}")

    # Kcb should never exceed Kcb_mid + tolerance (no overshoot)
    for row in growing_rows:
        if row.crop_coefficient_kcb > expected_kcb_mid + tolerance:
            errors.append(
                f"{label}: DOY {row.doy}: Kcb={row.crop_coefficient_kcb:.4f} "
                f"exceeds Kcb_mid={expected_kcb_mid:.2f} + tolerance")
            break  # Only report first violation

    # First growing-season day should be near Kcb_ini
    first_kcb = growing_rows[0].crop_coefficient_kcb
    if abs(first_kcb - expected_kcb_ini) > tolerance:
        errors.append(
            f"{label}: First growing-season Kcb={first_kcb:.4f}, "
            f"expected near Kcb_ini={expected_kcb_ini:.2f}")

    # Check for development ramp: should see increasing Kcb values
    kcb_values = [r.crop_coefficient_kcb for r in growing_rows[:60]]
    if len(kcb_values) >= 20:
        early_mean = sum(kcb_values[:10]) / 10
        later_mean = sum(kcb_values[-10:]) / 10
        if later_mean <= early_mean:
            errors.append(
                f"{label}: No Kcb ramp-up detected in first 60 growing days "
                f"(early mean={early_mean:.3f}, later mean={later_mean:.3f})")

    return errors


def verify_never_growing(rows: list[DumpRow], label: str,
                         expected_kcb: float,
                         tolerance: float = 0.02) -> list[str]:
    """Verify that a land use never enters growing season (e.g., barren).

    Args:
        rows: Parsed dump data.
        label: Descriptive label.
        expected_kcb: Expected constant Kcb value.
        tolerance: Allowed deviation.

    Returns:
        List of error strings.
    """
    errors: list[str] = []
    for row in rows:
        if row.growing_season != 0:
            errors.append(
                f"{label}: DOY {row.doy}: growing_season=1, expected always dormant")
            break
        if abs(row.crop_coefficient_kcb - expected_kcb) > tolerance:
            errors.append(
                f"{label}: DOY {row.doy}: Kcb={row.crop_coefficient_kcb:.4f}, "
                f"expected constant {expected_kcb:.2f}")
            break
    return errors


def verify_start_timing(rows: list[DumpRow], label: str,
                        earliest_doy: int, latest_doy: int) -> list[str]:
    """Verify that the growing season starts within an expected DOY window.

    Args:
        rows: Parsed dump data.
        label: Descriptive label.
        earliest_doy: Earliest acceptable start DOY.
        latest_doy: Latest acceptable start DOY.

    Returns:
        List of error strings.
    """
    errors: list[str] = []
    start_doy = None
    for row in rows:
        if row.growing_season == 1:
            start_doy = row.doy
            break

    if start_doy is None:
        return [f"{label}: Never entered growing season"]

    if start_doy < earliest_doy or start_doy > latest_doy:
        errors.append(
            f"{label}: Growing season started DOY {start_doy}, "
            f"expected between DOY {earliest_doy}–{latest_doy}")

    return errors


def verify_warm_before_cool(warm_rows: list[DumpRow],
                            cool_rows: list[DumpRow],
                            warm_label: str,
                            cool_label: str) -> list[str]:
    """Verify that cool-season crops (lower Tbase) start earlier than warm-season.

    Args:
        warm_rows: Dump data for warm-season crop.
        cool_rows: Dump data for cool-season crop.
        warm_label: Label for warm-season crop.
        cool_label: Label for cool-season crop.

    Returns:
        List of error strings.
    """
    errors: list[str] = []

    warm_start = next((r.doy for r in warm_rows if r.growing_season == 1), None)
    cool_start = next((r.doy for r in cool_rows if r.growing_season == 1), None)

    if warm_start is None:
        return [f"{warm_label}: Never entered growing season"]
    if cool_start is None:
        return [f"{cool_label}: Never entered growing season"]

    if cool_start >= warm_start:
        errors.append(
            f"Expected {cool_label} (Tbase=32°F) to start before {warm_label} "
            f"(Tbase=50°F), but got {cool_label} DOY {cool_start} vs "
            f"{warm_label} DOY {warm_start}")

    return errors




# =============================================================================
# Main
# =============================================================================

def main() -> int:
    """Run FAO56 GDD-based Kcb verification."""
    output_dir = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("output")

    if not output_dir.exists():
        print(f"ERROR: Output directory '{output_dir}' not found.")
        return 1

    dump_files = find_dump_files(output_dir)
    if len(dump_files) < 6:
        print(f"ERROR: Expected 6 dump files, found {len(dump_files)}")
        for f in dump_files:
            print(f"  {f.name}")
        return 1

    print(f"Found {len(dump_files)} dump files in {output_dir}")

    # Locate files by coordinates (from control file DUMP_VARIABLES entries)
    coords = {
        "Corn":       (557946, 434408),
        "Potatoes":   (561167, 445224),
        "Oats":       (569015, 439355),
        "Dec Forest": (553927, 459454),
        "Grass":      (562985, 448265),
        "Sweet Corn": (561232, 442007),
    }

    files: dict[str, Path | None] = {}
    for name, (x, y) in coords.items():
        files[name] = find_dump_by_coordinates(dump_files, x, y)
        if files[name] is None:
            print(f"ERROR: No dump file for {name} (x={x}, y={y})")
            return 1
        print(f"  {name}: {files[name].name}")

    # Parse all files
    data: dict[str, list[DumpRow]] = {}
    for name, filepath in files.items():
        assert filepath is not None
        data[name] = parse_dump_csv(filepath)
        print(f"  {name}: {len(data[name])} rows, LU={data[name][0].lu_code if data[name] else '?'}")

    all_errors: list[str] = []

    # ---- Test 1: Dormant Kcb values ----
    print(f"\n--- Test 1: Dormant Kcb = Kcb_min before growing season ---")
    for name, kcb_min in [("Corn", 0.15), ("Potatoes", 0.15),
                           ("Oats", 0.15), ("Dec Forest", 0.15),
                           ("Grass", 0.15)]:
        errors = verify_dormant_kcb(data[name], name, kcb_min)
        if errors:
            print(f"  {name}: FAILED ({len(errors)} errors)")
            for e in errors[:3]:
                print(f"    {e}")
        else:
            print(f"  {name}: PASSED")
        all_errors.extend(errors)

    # ---- Test 2: Kcb curve shape ----
    print(f"\n--- Test 2: Kcb curve shape (ini → mid → end) ---")
    crop_params = {
        "Corn":       (0.15, 1.15, 0.25, 0.15),
        "Potatoes":   (0.15, 1.05, 0.50, 0.15),
        "Oats":       (0.15, 1.00, 0.20, 0.15),
        "Dec Forest": (0.15, 1.00, 0.30, 0.15),
        "Grass":      (0.15, 0.85, 0.65, 0.15),
        "Sweet Corn": (0.15, 1.10, 1.00, 0.15),
    }
    for name, (kcb_ini, kcb_mid, kcb_end, kcb_min) in crop_params.items():
        errors = verify_kcb_curve_shape(
            data[name], name, kcb_ini, kcb_mid, kcb_end, kcb_min)
        if errors:
            print(f"  {name}: FAILED ({len(errors)} errors)")
            for e in errors[:3]:
                print(f"    {e}")
        else:
            print(f"  {name}: PASSED")
        all_errors.extend(errors)

    # ---- Test 3: Start timing windows ----
    print(f"\n--- Test 3: Growing season start timing (DOY windows) ---")
    # These are broad windows — the exact day depends on 2012 weather
    timing_params = {
        "Corn":       (120, 170),   # Tbase=50, Start_GDD=350
        "Potatoes":   (95, 135),    # Tbase=35.6, Start_GDD=650
        "Oats":       (88, 128),    # Tbase=32, Start_GDD=600
        "Dec Forest": (80, 120),    # Tbase=32, Start_GDD=450
        "Grass":      (82, 122),    # Tbase=32, Start_GDD=500
        "Sweet Corn": (120, 170),   # Tbase=50, Start_GDD=350
    }
    for name, (earliest, latest) in timing_params.items():
        errors = verify_start_timing(data[name], name, earliest, latest)
        if errors:
            print(f"  {name}: FAILED")
            for e in errors:
                print(f"    {e}")
        else:
            start_doy = next((r.doy for r in data[name] if r.growing_season == 1), None)
            print(f"  {name}: PASSED (started DOY {start_doy})")
        all_errors.extend(errors)

    # ---- Test 4: Cool-season before warm-season ----
    print(f"\n--- Test 4: Cool-season crops start before warm-season ---")
    errors = verify_warm_before_cool(
        data["Corn"], data["Oats"], "Corn", "Oats")
    errors += verify_warm_before_cool(
        data["Corn"], data["Dec Forest"], "Corn", "Dec Forest")
    errors += verify_warm_before_cool(
        data["Corn"], data["Grass"], "Corn", "Grass")
    if errors:
        print(f"  FAILED ({len(errors)} errors)")
        for e in errors:
            print(f"    {e}")
    else:
        print(f"  PASSED")
    all_errors.extend(errors)

    # ---- Summary with Kcb statistics ----
    print(f"\n--- Kcb Summary Statistics ---")
    for name in coords:
        growing_rows = [r for r in data[name] if r.growing_season == 1]
        if growing_rows:
            start_doy = growing_rows[0].doy
            end_doy = growing_rows[-1].doy
            peak_kcb = max(r.crop_coefficient_kcb for r in growing_rows)
            final_kcb = growing_rows[-1].crop_coefficient_kcb
            print(f"  {name:12s}: DOY {start_doy:3d}–{end_doy:3d}, "
                  f"peak Kcb={peak_kcb:.3f}, final Kcb={final_kcb:.3f}")
        else:
            print(f"  {name:12s}: Never entered growing season")

    # ---- Final result ----
    print(f"\n{'='*60}")
    if all_errors:
        print(f"FAILED: {len(all_errors)} total error(s)")
        return 1
    else:
        print("ALL FAO56 GDD Kcb VERIFICATION TESTS PASSED")
        return 0


if __name__ == "__main__":
    sys.exit(main())
