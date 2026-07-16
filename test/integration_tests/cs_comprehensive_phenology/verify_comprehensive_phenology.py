"""Verify comprehensive phenology transitions in SWB2 DUMP_VARIABLES output.

Exercises all 5 phenology methods:
  1. PHENOLOGY_NONE      — LU 131 (Barren): always dormant
  2. DOY_BASED           — LU 141 (Deciduous Forest): growing DOY 105–288
  3. GDD_THRESHOLD       — LU   1 (Corn): GDD>=200 until frost<=28F
  4. FAO56_DATES         — LU   5 (Soybeans): start=05/20, L=20/30/60/25
  5. FAO56_GDD           — LU  12 (Sweet Corn): start_GDD=100, stages by GDD

Usage:
    python verify_comprehensive_phenology.py <output_directory>
"""

from pathlib import Path
import sys
import csv
from datetime import date


def parse_dump_csv(filepath: Path) -> list[dict]:
    """Parse a DUMP_VARIABLES CSV file into a list of row dictionaries."""
    rows: list[dict] = []
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
                headers = [h.lower() for h in fields]
                continue
            if headers is None:
                continue
            if not fields[0][:4].isdigit():
                continue

            row_dict = {h: fields[i] if i < len(fields) else ""
                        for i, h in enumerate(headers)}

            try:
                month = int(row_dict["month"])
                day = int(row_dict["day"])
                year = int(row_dict["year"])
                rows.append({
                    "date": row_dict.get("date", fields[0]),
                    "month": month,
                    "day": day,
                    "year": year,
                    "doy": date(year, month, day).timetuple().tm_yday,
                    "lu_code": int(row_dict["landuse_code"]),
                    "growing_season": int(row_dict["growing_season"]),
                    "gdd": float(row_dict["gdd"]),
                    "tmean": float(row_dict["tmean"]),
                    "interception_storage_max": float(row_dict["interception_storage_max"]),
                })
            except (KeyError, ValueError) as e:
                print(f"  WARNING: Could not parse row in {filepath.name}: {e}")
                continue

    return rows


# =============================================================================
# Test 1: PHENOLOGY_NONE — always dormant
# =============================================================================
def verify_none(rows: list[dict], label: str) -> list[str]:
    """Verify PHENOLOGY_NONE: always dormant, interception_storage_max at nongrowing value."""
    errors: list[str] = []
    if not rows:
        return [f"{label}: No data rows"]

    if rows[0]["lu_code"] != 131:
        return [f"{label}: Expected LU 131, got {rows[0]['lu_code']}"]

    for row in rows:
        if row["growing_season"] != 0:
            errors.append(f"{label}: DOY {row['doy']}: growing_season should be 0, got {row['growing_season']}")
        # Interception storage max should be at the nongrowing value (0.01)
        expected_max = 0.01
        if abs(row["interception_storage_max"] - expected_max) > 0.005:
            errors.append(
                f"{label}: DOY {row['doy']}: interception_storage_max={row['interception_storage_max']:.4f}, "
                f"expected {expected_max}")

    return errors


# =============================================================================
# Test 2: DOY_BASED — binary on/off at fixed dates
# =============================================================================
def verify_doy_based(rows: list[dict], label: str,
                     expected_lu: int, start_doy: int, end_doy: int,
                     storage_growing: float, storage_nongrowing: float) -> list[str]:
    """Verify DOY_BASED: growing within [start, end], dormant otherwise."""
    errors: list[str] = []
    if not rows:
        return [f"{label}: No data rows"]
    if rows[0]["lu_code"] != expected_lu:
        return [f"{label}: Expected LU {expected_lu}, got {rows[0]['lu_code']}"]

    winter_crop = start_doy > end_doy

    for row in rows:
        doy = row["doy"]
        if winter_crop:
            expected_growing = (doy >= start_doy) or (doy <= end_doy)
        else:
            expected_growing = (doy >= start_doy) and (doy <= end_doy)

        expected_gs = 1 if expected_growing else 0
        if row["growing_season"] != expected_gs:
            errors.append(
                f"{label}: DOY {doy}: growing_season={row['growing_season']}, expected {expected_gs}")

        # Verify interception_storage_max snaps between values
        expected_max = storage_growing if expected_growing else storage_nongrowing
        if abs(row["interception_storage_max"] - expected_max) > 0.005:
            errors.append(
                f"{label}: DOY {doy}: interception_storage_max={row['interception_storage_max']:.4f}, "
                f"expected {expected_max:.4f}")

    return errors


# =============================================================================
# Test 3: GDD_THRESHOLD — binary, GDD-driven with frost latch
# =============================================================================
def verify_gdd_threshold(rows: list[dict], label: str,
                         expected_lu: int, gdd_threshold: float,
                         killing_frost: float,
                         storage_growing: float, storage_nongrowing: float) -> list[str]:
    """Verify GDD_THRESHOLD: starts at GDD threshold, ends on killing frost."""
    errors: list[str] = []
    if not rows:
        return [f"{label}: No data rows"]
    if rows[0]["lu_code"] != expected_lu:
        return [f"{label}: Expected LU {expected_lu}, got {rows[0]['lu_code']}"]

    prev_gs = 0
    frost_killed = False

    for row in rows:
        gs = row["growing_season"]
        gdd = row["gdd"]
        tmean = row["tmean"]

        if frost_killed:
            expected_gs = 0
        elif prev_gs == 1:
            if tmean <= killing_frost:
                expected_gs = 0
                frost_killed = True
            else:
                expected_gs = 1
        else:
            if gdd >= gdd_threshold and tmean > killing_frost:
                expected_gs = 1
            else:
                expected_gs = 0

        if gs != expected_gs:
            errors.append(
                f"{label}: DOY {row['doy']}: growing_season={gs}, expected {expected_gs} "
                f"(GDD={gdd:.1f}, tmean={tmean:.1f}, frost_killed={frost_killed})")

        # Verify storage_max binary switch
        expected_max = storage_growing if (gs == 1) else storage_nongrowing
        if abs(row["interception_storage_max"] - expected_max) > 0.005:
            errors.append(
                f"{label}: DOY {row['doy']}: interception_storage_max={row['interception_storage_max']:.4f}, "
                f"expected {expected_max:.4f}")

        prev_gs = gs

    return errors


# =============================================================================
# Test 4: FAO56_DATES — continuous growth_fraction, verified via interception ramp
# =============================================================================
def verify_fao56_dates(rows: list[dict], label: str,
                       expected_lu: int, start_doy: int,
                       l_ini: int, l_dev: int, l_mid: int, l_late: int,
                       storage_growing: float, storage_nongrowing: float) -> list[str]:
    """Verify FAO56_DATES: growing_season on during stages, interception ramps smoothly."""
    errors: list[str] = []
    if not rows:
        return [f"{label}: No data rows"]
    if rows[0]["lu_code"] != expected_lu:
        return [f"{label}: Expected LU {expected_lu}, got {rows[0]['lu_code']}"]

    total_season = l_ini + l_dev + l_mid + l_late

    # Track transitions
    saw_growing = False
    saw_dormant_after_growing = False
    saw_intermediate_storage = False  # storage_max between nongrowing and growing

    for row in rows:
        doy = row["doy"]
        gs = row["growing_season"]
        storage_max = row["interception_storage_max"]

        # Days since planting
        days_since = doy - start_doy
        if days_since < 0:
            days_since += 365  # year wrap

        in_season = (0 <= days_since < total_season)

        # growing_season flag should match
        expected_gs = 1 if in_season else 0
        if gs != expected_gs:
            errors.append(
                f"{label}: DOY {doy}: growing_season={gs}, expected {expected_gs} "
                f"(days_since_planting={days_since})")

        if gs == 1:
            saw_growing = True
        elif saw_growing:
            saw_dormant_after_growing = True

        # Interception storage_max: should be between nongrowing and growing
        if storage_max < storage_nongrowing - 0.005:
            errors.append(
                f"{label}: DOY {doy}: interception_storage_max={storage_max:.4f} "
                f"below nongrowing={storage_nongrowing}")
        if storage_max > storage_growing + 0.005:
            errors.append(
                f"{label}: DOY {doy}: interception_storage_max={storage_max:.4f} "
                f"above growing={storage_growing}")

        # Check for intermediate values (proof of smooth ramp during DEV stage)
        if (storage_max > storage_nongrowing + 0.01
                and storage_max < storage_growing - 0.01):
            saw_intermediate_storage = True

    # Structural checks
    if not saw_growing:
        errors.append(f"{label}: Never entered growing season")
    if not saw_dormant_after_growing:
        errors.append(f"{label}: Never returned to dormancy after growing season")
    if not saw_intermediate_storage:
        errors.append(
            f"{label}: Never saw intermediate interception_storage_max "
            f"(expected smooth ramp during INI+DEV stages)")

    return errors


# =============================================================================
# Test 5: FAO56_GDD — continuous, GDD-driven with frost latch
# =============================================================================
def verify_fao56_gdd(rows: list[dict], label: str,
                     expected_lu: int, start_gdd: float,
                     killing_frost: float,
                     storage_growing: float, storage_nongrowing: float) -> list[str]:
    """Verify FAO56_GDD: GDD-driven stages with frost termination, smooth interception ramp."""
    errors: list[str] = []
    if not rows:
        return [f"{label}: No data rows"]
    if rows[0]["lu_code"] != expected_lu:
        return [f"{label}: Expected LU {expected_lu}, got {rows[0]['lu_code']}"]

    saw_growing = False
    saw_dormant_after_growing = False
    saw_intermediate_storage = False

    for row in rows:
        gs = row["growing_season"]
        gdd = row["gdd"]
        storage_max = row["interception_storage_max"]

        if gs == 1:
            saw_growing = True
            # GDD must be above start threshold
            if gdd < start_gdd - 1.0:
                errors.append(
                    f"{label}: DOY {row['doy']}: growing_season=1 but GDD={gdd:.1f} "
                    f"< start_gdd={start_gdd}")
        elif saw_growing and not saw_dormant_after_growing:
            saw_dormant_after_growing = True

        # Interception storage_max bounds check
        if storage_max < storage_nongrowing - 0.005:
            errors.append(
                f"{label}: DOY {row['doy']}: interception_storage_max={storage_max:.4f} "
                f"below nongrowing={storage_nongrowing}")
        if storage_max > storage_growing + 0.005:
            errors.append(
                f"{label}: DOY {row['doy']}: interception_storage_max={storage_max:.4f} "
                f"above growing={storage_growing}")

        # Check for intermediate values (smooth ramp)
        if (storage_max > storage_nongrowing + 0.01
                and storage_max < storage_growing - 0.01):
            saw_intermediate_storage = True

    # Structural checks
    if not saw_growing:
        errors.append(f"{label}: Never entered growing season (start_gdd={start_gdd})")
    if not saw_dormant_after_growing:
        errors.append(f"{label}: Never returned to dormancy (frost or season end)")
    if not saw_intermediate_storage:
        errors.append(
            f"{label}: Never saw intermediate interception_storage_max "
            f"(expected smooth ramp during INI+DEV stages)")

    return errors


# =============================================================================
# Main
# =============================================================================
def find_dump_by_coordinates(dump_files: list[Path], x: int, y: int) -> Path | None:
    """Find the dump file matching specific coordinates."""
    for f in dump_files:
        if f"x_{x}" in f.name and f"y_{y}" in f.name:
            return f
    return None


def main() -> int:
    """Run comprehensive phenology verification."""
    output_dir = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("output")

    if not output_dir.exists():
        print(f"ERROR: Output directory '{output_dir}' not found.")
        return 1

    dump_files = sorted(output_dir.glob("SWB2_variable_values__*.csv"))
    if len(dump_files) < 5:
        print(f"ERROR: Expected 5 dump files, found {len(dump_files)}")
        for f in dump_files:
            print(f"  {f.name}")
        return 1

    print(f"Found {len(dump_files)} dump files in {output_dir}")

    # Locate files by coordinates
    coords = {
        "Barren": (567261, 447588),
        "Forest": (553927, 459454),
        "Corn": (557946, 434408),
        "Soybeans": (560932, 443024),
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
    data: dict[str, list[dict]] = {}
    for name, filepath in files.items():
        data[name] = parse_dump_csv(filepath)
        print(f"  {name}: {len(data[name])} rows parsed")

    all_errors: list[str] = []

    # --- Test 1: Barren (PHENOLOGY_NONE) ---
    print(f"\n--- Test 1: Barren (LU 131) — PHENOLOGY_NONE ---")
    print(f"  Expected: always dormant, interception_storage_max = 0.01")
    errors = verify_none(data["Barren"], "Barren")
    if errors:
        print(f"  FAILED: {len(errors)} error(s)")
        for e in errors[:5]:
            print(f"    {e}")
        if len(errors) > 5:
            print(f"    ... and {len(errors) - 5} more")
    else:
        print("  PASSED")
    all_errors.extend(errors)

    # --- Test 2: Deciduous Forest (DOY_BASED) ---
    print(f"\n--- Test 2: Deciduous Forest (LU 141) — DOY_BASED ---")
    print(f"  Expected: growing DOY 105–288, storage_max=0.20/0.05")
    errors = verify_doy_based(data["Forest"], "Deciduous Forest",
                              expected_lu=141, start_doy=105, end_doy=288,
                              storage_growing=0.20, storage_nongrowing=0.05)
    if errors:
        print(f"  FAILED: {len(errors)} error(s)")
        for e in errors[:5]:
            print(f"    {e}")
        if len(errors) > 5:
            print(f"    ... and {len(errors) - 5} more")
    else:
        print("  PASSED")
    all_errors.extend(errors)

    # --- Test 3: Corn (GDD_THRESHOLD) ---
    print(f"\n--- Test 3: Corn (LU 1) — GDD_THRESHOLD ---")
    print(f"  Expected: growing when GDD>=200 until tmean<=28F, storage_max=0.15/0.03")
    errors = verify_gdd_threshold(data["Corn"], "Corn",
                                  expected_lu=1, gdd_threshold=200.0,
                                  killing_frost=28.0,
                                  storage_growing=0.15, storage_nongrowing=0.03)
    if errors:
        print(f"  FAILED: {len(errors)} error(s)")
        for e in errors[:5]:
            print(f"    {e}")
        if len(errors) > 5:
            print(f"    ... and {len(errors) - 5} more")
    else:
        print("  PASSED")
    all_errors.extend(errors)

    # --- Test 4: Soybeans (FAO56_DATES) ---
    print(f"\n--- Test 4: Soybeans (LU 5) — FAO56_DATES ---")
    print(f"  Expected: start=05/20 (DOY 140), L=20/30/60/25 days, smooth interception ramp")
    errors = verify_fao56_dates(data["Soybeans"], "Soybeans",
                                expected_lu=5, start_doy=140,
                                l_ini=20, l_dev=30, l_mid=60, l_late=25,
                                storage_growing=0.15, storage_nongrowing=0.03)
    if errors:
        print(f"  FAILED: {len(errors)} error(s)")
        for e in errors[:5]:
            print(f"    {e}")
        if len(errors) > 5:
            print(f"    ... and {len(errors) - 5} more")
    else:
        print("  PASSED")
    all_errors.extend(errors)

    # --- Test 5: Sweet Corn (FAO56_GDD) ---
    print(f"\n--- Test 5: Sweet Corn (LU 12) — FAO56_GDD ---")
    print(f"  Expected: start_GDD=100, stages by GDD thresholds, frost termination, smooth ramp")
    errors = verify_fao56_gdd(data["Sweet Corn"], "Sweet Corn",
                              expected_lu=12, start_gdd=100.0,
                              killing_frost=28.0,
                              storage_growing=0.15, storage_nongrowing=0.03)
    if errors:
        print(f"  FAILED: {len(errors)} error(s)")
        for e in errors[:5]:
            print(f"    {e}")
        if len(errors) > 5:
            print(f"    ... and {len(errors) - 5} more")
    else:
        print("  PASSED")
    all_errors.extend(errors)

    # --- Summary ---
    print(f"\n{'='*60}")
    if all_errors:
        print(f"FAILED: {len(all_errors)} total error(s)")
        return 1
    else:
        print("ALL 5 PHENOLOGY METHODS VERIFIED — comprehensive test passed")
        return 0


if __name__ == "__main__":
    sys.exit(main())
