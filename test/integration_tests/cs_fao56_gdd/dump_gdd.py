"""Extract GDD accumulation curves from dump files for calibration."""
import csv
from pathlib import Path
from datetime import date

output_dir = Path("output")
files = sorted(output_dir.glob("SWB2_variable_values__*.csv"))
target_doys = [1, 32, 60, 70, 75, 80, 85, 90, 95, 100, 110, 120, 130, 140, 150, 160, 180, 200, 220, 240, 260, 280, 300, 330]

for f in files:
    rows = []
    headers = None
    with open(f) as fh:
        reader = csv.reader(fh)
        for line in reader:
            if not line:
                continue
            fields = [x.strip() for x in line]
            if fields[0].startswith("#"):
                continue
            if headers is None and not fields[0][:4].isdigit():
                headers = [h.lower().strip() for h in fields]
                continue
            if headers is None or not fields[0][:4].isdigit():
                continue
            row = {h: fields[i] for i, h in enumerate(headers)}
            rows.append(row)

    lu = rows[0]["landuse_code"] if rows else "?"
    print(f"\n=== {f.stem.split('__')[1:]} (LU {lu}) ===")
    print(f"  DOY |    GDD | GS | Kcb    | Tmean")
    print(f"  ----|--------|----|---------|---------")
    for r in rows:
        m = int(r["month"])
        d = int(r["day"])
        y = int(r["year"])
        doy = date(y, m, d).timetuple().tm_yday
        if doy in target_doys:
            gdd = float(r["gdd"])
            gs = r["growing_season"]
            kcb = float(r["crop_coefficient_kcb"])
            tmean = float(r["tmean"])
            print(f"  {doy:3d} | {gdd:6.1f} | {gs:>2s} | {kcb:.4f} | {tmean:5.1f}")
