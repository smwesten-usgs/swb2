"""Look at DOY 50-100 for Rye to debug frost issue."""
import csv
from pathlib import Path
from datetime import date

f = Path(r"output/SWB2_variable_values__col_9__row_277__x_546094__y_438492.csv")
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
        m, d, y = int(row["month"]), int(row["day"]), int(row["year"])
        doy = date(y, m, d).timetuple().tm_yday
        if 50 <= doy <= 100:
            gdd = float(row["gdd"])
            gs = row["growing_season"]
            tmin = float(row["tmin"])
            tmean = float(row["tmean"])
            kcb = float(row["crop_coefficient_kcb"])
            print(f"DOY {doy:3d} | GDD={gdd:6.1f} | GS={gs} | tmin={tmin:5.1f} | tmean={tmean:5.1f} | Kcb={kcb:.4f}")
