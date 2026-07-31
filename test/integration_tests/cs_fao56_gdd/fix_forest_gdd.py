"""Fix forest/shrub GDD stage values — were in C-deg-days, need F-deg-days (x1.8)."""
from pathlib import Path

tsv = Path(r"Landuse_lookup_FAO56_GDD.tsv")
lines = tsv.read_text().splitlines()

# Forest/shrub LU codes that used Radtke et al. estimates in C-deg-days
forest_lus = {70, 141, 142, 143, 151, 152, 190, 195, 242, 250}

# Their current values (in C-deg-days, need x1.8):
# GDD_ini=100, GDD_dev=800, GDD_mid=700, GDD_late=700
# Shrubs have GDD_ini=100, GDD_dev=600, GDD_mid=800, GDD_late=600

header_found = False
gdd_ini_col = None
gdd_dev_col = None
gdd_mid_col = None
gdd_late_col = None
output_lines = []

for line in lines:
    if line.startswith("#") or line.strip() == "":
        output_lines.append(line)
        continue
    parts = line.split("\t")
    if not header_found:
        header_found = True
        gdd_ini_col = parts.index("GDD_ini")
        gdd_dev_col = parts.index("GDD_dev")
        gdd_mid_col = parts.index("GDD_mid")
        gdd_late_col = parts.index("GDD_late")
        output_lines.append(line)
        continue

    try:
        lu_code = int(parts[0])
    except ValueError:
        output_lines.append(line)
        continue

    if lu_code in forest_lus:
        old_ini = float(parts[gdd_ini_col])
        old_dev = float(parts[gdd_dev_col])
        old_mid = float(parts[gdd_mid_col])
        old_late = float(parts[gdd_late_col])
        new_ini = int(old_ini * 1.8)
        new_dev = int(old_dev * 1.8)
        new_mid = int(old_mid * 1.8)
        new_late = int(old_late * 1.8)
        parts[gdd_ini_col] = str(new_ini)
        parts[gdd_dev_col] = str(new_dev)
        parts[gdd_mid_col] = str(new_mid)
        parts[gdd_late_col] = str(new_late)
        print(f"LU {lu_code:3d}: ini {old_ini:.0f}->{new_ini}, dev {old_dev:.0f}->{new_dev}, "
              f"mid {old_mid:.0f}->{new_mid}, late {old_late:.0f}->{new_late}, "
              f"total {old_ini+old_dev+old_mid+old_late:.0f}->{new_ini+new_dev+new_mid+new_late}")

    output_lines.append("\t".join(parts))

tsv.write_text("\n".join(output_lines) + "\n")
print("\nDone. Forest/shrub GDD stages converted from C-deg-days to F-deg-days.")
