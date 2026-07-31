"""Update Growing_season_start_GDD values based on 2012 calibration."""
from pathlib import Path

tsv = Path(r"Landuse_lookup_FAO56_GDD.tsv")
lines = tsv.read_text().splitlines()

# Define new start_GDD by LU code based on 2012 GDD accumulation
new_start_gdd = {
    # Warm-season (Tbase=50): target DOY ~130, GDD(50)@DOY130 ≈ 350
    1: 350.0,    # Corn
    4: 350.0,    # Sorghum
    5: 350.0,    # Soybeans
    12: 350.0,   # Sweet Corn
    29: 350.0,   # Millet
    42: 350.0,   # Dry Beans
    47: 350.0,   # Misc Vegs
    50: 350.0,   # Cucumbers
    57: 350.0,   # Herbs
    182: 350.0,  # Cultivated Crops
    225: 350.0,  # Dbl Crop WinWht/Corn
    226: 350.0,  # Dbl Crop Oats/Corn
    241: 350.0,  # Dbl Crop Corn/Soybeans
    # Sunflower (Tbase=46.4): slightly earlier
    6: 400.0,    # Sunflower
    # Moderate Tbase (35-46): target DOY ~110, GDD(35.6)@DOY110 ≈ 650
    43: 650.0,   # Potatoes
    49: 650.0,   # Onions
    41: 650.0,   # Sugarbeets
    206: 650.0,  # Carrots
    207: 650.0,  # Asparagus
    221: 650.0,  # Strawberries
    243: 650.0,  # Cabbage
    38: 650.0,   # Camelina (Tbase=32, but spring-planted)
    # Cool-season cereals (Tbase=32): target DOY ~100, GDD(32)@DOY100 ≈ 680
    21: 600.0,   # Barley
    23: 600.0,   # Spring Wheat
    28: 600.0,   # Oats
    27: 600.0,   # Rye
    205: 600.0,  # Triticale
    30: 600.0,   # Speltz
    32: 600.0,   # Flaxseed
    # Winter crops (Tbase=32): resume growth earlier in spring
    24: 400.0,   # Winter Wheat
    26: 400.0,   # Dbl Crop WinWht/Soybeans
    # Grasses/forages (Tbase=32): green up DOY ~95, GDD(32)@DOY95 ≈ 600
    36: 500.0,   # Alfalfa
    37: 500.0,   # Other Hay
    58: 500.0,   # Clover/Wildflowers
    59: 500.0,   # Sod/Grass Seed
    171: 500.0,  # Grassland/Herbaceous
    176: 500.0,  # Grass/Pasture
    181: 500.0,  # Pasture/Hay
    53: 500.0,   # Peas
    # Forests (Tbase=32): leaf-out DOY ~100-110, GDD(32)@DOY100 ≈ 680
    141: 450.0,  # Deciduous Forest
    142: 350.0,  # Evergreen (earlier)
    143: 400.0,  # Mixed Forest
    151: 450.0,  # Dwarf Scrub
    152: 450.0,  # Shrubland
    190: 400.0,  # Woody Wetlands
    195: 400.0,  # Herbaceous Wetlands
    # Developed (Tbase=32): grass/trees green up
    121: 500.0,  # Developed/Open Space
    122: 500.0,  # Developed/Low Intensity
    123: 500.0,  # Developed/Medium Intensity
    124: 500.0,  # Developed/High Intensity
    # Berries/fruits
    242: 450.0,  # Blueberries
    250: 450.0,  # Cranberries
    70: 400.0,   # Christmas Trees
    # Fallow/waste
    61: 500.0,   # Fallow/Idle Cropland
    252: 500.0,  # Waste disposal grass
}

# Process file
header_found = False
start_gdd_col = None
output_lines = []

for line in lines:
    if line.startswith("#") or line.strip() == "":
        output_lines.append(line)
        continue
    parts = line.split("\t")
    if not header_found:
        header_found = True
        for j, col in enumerate(parts):
            if col == "Growing_season_start_GDD":
                start_gdd_col = j
                break
        output_lines.append(line)
        continue

    try:
        lu_code = int(parts[0])
    except ValueError:
        output_lines.append(line)
        continue

    if lu_code in new_start_gdd and start_gdd_col is not None:
        parts[start_gdd_col] = f"{new_start_gdd[lu_code]:.1f}"
    output_lines.append("\t".join(parts))

tsv.write_text("\n".join(output_lines) + "\n")
print("Updated Growing_season_start_GDD values.")
print(f"\nUpdated {len(new_start_gdd)} land uses.")
