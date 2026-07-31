"""Query bounds of Central Sands NetCDF files."""
import netCDF4 as nc
from pathlib import Path

f = Path(r"E:\projects\swb_development\git\swb2\test\test_data\cs\tmax_Daymet_v3_2012.nc")
ds = nc.Dataset(f)
print("Variables:", list(ds.variables.keys()))
print()

for var_name in ds.variables:
    v = ds.variables[var_name]
    if hasattr(v, "grid_mapping_name"):
        print(f"Projection ({var_name}):")
        for attr in v.ncattrs():
            print(f"  {attr}: {v.getncattr(attr)}")
        print()

for var_name in ["x", "y", "lat", "lon"]:
    if var_name in ds.variables:
        v = ds.variables[var_name]
        data = v[:]
        units = getattr(v, "units", "?")
        print(f"{var_name}: shape={v.shape}, min={data.min():.4f}, max={data.max():.4f}, units={units}")

# Compute lat/lon corners from x/y if projected
if "x" in ds.variables and "y" in ds.variables:
    x = ds.variables["x"][:]
    y = ds.variables["y"][:]
    print(f"\nProjected bounds (Daymet LCC):")
    print(f"  x: {x.min():.0f} to {x.max():.0f} m")
    print(f"  y: {y.min():.0f} to {y.max():.0f} m")

    # Convert to lat/lon using pyproj if available
    try:
        from pyproj import Transformer
        # Daymet LCC projection
        daymet_proj = "+proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m"
        transformer = Transformer.from_crs(daymet_proj, "EPSG:4326", always_xy=True)

        # Four corners
        corners_x = [x.min(), x.max(), x.min(), x.max()]
        corners_y = [y.min(), y.min(), y.max(), y.max()]
        lons, lats = transformer.transform(corners_x, corners_y)

        print(f"\nLat/Lon bounds:")
        print(f"  Latitude:  {min(lats):.4f} to {max(lats):.4f}")
        print(f"  Longitude: {min(lons):.4f} to {max(lons):.4f}")
        print(f"\nCorners (lon, lat):")
        labels = ["SW", "SE", "NW", "NE"]
        for i, label in enumerate(labels):
            print(f"  {label}: ({lons[i]:.4f}, {lats[i]:.4f})")
    except ImportError:
        print("\n  (pyproj not available for lat/lon conversion)")

ds.close()
