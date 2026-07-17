"""Plot Kcb curves from SWB2 DUMP_VARIABLES CSV files.

Generates two types of figures:
  1. Overlay figure: All years overlaid on the same DOY axis per panel,
     showing year-to-year variation in GDD-driven Kcb timing.
  2. Individual year figures: One figure per year with all points shown,
     including GDD on a secondary axis.

Usage:
    python plot_kcb_curves.py [output_directory]

Output:
    kcb_curves_overlay.png  — all years overlaid per DUMP point
    kcb_curves_YYYY.png     — one figure per year with GDD axis
"""

import csv
from pathlib import Path
from datetime import date
import sys
from collections import defaultdict

try:
    import matplotlib.pyplot as plt
    import matplotlib.cm as cm
except ImportError:
    print("ERROR: matplotlib is required.")
    print("  Run with: mamba run -n py313 python plot_kcb_curves.py output")
    sys.exit(1)


def parse_dump_csv(filepath: Path) -> dict:
    """Parse a DUMP_VARIABLES CSV into per-year data.

    Returns:
        Dictionary with keys:
            lu_code: int
            filename: str
            years: dict mapping year -> {doy, gdd, kcb, gs, tmean}
    """
    headers: list[str] | None = None
    lu_code: int = 0
    years: dict[int, dict] = defaultdict(lambda: {
        "doy": [], "gdd": [], "kcb": [], "gs": [], "tmean": []
    })

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
            if headers is None or not fields[0][:4].isdigit():
                continue

            row = {h: fields[i] for i, h in enumerate(headers)}
            try:
                m = int(row["month"])
                d = int(row["day"])
                y = int(row["year"])
                doy = date(y, m, d).timetuple().tm_yday
                lu_code = int(row["landuse_code"])
                years[y]["doy"].append(doy)
                years[y]["gdd"].append(float(row["gdd"]))
                years[y]["kcb"].append(float(row["crop_coefficient_kcb"]))
                years[y]["gs"].append(int(row["growing_season"]))
                years[y]["tmean"].append(float(row["tmean"]))
            except (KeyError, ValueError):
                continue

    return {"lu_code": lu_code, "years": dict(years), "filename": filepath.stem}


def setup_month_axis(ax) -> None:
    """Configure x-axis with month abbreviation labels."""
    month_starts = [1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335]
    month_names = ["J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D"]
    ax.set_xticks(month_starts)
    ax.set_xticklabels(month_names, fontsize=7)
    ax.set_xlim(1, 365)


def plot_overlay(datasets: list[dict], year_list: list[int]) -> Path:
    """Create overlay figure: all years as colored traces per panel.

    Args:
        datasets: List of parsed dump data (one per DUMP_VARIABLES point).
        year_list: Sorted list of all years in the data.

    Returns:
        Path to saved figure.
    """
    n_files = len(datasets)
    n_years = len(year_list)
    n_cols = min(2, n_files)
    n_rows = (n_files + n_cols - 1) // n_cols

    # Color map for years
    if n_years == 1:
        year_colors = {year_list[0]: "steelblue"}
    else:
        cmap = cm.get_cmap("tab10", n_years)
        year_colors = {yr: cmap(i) for i, yr in enumerate(year_list)}

    fig, axes = plt.subplots(n_rows, n_cols, figsize=(7 * n_cols, 3.5 * n_rows),
                             squeeze=False)

    title_years = f"{year_list[0]}" if n_years == 1 else f"{year_list[0]}–{year_list[-1]}"
    fig.suptitle(f"FAO56 GDD-Based Kcb Curves — Central Sands {title_years} (overlay)",
                 fontsize=13, y=0.995)

    for idx, data in enumerate(datasets):
        row_idx = idx // n_cols
        col_idx = idx % n_cols
        ax = axes[row_idx, col_idx]
        lu = data["lu_code"]
        peak_kcb_all = 0.0

        for yr in sorted(data["years"].keys()):
            yr_data = data["years"][yr]
            doys = yr_data["doy"]
            kcbs = yr_data["kcb"]
            gs = yr_data["gs"]
            color = year_colors[yr]

            if not doys:
                continue

            peak_kcb_all = max(peak_kcb_all, max(kcbs))

            label = str(yr) if n_years > 1 else "Kcb"
            ax.plot(doys, kcbs, "-", color=color, linewidth=1.2, alpha=0.85,
                    label=label)

            # Season boundary markers
            gs_start = next((doys[i] for i in range(len(gs)) if gs[i] == 1), None)
            gs_end = next((doys[i] for i in range(len(gs) - 1, -1, -1) if gs[i] == 1), None)
            if gs_start is not None:
                ax.axvline(gs_start, color=color, linestyle=":", alpha=0.4, linewidth=0.7)
            if gs_end is not None:
                ax.axvline(gs_end, color=color, linestyle=":", alpha=0.4, linewidth=0.7)

        ax.set_title(f"LU {lu}", fontsize=10)
        ax.set_xlabel("Month", fontsize=8)
        ax.set_ylabel("Kcb", fontsize=9)
        ax.tick_params(axis="both", labelsize=7)
        ax.set_ylim(0, max(peak_kcb_all * 1.15, 0.3))
        ax.grid(True, alpha=0.3)
        setup_month_axis(ax)

        if n_years > 1:
            ax.legend(fontsize=6, loc="upper right", ncol=min(n_years, 5),
                      framealpha=0.7)

    # Hide unused axes
    for idx in range(n_files, n_rows * n_cols):
        axes[idx // n_cols, idx % n_cols].set_visible(False)

    plt.tight_layout()
    out_path = Path("kcb_curves_overlay.png")
    plt.savefig(out_path, dpi=150, bbox_inches="tight")
    plt.close()
    return out_path


def plot_single_year(datasets: list[dict], year: int) -> Path:
    """Create a figure for a single year with GDD on secondary axis.

    Args:
        datasets: List of parsed dump data.
        year: The year to plot.

    Returns:
        Path to saved figure.
    """
    n_files = len(datasets)
    n_cols = min(2, n_files)
    n_rows = (n_files + n_cols - 1) // n_cols

    fig, axes = plt.subplots(n_rows, n_cols, figsize=(7 * n_cols, 3.5 * n_rows),
                             squeeze=False)
    fig.suptitle(f"FAO56 GDD-Based Kcb Curves — Central Sands {year}",
                 fontsize=13, y=0.995)

    for idx, data in enumerate(datasets):
        row_idx = idx // n_cols
        col_idx = idx % n_cols
        ax = axes[row_idx, col_idx]
        lu = data["lu_code"]

        if year not in data["years"]:
            ax.set_title(f"LU {lu} — no data for {year}", fontsize=10)
            ax.set_visible(False)
            continue

        yr_data = data["years"][year]
        doys = yr_data["doy"]
        kcbs = yr_data["kcb"]
        gs = yr_data["gs"]
        gdds = yr_data["gdd"]

        if not doys:
            ax.set_visible(False)
            continue

        peak_kcb = max(kcbs)

        # Plot Kcb
        ax.plot(doys, kcbs, "b-", linewidth=1.5, label="Kcb")

        # Shade growing season
        for i in range(len(doys)):
            if gs[i] == 1:
                ax.axvspan(doys[i] - 0.5, doys[i] + 0.5, alpha=0.08, color="green")

        # Season boundaries
        gs_start = next((doys[i] for i in range(len(gs)) if gs[i] == 1), None)
        gs_end = next((doys[i] for i in range(len(gs) - 1, -1, -1) if gs[i] == 1), None)
        if gs_start:
            ax.axvline(gs_start, color="green", linestyle="--", alpha=0.7, linewidth=0.8)
        if gs_end:
            ax.axvline(gs_end, color="red", linestyle="--", alpha=0.7, linewidth=0.8)

        # Secondary axis for GDD
        ax2 = ax.twinx()
        ax2.plot(doys, gdds, "r-", linewidth=0.8, alpha=0.5, label="GDD")
        ax2.set_ylabel("Cumulative GDD (°F·d)", color="red", fontsize=8)
        ax2.tick_params(axis="y", labelcolor="red", labelsize=7)

        # Title with season summary
        season_str = f"DOY {gs_start}–{gs_end}" if gs_start else "no growing season"
        ax.set_title(f"LU {lu} — {season_str}, peak Kcb={peak_kcb:.2f}", fontsize=10)
        ax.set_xlabel("Month", fontsize=8)
        ax.set_ylabel("Kcb", color="blue", fontsize=9)
        ax.tick_params(axis="y", labelcolor="blue", labelsize=7)
        ax.tick_params(axis="x", labelsize=7)
        ax.set_ylim(0, max(peak_kcb * 1.15, 0.3))
        ax.grid(True, alpha=0.3)
        setup_month_axis(ax)

    # Hide unused axes
    for idx in range(n_files, n_rows * n_cols):
        axes[idx // n_cols, idx % n_cols].set_visible(False)

    plt.tight_layout()
    out_path = Path(f"kcb_curves_{year}.png")
    plt.savefig(out_path, dpi=150, bbox_inches="tight")
    plt.close()
    return out_path


def main() -> None:
    """Generate Kcb curve plots for all DUMP_VARIABLES files."""
    output_dir = Path(sys.argv[1]) if len(sys.argv) > 1 else Path("output")

    if not output_dir.exists():
        print(f"ERROR: Output directory '{output_dir}' not found.")
        sys.exit(1)

    dump_files = sorted(output_dir.glob("SWB2_variable_values__*.csv"))
    if not dump_files:
        print("ERROR: No dump files found.")
        sys.exit(1)

    n_files = len(dump_files)
    print(f"Found {n_files} dump files in {output_dir}")

    # Parse all files
    datasets: list[dict] = []
    for f in dump_files:
        data = parse_dump_csv(f)
        n_years = len(data["years"])
        all_years = sorted(data["years"].keys())
        datasets.append(data)
        yr_str = f"{all_years[0]}–{all_years[-1]}" if n_years > 1 else str(all_years[0])
        print(f"  LU {data['lu_code']:3d}: {n_years} year(s) ({yr_str})")

    # Determine year range
    all_years_global: set[int] = set()
    for d in datasets:
        all_years_global.update(d["years"].keys())
    year_list = sorted(all_years_global)

    # 1. Overlay figure (all years per panel)
    out = plot_overlay(datasets, year_list)
    print(f"\nSaved overlay: {out.resolve()}")

    # 2. Individual year figures
    for year in year_list:
        out = plot_single_year(datasets, year)
        print(f"Saved {year}:    {out.resolve()}")

    print(f"\nDone — {1 + len(year_list)} figures generated.")


if __name__ == "__main__":
    main()
