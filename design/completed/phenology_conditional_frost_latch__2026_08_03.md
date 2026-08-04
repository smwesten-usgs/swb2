# Phenology Fix: Conditional Frost Kill Hard Latch

**Date:** August 3, 2026  
**Status:** Implemented  
**Affects:** `src/phenology.F90` — subroutines `phenology_update_fao56_gdd` and `phenology_update_gdd_threshold`  
**Branch:** `phenology_phase_1`  

---

## Problem

The frost kill hard latch (`frost_killed_season`) was engaging unconditionally whenever a killing frost occurred after the GDD start threshold was reached. In the McDonalds Branch (NJ) test case, this caused a catastrophic failure in 2017:

1. **Feb 22, 2017:** GDD reaches 220 (> start threshold of 200), growing season starts (INI stage, `gdd_since_planting` = 20)
2. **Mar 5, 2017:** Mean air temperature drops to ~25°F (tmin=13.1°F), below the killing frost threshold of 28°F
3. **Hard latch engages:** `frost_killed_season = TRUE`
4. **Rest of 2017:** Growing season never restarts. Kcb pinned at KCB_min (0.25) for the entire year. No summer ET peak.

The latch was designed to prevent fall frost-kill from being followed by an impossible restart. But it also prevented recovery from *spring* false starts — a warm February spell triggering GDD start, followed by a normal late-winter frost.

All other years (2015, 2016, 2018–2022) worked correctly because they didn't have the specific sequence of early GDD start + subsequent frost before development was underway.

---

## Solution: Conditional Hard Latch

The hard latch now only engages when the plant has developed sufficiently far into the growing season that a frost represents a true "fall kill" rather than a spring false start.

### `phenology_update_fao56_gdd`

**Latch condition:** `gdd_since_planting >= gdd_ini + gdd_dev` (i.e., plant is in MID or LATE stage)

- Frost during INI or DEV: go dormant for the day, but do NOT set `frost_killed_season`. The next non-frost day, GDD is still above the start threshold, so the season resumes from wherever the GDD accumulation places it.
- Frost during MID or LATE: set `frost_killed_season = TRUE`. Season is over for the year (true fall frost-kill).

### `phenology_update_gdd_threshold`

**Latch condition:** `current_gdd >= 3.0 * growing_season_start_gdd`

Since GDD_THRESHOLD has no explicit stage boundaries, a simple multiplier (3×) is used as a proxy for "the season is well-established." This is less critical than the FAO56_GDD fix because GDD_THRESHOLD doesn't control crop coefficients.

---

## What Was NOT Changed

- **No new parameters or lookup table columns.** The fix is entirely internal logic.
- **No GDD reset after early frost.** GDD continues accumulating normally; the plant simply resumes growth from its current GDD position once temperatures recover. A future "pre-scan" approach (scanning the full temperature record at initialization to identify the last spring frost date) would be the more physically correct solution but requires architectural changes.
- **No changes to DOY_BASED or FAO56_DATES methods.** These are calendar-driven and don't use the frost latch.
- **Fall frost-kill behavior is preserved.** Once a plant is in MID or LATE stage, a killing frost still permanently ends the season for that year.

---

## Expected Behavior After Fix (2017 Test Case)

- Feb 22: GDD=220, season starts, INI stage (`gdd_since_planting`=20, `end_dev`=850 for mixed forest 143)
- Mar 5: frost occurs, `gdd_since_planting` ≈ 110 < 850 → dormant for the day, **no hard latch**
- Mar 6+: GDD still > start threshold, no frost → season resumes, progresses INI → DEV → MID → LATE normally
- Fall: first frost during MID or LATE → hard latch engages, season done

---

## Relation to Design Documents

This change is consistent with the phenology module design (`design/completed/phenology_module_design.md`) and implementation plan (`design/completed/phenology_implementation_plan.md`). The implementation plan (Phase 3, Step 3.3) noted that GDD reset/frost-kill behavior needed a documented decision. This fix documents that decision:

> Killing frost terminates growth at any stage, but the *permanent* latch (preventing restart for the remainder of the year) only engages once the plant is past the development phase. This balances biological realism (early tissue isn't vulnerable in the same way mature canopy is) with practical robustness (avoids losing an entire year's growing season to a February warm spell followed by a normal March frost).

A more rigorous approach — pre-scanning the temperature record to identify the last spring frost date and using that as a hard floor for growing season start — is noted as a future improvement but deferred to avoid adding architectural complexity (the phenology module would need access to the full weather timeseries at initialization time).

---

## Testing

Recompile the `phenology_phase_1` branch and re-run the McDonalds Branch example:

```bat
cd E:\projects\swb_development\git\swb2_pest_example\run
d:\bin\swb2.exe --data_dir=..\model_inputs --weather_data_dir=D:\weather_and_geodata\daymet\daymet_v4 --output_dir=output --logfile_dir=logfile --lookup_dir=..\model_inputs mcdonalds_branch.ctl
```

Verify:
1. 2017 growing season persists through summer (GS_days > 100, not 10)
2. Kcb reaches 0.95 during JJA 2017
3. All other years remain unchanged (same GS start/end dates, same Kcb curves)
4. Fall frost-kill still works correctly (season ends in Aug/Sep as before)
