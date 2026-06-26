# Plan: BMI for SWB2 to Enable Coupled SWB2-MODFLOW Simulation

**Date:** June 2026  
**Status:** Proposed  

---

## 1. Context and Coupling Architecture

The goal is to run SWB2 and MODFLOW 6 together — SWB2 calculates transient net infiltration (recharge), runoff, and ET at a daily timestep over a gridded domain, and MODFLOW receives this as forcing to simulate the saturated-zone response (1–2 layer model with SFR routing). The key precedent is the MODFLOW API-Ag Package (Larsen et al. 2024), which couples a Python agricultural demand model to MODFLOW 6 via the extended BMI (XMI/xmipy). However, since SWB2 is Fortran, a more natural coupling exists: both SWB2 and MODFLOW 6 can be compiled as shared libraries with BMI/XMI interfaces, and a **driver program** (Fortran or Python) orchestrates both.

### References

- **CSDMS BMI Fortran spec:** https://github.com/csdms/bmi-fortran (`bmif_2_0` module, abstract type `bmi`)
- **MODFLOW 6 BMI:** `mf6bmi.f90` — flat C-bound DLL-exported functions, memory-manager-based variable access
- **MODFLOW 6 XMI:** `mf6xmi.F90` — extends BMI with `prepare_time_step`, `do_time_step`, `finalize_time_step`, plus `prepare_solve`/`solve`/`finalize_solve` for within-Picard-iteration coupling
- **xmipy:** Python wrapper for XMI (Deltares/USGS joint development)
- **modflowapi:** Python package extending xmipy for MODFLOW-specific coupling
- **API-Ag Package:** Larsen et al. 2024, _Groundwater_ v.62(1):157–166, doi:10.1111/gwat.13367
- **imod_coupler:** Deltares tool coupling MetaSWAP + MODFLOW 6 via xmipy (shared control volume, within-Picard exchange)

### Coupling Topology Options

| Option | Description | Pros | Cons |
|--------|-------------|------|------|
| **A. Python driver** | Both libmf6 and libswb2 loaded as DLLs via xmipy/modflowapi from Python | Maximum flexibility; Python ecosystem for pre/post; matches API-Ag pattern | Performance overhead for large grids; two DLLs + Python |
| **B. Fortran driver** | A small Fortran executable links both libswb2 and libmf6, calling their BMI/XMI functions directly | Maximum performance; single binary; no interpreter | More rigid; less accessible to users unfamiliar with Fortran |
| **C. SWB2 as driver** | SWB2 main loop loads libmf6 (via dlopen/LoadLibrary) and drives MODFLOW | Simpler deployment for existing SWB2 users | Tight coupling in one direction only; harder to maintain |

**Recommendation:** Implement Option A as the primary target (Python driver calling both DLLs), with Option B as a follow-on for production runs. This is exactly the pattern used by imod_coupler (Deltares) which couples MetaSWAP + MODFLOW 6 via xmipy.

---

## 2. SWB2 BMI Implementation Plan

### 2.1 Refactor SWB2 into a Shared Library

SWB2 already builds a static library (`swb_library`) that `main.F90` links against. The refactoring needed:

1. **Separate the `main.F90` entry point from core logic** — already done (main just calls `initialize_all`, `iterate_over_simulation_days`, cleanup).

2. **Create `swb2_bmi.F90`** — a new module implementing the BMI interface. Given that the coupling target is MODFLOW 6 which uses C-bound flat functions (not type-bound), **adopt the same flat-function, C-interoperable DLL-export style as mf6bmi.f90**.

3. **Build as a shared library** — add a Meson build target for `libswb2.dll` / `libswb2.so` that exports the BMI+XMI symbols.

### 2.2 BMI Functions to Implement

Following the CSDMS BMI spec and MODFLOW's conventions:

**Core IRF (Initialize/Run/Finalize):**

| Function | SWB2 Implementation |
|----------|-------------------|
| `bmi_initialize()` | Calls `read_control_file()` + `initialize_all()` from config in CWD |
| `bmi_update()` | Advance one day: `get_weather_data()`, `perform_daily_calculation()`, `write_output()`, `SIM_DT%addDay()` |
| `bmi_finalize()` | Calls `finalize_output()`, deallocates |

**XMI Extensions (finer granularity for coupling):**

| Function | SWB2 Implementation |
|----------|-------------------|
| `xmi_prepare_time_step(dt)` | Read weather data, update land use codes |
| `xmi_do_time_step()` | Call `perform_daily_calculation()` |
| `xmi_finalize_time_step()` | Write output, advance date |

These XMI extensions allow the driver to **inject modified values** (e.g., water-table depth from MODFLOW affecting max infiltration, or rejected recharge returned from MF6) between prepare and do.

**Variable Access:**
- `get_value` / `set_value` / `get_value_ptr` for key exchange variables (see §2.3)
- `get_var_type`, `get_var_itemsize`, `get_var_nbytes`, `get_var_rank`, `get_var_shape`
- `get_input_var_names` / `get_output_var_names`

**Time:**
- `get_start_time`, `get_end_time`, `get_current_time`, `get_time_step`
- Time units: `"d"` (days)

**Grid:**
- `get_grid_type` → `"uniform_rectilinear"`
- `get_grid_rank` → 2
- `get_grid_shape` → (nrow, ncol)
- `get_grid_spacing` → (cell_size, cell_size)
- `get_grid_origin` → (X_ll, Y_ll)

### 2.3 Exchange Variables (BMI Variable Names)

These are the state/flux arrays on `MODEL_DOMAIN_T` that the coupling driver needs to read or write:

**SWB2 Outputs (read by driver, passed to MODFLOW):**

| BMI Variable Name | MODEL_DOMAIN_T Member | Description | Units |
|---|---|---|---|
| `net_infiltration` | `net_infiltration(:)` | Recharge flux to water table | in/day |
| `runoff` | `runoff(:)` | Surface runoff (potential SFR inflow) | in/day |
| `rejected_net_infiltration` | `rejected_net_infiltration(:)` | Infiltration exceeding max (→ SFR) | in/day |
| `actual_et` | `actual_et(:)` | Actual ET | in/day |
| `soil_storage` | `soil_storage(:)` | Current soil moisture storage | in |
| `runoff_outside` | `runoff_outside(:)` | Runoff leaving domain (D8 terminal) | in/day |

**SWB2 Inputs (set by driver from MODFLOW state):**

| BMI Variable Name | MODEL_DOMAIN_T Member | Description | Units |
|---|---|---|---|
| `water_table_depth` | *(new member needed)* | Depth to water table from land surface | length |
| `gw_discharge_to_land_surface` | *(new member needed)* | GW seepage / rejected recharge return | in/day |
| `stream_leakage_to_soil` | *(new member needed)* | SFR leakage back to soil zone | in/day |

The `water_table_depth` would allow SWB2 to dynamically adjust:
- Maximum soil storage (reduce if water table is within root zone)
- Maximum net infiltration rate
- ET from groundwater (if within extinction depth)

### 2.4 Internal SWB2 Changes Required

1. **Add new state arrays** to `MODEL_DOMAIN_T`:
   - `water_table_depth(:)` — set externally via BMI
   - `gw_et(:)` — groundwater contribution to ET
   - Optional: `gw_discharge_to_land_surface(:)`

2. **Modify soil mass balance logic** to optionally cap soil storage based on water-table depth (when water table is shallow, soil storage max decreases).

3. **Modify `maximum_net_infiltration`** to optionally use a rate derived from aquifer properties (K_vertical) instead of a fixed cap.

4. **Expose `MODEL` as a module-level persistent object** — already the case (`MODEL` is in `model_domain` module with implicit `save` attribute).

5. **Unit conversion layer** — SWB2 works in inches internally; MODFLOW works in length/time units specified in the simulation. The BMI layer should expose values in consistent units (meters/day recommended) with internal conversion.

---

## 3. Coupling Time-Step Strategy

### The Time-Step Mismatch Problem

- SWB2 operates at a **daily** timestep (fixed, 1 day)
- MODFLOW stress periods can be days to years; time steps within a stress period can be sub-daily to multi-day

### Proposed Strategy: SWB2 Accumulates, MODFLOW Receives Per Stress Period

```python
# Driver pseudo-code (Python with xmipy)
swb2 = XmiWrapper("libswb2.dll")
mf6 = XmiWrapper("libmf6.dll")

swb2.initialize()
mf6.initialize()

while mf6.get_current_time() < mf6.get_end_time():
    mf6_dt = mf6.get_time_step()
    mf6.prepare_time_step(mf6_dt)

    # Determine how many SWB2 days fall in this MF6 time step
    n_days = int(mf6_dt)

    # Run SWB2 for those days, accumulating recharge
    accumulated_recharge = np.zeros(n_cells)
    accumulated_runoff = np.zeros(n_cells)

    for day in range(n_days):
        # Optionally feed back MF6 heads to SWB2
        heads = mf6.get_value_ptr("model/X")
        wt_depth = land_surface_elev - heads[top_layer]
        swb2.set_value("water_table_depth", wt_depth)

        swb2.update()  # one day

        accumulated_recharge += swb2.get_value("net_infiltration")
        accumulated_runoff += swb2.get_value("runoff")

    # Convert accumulated fluxes to MODFLOW rates
    recharge_rate = accumulated_recharge / mf6_dt

    # Set MODFLOW RCH array
    mf6.set_value("model/RCH-1/RECHARGE", recharge_rate)

    # Set SFR inflows from accumulated runoff
    sfr_inflows = route_runoff_to_sfr_reaches(accumulated_runoff)
    mf6.set_value("model/SFR-1/INFLOW", sfr_inflows)

    # Solve MODFLOW time step
    mf6.do_time_step()
    mf6.finalize_time_step()

swb2.finalize()
mf6.finalize()
```

### Tighter Coupling (Within Picard Iteration)

For applications where the water table significantly affects soil storage or ET, a tighter coupling within the MODFLOW nonlinear iteration is possible using XMI:

```python
mf6.prepare_time_step(dt)
mf6.prepare_solve(1)

converged = False
while not converged:
    # Get current head estimate
    heads = mf6.get_value_ptr("model/X")
    wt_depth = land_surface - heads[top_layer]

    # Update SWB2 soil-zone response with new water table
    swb2.set_value("water_table_depth", wt_depth)
    swb2.xmi_do_time_step()  # re-solve daily balance

    # Update MODFLOW recharge from SWB2
    recharge_rate = swb2.get_value("net_infiltration") / dt
    mf6.set_value("model/RCH-1/RECHARGE", recharge_rate)

    # MODFLOW linear solve iteration
    has_converged = mf6.solve(1)
    converged = (has_converged == 1)

mf6.finalize_solve(1)
mf6.finalize_time_step()
```

This mimics exactly the MetaSWAP–MODFLOW 6 coupling approach (shared control volume, within-Picard exchange).

---

## 4. Grid Mapping

SWB2 and MODFLOW grids may not be identical. The driver must handle mapping:

- **Same grid (simplest):** 1:1 mapping of SWB2 cells to MODFLOW cells (top layer). Recommended for initial implementation.
- **Different grids:** Requires area-weighted transfer of fluxes. Could be handled in the driver using pre-computed mapping weights.

For the initial implementation, require that:
- The SWB2 grid and MODFLOW model grid have the same cell size and origin
- MODFLOW model is 1–2 layers, structured grid (DIS package)
- SWB2 active mask aligns with MODFLOW IDOMAIN

---

## 5. SFR Integration

SWB2's D8 routing produces `runoff_outside` (runoff leaving the domain) and cell-to-cell `runoff`. To feed SFR:

1. **Define a mapping** from SWB2 D8 terminal cells (cells whose flow direction exits the grid or reaches a stream cell) to SFR reach numbers.
2. **Accumulate routed runoff** arriving at mapped stream cells.
3. **Set `SFR/INFLOW`** via MODFLOW BMI for each reach, equal to the sum of SWB2 runoff arriving at that reach.
4. Optionally, **SFR leakage** (stream loss to aquifer) could be fed back to SWB2 as additional infiltration in the next timestep.

This could be configured via a simple CSV mapping file: `(swb2_cell_index, sfr_reach_number)`.

---

## 6. Implementation Phases

### Phase 1: Minimal SWB2 BMI (Standalone Testable)

- Create `swb2_bmi.F90` with flat C-binding functions
- Implement: `initialize`, `update`, `finalize`, `get_current_time`, `get_time_step`, `get_start_time`, `get_end_time`
- Implement: `get_value_ptr` for `net_infiltration`, `runoff`, `actual_et`, `soil_storage`
- Implement grid info functions for the uniform rectilinear case
- Build as shared library (`libswb2.dll` / `libswb2.so`)
- Test: load from Python, initialize, step through days, read recharge array

### Phase 2: Python Driver with MODFLOW 6

- Write a Python coupling script (following the API-Ag pattern)
- Use `xmipy.XmiWrapper` to load both `libswb2` and `libmf6`
- Implement the sequential coupling loop (SWB2 daily → accumulate → set MF6 RCH)
- Test with a simple synthetic 1-layer MODFLOW model + SWB2 on same grid
- Validate: compare coupled recharge vs. standalone SWB2 recharge

### Phase 3: SFR Coupling

- Implement the D8-terminal → SFR reach mapping
- Accumulate SWB2 surface runoff into SFR inflows
- Add SFR leakage feedback to SWB2 (optional)
- Test with a domain having a simple stream network

### Phase 4: Tight Coupling (Water Table Feedback)

- Add `water_table_depth` input variable to SWB2 BMI
- Modify `maximum_net_infiltration` and/or soil storage max based on water-table depth
- Implement within-Picard coupling loop (XMI pattern)
- Evaluate convergence behavior

### Phase 5: Production Hardening

- Fortran driver option (for users who want a single executable)
- Unit conversion robustness (support feet, meters, inches)
- Error handling and graceful failure modes
- Documentation and example problems

---

## 7. Key Design Decisions

1. **Variable naming convention** — Follow short internal names (matching MODEL_DOMAIN_T members) rather than full CSDMS standard names. Add a mapping table for CSDMS names later if interoperability with other BMI models is desired.

2. **Curated variable subset vs. expose everything** — Expose a curated subset of ~10–15 exchange variables rather than all internal arrays. SWB2 doesn't have the multi-model complexity that justifies MODFLOW's "expose everything via memory manager" approach.

3. **Units in the BMI layer** — The BMI layer should always expose SI units (m, m/d) even though SWB2 works in inches internally. Add a conversion layer in the BMI functions. This ensures compatibility with MODFLOW models using meters.

4. **Grid orientation** — MODFLOW uses row 0 at the top (north); SWB2 grids read from Arc ASCII with row 0 at top as well. Verify that the 1D packing order is compatible or document the mapping.

5. **Build dependency on bmif_2_0** — Adopt the MODFLOW approach (flat C-bound functions without type-bound-ness) for DLL compatibility. The CSDMS abstract type is useful for Fortran-only usage but cannot have `bind(C)`.

---

## 8. Dependencies and Prerequisites

- MODFLOW 6 compiled as shared library (available from conda-forge or USGS releases)
- `xmipy` and `modflowapi` Python packages (for the Python driver)
- SWB2 build system update (Meson) to produce a shared library target
- No new Fortran external dependencies needed for SWB2 itself

---

## 9. Relationship to Existing SWB2 Architecture

The current SWB2 execution flow is:

```
main.F90
  ├─ read_control_file() → PARAMS_DICT
  ├─ initialize_all()
  ├─ iterate_over_simulation_days(MODEL)
  │     └─ DO WHILE (SIM_DT%curr <= SIM_DT%end)
  │           ├─ get_weather_data()
  │           ├─ perform_daily_calculation(MODEL)
  │           ├─ write_output(MODEL)
  │           └─ SIM_DT%addDay()
  └─ finalize_output()
```

The BMI wraps this as:
- `bmi_initialize()` = `read_control_file()` + `initialize_all()`
- `bmi_update()` = one iteration of the daily loop (weather + calculate + output + advance)
- `bmi_finalize()` = `finalize_output()` + cleanup

The XMI further splits `bmi_update()`:
- `xmi_prepare_time_step()` = `get_weather_data()` + `update_landuse_codes()`
- `xmi_do_time_step()` = `perform_daily_calculation()`
- `xmi_finalize_time_step()` = `write_output()` + `SIM_DT%addDay()`

This decomposition gives the driver full control over when external data (water-table depth) is injected into the SWB2 state arrays.
