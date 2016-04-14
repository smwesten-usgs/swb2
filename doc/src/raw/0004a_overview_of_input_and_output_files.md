## Overview of Input and Output Files {#overview_of_input_and_output_files}

The Soil-Water-Balance Code must be supplied with at least three gridded datasets in order to run:

1. Available water capacity, in inches per foot [real valued grid]
2. Landuse classification [integer grid]
3. Hydrologic Soils Group [integer grid]

A fourth grid must be supplied if flow routing is enabled in the simulation. This grid is an integer-valued grid of "D8" flow directions, wherein 1 signifies flow from the cell to the east, 2 to the southeast, 4 to the south, 8 to the southwest, 16 to the west, 32 to the northwest, 64 to the north, and 128 to the northeast.

If flow routing is not enabled, the flow direction grid may be specified in the control file as something like:

```
FLOW_DIRECTION CONSTANT 1
```
This specification will satisfy SWB's requirement for flow direction data by simply supplying a grid of constant values.