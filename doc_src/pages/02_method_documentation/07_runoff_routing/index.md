title: Runoff Routing

SWB allows excess water generated at a grid cell to flow to the next
downslope cell using a D8 flow-routing scheme to define the linkages
between cells. Activation of the overland flow-routing method within SWB
allows runoff from one or more cells to become run-on to downslope
cells. All runoff from a cell is assumed to infiltrate in downslope
cells or be routed out of the model domain on the same day in which the
runoff originated as rainfall or snowmelt. Runoff flow routing may be
disabled and also may be configured such that only some fraction of
runoff is routed to the downslope cell.

During model initialization, SWB examines the connectivity between each
active cell. Based on this connectivity, SWB creates a master list of
cell identifications and sorts them from upslope to downslope. When the
model solution is calculated each day, the code begins with the cell
furthest upslope, performs all mass-balance calculations, and then
proceeds to perform the same calculation on the next cell in the list.

Connectivity is defined on the basis of an input D8 flow-direction grid;
this is a scheme by which connections between cells are encoded as an
integer value within the flow-direction grid (O’Callaghan and Mark,
1984), with flow directions defined as shown in figure 1–3*B*. The
original algorithm assigns a unique flow direction to each grid cell by
determining the steepest slope between the central cell and its eight
neighboring cells. For the cells shown in figure 1–3*A*, the steepest
descent algorithm results in flow from the central cell to the
southwest; the corresponding cell figure 1–3*B*, located to the
southwest of the central cell, contains the number 8. By convention,
therefore, the D8 flow direction for the cell shown in figure 1–3*A* is
8.

3.  Examples of A, elevation grid values, in meters and B, resulting D8
    flow-direction encoding.

Once water is routed to a closed surface depression and
evapotranspiration and soil-moisture demands are met, the only loss
mechanism is net infiltration. The simplified nature of the flow routing
results in cases where maximum net-infiltration values of hundreds or
thousands of inches per year are calculated. These values are
unrealistic and likely result from the simplified treatment of overland
flow routing. SWB allows the user to enter a maximum recharge rate for
each land cover and soil group combination. This feature offers a way to
restrict the estimated net-infiltration values to a more reasonable
range; however, the rejected net infiltration, nonetheless, is removed
from the model domain on the same day in which the net infiltration
originated as precipitation or snowmelt.

If desired, only a fraction of the calculated runoff can be routed
downslope. A user-specified routing fraction grid may be specified in
order to split runoff between RUNOFF\_OUTSIDE (in other words, assumed
to join a surface-water feature and exit the model grid) and inflow to
the next downslope cell. For cases in which flow routing is undesirable,
the runoff flow routing routine may be disabled altogether.
