title: Online Documentation

<style>
table {
width: 100%;
}
table,th,td {
border: 1px solid black;
border-collapse: collapse;
}
th, td {
padding: 5px;
}
th {
text-align: center;
}
</style>

The U.S. Geological Survey’s Soil-Water-Balance (SWB) code was developed as a tool to estimate distribution and timing of net infiltration out of the root zone by means of an approach that uses readily available data and minimizes user effort required to begin a SWB application. SWB calculates other components of the water balance, including soil moisture, reference and actual evapotranspiration, snowfall, snowmelt, canopy interception, and crop-water demand. SWB is based on a modified Thornthwaite-Mather soil-water-balance approach, with components of the soil-water balance calculated at a daily time step. Net-infiltration calculations are computed by means of a rectangular grid of computational elements, which allows the calculated infiltration rates to be imported into grid-based regional groundwater-flow models. SWB makes use of gridded datasets, including datasets describing hydrologic soil groups, moisture-retaining capacity, flow direction, and land use. Climate data may be supplied in gridded or tabular form. The SWB 2.0 code described in this report extends capabilities of the original SWB version 1.0 model by adding new options for representing physical processes and additional data input and output capabilities. New methods included in SWB 2.0 allow for direct gridded input of externally calculated water-budget components (fog, septic, and storm-sewer leakage), simulation of canopy interception by several alternative processes, and a crop-water demand method for estimating irrigation amounts. New input and output capabilities allow for grids with differing spatial extents and projections to be combined without requiring the user to resample and resize the grids before use.
