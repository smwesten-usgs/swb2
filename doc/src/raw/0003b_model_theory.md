## Model Theory {#model_theory} 

The SWB code uses a modified Thornthwaite-Mather soil-moisture accounting method [@thornthwaite_instructions_1957] to calculate recharge; recharge is calculated separately for each grid cell in the model domain. Sources and sinks of water within each grid cell are determined on the basis of input climate data and landscape characteristics; recharge is calculated as the difference between the change in soil moisture and these sources and sinks:

$recharge = (rainfall + snowmelt + inflow) - (interception + outflow + ET) - \Delta soilmoisture$

Each of the water-budget components given in equation ### is handled by one or more modules within the SWB model. Specific water-balance components are discussed briefly below.

**rainfall.** Precipitation data are input as daily values either as a time series at a single gage or as a series of daily Arc ASCII or Surfer grid files created by the user. Precipitation-gage records from an unlimited number of sites may be used if the user supplies precipitation as a series of grid files. SWB converts partitions precipitation into rainfall and snowfall components on the basis of daily minimum and maximum air temperatures.

**snowmelt.** Snow is allowed to accumulate and\ or melt on a daily basis. The daily mean, maximum, and minimum air temperatures are used to determine whether precipitation takes the form of rain or snow. Precipitation that falls on a day when the mean temperature minus one-third the difference between the daily high and low temperatures is less than or equal to the freezing point of water is considered to fall as snow [@dripps_simple_2007].

Snowmelt is based on a temperature-index method. In the SWB code it is assumed that 1.5 mm (0.059 in.) of water-equivalent snow melts per day per average degree Celsius that the daily maximum temperature is above the freezing point [@dripps_simple_2007].

**inflow.** Inflow is calculated by use of a flow-direction grid derived from a digital elevation model to route outflow (surface runoff) to adjacent downslope grid cells. Inflow is considered to be zero if flow routing is turned off.

**interception.** Interception is treated simply by means of a "bucket" model approach---a user-specified amount of rainfall is assumed to be trapped and used by vegetation and evaporated or transpired from plant surfaces. Daily precipitation values must exceed the specified interception amount before any water is assumed to reach the soil surface. Interception values may be specified for each land-use type and season (growing and dormant).

**outflow (or surface runoff).** from a cell is calculated by use of the U.S. Department of Agriculture, Natural Resources Conservation Service (NRCS) curve number rainfall-runoff relation [@cronshey_urban_1986]. This rainfall-runoff relation is based on four basin properties: soil type, land use, surface condition, and antecedent runoff condition.


