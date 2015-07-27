
# Description of Soil-Moisture Retention / Actual Evapotranspiration Process Options {#soil_moisture_retention}

[TOC]

# Thornthwaite-Mather {#sm_thornthwaite_mather}

When SWB is run with this process option soil moisture is tabulated by means of the soil-water-balance methods published in Thornthwaite [-@thornthwaite_approach_1948] and Thornthwaite and Mather [-@thornthwaite_water_1955; -@thornthwaite_instructions_1957]. In order to track changes in soil moisture, several intermediary values are calculated, including precipitation minus potential evapotranspiration (P-PE), accumulated potential water loss (APWL), actual evapotranspiration, soil-moisture surplus, and soil-moisture deficit. These terms are described below.

*P minus PE* $(P-PE)$. The first step in calculating a new soil moisture value for any given grid cell is to subtract potential evapotranspiration from the daily precipitation ($P-PE$). Negative values of $P-PE$ represent a potential deficiency of water, whereas positive $P-PE$ values represent a potential surplus of water.

*Accumulated Potential Water Loss (APWL)*. The accumulated potential water loss is calculated as a running total of the daily $P-PE$ values during periods when $P-PE$ is negative. This running total represents the total amount of unsatisfied potential evapotranspiration to which the soil has been subjected. Soils typically yield water more easily during the first days in which $P-PE$ is negative. On subsequent days, as the APWL grows, soil moisture is less readily given up. The nonlinear relation between soil moisture and the accumulated potential water loss was described by Thornthwaite and Mather (1957) in a series of tables. These tables are incorporated into the SWB code.

Note that the accumulated potential water loss can grow without bound; it represents the cumulative daily potential water loss given the potential evapotranspiration and observed precipitation. 

*Soil moisture*, $\Delta\,soil\,moisture$. The soil-moisture term represents the amount of water held in soil storage for a given grid cell. Soil moisture has an upper bound that corresponds to the soil's maximum water-holding capacity (roughly equivalent to the field capacity); soil moisture has a lower bound that corresponds to the soil's permanent wilting point.

When $P-PE$ is positive, the new soil-moisture value is found by adding this $P-PE$ term directly to the preceding soil-moisture value. If the new soil-moisture value is still below the maximum water-holding capacity, the Thornthwaite-Mather soil-moisture tables are consulted to back-calculate a new, reduced accumulated potential water-loss value. If the new soil-moisture value exceeds the maximum water-holding capacity, the soil-moisture value is capped at the value of the maximum water-holding capacity, the excess moisture is converted to recharge, and the accumulated potential water-loss term is reset to zero.

When $P-PE$ is negative, the new soil-moisture term is found by looking up the soil-moisture value associated with the current accumulated potential water-loss value in the Thornthwaite-Mather tables.

*Actual ET*. When $P-PE$ is positive, the actual evapotranspiration equals the potential evapotranspiration. When $P-PE$ is negative, the actual evapotranspiration is equal only to the amount of water that can be extracted from the soil ($\Delta$ soil moisture). 

*Soil moisture SURPLUS*. If the soil moisture reaches the maximum soil-moisture capacity, any excess precipitation is added to the daily soil-moisture surplus value. Under most conditions, the soil-moisture surplus value is equivalent to the daily groundwater recharge value.

*Soil moisture DEFICIT*. The daily soil-moisture deficit is the amount by which the actual evapotranspiration differs from the potential evapotranspiration. 

The soil-moisture surplus and deficit terms have no direct bearing on the calculation of recharge; these terms feature rather prominently in the original work by [@thornthwaite_water_1955; @thornthwaite_instructions_1957] and are included here for completeness.

~~~~~~~~
CONTROL_FILE_STATEMENT 1
CONTROL_FILE_STATEMENT_2
~~~~~~~~

Etiam rhoncus. Maecenas tempus, tellus eget condimentum rhoncus, sem quam semper libero, sit amet adipiscing sem neque sed ipsum. Nam quam nunc, blandit vel, luctus pulvinar, hendrerit id, lorem. Maecenas nec odio et ante tincidunt tempus. 

# FAO-56 {#sm_fao_56}

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede mollis pretium.

Etiam rhoncus. Maecenas tempus, tellus eget condimentum rhoncus, sem quam semper libero, sit amet adipiscing sem neque sed ipsum. Nam quam nunc, blandit vel, luctus pulvinar, hendrerit id, lorem. Maecenas nec odio et ante tincidunt tempus. 


