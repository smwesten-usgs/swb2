
### Soil-Moisture Retention / Actual Evapotranspiration {#soil_moisture_retention}

[TOC]

In the days following a rainstorm, soil moisture is close to field capacity, and moisture is evaporated from bare soil and transpired by plants at rates close to the maximum rate sustainable given climatic conditions. Assuming no further precipitation, in subsequent days the evaporation and transpiration rates decrease as remaining soil-moisture is held more tightly within the soil matrix [@dunne_water_1978].

One simple way of simulating decreasing rates of soil-moisture evapotranspiration is to assume that the actual evapotranspiration is some function of the potential or reference evapotranspiration and the current soil-moisture amount @eq:SM_function_AET_to_PET.

$$AET = PET \cdot f\left( {\frac{\theta }{{{\theta _{fc}}}}} \right)$$ {#eq:SM_function_AET_to_PET}

where
$AET$ is the actual evapotranspiration,
$PET$ is the potential evapotranspiration,
$\theta$ is the current soil-moisture amount [L], and
$\theta_{fc}$ is the soil field-capacity.

This section discusses the two soil-moisture retention functions implemented in SWB, one developed by Thornthwaite [-@thornthwaite_approach_1948] and the other included in the FAO-56 approach [-@allen_crop_1998].

#### Thornthwaite-Mather {#sm_thornthwaite_mather}

The first versions of SWB included full tabularized versions of the soil-moisture retention function, along with methods to interpolate between the various table values. The original published Thornthwaite-Mather [-@thornthwaite_instructions_1957] method also introduces a variable (accumulated potential water loss, APWL) to track the cumulative unmet potential evapotranspiration as a way to make calculation simpler. Ultimately, the many steps involved in the published method are designed to estimate actual evapotranspiration as directly proportional to the soil moisture relative to field capacity +@fig:aet_to_pet_thornthwaite.

![ Thornthwaite [-@thornthwaite_approach_1948] soil-moisture retention function. file: Actual_ET__FAO56.png]( ../images/Actual_ET__Thornthwaite.png ) {width=4.5in #fig:aet_to_pet_thornthwaite}

Daily soil moisture may be estimated from this relation by first defining the instantaneous soil evapotranspiration as equal to the change in storage:

$$et_a =  - \frac{{d\theta }}{{dt}}$$ {#eq:SM_TM_deriv_001}

where
$et_a$ is the instantaneous actual evapotranspiration, and
$\frac{{d\theta }}{{dt}}$ is the rate of change in soil moisture relative to time.

The relation shown in +@fig:aet_to_pet_thornthwaite can be used to define a function relating actual and potential evapotranspiration as:

$$et_a = et_p \cdot \frac{\theta }{{{\theta _{fc}}}}$$

where
$et_a$ is the instantaneous actual evapotranspiration,
$et_p$ is the instantaneous potential evapotranspiration,
$\theta$ is the soil moisture, and
$\theta_{fc}$ is the soil moisture value at field capacity.

$$ - \frac{{d\theta }}{{dt}} = et_p \cdot \frac{\theta }{{{\theta _{fc}}}}$$

$$ - \frac{{d\theta }}{\theta } = \frac{{et_p}}{{{\theta _{fc}}}}dt$$

$$ - \int{\frac{{d\theta }}{\theta }}  = \frac{1}{{{\theta _{fc}}}}\int {et_p dt} $$

$$ \left. { - \ln \theta } \right|_{{\theta _{t - 1}}}^{{\theta _t}} = \frac{1}{{{\theta _{fc}}}}E{T_p}$$

$$ {\theta _t} = {\theta _{t - 1}} \cdot {e^{\left( { - \frac{{E{T_p}}}{{{\theta _{fc}}}}} \right)}}$$

#### FAO-56 {#sm_fao_56}

The FAO-56 method for determining actual evapotranspiration considers the process in two phases @fig:fao56_soil_moisture. In the first phase, soil moisture levels are between a threshold soil moisture level and field capacity, and the actual ET is assumed to be equal to the potential ET. At soil moisture levels below the threshold level, the ratio between actual and potential ET is assumed to decrease linearly, with the ratio having a value of zero as the soil moisture reaches the permanent wilting point.

![FAO-56 soil-moisture retention function. filename is Actual_ET__FAO56.png]( ../images/Actual_ET__FAO56.png )  {#fig:fao56_soil_moisture width=4.5in}

The relation shown in figure 6 may be used to update the current soil moisture by considering three cases:

1. $\theta{t-1}$ and $\theta_{interim}$ are both greater than the threshold soil moisture $\theta_{threshold}$,
2. $\theta{t-1}$ exceeds $\theta_{interim}$, but $\theta_{interim}$ is less than $\theta_{threshold}$, or
3. $\theta{t-1}$ and $\theta_{interim}$ are both less than $\theta_{threshold}$.

In the first case where both the previous days’ and interim soil moistures lie on the horizontal portion of the line in figure 6, evapotranspiration proceeds at the rate of the potential ET value, and soil moisture is calculated:

$$\theta_t = \theta_{interim} - ET_p$$

In the third case, both the previous days’ and interim soil moistures lie on the sloped portion of the line in @fig:fao56_soil_moisture, and evapotranspiration is some fraction of the potential ET value. The derivation of the equation describing this case is similar to that shown in the previous section, except that   replaces  :	 

The second case is simply a linear combination of the first and third cases. We can define  as the fraction of the soil moisture band between  and  that exceeds the threshold soil moisture:

The new soil moisture for the second case can then be found as:
