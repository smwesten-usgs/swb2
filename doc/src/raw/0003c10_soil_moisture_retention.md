
### Soil-Moisture Retention / Actual Evapotranspiration {#soil_moisture_retention}

[TOC]

In the days following a rainstorm, soil moisture is close to field capacity, and moisture is evaporated from bare soil and transpired by plants at rates close to the maximum rate sustainable given climatic conditions. Assuming no further precipitation, in subsequent days the evaporation and transpiration rates decrease as remaining soil-moisture is held more tightly within the soil matrix [@dunne_water_1978].

One simple way of simulating decreasing rates of soil-moisture evapotranspiration is to assume that the actual evapotranspiration is some function of the potential or reference evapotranspiration and the current soil-moisture amount +@eq:SM_function_AET_to_PET.

$$AET = PET \cdot f\left( {\frac{\theta }{{{\theta _{fc}}}}} \right)$$ {#eq:SM_function_AET_to_PET}

where
$AET$ is the actual evapotranspiration,
$PET$ is the potential evapotranspiration,
$\theta$ is the current soil-moisture amount [L], and
$\theta_{fc} is the soil field-capacity.

This section discusses the two soil-moisture retention functions implemented in SWB, one developed by Thornthwaite [-@thornthwaite_approach_1948] and the other included in the FAO-56 approach [-@allen_crop_1998].

#### Thornthwaite-Mather {#sm_thornthwaite_mather}

The first versions of SWB included full tabularized versions of the soil-moisture retention function, along with methods to interpolate between the various table values. The original published method also introduces a variable (accumulated potential water loss, APWL) to track the cumulative unmet potential evapotranspiration as a way to make calculation simpler. Ultimately, the many steps involved in the published method are designed to estimate actual evapotranspiration as directly proportional to the soil moisture relative to field capacity +@fig:aet_to_pet_thornthwaite.

![ Thornthwaite [-@thornthwaite_approach_1948] soil-moisture retention function. file: Actual_ET__FAO56.png]( ../images/Actual_ET__Thornthwaite.png ) {#fig:aet_to_pet_thornthwaite}

Daily soil moisture may be estimated from this relation by first defining the instantaneous soil evapotranspiration as equal to the change in storage:

$$ET_a^' =  - \frac{{d\theta }}{{dt}}$$ {#eq:SM_TM_deriv_001}

where
$ET_a^'$ is the instantaneous actual evapotranspiration, and
$\frac{{d\theta }}{{dt}}$ is the rate of change in soil moisture relative to time.

The relation shown in +@fig:aet_to_pet_thornthwaite can be used to define a function relating actual and potential evapotranspiration as:

$$ET_a^' = ET_p^' \cdot \frac{\theta }{{{\theta _{fc}}}}$$

where
$ET_a^'$ is the instantaneous actual evapotranspiration,
$ET_p^'$ is the instantaneous potential evapotranspiration,
$\theta$ is the soil moisture, and
$\theta_{fc}$ is the soil moisture value at field capacity.



#### FAO-56 {#sm_fao_56}

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede mollis pretium.

![FAO-56 [-@allen_crop_1998] soil-moisture retention function. file: Actual_ET__FAO56.png]( ../images/Actual_ET__FAO56.png )


Etiam rhoncus. Maecenas tempus, tellus eget condimentum rhoncus, sem quam semper libero, sit amet adipiscing sem neque sed ipsum. Nam quam nunc, blandit vel, luctus pulvinar, hendrerit id, lorem. Maecenas nec odio et ante tincidunt tempus.
