title: Rooting Depth

The dynamic rooting-depth method is a way to account for the fact that
when plants are small, less of the moisture within the soil-moisture
reservoir is actually available for uptake and growth. When this method
is selected, the current rooting depth is allowed to range between a
small value (0.15 foot) and the maximum root-zone depth specified in the
lookup table. The minimum root-zone depth is assumed during initial
plant growth (*L<sub>ini</sub>* in fig. 1–6) and is assumed to increase
linearly during plant development (*L<sub>dev</sub>* in fig. 1–6),
reaching the maximum root-zone depth at the start of the primary
plant-growth stage (*L<sub>mid</sub>* in fig. 1–6).

Dynamic rooting depth has no effect on calculations unless FAO–56 crop
coefficients are being used in the simulation. With dynamic rooting
depth enabled, the amount of total available water is allowed to grow
larger as plant growth increases. Soil-moisture conditions in springtime
often result in plant water stress when rooting depths are at a minimum.
As the total available water and current rooting depths increase through
the growing season, a larger soil-moisture reservoir is available to
plants; therefore, the soil-moisture conditions are less likely to
result in plant stress. Of course, those generalizations assume a
constant amount of precipitation.
