title: Crop Coefficients

The FAO–56 methodology links the estimation of actual evapotranspiration
to growth patterns of vegetation and crops by means of a
crop-coefficient curve that changes during a growing season. The amount
of water required by the vegetation of a crop at any point during a
growing season is given in equation 1–33:

(1–33)

where

is crop evapotranspiration,

is the crop coefficient, and

is the reference evapotranspiration.

The crop evapotranspiration equation (eq. 1–33) is valid for ideal or
standard conditions––a condition in which soil moisture stays close to
field capacity regardless of plant water use. An example of the
simplified crop-coefficient curve used in the FAO–56 calculation is
shown in figure 1–6. The curve is made up of a set of piecewise linear
functions that define the overall shape of the crop-coefficient curve
during a growing season. Specification of this curve for each crop and
plant type (land-use type) in the model requires that a set of growth
stage lengths *L<sub>ini</sub>, L<sub>dev</sub>, L<sub>mid</sub>,
L<sub>late</sub>, and L<sub>fallow</sub>*, and a set of corresponding
crop coefficients that are included in the irrigation lookup table. The
growth stage lengths represent the number of days each growth stage
lasts. The growth stage lengths are added to the day of year at time of
planting which yields the day of year associated with the inflection
points defining the crop coefficient curve in figure 1–6. Note that
because the crop-coefficient values may have values greater than one,
the crop water requirement can easily exceed the potential or reference
ET values during peak growth periods. The length values that form the
crop-coefficient curve may be replaced with growing degree days. SWB
calculates, if desired, the growing degree days specific to each crop
type.

6.  Example simplified crop-coefficient curve.

## Single-Factor Crop Evapotranspiration

The calculation of actual evapotranspiration during nonstandard
(moisture-limited) growing conditions is to multiply the crop
evapotranspiration amount by a plant water stress factor whose value may
range between 0.0 and 1.0. The water stress factor declines toward zero
as the amount of available soil moisture decreases. The adjusted crop
evapotranspiration is given in equation 1–34.

(1–34)

where

is adjusted crop evapotranspiration,

is the plant water stress factor,

is the crop coefficient, and

is the reference or potential evapotranspiration.

The plant water stress factor is defined by the soil-moisture deficit
relative to two soil-moisture amounts—the readily available water and
total available water amounts. Total available water is defined as the
maximum amount of water that can be present within the root zone and is
calculated in SWB as:

(1–35)

where

is the total available water;

is the available water capacity, in inches per foot;

is the current rooting depth of vegetation, in feet;

is the soil-moisture amount at the field capacity of the soil, in inches
; and

is the soil-moisture amount at the permanent wilting point, in inches.

Readily available water is defined as the amount of water that can be
withdrawn by a plant from soil-moisture storage without the plant
suffering water stress. Readily available water may be defined as some
fraction of the total available water:

(1–36)

where

*RAW* is readily available water, and

is the fraction of total available water (*TAW*) that can be removed
from soil-moisture storage before a plant begins suffering from water
stress; *p* is called the plant\_stress\_depletion\_fraction in the SWB
irrigation lookup table.

The soil-moisture deficit is calculated as and represents the amount by
which the current daily soil moisture departs from the total available
moisture storage capacity of the soil. At soil-moisture deficits less
than the readily available water amount, plants are assumed to have
adequate available moisture for growth; plants are assumed to not have
water stress, and the plant water stress factor has a value of one.

Once soil-moisture deficit increases beyond the readily available water
amount, the plant water stress factor decreases linearly, reaching a
value of zero as the soil-moisture deficit approaches the total
available water value, or alternatively, as the daily soil-moisture
value reaches a value close to the wilting point. How the plant water
stress factor changes with changing soil-moisture amounts is shown in
figure 1–7.

7.  Plant water stress coefficient as a function of the soil-water
    content.

In the SWB irrigation lookup table, the
plant\_stress\_depletion\_fraction (*p*) defines the soil-moisture
conditions below which the actual to potential ET ratio begins to
decline toward zero. SWB uses this parameter to define the soil-water
content threshold at which plant water depletion begins to stress
vegetation and reduce evapotranspiration, as indicated in equation 1–37.

(1–37)

where

is the soil-moisture amount below which plant stress occurs, in inches;

is the soil-moisture amount at the permanent wilting point, in inches;

is the plant stress depletion fraction (dimensionless);

is the soil-moisture amount at the field capacity, in inches; and

is the total available soil-moisture storage amount, in inches.
