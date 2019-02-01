title: Soil Moisture Retention

Actual evapotranspiration is the soil moisture that can be extracted
from a soil of a given soil-moisture condition; by definition, actual
evapotranspiration will be equal to or less than the potential
evapotranspiration. In the days following a rainstorm, soil moisture is
close to field capacity, and moisture is evaporated from bare soil and
transpired by plants at rates close to the maximum rate sustainable for
given climatic conditions. Assuming no further precipitation, in
subsequent days the evaporation and transpiration rates decrease as
remaining soil moisture is held more tightly within the soil matrix
(Dunne and Leopold, 1978).

Common terms regarding these concepts are listed and defined in table
1–10. More detail about each of these concepts is in Allen and others
(1998).

10. Definitions of common terms used in describing soil-moisture
    retention and actual
evapotranspiration.

| Term           | Units        | Definition                                                                                                                                                                                                                                                     |
| -------------- | ------------ | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Field capacity | inches       | Soil-moisture value at which the soil matrix is nearly saturated and gravity drainage from the soil ceases.                                                                                                                                                    |
| Wilting point  | inches       | Soil-moisture value below which plants are incapable of extracting further soil moisture for growth. Sometimes called the permanent wilting point because irreversible plant stress and subsequent death are common once the soil reaches this moisture value. |
|                | inches       | Soil-moisture amount equivalent to a soil’s field capacity.                                                                                                                                                                                                    |
|                | inches       | Soil-moisture amount below which most plants experience irreversible wilting.                                                                                                                                                                                  |
|                | inches       | Total available water. This is the amount of water available in the soil for potential plant growth and can be calculated as .                                                                                                                                 |
|                | inches       | Readily available water. In the FAO–56 methodology, the readily available water is available to plants at a rate equal to potential evapotranspiration.                                                                                                        |
|                | \[unitless\] | Plant water stress depletion fraction. This is the fraction of total available water that may be used by plants before they begin to experience water stress (and subsequent reduction in actual evapotranspiration rates); .                                  |
|                | inches       | Threshold soil moisture. This is the soil moisture below which plants begin to experience water stress; .                                                                                                                                                      |

A way of simulating decreasing rates of soil-moisture evapotranspiration
is to assume that the actual evapotranspiration is some function of the
potential or reference evapotranspiration and the current soil-moisture
amount (eq. 1–16).

(1–16)

where

is the actual evapotranspiration, in inches;

is the potential evapotranspiration, in inches;

is a function of arbitrary shape;

is the current soil-moisture amount, in inches;

is the soil-moisture amount at the permanent wilting point, in inches;
and

is the soil-moisture amount at field-capacity, in inches.

Many different functions have been developed to relate potential
evapotranspiration to actual evapotranspiration. Veihmeyer (1938)
suggested that soil evapotranspiration is equal to potential
evapotranspiration regardless of how close the soil moisture is to the
wilting point. Zahner (1967) proposed a set of relations that changed
depending on whether the soils in question were predominantly sand,
loam, or clay. The relation included in FAO–56 (Allen and others, 1998)
assumes that actual evapotranspiration and potential evapotranspiration
are equal up to some critical soil-moisture level. At the point of
critical soil moisture (coinciding with the onset of plant water
stress), the ratio of actual to potential ET decreases until the soil
moisture equals the wilting point where the ratio of actual to potential
ET reaches a value of zero. At soil moisture equal to the field
capacity, Thornthwaite and Mather (1957) considered the ratio of actual
to potential ET to be equal to one, decreasing linearly to zero at a
soil moisture equal to the wilting point.

The two soil-moisture retention methods implemented in SWB are discussed
in this section; one method developed by Thornthwaite and Mather (1957)
and the other method included in the FAO–56 approach (Allen and others,
1998).

## Thornthwaite-Mather Method

In the late 1940s and early 1950s, C.W. Thornthwaite and his associates
studied plant growth and water utilization. As a result of this work,
Thornthwaite observed that the relation between the actual ET to
potential ET ratio and the soil moisture was linear (fig. 1–4).

4.  Thornthwaite-Mather relation between actual and potential
    evapotranspiration.

The first versions of SWB included full-tabularized versions of the
soil-moisture retention function, along with methods to interpolate
among the various table values. The original published method
(Thornthwaite and Mather, 1957) also introduced a variable, accumulated
potential water loss (APWL), to track the cumulative unmet potential
evapotranspiration; this term APWL was developed in an age before easy
access to computers and calculators; when used with the table values,
this set of tabulated, APWL values made calculation of the daily water
balance simpler. SWB updates the soil-moisture value by means of the
relation derived in equations 1–17 through 1–25. Daily soil moisture may
be estimated from this relation by first defining the instantaneous soil
evapotranspiration as equal to the change in soil-moisture storage:

(1–17)

where

is the instantaneous actual evapotranspiration, and

is the rate of change in soil moisture relative to time.

The relation shown in figure 1–4 can be used to define a function
relating actual and potential evapotranspiration as:

(1–18)

where

is the instantaneous actual evapotranspiration,

is the instantaneous potential evapotranspiration,

is the soil moisture, and

is the soil-moisture value at field capacity.

Equation 1–17 and equation 1–18 can be set equal to one another, and the
terms can be rearranged and integrated to yield an estimate of the
current daily soil moisture:

, (1–19)

, and (1–20)

(1–21)

where

is the rate of change in soil moisture relative to time,

is the instantaneous potential evapotranspiration,

is the soil moisture,

is the soil-moisture value at field capacity,

is the change in soil-moisture, and

*dt* is the change in time.

The integral of the instantaneous potential ET during the course of a
day is equal to the total daily reference ET<sub>0</sub> value\[. An
interim soil-moisture value may be defined as:

(1–22)

where

is the interim soil moisture, and

is the soil moisture on the previous day.

The integral of soil moisture is evaluated from to :

(1–23)

where

is the soil moisture on the current day,

is the interim soil moisture,

is the reference evapotranspiration, and

is the soil moisture at the field capacity for the soil.

Exponentiating both sides and solving for the current soil moisture
yields:

(1–24)

where terms are the same as those defined for equation 1–23.

The actual ET value is the difference between the interim and final
soil-moisture values:

(1–25)

## FAO–56 Method

The FAO–56 method for determining actual evapotranspiration considers
the process in two phases (fig. 1–5). In the first phase, soil-moisture
levels are between a threshold soil-moisture level and field capacity,
and the actual ET is assumed to be equal to the reference
ET<sub>0</sub>. At soil-moisture levels below the threshold level, the
ratio between actual and reference ET<sub>0</sub> is assumed to decrease
linearly, with the ratio having a value of zero as the soil moisture
reaches the permanent wilting point.

5.  FAO–56 relation between actual and reference evapotranspiration.

The first step toward estimating the daily actual evapotranspiration is
to eliminate evapotranspiration from the water balance equation and
calculate an interim soil-moisture:

(1–26)

where

is the interim soil moisture, and

is the soil moisture on the previous day.

The relation shown in figure 1–5 may be used to update the interim soil
moisture by considering the following three cases:

1.  , which eliminates the sloped part of figure 1–5;

2.  , which implies that actual ET equals reference ET<sub>0</sub> for
    all or part of the day; and

3.  , which means that the actual ET is some fraction of reference
    ET<sub>0</sub> for the entire day.

In the first case, actual ET equals reference ET<sub>0</sub>, so the new
soil-moisture value is:

(1–27)

where

is the interim soil-moisture amount, in inches; and

is the daily reference evapotranspiration amount, in inches.

The second case is a linear combination of the first and third cases,
and can be defined as the fraction of the day that the soil-moisture
value would exceed the threshold value:

(1–28)

where

is the interim soil-moisture amount, in inches;

is the soil-moisture amount below which plant stress occurs, in inches;
and

is the daily reference evapotranspiration amount, in inches.

The new soil-moisture value for the current day can then be determined
as:

(1–29)

where

is the soil-moisture amount below which plant stress occurs, in inches;

is the fraction of the day that soil moisture exceeds the threshold (eq.
1–28), in inches; and

is the daily reference evapotranspiration amount, in inches.

The actual ET value is then the difference between the interim and final
soil-moisture values:

(1–30)

where

is the interim soil-moisture amount, in inches; and

is the daily soil-moisture amount, in inches.

In the third case, new soil-moisture value for the current day can be
determined as:

(1–31)
