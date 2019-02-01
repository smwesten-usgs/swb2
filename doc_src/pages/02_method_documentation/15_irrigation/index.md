title: Irrigation

Once the FAO–56 procedure has been used to estimate the crop-water
requirements, the next step in the process of simulating
irrigation-water demand is to apply the water in a realistic manner. The
applied irrigation water is not a source that is explicitly modeled by
SWB; the irrigation water is supplied as an unspecified source external
to SWB. Linkage to the groundwater system could be accomplished by
postprocessing the SWB-calculated irrigation amounts and supplying those
values to the MODFLOW WELL package (McDonald and Harbaugh, 1988) along
with the SWB-calculated net-infiltration values.

The first step in simulating irrigation application is to compare the
depletion fraction value to the user-specified maximum allowable
soil-moisture deficit. The depletion fraction is calculated as follows:

(1–38)

where

is the soil-moisture amount calculated on the previous day, in inches;

is the soil-moisture amount at the permanent wilting point of the soil,
in inches; and

is the soil-moisture amount at the field capacity of the soil, in
inches.

The depletion fraction in equation 1–38 takes on a value of zero when
soil-moisture values are at the soil’s field capacity and a value of one
when soil-moisture values reach the wilting point. Like the depletion
fraction, the maximum allowable deficit takes on values in the range
from 0 to 1. A value of zero indicates that no soil-moisture deficit is
tolerable for the crop and will result in almost continuous irrigation.
A value of one effectively indicates that depletion of the soil
reservoir is tolerable and will result in irrigation almost never being
applied. The maximum allowable depletion fraction is often set equal to
the plant water stress fraction (see table 1–10 for definition). Setting
the maximum allowable depletion equal to the plant stress depletion
fraction in simulated irrigation being applied whenever the soil
moisture declines below the threshold soil-moisture value at which plant
growth begins to become impaired.

Once a cell’s soil-moisture status has triggered a need for simulated
irrigation, SWB has a few rules that may be specified per land-use or
crop type. These rules determine how much of the soil-moisture deficit
is eliminated in the simulated irrigation event. The following is a list
of these rules.

*Restore soil moisture to field capacity*—Complete elimination of
soil-moisture deficit on a cell by cell basis. This option calculates
the amount of water to be applied as the difference between the maximum
soil-moisture value and the soil-moisture value from the previous day.
Thus, this amount ignores the current day’s water-balance components;
the same irrigation amount will be calculated regardless of rainfall
conditions.

*Restore soil moisture to some fraction of field capacity—*Restore soil
moisture to some specified tolerable level of soil-moisture deficit
(deficit irrigation). This option calculates the amount of water to be
applied as the difference between the soil moisture at some preset
deficit amount and the soil-moisture value from the previous day. The
parameter value supplied to SWB to define this deficit amount is best
described as the fraction of the maximum soil moisture that should be
used as the baseline value in the calculation. In other words,

(1–39)

*Apply fixed amount of irrigation*— Apply the same, constant amount of
water once the soil-moisture deficit exceeds the maximum allowable
deficit. Many irrigators have sized their equipment to handle
application events of average size; for example, a center-pivot
irrigation setup might only be capable of delivering water within a
narrow range of values. Under this option, a set amount of water is
applied to the cell. If the set amount brings the soil moisture to a
value in excess of field capacity, a recharge event will be triggered.

*Apply demand-based amount on a prescribed monthly schedule*— This
option is similar to the “restore soil moisture to field capacity”
option, except that the calculated irrigation amount accounts for the
daily or monthly rainfall and runoff amount and only is applied on a set
schedule. This option was extracted from the Hawaii Water Budget code
and is designed to simulate the unique irrigation conditions in the
Pacific Islands; the calculation is dependent on the monthly, rather
than daily, datasets that generally are available in the Pacific
Islands.

The irrigation amount for each day that irrigation is scheduled is
calculated as:

(1–40)

where

is the crop evapotranspiration value, the amount of water the plant
would use if not water limited;

is the number of irrigation days in the month;

is the monthly total runoff amount;

is the monthly total rainfall amount; and

is the number of days in the month.

The number of irrigation days in the month is determined by the monthly
irrigation schedule parameter, which is a pattern of zeros and ones. The
monthly irrigation schedule parameter values are 31 characters long,
which determine the timing of irrigation (table 1–11).

11. Example of monthly irrigation schedule parameter values for several
    irrigation frequencies.

| Irrigation frequency | Monthly irrigation schedule value |
| -------------------- | --------------------------------- |
| Weekly (approximate) | 1000001000001000001000001000000   |
| Every other day      | 0101010101010101010101010101010   |
| Every 3 days         | 0010010010010010010010010010010   |

Irrigation can be defined for whole crop and vegetation types; however,
at times, more control is needed regarding the simulation of irrigation
at a particular location. A supplemental irrigation mask grid can be
provided to SWB to fine tune which portions of various crop and
vegetation types are given simulated irrigation. A value of one means
the cell is allowed to receive irrigation, whereas a value of zero
blocks the cell from receiving irrigation water. An example of an
irrigated lands mask is shown in figure 1–8.

8.  Example of irrigated lands mask.
