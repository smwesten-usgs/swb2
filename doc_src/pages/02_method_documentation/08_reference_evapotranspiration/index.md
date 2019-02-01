title: Reference Evapotranspiration

Thornthwaite (1948) classified the world's climate and observed that
"...there is a distinction...between the amount of water that actually
transpires and evaporates and that which would transpire and evaporate
if it were available.” When water supply increases, as in a desert
irrigation project, evapotranspiration rises to a maximum that depends
only on the climate. This we may call ‘potential evapotranspiration,’ as
distinct from actual evapotranspiration.”

At about the same time that Thornthwaite (1948) was making climate
observations, agronomists were struggling with the notion of plant
evapotranspiration. The Blaney-Criddle and Hargreaves-Samani methods
attempt to link a method to a specific vegetation type and condition or
reference crop. The potential evapotranspiration associated with a
specific crop may be considered to be a reference evapotranspiration
amount. The Blaney-Criddle method, for example, links the potential
evapotranspiration to a 80–150 millimeters tall actively growing
green-grass cover, “completely shading the ground and not short of
water” (Allen and Pruitt, 1986).

SWB provides the Jensen-Haise (Jensen and Haise, 1963) and the
Hargreaves-Samani (Hargreaves and Samani, 1985) methods for estimating
potential or reference evapotranspiration. The Jensen-Haise method for
estimating potential evapotranspiration (ET) was developed with
evapotranspiration data for several crop types common to the
southwestern United States. The Hargreaves-Samani method for estimating
reference evapotranspiration (ET<sub>0</sub>) was developed with
evapotranspiration data pertaining to a reference crop of fescue grass
of known length. In SWB, both of these method rely on air temperature
observations (table 1–9) to estimate the amount of extraterrestrial
solar radiation that reaches the crop surface. Both methods will likely
return similar values. The Jensen-Haise method may be more appropriate
for sites in the southwestern United States. Users should examine the
SWB-estimated potential evapotranspiration amounts and compare them to
estimates published by university agricultural extension services and
others.

9.  Data requirements for the reference/potential evapotranspiration
    estimation methods included in SWB.

<table>
<thead>
<tr class="header">
<th>Method</th>
<th>Minimum air temperature (degrees Fahrenheit)</th>
<th>Maximum air temperature (degrees Fahrenheit)</th>
<th><p>Gridded monthly</p>
<p>estimates</p>
<p>(inches/month)</p></th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td>Jensen-Haise</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr class="even">
<td>Hargreaves-Samani</td>
<td></td>
<td></td>
<td></td>
</tr>
<tr class="odd">
<td>Monthly gridded</td>
<td></td>
<td></td>
<td></td>
</tr>
</tbody>
</table>

The distinction between potential ET and reference ET<sub>0</sub> is
more important if the FAO–56 crop coefficients are to be applied as
modifiers to the potential or reference evapotranspiration values. Crop
coefficients are often determined and published with a particular
reference crop in mind. The crop coefficients published in Allen and
others (1998) are developed with the same reference crop Festuca altaica
(Alta fescue) grass that was used to develop the Hargreaves-Samani
method. Solar radiation at the top of the atmosphere (extraterrestrial
solar radiation) is calculated for both the Jensen-Haise and the
Hargreaves-Samani methods by making use of standard estimation equations
that take into account Earth’s position and tilt relative to the sun and
the position of the grid cell upon the Earth. The equations are applied
using the latitude and longitude of each grid cell for each day of the
year; the form of the equations used to calculate extraterrestrial solar
radiation are in Meeus (1991).

## Jensen-Haise Method

Jensen and Haise (1963) developed an empirical method that related
potential evapotranspiration to solar radiation and air temperature for
several crops grown in the southwestern United States—crop types
included alfalfa, oats, cotton, and winter wheat. The equation is as
follows:

(1–12)

where

is the daily potential evapotranspiration, in inches;

is the mean daily air temperature, in degrees Fahrenheit; and

is the solar radiation received at the crop surface, in inches per day.

Solar radiation at the crop surface is estimated as a function of the
percentage of total possible sunshine that was received on a given day:

(1–13)

where

is the solar radiation received at the crop surface, in inches per day;

is the fraction of total solar radiation received on an overcast day
(dimensionless), often 0.25;

is the fraction of total solar radiation received on a clear day
(dimensionless), often 0.75;

is the amount of daily sunshine as a fraction of total possible sunshine
(dimensionless); and

is the extraterrestrial solar radiation, in inches per day.

The use of equation 1–13 requires data on the fraction of total
sunshine, which is usually difficult to find on a consistent basis. SWB
uses minimum and maximum air temperature to estimate the fraction of
total possible sunshine (Allen and Pruitt, 1986):

(1–14)

where

is the amount of daily sunshine as a fraction of total possible sunshine
(dimensionless),

is the maximum daily air temperature, in Kelvin; and

is the minimum daily air temperature, in Kelvin.

## Hargreaves-Samani Method

The Hargreaves and Samani equation was developed as a way to calculate
reference evapotranspiration using only limited data. The coefficients
were developed with data derived from a weighing lysimeter planted with
Alta fescue grass (Hargreaves and Samani, 1985). The equation is as
follows:

(1–15)

where

is the grass-reference evapotranspiration, in millimeters per day;

is the mean daily air temperature, in degrees Celsius;

is the maximum air temperature, in degrees Celsius;

is the mean daily air temperature, in degrees Celsius; and

is extraterrestrial solar radiation, in millimeters per day.

## Monthly Grid

If available, users also may use gridded daily or monthly estimates of
reference or potential evapotranspiration. These gridded estimates allow
SWB to be run using more sophisticated evapotranspiration estimates,
including satellite-derived estimates.
