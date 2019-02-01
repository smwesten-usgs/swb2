title: Interception

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

Interception of rain or snow by vegetation is an important part of the
water budget; estimates of interception as a percentage of total
precipitation range from 20 to 50 percent in some forested areas
(Savenije, 2004). The three interception methods included in SWB are
bucket, Gash, and Horton.

## Bucket

The bucket method of interception is the original interception process
method that was coded into SWB. The bucket method assumes that a
constant, user-defined amount of rainfall or snowfall must fall before
the soil will receive any precipitation.

## Gash

Another option in SWB for calculating canopy interception is a modified
version of the method described by Gash (1979). Using this approach,
canopy evaporation for a given day and location depends on forest
structure and the mean rates of evaporation and precipitation. The Gash
method was modified so that (1) precipitation includes rainfall and fog
interception, instead of rain only and (2) water cannot be stored on the
forest canopy for more than a day (Izuka and others, 2010). The forest
structure is characterized in terms of canopy cover, canopy capacity,
trunk-storage capacity, and the proportion of precipitation diverted to
stemflow. Canopy cover, *c*, is the fraction of a forested area that is
covered by leaves, stems, and branches of trees. Canopy capacity, *S*,
is the depth of water left on the canopy when rainfall and throughfall
have ceased (Gash and Morton, 1978). Evaporation of water from tree
trunks is accounted for using the fraction of precipitation that is
diverted to stemflow, *p*, and trunk-storage capacity, *k*, which is
considered in terms of an equivalent depth of precipitation. The last
parameter needed for the Gash model is the ratio of the mean evaporation
rate to the mean precipitation rate during saturated conditions, *V*.

To calculate canopy interception, the first step is to determine the
minimum depth of precipitation necessary to saturate the forest canopy,
*P<sub>sat</sub>*. Forest canopy is calculated as

(1–1)

where

*P<sub>sat</sub>* is precipitation necessary to saturate the canopy, in
inches;

*S* is canopy storage capacity, in inches (a constant);

*c* is fraction of ground area covered by canopy (dimensionless);

*V* is ratio of mean evaporation rate to mean precipitation rate during
saturated conditions (dimensionless).

On the basis of the revised analytical form of the Gash model presented
in table 1 of Gash and others (1995), canopy interception for a given
day, *I*, is calculated for three canopy conditions as listed in table
1–2.

2.  Equations for calculating canopy interception for various
    precipitation conditions.

\[, trunk storage capacity \[L\] (a constant); , proportion of
precipitation diverted to stemflow (dimensionless); , fraction of ground
surface covered by vegetative canopy (dimensionless); , ratio of mean
evaporation rate to mean precipitation rate (dimensionless); , total
daily precipitation (inches); , precipitation amount required to fully
saturate the forest canopy; , canopy interception (inches) \]

| Condition | Interception calculation |
| --------- | ------------------------ |
|           |                          |
| and       |                          |
| and       |                          |

Advantages of the Gash method are as follows: (1) the method accounts
for gaps in the forest canopy, which allows for a sparse canopy to be
differentiated from a dense canopy; (2) the canopy interception during a
period of precipitation is dependent on the amount of precipitation
during that period; and (3) the method can account for spatial
differences in climate. Disadvantages of the Gash model are that the
method is theoretical and may be difficult to parameterize.

## Horton

Robert Horton made countless observations of various hydrological
processes at his hydrologic laboratory in the early 1900s, including
observations of canopy interception. The Horton model begins with a
bucket that must be filled regardless of total storm volume and adds a
linear relation that produces an increasing canopy interception value
proportional to increasing storm volume (Horton, 1919). Some of Horton’s
working equations that are based on his analysis of rainfall and
interception are listed in table 1–3. These relations represent an
improvement from the bucket model approach, which does not consider the
total daily precipitation.

3.  Horton's working equations for estimating intercepted rainfall.

\[, interception, in inches; , precipitation received during a storm
event, in inches; , plant height, in
feet\]

| Vegetation type                                          | Working equation |
| -------------------------------------------------------- | ---------------- |
| Orchard                                                  |                  |
| Chestnut, hedge and open                                 |                  |
| Chestnut, in woods                                       |                  |
| Ash, hedges and open                                     |                  |
| Ash, in woods                                            |                  |
| Beech, hedges and open                                   |                  |
| Beech, woods                                             |                  |
| Oak, hedges and open                                     |                  |
| Oak, woods                                               |                  |
| Maple, hedges and open                                   |                  |
| Maple, woods                                             |                  |
| Willow shrubs                                            |                  |
| Elm, hedges and open                                     |                  |
| Elm, woods                                               |                  |
| Basswood, hedges and open                                |                  |
| Basswood, woods                                          |                  |
| Hemlock and pine, hedges and open                        |                  |
| Hemlock and pine, woods                                  |                  |
| Clover and meadow grass                                  |                  |
| Forage, alfalfa, vetch, millet, etc.                     |                  |
| Beans, potatoes, cabbage, and other small-hilled crops   |                  |
| Tobacco                                                  |                  |
| Cotton                                                   |                  |
| Buckwheat                                                |                  |
| Corn, planted in hills or rows                           |                  |
| Fodder corn, sorghum, Kaffir corn, etc., sowed in drills |                  |

To use Horton’s working equations in an SWB simulation, the user must
supply the constant, slope, and exponent as given in table 1–3. No
attempt is made to incorporate plant height; the user must modify the
equation with the approximate plant height. Thus, the equation for
8-foot corn would be *I=(0.005 + 0.08P<sub>s</sub>)∙8 = 0.04 +
0.04P<sub>s</sub>*; the constant, slope, and exponent supplied to SWB
would be 0.04, 0.04, and 1.0, respectively.
