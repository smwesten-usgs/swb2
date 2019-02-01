title: Runoff

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

Runoff may be calculated by means of the Natural Resources Conservation
Service curve number method or may be related to precipitation values as
a set of user-defined ratios. This section describes both methods.

## Curve Number

The curve number method defines runoff in relation to the difference
between precipitation and an initial abstraction term. Conceptually,
this initial abstraction term represents the summation of all processes
that might act to reduce runoff, including interception by plants and
fallen leaves, depression storage, and infiltration (Woodward and
others, 2003). Equation 1–4 is used to calculate runoff volumes
(Woodward and others, 2003):

(1–4)

where

is runoff, in inches;

is daily precipitation, in inches;

is initial abstraction, in inches, the amount of precipitation that must
fall before runoff is generated; and

is the maximum soil-moisture holding capacity, in inches.

In the original curve number methodology, the initial abstraction term
is assumed to be *I<sub>a</sub> = 0.2S<sub>max</sub>*; SWB modifies the
initial abstraction term to that indicated in equation 1–5. The initial
abstraction (*I<sub>a</sub>*) term is related to a maximum storage term
(*S<sub>max</sub>*) as follows:

(1–5)

where

is initial abstraction, in langleys, the amount of precipitation that
must fall before runoff is generated; and

is the maximum soil-moisture holding capacity, in langleys.

This modification implies that runoff will begin for smaller
precipitation events than with the original method; this change has been
determined to result in more realistic continuous simulations (Woodward
and others, 2003).

The maximum storage term, in inches, is defined by the curve number for
the land cover and infiltration capacity that is being considered:

> (1–6)

where

is the maximum soil-moisture holding capacity, in inches; and

*CN* is the curve number.

For convenience, the curve number method assigns all soils surveyed in
the United States into one of four groups (A, B, C, D) on a continuum
ranging from A soils, which represent porous soils of high infiltration
capacity, to D soils, which represent fine textured soils of low
infiltration capacity (Hawkins and others, 2009). Assumed
characteristics of the four standard hydrologic soil groups are listed
in table 1–4.

4.  Characteristic and texture classes for the hydrologic soil groups.

\[From Hawkins and others (2009). \>, greater than; \<, less
than\]

| Hydrologic soil group | Characteristics                                                                                                                                                                                                                                                     | Texture                                                     | Infiltration rate (inches per hour) |
| --------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- | ----------------------------------------------------------- | ----------------------------------- |
| A                     | Low runoff potential and high infiltration rates, consisting primarily of deep, well- to excessively-drained sand or gravel.                                                                                                                                        | Sand, loamy sand, sandy loam                                | \>0.30                              |
| B                     | Moderate infiltration rates when wetted consisting of moderately deep to deep, moderately well-drained to well-drained soils of moderately fine to coarse texture.                                                                                                  | Silt loam or loam                                           | 0.15–0.30                           |
| C                     | Low infiltration rates when wetted consisting primarily of (1) soils that have an underlying layer impeding downward movement of water and (2) soils with moderately fine to fine texture.                                                                          | Sandy clay loam                                             | 0.05–0.15                           |
| D                     | Very low infiltration rates and high runoff potential when wetted, consisting primarily of clay soils with (1) high swelling potential, (2) high permanent water table, (3) clay or claypan near the surface, or (4) shallow soils over nearly impervious material. | Clay loam, silty clay loam, sandy clay, silty clay, or clay | \<0.05                              |

Curve numbers are user-defined; a separate curve number is supplied in
the SWB lookup table for each combination of land use and hydrologic
soil group. Curve numbers can range from 0 to 100, but the useful range
of curve numbers is far less depending on the hydrologic soil group. The
range of typical curve numbers for the four hydrologic soil groups are
listed in table 1–5; these values should be considered when assigning
curve numbers to the various land-use categories included in an SWB
lookup table.

5.  Range of typical curve numbers for the hydrologic soil groups.

\[From Hawkins and others (2009)\]

| Hydrologic soil group | Minimum | Central | Maximum |
| --------------------- | ------- | ------- | ------- |
| A                     | 25      | 51–68   | 77      |
| B                     | 48      | 62–77   | 86      |
| C                     | 65      | 70–84   | 91      |
| D                     | 73      | 77–88   | 94      |

Equations 1–5 and 1–6 can be used to back calculate the implied initial
abstraction values associated with the curve number ranges listed in
table 1–5. For a D soil, the maximum storage term (Smax) ranges from
about 0.63 to 3.7 inches. Use of an initial abstraction term of
0.05*S*<sub>max</sub> as suggested by Woodward and others (2003) implies
that between 0.03 and 0.18 inch of precipitation must fall before runoff
begins. For an A soil, the maximum storage term ranges from 3.0 to
30 inches, which implies that between 0.15 and 1.5 inches of
precipitation must fall before runoff begins.

### Published Curve Numbers

An attractive feature of the curve number method is that published
tables of curve numbers exist that serve as useful starting values for
use in the SWB lookup tables. A subset of the values published with one
of the curve number method publications (Cronshey and others, 1986) is
listed in table 1–6; curve numbers for more land uses are given in the
original publication along with details regarding appropriate choice and
application of those curve numbers. Other researchers may have published
curve numbers applicable to vegetation types not included in the
official publications; researchers have published curve numbers intended
for application to specific areas such as rangelands of the northern
plains of the United States (Hanson and others, 1981), croplands in the
southern plains of the United States (Hauser and Jones, 1991), and
pineapple and sugarcane fields in Hawaii (Cooley and Lane, 1982).

6.  Recommended initial curve numbers for select land uses and
    hydrologic soil groups.

\[From Cronshey and others, 1986. \<, less than; \>, greater than; --,
no
data\]

| Cover description  | Curve numbers for hydrologic soil group                 |
| ------------------ | ------------------------------------------------------- | ---------------------------------------------------------------------------- | -- | -- | -- | -- |
| Land use           | Specifics                                               | Hydrologic condition                                                         | A  | B  | C  | D  |
| Open space         | Lawns, parks, golf courses, cemeteries                  | Poor (grass cover \<50 percent)                                              | 68 | 79 | 86 | 89 |
|                    |                                                         | Fair (grass cover 50 percent to 75 percent)                                  | 49 | 69 | 79 | 84 |
|                    |                                                         | Good (grass cover \>75 percent)                                              | 39 | 61 | 74 | 80 |
| Impervious areas   | Paved parking lots, rooftops, driveways                 | \--                                                                          | 98 | 98 | 98 | 98 |
|                    | Paved streets and roads—with curb and gutter            | \--                                                                          | 98 | 98 | 98 | 98 |
|                    | Paved streets and roads—with open ditches               | \--                                                                          | 83 | 89 | 92 | 93 |
|                    | Gravel road                                             | \--                                                                          | 76 | 85 | 89 | 91 |
| Urban              | Commercial and business                                 | \--                                                                          | 89 | 92 | 94 | 95 |
|                    | Industrial                                              | \--                                                                          | 81 | 88 | 91 | 93 |
| Residential        | Lot size is \< 1/8 acre                                 | \--                                                                          | 77 | 85 | 90 | 92 |
|                    | Lot size is 1/8 to 1/4 acre                             | \--                                                                          | 61 | 75 | 83 | 87 |
|                    | Lot size is 1/4 to 1/3 acre                             | \--                                                                          | 57 | 72 | 81 | 86 |
|                    | Lot size is 1/3 to 1/2 acre                             | \--                                                                          | 54 | 70 | 80 | 85 |
|                    | Lot size is 1/2 to 1 acre                               | \--                                                                          | 51 | 68 | 79 | 84 |
|                    | Lot size is 1 to 2 acres                                | \--                                                                          | 46 | 65 | 77 | 82 |
| Newly graded areas | Pervious areas only, no vegetation                      | \--                                                                          | 77 | 86 | 91 | 94 |
| Fallow             | Bare soil                                               | \--                                                                          | 77 | 86 | 91 | 94 |
|                    | Crop residue cover                                      | Poor                                                                         | 76 | 85 | 90 | 93 |
|                    |                                                         | Good                                                                         | 74 | 83 | 88 | 90 |
| Row crops          | Straight row                                            | Poor                                                                         | 72 | 81 | 88 | 91 |
|                    |                                                         | Good                                                                         | 67 | 78 | 85 | 89 |
|                    | Straight row plus crop residue                          | Poor                                                                         | 71 | 80 | 87 | 90 |
|                    |                                                         | Good                                                                         | 64 | 75 | 82 | 85 |
|                    | Contoured                                               | Poor                                                                         | 70 | 79 | 84 | 88 |
|                    |                                                         | Good                                                                         | 65 | 75 | 82 | 86 |
|                    | Contoured plus crop residue cover                       | Poor                                                                         | 69 | 78 | 83 | 87 |
|                    |                                                         | Good                                                                         | 64 | 74 | 81 | 85 |
|                    | Contoured and terraced                                  | Poor                                                                         | 66 | 74 | 80 | 82 |
|                    |                                                         | Good                                                                         | 62 | 71 | 78 | 81 |
|                    | Contoured and terraced plus crop residue cover          | Poor                                                                         | 65 | 73 | 79 | 81 |
|                    |                                                         | Good                                                                         | 61 | 70 | 77 | 80 |
| Small grain        | Straight row                                            | Poor                                                                         | 65 | 76 | 84 | 88 |
|                    |                                                         | Good                                                                         | 63 | 75 | 83 | 87 |
|                    | Straight row plus crop residue                          | Poor                                                                         | 64 | 75 | 83 | 86 |
|                    |                                                         | Good                                                                         | 60 | 72 | 80 | 84 |
|                    | Contoured                                               | Poor                                                                         | 63 | 74 | 82 | 85 |
|                    |                                                         | Good                                                                         | 61 | 73 | 81 | 84 |
|                    | Contoured plus crop residue cover                       | Poor                                                                         | 62 | 73 | 81 | 84 |
|                    |                                                         | Good                                                                         | 60 | 72 | 80 | 83 |
|                    | Contoured and terraced                                  | Poor                                                                         | 61 | 72 | 79 | 82 |
|                    |                                                         | Good                                                                         | 59 | 70 | 78 | 81 |
|                    | Contoured and terraced plus crop residue cover          | Poor                                                                         | 60 | 71 | 78 | 81 |
|                    |                                                         | Good                                                                         | 58 | 69 | 77 | 80 |
| Pasture, grassland | Continuous forage for grazing                           | Poor (\<50 percent ground cover or heavily grazed with no mulch)             | 68 | 79 | 86 | 89 |
|                    |                                                         | Fair (50 percent to 75 percent ground cover and not heavily grazed)          | 49 | 69 | 79 | 84 |
|                    |                                                         | Good (\>75 percent ground cover and only lightly grazed)                     | 39 | 61 | 74 | 80 |
| Meadow             | Continuous grass, protected from grazing, mowed for hay | \--                                                                          | 30 | 58 | 71 | 78 |
| Brush              | Brush-weed-grass mixture, with brush the major element  | Poor (\<50 percent ground cover)                                             | 48 | 67 | 77 | 83 |
|                    |                                                         | Fair (50 percent to 75 percent ground cover)                                 | 35 | 56 | 70 | 77 |
|                    |                                                         | Good (\>75 percent ground cover)                                             | 30 | 48 | 65 | 73 |
| Woods              |                                                         | Poor (litter, small trees and brush destroyed by grazing or regular burning) | 45 | 66 | 77 | 83 |
|                    |                                                         | Fair (woods are grazed but not burned; some forest litter present)           | 36 | 60 | 73 | 79 |
|                    |                                                         | Good (woods protected from grazing; liter and brush adequately cover soil)   | 30 | 55 | 70 | 77 |

### Antecedent Runoff Conditions

SWB adjusts the user-specified curve numbers upward or downward
depending on how much precipitation has fallen in the previous 5-day
period. The amount of precipitation that has fallen in the previous
5-day period is used to describe soil-moisture conditions; three classes
of moisture conditions are defined and are called antecedent runoff
condition I, II, and III (table 1–7). The base (user-defined) curve
numbers are assumed to represent antecedent runoff condition II.

7.  Antecedent runoff conditions.

\[Nongrowing season and growing season antecedent runoff conditions are
given in
inches\]

| Runoff condition number | Description     | Nongrowing season | Growing season |
| ----------------------- | --------------- | ----------------- | -------------- |
| I                       | Dry             | 0.05              | 1.4            |
| II                      | Average         | 0.5–1.1           | 1.4–2.1        |
| III                     | Near saturation | 1.1               | 2.1            |

For example, assume that in the previous 5 days 1 inch of precipitation
fell on a grid cell; the runoff condition number would be I, and the
runoff curve number would be adjusted down from the base (user-supplied)
value. As another example, if a 5-day total of 1.5 inches of
precipitation were to fall on a grid cell, the antecedent runoff
condition would be III, and the curve number would be adjusted upwards
from the base (user-supplied) value.

If the soils are nearly saturated, as in antecedent runoff condition
III, the curve number for a grid cell is adjusted upward from antecedent
runoff condition II (eq. 1–7) to account for generally higher runoff
amounts observed when precipitation falls on saturated soil (Mishra and
Singh, 2003):

(1–7)

where

*CN* is the curve number,

*ARC(III)* is the antecedent runoff condition III, and

*ARCII* is the antecedent runoff condition II.

Conversely, when soils are dry, as in antecedent runoff condition I,
curve numbers are adjusted downward from antecedent runoff condition II
(eq. 1–8) in an attempt to reflect the increased infiltration rates of
dry soils (Mishra and Singh, 2003).

(1–8)

where

*CN* is the curve number,

*ARC(I)* is the antecedent runoff condition I, and

*ARCII* is the antecedent runoff condition II.

Between dry and nearly saturated conditions is antecedent runoff
condition II, which represents an average rainfall-runoff relation for
moderate soil-moisture conditions.

### Continuous Frozen Ground Index (CFGI)

Runoff from frozen ground is simulated by adjusting the base curve
numbers toward antecedent runoff condition III when frozen ground
conditions exist. Frozen ground conditions are tracked by use of a
continuous frozen ground index (CFGI; Molnau and Bissell, 1983):

(1–9)

where

*CFGI<sub>i</sub>* is continuous frozen ground index on the current day,
in Celsius degree days;

*A* is daily decay coefficient, unitless;

*CFGI<sub>i-1 </sub>* is continuous frozen ground index on the previous
day, in Celsius degree days;

*T* is daily mean air temperature, in degrees Celsius;

*K* is snow reduction coefficient, per centimeter; and

*D* is depth of snow on ground, in centimeters.

The values for the coefficients *A* and *K* are defined in the same
manner as described by Molnau and Bissel (1983):
*K*=0.5<sup>-centimeter</sup> for above-freezing periods,
*K*=0.08<sup>-centimeter</sup> for below-freezing periods, and *A*=0.97.
During conditions of no snow cover, the CFGI represents the running sum
by which the average air temperature deviates from the freezing point of
water; snow conditions cause the CFGI to grow or shrink at a slower
rate.

The CFGI is applied by allowing for a transition range to be applied
through which runoff enhancement ranges from negligible to strong
(Molnau and Bissell, 1983).

In the SWB code, a probability of runoff enhancement factor*,
P<sub>f</sub>* , is used to linearly interpolate between the curve
numbers at antecedent runoff condition II and antecedent runoff
condition III; *P<sub>f</sub>* is defined as given in equation 1–10.

(1–10)

where

*P<sub>f</sub>* is the probability that runoff will be enhanced by
frozen ground conditions;

*CFGI* is continuous frozen ground index, in Celsius degree days;

*UL* is the upper limit of the *CFGI*, above which frozen ground
conditions exist, in Celsius degree days; and

*LL* is the lower limit of the *CFGI*, below which frozen ground
conditions do not exist, in Celsius degree days.

If no values are assigned for and , default values of 9999 are assigned
to both, effectively disabling the CFGI option; this behavior is
unchanged from SWB version 1.0. If the CFGI option is used, Molnau and
Bissel (1983) recommend starting with a value of 83 Celsius degree days
for the upper limit and a value of 56 Celsius degree days for the lower
limit. These values were developed for the Pacific Northwestern United
States and may not be applicable elsewhere.

## Monthly Runoff Fraction Grid

Grids containing monthly runoff ratios relative to precipitation may be
used instead of the curve number approach. A series of grids may be
supplied as discussed in the user guide in appendix 2.

# Impervious Surface Runoff

Runoff from impervious surfaces may be simulated in a more detailed
manner by including a gridded dataset defining the proportion of each
grid cell that is comprised of impervious materials. Data may be
supplied as either a fraction (0.0–1.0) or percentage (0–100 percent) of
either pervious or impervious surface area.

Any cell that is assigned an impervious surface fraction or percent that
is greater than zero will operate in a fundamentally different way than
in original SWB code; in these cells, mass-balance calculations will be
performed on an additional impervious surface storage reservoir (fig.
1–2), the capacity of which is determined by the impervious surface
rainfall-retention depth.

2.  Conceptual diagram showing treatment of impervious surface runoff.

For grid cells with impervious surfaces, a temporary impervious storage
amount is determined using the following conditions:

(1–11)

where

*imperv\_stor<sub>temp</sub>* is the temporary impervious storage
amount,

is the daily rainfall amount,

is the daily snowmelt amount,

is the previous days’ impervious storage amount, and

is the daily evaporation of water from the impervious surface.

The final amount of water stored in the impervious storage reservoir is
dependent on the value of *imperv\_stor<sub>temp</sub>*. The values of
the daily ending impervious storage amount and the impervious storage
excess are calculated as listed in table 1–8 depending on whether the
temporary impervious storage amount is less than or greater than the
maximum impervious storage amount.

8.  Equations for determining impervious surface storage and impervious
    surface excess.

\[*imperv\_stor<sub>t</sub>*, daily ending impervious storage amount, in
inches; *imperv\_stor<sub>temp</sub>*, temporary impervious storage
amount, in inches; *imperv\_stor<sub>max</sub>*, maximum impervious
storage amount in inches; *f*, ratio of impervious surface fraction to
the pervious surface fraction (dimensionless); *imperv\_frac*, fraction
of the grid cell covered by impervious surfaces
(dimensionless)\]

| Condition | Value of *imperv\_stor<sub>t</sub>* | Value of *imperv\_stor<sub>excess</sub>* |
| --------- | ----------------------------------- | ---------------------------------------- |
|           |                                     | Zero                                     |
|           |                                     |                                          |

The resulting impervious surface excess is distributed to the pervious
fraction of the cell or is directed to a storm sewer for immediate
removal of the surface excess from the model domain. Specifying a
storm-drain capture fraction greater than zero will result in that
fraction of impervious surface excess being diverted and extracted from
the model domain. The model default is zero, or no, storm-drain capture
and zero, or no, fraction impervious surface. The storm-drain capture
fraction may be supplied in a lookup table or in gridded form.
