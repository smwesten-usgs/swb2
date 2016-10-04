
### Interception ### {#interception}

[TOC]

#### Bucket {#bucket}

The "bucket" model of interception is the original interception process module that was coded into SWB. The bucket model simply assumes that a set and constant amount of rainfall or snowfall must fall before the soil will receive any precipitation.

#### Gash {#gash}

Another option in SWB for calculating canopy interception is a modified version of the model described by Gash and others [-@gash_analytical_1979], herein referred to as the Gash model. Using this approach, canopy evaporation for a given day and location depends on precipitation, forest structure, and the mean rates of evaporation and precipitation. The Gash model was modified so that (1) precipitation includes rainfall and fog interception, instead of rain only, and (2) water cannot be stored on the forest canopy for more than a day. The forest structure is characterized in terms of canopy cover, canopy capacity, trunk-storage capacity, and the proportion of precipitation diverted to stemflow. Canopy cover, $c$ , is the fraction of a forested area that is covered by leaves, stems, and branches of trees. Canopy capacity, $S$, is the depth of water left on the canopy when rainfall and throughfall have ceased [@gash_application_1978]. Evaporation of water from tree trunks is accounted for using the proportion of precipitation that is diverted to stemflow, $p$, and trunk-storage capacity, $k$, which is considered in terms of an equivalent depth of precipitation. The last parameter needed for the Gash model is the ratio of the mean evaporation rate to the mean precipitation rate during saturated conditions, $V$.

To calculate canopy interception, the first step is to determine the minimum depth of precipitation necessary to saturate the forest canopy, P’, which is calculated on the basis of equation 2 in Gash and others (1995) as:

$$P' =  - \frac{S}{{c \cdot V}}\ln \left( {1 - V} \right)$$ {#eq:gash01}

where

$P’$	=	precipitation necessary to saturate the canopy [L],

$S$	=	canopy capacity per unit of ground area [L] (a constant),

$c$	=	canopy cover per unit of ground area [dimensionless], and

$V$	=	ratio of mean evaporation rate to mean precipitation rate during saturated conditions [dimensionless].  

On the basis of the revised analytical form of the Gash model presented in table 1 of Gash and others (1995), canopy interception for a given day, (CE)i, is calculated for three canopy conditions as follows:

for $P_i < P’$,

$(CE)_i  = c \cdot  P_i$ ,				

for ${P_i} \geqslant {P’}$ and ${P_i} \leqslant  frac{k}{p}$,

$(CE)_i  = c \cdot  {P’}+ c \cdot  V \cdot  (P_i – P’) + p \cdot  P_i$,		

for ${P_i} \geqslant {P'}$ and ${P_i} > \frac{k}{p}$,

$(CE)_i  = c \cdot  P’ + c \cdot  V \cdot  ({P_i} – P’) + k$,

where:

$k$	=	trunk-storage capacity [L] (a constant), and

$p$	=	proportion of precipitation diverted to stemflow [dimensionless].

Advantages of the Gash model are: (1) it accounts for gaps in the forest canopy, which allows for a sparse canopy to be differentiated from a dense canopy; (2) canopy interception during a period of precipitation is dependent on the amount of precipitation during that period; and (3) the Gash model can account for spatial differences in climate. Disadvantages of the Gash model are that it is theoretical and may be difficult to adequately parameterize.

#### Horton {#horton}

Robert Horton made countless observations at his hydrologic laboratory in the early 1900's. One of the hydrologic processes to receive his attention is that of canopy interception. The Horton model begins with a "bucket" that must be filled regardless of total storm volume, and adds a linear relation that produces an increasing canopy interception value proportional to increasing storm volume [@horton_rainfall_1919].

Table: Horton's working equations for estimating intercepted rainfall. {#tbl:horton_intcp}

Vegetation Type                     | Working Equation
------------------------------------|----------------------------------:
Orchard                             | $J=0.04 + 0.18P_s$
Chestnut, hedge and open            | $J=0.04+0.20P_s$
Chestnut, in woods                  | $J=0.06+0.15P_s$
Ash, hedges and open                | $J=0.015+0.23P_s$
Ash, in woods                       | $J=0.02+0.18P_s$
