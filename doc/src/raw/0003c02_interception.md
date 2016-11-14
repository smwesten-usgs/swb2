
### Interception ### {#interception}

[TOC]

The interception process is sometimes overlooked in hydrological models, but can amount to a significant part of the water budget [@gerrits_role_2010; @savenije_importance_2004]. The original SWB code used a simple “bucket” model to estimate the amount of recharge. This approach ignores the relation between precipitation amounts and total intercepted (and evaporated) water in the canopy. Two additional interception process formulations have been added in an attempt to model this part of the water budget more accurately.

#### Bucket {#bucket}

The "bucket" model of interception is the original interception process module that was coded into SWB. The bucket model simply assumes that a set and constant amount of rainfall or snowfall must fall before the soil will receive any precipitation.

#### Gash [SWB 2.0 only]{#gash}

Another option in SWB for calculating canopy interception is a modified version of the model described by Gash and others [-@gash_analytical_1979], herein referred to as the Gash model. Using this approach, canopy evaporation for a given day and location depends on precipitation, forest structure, and the mean rates of evaporation and precipitation. The Gash model was modified so that (1) precipitation includes rainfall and fog interception, instead of rain only, and (2) water cannot be stored on the forest canopy for more than a day. The forest structure is characterized in terms of canopy cover, canopy capacity, trunk-storage capacity, and the proportion of precipitation diverted to stemflow. Canopy cover, $c$ , is the fraction of a forested area that is covered by leaves, stems, and branches of trees. Canopy capacity, $S$, is the depth of water left on the canopy when rainfall and throughfall have ceased [@gash_application_1978]. Evaporation of water from tree trunks is accounted for using the proportion of precipitation that is diverted to stemflow, $p$, and trunk-storage capacity, $k$, which is considered in terms of an equivalent depth of precipitation. The last parameter needed for the Gash model is the ratio of the mean evaporation rate to the mean precipitation rate during saturated conditions, $V$.

To calculate canopy interception, the first step is to determine the minimum depth of precipitation necessary to saturate the forest canopy, $P_{sat}$, which is calculated on the basis of equation 2 in Gash and others (1995) as:

$$P_{sat} =  - \frac{S}{{c \cdot V}}\ln \left( {1 - V} \right)$$ {#eq:gash01}

where

$P_{sat}$	=	precipitation necessary to saturate the canopy [L],

$S$	=	canopy capacity per unit of ground area [L] (a constant),

$c$	=	canopy cover per unit of ground area [dimensionless], and

$V$	=	ratio of mean evaporation rate to mean precipitation rate during saturated conditions [dimensionless].  

On the basis of the revised analytical form of the Gash model presented in table 1 of Gash and others (1995), canopy interception for a given day, (CE)i, is calculated for three canopy conditions as follows:

for $P_i < P_{sat}$,

$(CE)_i  = c \cdot  P_i$ ,				

for ${P_i} \geqslant {P_{sat}}$ and ${P_i} \leqslant  \frac{k}{p}$,

$(CE)_i  = c \cdot  {P_{sat}}+ c \cdot  V \cdot  (P_i – P_{sat}) + p \cdot  P_i$,		

for ${P_i} \geqslant {P_{sat}}$ and ${P_i} > \frac{k}{p}$,

$(CE)_i  = c \cdot  P_{sat} + c \cdot  V \cdot  ({P_i} – P_{sat}) + k$,

where:

$k$	=	trunk-storage capacity [L] (a constant), and

$p$	=	proportion of precipitation diverted to stemflow [dimensionless].

Advantages of the Gash model are: (1) it accounts for gaps in the forest canopy, which allows for a sparse canopy to be differentiated from a dense canopy; (2) canopy interception during a period of precipitation is dependent on the amount of precipitation during that period; and (3) the Gash model can account for spatial differences in climate. Disadvantages of the Gash model are that it is theoretical and may be difficult to adequately parameterize.

#### Horton {#horton}

Robert Horton made countless observations at his hydrologic laboratory in the early 1900's. One of the hydrologic processes to receive his attention is that of canopy interception. The Horton model begins with a "bucket" that must be filled regardless of total storm volume, and adds a linear relation that produces an increasing canopy interception value proportional to increasing storm volume [@horton_rainfall_1919].

: Horton's working equations for estimating intercepted rainfall. {#tbl:horton_intcp}

Vegetation Type	                    |  Working Equation
------------------------------------|------------------------
Orchard	                            | $J = 0.04 + 0.18{P_s}$
Chestnut, hedge and open	          | $J = 0.04 + 0.20{P_s}$
Chestnut, in woods	                | $J = 0.06 + 0.15{P_s}$
Ash, hedges and open	              | $J = 0.015 + 0.23{P_s}$
Ash, in woods	                      | $J = 0.02 + 0.18{P_s}$
Beech, hedges and open	            | $J = 0.03 + 0.23{P_s}$
Beech, woods	                      | $J = 0.04 + 0.18{P_s}$
Oak, hedges and open	              | $J = 0.03 + 0.22{P_s}$
Oak, woods	                        | $J = 0.05 + 0.18{P_s}$
Maple, hedges and open	            | $J = 0.03 + 0.23{P_s}$
Maple, woods	                      | $J = 0.04 + 0.18{P_s}$
Willow shrubs	                      | $J = 0.02 + 0.4{P_s}$
Elm, hedges and open	              | $J = 0.03 + 0.23P_S^{0.5}$
Elm, woods	                        | $J = 0.04 + 0.18P_S^{0.5}$
Basswood, hedges and open	          | $J = 0.03 + 0.13P_S^{0.5}$
Basswood, woods	                    | $J = 0.05 + 0.1P_S^{0.5}$
Hemlock and pine, hedges and open	  | $J = 0.03 + 0.2P_S^{0.5}$  
Hemlock and pine, woods	            | $J = 0.05 + 0.2P_S^{0.5}$
Clover and meadow grass	            | $J = \left( {0.005 + 0.08{P_S}} \right)h$
Forage, alfalfa, vetch, millet, etc.| $J = \left( {0.01 + 0.1{P_S}} \right)h$  
Beans, potatoes, cabbage, and other small-hilled crops       | $J = \left( {0.02 + 0.15{P_S}} \right)h$
Tobacco	                            | $J = \left( {0.01 + 0.08{P_S}} \right)h$
Cotton	                            | $J = \left( {0.015 + 0.1{P_S}} \right)h$
Buckwheat	                          | $J = \left( {0.01 + 0.12{P_S}} \right)h$
Corn, planted in hills or rows	    | $J = \left( {0.005 + 0.005{P_S}} \right)h$
Fodder corn, sorghum, Kaffir corn, etc., sowed in drills	| $J = \left( {0.007 + 0.006{P_S}} \right)h$

In order to use Horton’s working equations in a SWB simulation, the user must supply the constant, slope, and exponent as given in the table above. No attempt is made to incorporate plant height; the user must modify the equation with the approximate plant height. Thus, the equation for 8-foot tall corn would be $J = \left( {0.005 + 0.08{P_S}} \right) \cdot 8 = 0.04 + 0.04{P_S}$; the constant, slope, and exponent supplied to SWB would be 0.04, 0.04, and 1.0, respectively.
