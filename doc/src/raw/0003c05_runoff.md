
### Runoff {#runoff}

[TOC]

#### Soil Conservation Service Curve Number {#curve\_number}

The curve number method defines runoff in relation to the difference between precipitation and an \quotes{initial abstraction} term. Conceptually, this initial abstraction term represents the summation of all processes that might act to reduce runoff, including interception by plants and fallen leaves, depression storage, and infiltration [[@woodward\_runoff\_2003](). Equation 2 is used to calculate runoff volumes [@woodward\_curve\_2002]:


$R =  \dfrac{(P - I\_a)^2}{(P + [S\_{max} - I\_a])}$

where $R$ is runoff, $P$ is daily precipitation, $S\_{max}$ is the maximum soil-moisture holding capacity, and $I\_a$ is initial abstraction, the amount of precipitation that must fall before any runoff is generated.

The initial abstraction ($I\_a$) term is related to a maximum storage term ($S\_{max}$) as follows:

$I\_a = 0.2 S\_{max}$

The maximum storage term is defined by the curve number for the land-cover type under consideration:

$S\_{max} = \left( \dfrac{1000}{CN}\right) - 10$

Curve numbers are adjusted upward or downward depending on how much precipitation has occurred in the previous 5-day period. The amount of precipitation that has fallen in the previous 5-day period is used to describe soil-moisture conditions; three classes of moisture conditions are defined and are called antecedent runoff condition I, II, and III, defined as shown in table ###.

Table: Antecedent Runoff Conditions 

| Runoff condition number | Description | Nongrowing Season | Growing Season |
| ----------|-----------------------|------------|--------------------|
|    I     |    Dry                | 0.05       | 1.4                 |
|    II    |    Average            | 0.5 -- 1.1 | 1.4 -- 2.1          |
|    III   |    Near Saturation    | 1.1        | 2.1                 |

#### Monthly Runoff Fraction Grid {#monthly\_runoff\_grid}


