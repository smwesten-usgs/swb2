title: Model Description

The SWB code uses a modified Thornthwaite-Mather soil-moisture
accounting method (Thornthwaite and Mather, 1955; Thornthwaite and
Mather, 1957) to calculate net infiltration; net infiltration is
calculated separately for each grid cell in the model domain. Sources
and sinks of water within each grid cell are determined on the basis of
input climate data and landscape characteristics (fig. 1). Soil moisture
is updated on a daily basis as the difference among these sources and
sinks as in equation 1. The terms in equation 1 are expressed in units
of length; SWB uses units of inches.

$$
\begin{equation}
  {\theta _t} = {\theta _{t - 1}} + rainfall + runon + snowmelt + fog\;interception + irrigation - interception - runoff - ET
\end{equation}
$$

where

\({\theta _t}\)	is the soil moisture for the current simulation day,

\({\theta _{t - 1}}\)	is the soil moisture on the previous simulation day, and

\(ET\)	is the actual evapotranspiration.

![SWB Processes](|media|/fig_1_simplified_water_budget_diagram.png){: width="800"}
{: style="text-align: center"}

1.  Conceptual diagram of Soil-Water-Balance storage reservoirs and
    processes.

How the terms from equation 1 relate is shown in figure 1. In addition
to the soil-moisture reservoir described by equation 1, two additional
storage reservoirs are tracked by SWB—interception and snow. The daily
calculation for the interception amounts to new intercepted rainfall and
snowfall minus any evaporated interception water. The daily calculation
for the snow reservoir is simply the running sum of snowfall minus
snowmelt.

The range of possible soil-moisture values described by equation 1 is
assumed to be bounded by two values—the field capacity and the permanent
wilting point. The field capacity of a soil is defined as the amount of
moisture remaining in a soil after it has been saturated and allowed to
drain freely. The permanent wilting point of a soil is defined as the
moisture content at which plants will wilt and fail to recover even when
later supplied with sufficient moisture (Barker and others, 2005). The
available water capacity—one of the gridded datasets required by SWB—is
defined as the difference between a soil’s field capacity and its
permanent wilting point. The total available water for the soil in a
given grid cell is calculated as shown in equation 2.

$$
\begin{equation}
TAW = \left( {{\theta _{FC}} - {\theta _{WP}}} \right) \cdot rooting\;depth
\end{equation}
$$

where

\(TAW\) 	is total available water, in inches;

\({\theta _{FC}}\) 	is field capacity, in inches per foot;

\({\theta _{WP}}\) 	is permanent wilting point, in inches per foot; and

\(rooting\;depth\) 	is the effective rooting depth of vegetation, in feet.


Net infiltration is assumed to take place any time the soil-moisture value (eq. 1) exceeds the total available water (eq. 2) for the cell. The following is a list of steps to calculate net infiltration.

1.	Precipitation is partitioned into gross rainfall or gross snowfall, or both.
2.	Intercepted rain or snow is added to the interception storage reservoir.
3.	Net snowfall is added to the snow storage reservoir.
4.	Snowmelt (if any) is calculated.
5.	Potential evapotranspiration (PET) is calculated.
6.	Interim soil moisture is calculated as \({\theta _{interim}} = {\theta _{t - 1}} + rainfall + snowmelt + runon - runoff\).
7.	Direct additions to soil moisture, if any, are added to \({\theta _{interim}}\).
8.	Interim soil-moisture fraction is calculated as \(f = {{\left( {{\theta _{interim}} - {\theta _{WP}}} \right)} \over {\left( {{\theta _{FC}} - {\theta _{WP}}} \right)}}\).
9.	Actual evapotranspiration (AET) from the soil storage reservoir is calculated as some function of \(f\) and \(PET\).
10.	Updated soil moisture is calculated as \({\theta _t} = {\theta _{t - 1}} + rainfall + snowmelt + runon - runoff - AET\).
11.	If the updated soil moisture (\({\theta _t}\)) exceeds the field capacity of the soil, the updated soil moisture is set to \({\theta _{FC}}\), making the change in soil moisture \(\Delta \theta  = {\theta _{FC}} - {\theta _{t - 1}}\).
12.	If the updated soil moisture is less than the field capacity, net infiltration is considered to be zero.
13.	Otherwise, net infiltration is calculated as \(net\;infiltration = {\theta _t} - {\theta _{FC}}\).
14.	Direct net-infiltration amounts, if any, are added to the \(net\;infiltration\) amount calculated in step 13.

Surface runoff from a cell may be routed to the next downslope cell or may be considered to have reached an unmodeled surface-water feature (stream, lake, ditch) and removed from the model domain. In urban areas
or in areas with significant impervious surfaces, results of simulating runoff and net infiltration in a more detailed manner might be desirable. This option is triggered in SWB 2.0 when a percent or fraction impervious area grid is supplied to the code. When this option is active, an additional storage reservoir is created—impervious surface storage. In addition, the effect of storm drains can be taken into account by supplying the fraction of impervious surface storage that is intercepted by storm drains.
