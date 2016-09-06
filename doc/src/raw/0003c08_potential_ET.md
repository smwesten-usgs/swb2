### Potential Evapotranspiration {#potential_et}

[TOC]

#### Jensen-Haise {#et_jensen_haise}


#### Hargreaves-Samani {#et_hargreaves_samani}

$E{T_0} = 0.0023\,\,\left( {{T_{mean}} + 17.8} \right)\sqrt {{T_{\max }} - {T_{\min }}} \;{R_a}$

where:

$E{T_0}$ is the grass-reference evapotranspiration, in mm per day,

$T$ is the (minimum/mean/maximum) air temperature in degrees Celcius, and

$R_a$ is extraterrestrial solar radiation, in mm per day.

Extraterrestrial solar radiation is calculated using the latitude and longitude of each grid cell for each day of the year. The equations used to calculate solar radiation may be found in Meeus [-@meeus_astronomical_1991].

#### Monthly Potential Evapotranspiration Grid {#et_monthly_grid}

Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate eget, arcu. In enim justo, rhoncus ut, imperdiet a, venenatis vitae, justo. Nullam dictum felis eu pede mollis pretium.

Etiam rhoncus. Maecenas tempus, tellus eget condimentum rhoncus, sem quam semper libero, sit amet adipiscing sem neque sed ipsum. Nam quam nunc, blandit vel, luctus pulvinar, hendrerit id, lorem. Maecenas nec odio et ante tincidunt tempus.

#### Turc {#et_turc}

$E{T_0} = {a_T}0.013\frac{{{T_{mean}}}}{{{T_{mean}} + 15}}\frac{{23.8856{R_s} + 50}}{\lambda }$


#### Thornthwaite-Mather


${I_{monthly}} = {\left( {\frac{{\bar T}}{5}} \right)^{1.514}}$

$I = \sum {{I_{monthly}}}$

$a = 6.75 \times {10^{ - 7}} \cdot {I^3} - 7.71 \times {10^{ - 5}} \cdot {I^2} + 1.7921 \times {10^{ - 2}} \cdot I + 0.49239$

$E{T_p} = \frac{{16}}{{30}}{\left( {\frac{{10T}}{I}} \right)^a}$
