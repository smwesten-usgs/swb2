Appendix 5. Earlier implementations of soil moisture retention relations. {\#appendix\_5\_earlier\_SM\_relations}
=================================================================================================================

\[TOC\]

SWB has evolved over time, incorporating additional and improved process
formulations whenever possible. This is how SWB and SWB 2.0 ended up
with no less than three ways by which to update soil moisture values by
means of the Thornthwaite-Mather approach
[@thornthwaite_approach_1948; @thornthwaite_water_1955; @thornthwaite_instructions_1957].
The direct solution method is now the method of choice; the other two
methods are included here to document the functioning of earlier
versions of the code.

Table Values
------------

The original version of the SWB model reads in digitized versions of the
Thornthwaite-Mather soil-moisture retention tables and follows the
original instructions for calculation faithfully. In order to track
changes in soil moisture, several intermediary values are calculated,
including precipitation minus potential evapotranspiration ($P-PE$),
accumulated potential water loss ($APWL$), actual evapotranspiration,
soil-moisture surplus, and soil-moisture deficit. These terms are
described below.

*P minus PE* $(P-PE)$. The first step in calculating a new soil moisture
value for any given grid cell is to subtract potential
evapotranspiration from the daily precipitation ($P-PE$). Negative
values of $P-PE$ represent a potential deficiency of water, whereas
positive $P-PE$ values represent a potential surplus of water.

*Soil moisture*, $\Delta\,soil\,moisture$. The soil-moisture term
represents the amount of water held in soil storage for a given grid
cell. Soil moisture has an upper bound that corresponds to the soil's
maximum water-holding capacity (roughly equivalent to the field
capacity); soil moisture has a lower bound that corresponds to the
soil's permanent wilting point.

When $P-PE$ is positive, the new soil-moisture value is found by adding
this $P-PE$ term directly to the preceding soil-moisture value. If the
new soil-moisture value is still below the maximum water-holding
capacity, the Thornthwaite-Mather soil-moisture tables are consulted to
back-calculate a new, reduced accumulated potential water-loss value. If
the new soil-moisture value exceeds the maximum water-holding capacity,
the soil-moisture value is capped at the value of the maximum
water-holding capacity, the excess moisture is converted to recharge,
and the accumulated potential water-loss term is reset to zero.

When $P-PE$ is negative, the new soil-moisture term is found by looking
up the soil-moisture value associated with the current accumulated
potential water-loss value in the Thornthwaite-Mather tables.

*Actual ET*. When $P-PE$ is positive, the actual evapotranspiration
equals the potential evapotranspiration. When $P-PE$ is negative, the
actual evapotranspiration is equal only to the amount of water that can
be extracted from the soil ($\Delta$ soil moisture).

During the course of a model run, the soil layer is considered to be in
one of three states:

  --------------------------------------------------------------------------
  $P - PE                Soil Status    $SM_{t}$ $APWL_{ $AET_{       Excess
        $                                            t}$    t}$   (potential
                                                                   recharge)
  ------- -------------------------- ----------- ------- ------ ------------
   &lt; 0                     drying SM from T-M $APWL_{ $SM_{t          0.0
                                          tables t-1} +  -1} -  
                                                 (P-PE)$ SM_{t} 
                                                              $ 

   &gt; 0              wetting *and* $SM_{t-1} +  $APWL$   $PE$          0.0
          $SM_{t-1} + (P - PE) < SM_     (P-PE)$    from        
                              {max}$              tables        

   &gt; 0              wetting *and*  $SM_{max}$     0.0   $PE$ $SM_{t-1} + 
          $SM_{t-1} + (P - PE) > SM_                            (P - PE) - S
                              {max}$                                M_{max}$
  --------------------------------------------------------------------------

  : Table 1: Soil moisture states.

Fitted Equations
----------------

The amount of computing involved in negotiating the lookup tables and
interpolating a result was significant enough to warrant generalization;
in addition, small roundoff errors were accumulated in the course of
repeated conversions between accumulated potential water loss and the
corresponding soil moisture values. In order to avoid the use of lookup
tables altogether a generalized equation was developed, using the
Thornthwaite and Mather [-@thornthwaite_instructions_1957] table values
as the basis for the equations.

Two equations were fitted and implemented in the SWB 1.0 code. The first
equation relates the current soil moisture to an equivalent accumulated
potential water loss +eq. 1:

$$soil\,moisture = {10^{({{\log }_{10}}\theta }}^{ - APWL\, \cdot \,0.4788{\kern 1pt} {\theta ^{ - 1.037}})}\qquad(1)$$

The second equation is used to back-calculate the equivalent accumulated
potential water loss value for a given soil moisture amount:

$$APWL = {\log_{10}}({\theta_{fc}}) - {\log_{10}}\left( {\frac{\theta }{{\left( {0.4788\,{\theta_{fc}}^{ - 1.037}} \right)}}} \right)\qquad(2)$$

References
----------
