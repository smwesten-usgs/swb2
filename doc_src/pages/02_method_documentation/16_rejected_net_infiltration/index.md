title: Rejected Net Infiltration

Specification of maximum daily net-infiltration values is a crude but
effective way of preventing SWB from calculating unreasonably high
net-infiltration values. With flow routing enabled, downslope cells can
have large amounts of water diverted to them. The resulting calculated
net-infiltration values sometimes exceed the values that might be
reasonable given the soils and underlying geology. Setting a maximum
daily net-infiltration value will prevent these cells from taking on
unrealistic net-infiltration values.

In cases where the calculated daily net infiltration is greater than the
cell's maximum daily net-infiltration limit, the net infiltration for
the cell is set to the maximum daily net-infiltration value, and the
remaining water is converted to rejected net infiltration. This rejected
net infiltration is then routed to the next downslope cell where the
water becomes available for runoff, recharge, or evapotranspiration.
Maximum net-infiltration values are user-defined and are often set to
values approximating the vertical hydraulic-conductivity values for the
underlying MODFLOW application. If left unspecified, SWB enforces no
limits on calculated net-infiltration rates.
