### Rejected recharge

Specification of maximum daily recharge amounts is a crude but effective way of preventing SWB from calculating unreasonably high potential recharge values. With flow routing enabled, downslope cells can have significant amounts of water diverted to them. The resulting calculated potential recharge values sometimes greatly exceed the values that might be reasonable given the soils and underlying geology. Setting a maximum daily recharge value will prevent these cells from taking on unrealistic recharge values.

In cases where the calculated daily potential recharge is greater than the cell's maximum daily recharge limit, the potential recharge for the cell is set to the maximum daily recharge value, and the remaining water is converted to "rejected recharge". This rejected recharge is then routed to the next downslope cell, where the water becomes available for runoff, recharge, or evapotranspiration.

In previous SWB applications, the maximum recharge values were set to values approximating the vertical hydraulic conductivity values for the underlying MODFLOW application.
