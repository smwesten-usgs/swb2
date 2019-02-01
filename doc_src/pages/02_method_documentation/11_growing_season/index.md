title: Method Documentation

SWB keeps track of a binary state variable (a variable having a value of
either zero or one) that indicates growing season status. Growing season
in SWB is either active or inactive. Growing season status affects plant
interception calculations and antecedent runoff conditions (table 1–7).

The growing season is tracked independently of other plant-growth
related parameters, such as the planting date defined for use with the
crop-coefficient module. These parameters remain unlinked and unrelated
because the crop-coefficient module will not necessarily be active for
every SWB simulation.

Two methods are available for keeping track of the growing/nongrowing
season status. The simplest method allows a day of year or month and day
to be input for each land-use type in order to control when the growing
season status is flipped. The second method allows a minimum growing
degree-day value to be set to initiate plant growth and a minimum
mean-air temperature (killing air temperature) to be set to halt plant
growth.

The two methods may be mixed, in other words, a set of starting and
ending dates may be defined for a particular land-use or crop type,
whereas a growing degree-day threshold and killing air temperature may
be specified for another land-use or crop type.
