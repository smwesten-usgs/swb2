# Appendix 1. Data and Parameter Requirements by Process Option {#appendix\_1\_data\_and\_param\_requirements}

[TOC]

SWB 2.0 was designed so that parameters may be supplied to the model on an as-needed basis. The original SWB 1.0 input requirements grew ever larger as more possible calculations methods were added. For this reason, we created a flexible table-based format that allows for parameters to be supplied in any order convenient to the user.

For table-based parameter entry, the crucial detail is to enter the proper parameter name in the header of the file. Case does not matter for these heading entries: "DEPLETION_FRACTION" will work as well as "depletion_fraction" or "Depletion_Fraction". For some modules, multiple heading values are recognized as equivalent to one another. For example, to identify a particular table column as holding landuse / land cover codes, SWB 2.0 recognizes any of the following: "LU_Code", "Landuse_Code", or "Landuse Lookup Code". The idea is that whatever identification makes sense to the modeler should be recognized by SWB and acted upon.

This section describes in detail the parameter and control file requirements for each module currently implemented in the SWB 2.0 code.

# Actual ET: FAO-56
