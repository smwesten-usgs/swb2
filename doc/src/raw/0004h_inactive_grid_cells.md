### Inactive grid cells ###

SWB will use information from certain standard grids to determine which gridcells should remain active during the course of a simulation, namely, the landuse, soil type, and available water capacity grids. A negative value in any one of these three grids causes SWB to mark the cell as inactive; the cell will be removed from all further calculations. The missing value treatments discussed in the previous section could interfere with this interpretation.

The reason this behavior was adopted was the desire to avoid carrying more gridcells than necessary through the calculation process. Since integer grids with missing values are often encoded with “-9999”, it made sense to use this information to help define active and inactive grid cells.

This behavior implies that any legitimately missing values that are encoded as “-9999” in the landuse or soil type grids will be inactivated. If this is not the desired behavior, some GIS pre-processing will be needed to ensure that SWB can separate “inactive” cells from those with missing values. One strategy might be to convert active-cell missing values to an extremely large positive number, then use SWB’s control file directives to find these values and convert them to appropriate values. Of course, it might be easier to eliminate the errant codes in GIS altogether.
