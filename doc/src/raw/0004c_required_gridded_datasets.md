## Required Gridded Datasets {#required_gridded_datasets}

SWB can ingest gridded data in three formats: Surfer, ESRI Arc ASCII, or netCDF.


### Hydrologic Soils Group

The U.S. Department of Agrigulture, Natural Resources Conservation Service (NRCS), formerly the Soil Conservation Service (SCS), has categorized more than 14,000 soil series within the United States into one of four hydrologic soil groups (A - D) on the basis of infiltration capacity. NRCS hydrologic soil group information may be input to the model as an Arc ASCII or Surfer integer grid with values ranging from 1 (soil group A) to 4 (soil group D). The NRCS hydrologic soil group A soils have a high infiltration capacity and, consequently, a low overland flow potential. Group D soils, in contrast, have a very low infiltration capacity and, consequently, a high overland flow potential (table 6).



### Available Water Capacity

The SWB model uses soil information, together with land-cover information, to calculate a maximum soil water-holding capacity for each grid cell. The maximum soil-water capacity is calculated as $maximum soil water capacity = available soil water capacity \cdot root-zone depth$.

Soil classifications, which include an estimate of the available water capacity or textural information, are typically available through the state offices of the NRCS, or on the world-wide web at [http:\\ soils.usda.gov\ ]. Each soil type or soil series within the model area must be assigned an available water capacity. If data for available water capacity are not available, the user can use soil texture to assign a value, shown in [@tbl:available_water_capacity] (Dripps, 2003; original source table 10, Thornthwaite and Mather, 1957).

: Estimated available water capacities for various soil-texture groups. {#tbl:available_water_capacity}

Soil Texture | Available Water Capacity (inches per foot of thickness)
----------------|-------------------
Sand            | 1.20
Loamy sand      | 1.40
Sandy loam      | 1.60
Fine sandy loam | 1.80
Very fine sandy loam | 2.00
Loam            | 2.20
Silt loam       | 2.4
Silt            | 2.55
Sandy clay loam | 2.70
Silty clay loam | 2.85
Clay loam       | 3.00
Sandy clay      | 3.20
Silty clay      | 3.40
Clay            | 3.60

The available water capacity of a soil is typically given as inches of water holding capacity per foot of soil thickness. For example, if a soil type has an available water capacity of 2 in\slash ft and the root-zone depth of the cell under consideration is 2.5 ft, the maximum water capacity of that grid cell would be 5.0 in. This is the maximum amount of soil-water storage that can take place in the grid cell. Water added to the soil column in excess of this value will become recharge.

Note that a grid containing the maximum soil-water capacity may be input directly into the SWB code, bypassing the internal calculation of the maximum soil-water capacity.

### Landuse Code

The model uses land-use information, together with the soil available-water-capacity information, to calculate surface runoff and assign a maximum soil-moisture holding capacity for each grid cell. The original model required that land-use classifications follow a modified Anderson Level II Land Cover Classification [@anderson_land_1976]. Current versions of the model can handle any arbitrary land-use classification method as long as the accompanying land-use lookup table contains curve-number, interception, maximum-recharge, and rooting-depth data for each land-use type contained in the grid.

### D8 Flow Direction

The SWB code requires a flow-direction grid for the entire model domain. The code uses this grid to determine how to route overland flow between cells. The user must create the flow direction grid consistent with the D8 flow-routing algorithm [@ocallaghan_extraction_1984], with flow directions defined as shown in figure~\ref{fig:D8_flow_direction}. The original D8 algorithm assigns a unique flow direction to each grid cell by finding the steepest slope between the central cell and its eight neighboring cells.

Some implementations of the D8 algorithm are capable of accommodating flows to several neighboring cells by assigning a combination of the numbers shown in figure 5 (Jenson and Domingue, 1988). For example, consider a cell (blue cell in fig. 5) that has the same downhill slope in the direction of the two neighboring cells to the west and northwest. Flow could reasonably be expected to go to both of these neighboring cells. Flow only to the cell to the west would be assigned a flow direction of 16; flow only to the cell to the northwest would be assigned a flow direction of 32. Flow to both cells would be indicated by adding the two individual flow-direction values, resulting in a flow-direction value of 48. Note that the SWB model is not designed to accommodate flows to more than one cell.

In the SWB code, a cell for which the flow-direction value is not a power of 2 (as shown in fig. 5) is considered to indicate a closed depression. The SWB code does not attempt to split flows between two or more cells; when a cell with more than one possible flow direction is encountered, it is identified as a closed depression. The SWB code allows no further surface runoff to be generated or ponding to occur but instead requires water in excess of the soil-moisture capacity to contribute to recharge.

ArcInfo software, with the GRID extension, may be used to generate a D8 flow-direction grid from a digital elevation model (DEM) file using the GRID command FLOWDIRECTION.
