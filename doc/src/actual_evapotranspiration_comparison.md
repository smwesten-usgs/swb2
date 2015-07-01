
# _Actual Evapotranspiration:_ Comparison between SWB and HWB #

## Preliminaries ##

### Bring in Python modules ###


    %matplotlib inline
    
    import os
    import gdal
    import matplotlib.pyplot as plt
    import matplotlib.colors as colors
    from netCDF4 import Dataset as netcdf_dataset
    import numpy as np
    import numpy.ma as ma

### Set variables that determine the model output being compared ###


    SWB_param_name             = "actual_ET"
    SWB_grid_dimensions        = "512_by_750"
    
    Simulation_start_year      = "2001"
    Simulation_stop_year       = "2002"
    Simulation_length_in_years = 2.0
    
    HWB_param_name             = "actual_ET"
    
    Plot_title_param           = "Actual Evapotranspiration"
    Plot_title_units           = ", in Inches"

### Define basic plot types ###


    # the definitions for the table and plotting functions live in another
    # iPython notebook, so that a single copy can be maintained without copying
    # and pasting between notebooks.
    
    %run plot_and_table_functions.ipynb

### Open and read model output files ###


    # Define the pathname to the SWB output file. 
    fname_swb = os.path.join( '..', SWB_param_name + '_'
                         + Simulation_start_year 
                         + '_' + Simulation_stop_year
                         + '__' + SWB_grid_dimensions + '.nc' )
    dataset_swb = netcdf_dataset( fname_swb )
    if dataset_swb is None:
        print 'Could not open netCDF grid file'
        sys.exit(1)
    
    # Define pathname to the HWB output file
    fname_hwb = os.path.join('..', 'reference_HWB_output', HWB_param_name + '.asc' )
    dataset_hwb = gdal.Open( fname_hwb )
    if dataset_hwb is None:
        print 'Could not open data grid'
        sys.exit(1)
    
    # Define the pathname to the landuse Arc ASCII file output by SWB
    fname_lu = os.path.join('..', 'Landuse_land_cover__as_read_into_SWB.asc' )
    dataset_lu = gdal.Open( fname_lu )
    if dataset_lu is None:
        print 'Could not open landuse grid'
        sys.exit(1)
        
    # Define the pathname to the SWB landuse lookup table
    fname_lu_table = os.path.join('..', 'std_input', 'LU_lookup_Engott_v3_3.txt' )    
    lu_table = np.genfromtxt( fname_lu_table, names=True, delimiter='\t', dtype=None)    
        
    # Last, define the pathname to the Hawaii Aquifer Code grid
    fname_aquifer_cd = os.path.join('..', 'reference_HWB_output', 'aquifer_code.asc' )    
    dataset_aquifer = gdal.Open( fname_aquifer_cd )
    if dataset_aquifer is None:
        print 'Could not open data grid'
        sys.exit(1)

## Plots and Comparisons ##
### Plot of Soil-Water-Balance Model output ###


    SWB_values = dataset_swb.variables[ SWB_param_name ][:, :, :]
    # the netCDF values come in with not-a-number (NaN) values in the inactive cells;
    # need to deal with these NaNs before doing any processing
    SWB_values = ma.masked_where( np.isnan(SWB_values), SWB_values ) 
    
    # sum daily model outputs over the time dimension (axis "0")
    SWB_values = SWB_values.sum(axis=0) / Simulation_length_in_years
    
    x = dataset_swb.variables['x'][:]
    y = dataset_swb.variables['y'][:]
    
    make_plot(x=x, y=y, var=SWB_values, 
              title="SWB 2.0 Mean Annual " + Plot_title_param + " (" 
              + Simulation_start_year + "-" + Simulation_stop_year + " )",
              barlabel=Plot_title_param + Plot_title_units )


![png](output_11_0.png)


### Plot of Hawaii-Water-Budget Model output ###


    HWB_values = dataset_hwb.ReadAsArray()
    # the HWB output comes in as an Arc ASCII grid, with "-9999" values denoting the
    # areas of inactive model domain; as with the previous grid, must deal with these
    # before doing any further analysis. numpy "masked array" does what we need.
    HWB_values = ma.masked_where( HWB_values < 0, HWB_values )
    
    make_plot(x=x, y=y, var=HWB_values,
              title="HWB Mean Annual " + Plot_title_param + " (" 
              + Simulation_start_year + "-" + Simulation_stop_year + " )",
              barlabel=Plot_title_param + Plot_title_units )


![png](output_13_0.png)


### Plot of differences: SWB output minus HWB output ###


    mean_grid = ( SWB_values + HWB_values ) / 2.0
    pct_diff = (SWB_values - HWB_values) / mean_grid * 100.
    make_diffplot(x=x, y=y, var=pct_diff,
              title="SWB minus HWB",
              barlabel=Plot_title_param + ", percent difference" )


![png](output_15_0.png)


### Plot of SWB and HWB model outputs ###


    lu = dataset_lu.ReadAsArray()
    lu = ma.masked_where( lu <= 0, lu )
    make_scatter(x=SWB_values, y=HWB_values, color=lu, title=Plot_title_param 
                 + ": SWB versus HWB", xlab="Soil Water Balance",
                 ylab="Hawaii Water Balance")


![png](output_17_0.png)


_Comments:_ The color coding above corresponds to the landuse code from the LU_code field in the landuse lookup table. Linear strings of points that do not fall on the 1:1 slope line appear to be caused by mismatches in grid cell coverage and resolution in the various input files.

### Plot of landuse as read in by SWB ###


    cmap = colors.ListedColormap ( np.random.rand ( 256,3))
    make_plot(x=x, y=y, var=lu,
              title="Landuse grid as read into SWB",
              barlabel="Landuse ID number",
              cmap=cmap, 
              each=True)


![png](output_20_0.png)


### Table of model differences by landuse code ###


    np.unique(lu)
    lu_descriptions=lu_table['Description']
    lu_lookup_values=lu_table['LU_code']
    make_comparison_table( x=SWB_values, y=HWB_values, factor=lu, 
                           description=lu_descriptions,
                           lookup_vals=lu_lookup_values)


<div style="max-height:1000px;max-width:1500px;overflow:auto;">
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>Code</th>
      <th>Description</th>
      <th>Count</th>
      <th>SWB Mean</th>
      <th>HWB Mean</th>
      <th>Percent Difference</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>0</td>
      <td>Background</td>
      <td>195738</td>
      <td>0.000</td>
      <td>27.979</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>1</th>
      <td>1</td>
      <td>Sugarcane</td>
      <td>17087</td>
      <td>2.954</td>
      <td>85.679</td>
      <td>-96.552</td>
    </tr>
    <tr>
      <th>2</th>
      <td>2</td>
      <td>Pineapple</td>
      <td>766</td>
      <td>0.000</td>
      <td>21.210</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>3</th>
      <td>3</td>
      <td>Coffee</td>
      <td>233</td>
      <td>0.000</td>
      <td>76.861</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>4</th>
      <td>4</td>
      <td>Diversified Agriculture</td>
      <td>771</td>
      <td>0.000</td>
      <td>40.959</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>5</th>
      <td>5</td>
      <td>Macadamia</td>
      <td>612</td>
      <td>0.000</td>
      <td>19.658</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>6</th>
      <td>6</td>
      <td>Fallow_grassland</td>
      <td>4675</td>
      <td>0.000</td>
      <td>31.508</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>7</th>
      <td>7</td>
      <td>Developed Open Space</td>
      <td>8242</td>
      <td>0.000</td>
      <td>27.892</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>8</th>
      <td>8</td>
      <td>Developed Low intensity</td>
      <td>4331</td>
      <td>0.000</td>
      <td>26.975</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>9</th>
      <td>9</td>
      <td>Developed Medium intensity</td>
      <td>2138</td>
      <td>0.000</td>
      <td>21.571</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>10</th>
      <td>10</td>
      <td>Developed High intensity</td>
      <td>1575</td>
      <td>0.000</td>
      <td>20.914</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>11</th>
      <td>11</td>
      <td>Water body</td>
      <td>169</td>
      <td>0.000</td>
      <td>93.974</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>12</th>
      <td>12</td>
      <td>Wetland</td>
      <td>63</td>
      <td>0.000</td>
      <td>35.666</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>13</th>
      <td>13</td>
      <td>Sparsely vegetated</td>
      <td>11246</td>
      <td>0.000</td>
      <td>13.815</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>14</th>
      <td>14</td>
      <td>Grassland</td>
      <td>45814</td>
      <td>0.000</td>
      <td>24.742</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>15</th>
      <td>15</td>
      <td>Shrubland</td>
      <td>15849</td>
      <td>0.000</td>
      <td>23.612</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>16</th>
      <td>16</td>
      <td>Native forest</td>
      <td>10085</td>
      <td>0.000</td>
      <td>22.240</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>17</th>
      <td>17</td>
      <td>Alien forest</td>
      <td>26536</td>
      <td>3.721</td>
      <td>24.129</td>
      <td>-84.579</td>
    </tr>
    <tr>
      <th>18</th>
      <td>18</td>
      <td>Tree plantation</td>
      <td>294</td>
      <td>7.898</td>
      <td>36.099</td>
      <td>-78.120</td>
    </tr>
    <tr>
      <th>19</th>
      <td>19</td>
      <td>Reservoirs</td>
      <td>88</td>
      <td>0.000</td>
      <td>37.633</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>21</th>
      <td>21</td>
      <td>Native forest fog</td>
      <td>28459</td>
      <td>0.000</td>
      <td>14.178</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>22</th>
      <td>22</td>
      <td>Alien forest fog</td>
      <td>7601</td>
      <td>0.000</td>
      <td>16.755</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>23</th>
      <td>23</td>
      <td>Tree plantation fog</td>
      <td>635</td>
      <td>0.000</td>
      <td>15.865</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>24</th>
      <td>24</td>
      <td>Golf course</td>
      <td>800</td>
      <td>0.000</td>
      <td>32.939</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>25</th>
      <td>25</td>
      <td>Taro</td>
      <td>22</td>
      <td>0.000</td>
      <td>66.004</td>
      <td>-100.000</td>
    </tr>
    <tr>
      <th>30</th>
      <td>30</td>
      <td>HC&amp;S reservoirs</td>
      <td>171</td>
      <td>0.000</td>
      <td>63.866</td>
      <td>-100.000</td>
    </tr>
  </tbody>
</table>
</div>



    aquifer_codes = dataset_aquifer.ReadAsArray()
    aquifer_codes = ma.masked_where( aquifer_codes <= 0, aquifer_codes )
    aquifer_descriptions=np.unique( aquifer_codes.data )
    aquifer_lookup_values=np.unique( aquifer_codes.data )
    make_comparison_table( x=SWB_values, y=HWB_values, factor=aquifer_codes, 
                           description=np.sort(aquifer_descriptions[aquifer_descriptions > 0]),
                           lookup_vals=np.sort(aquifer_lookup_values[aquifer_lookup_values > 0]))


<div style="max-height:1000px;max-width:1500px;overflow:auto;">
<table border="1" class="dataframe">
  <thead>
    <tr style="text-align: right;">
      <th></th>
      <th>Code</th>
      <th>Description</th>
      <th>Count</th>
      <th>SWB Mean</th>
      <th>HWB Mean</th>
      <th>Percent Difference</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <th>0</th>
      <td>60101</td>
      <td>60101</td>
      <td>4487</td>
      <td>0.255</td>
      <td>24.427</td>
      <td>-98.957</td>
    </tr>
    <tr>
      <th>1</th>
      <td>60102</td>
      <td>60102</td>
      <td>6565</td>
      <td>0.383</td>
      <td>21.116</td>
      <td>-98.185</td>
    </tr>
    <tr>
      <th>2</th>
      <td>60103</td>
      <td>60103</td>
      <td>3282</td>
      <td>0.381</td>
      <td>24.296</td>
      <td>-98.431</td>
    </tr>
    <tr>
      <th>3</th>
      <td>60104</td>
      <td>60104</td>
      <td>2834</td>
      <td>0.711</td>
      <td>21.989</td>
      <td>-96.768</td>
    </tr>
    <tr>
      <th>4</th>
      <td>60201</td>
      <td>60201</td>
      <td>3653</td>
      <td>0.555</td>
      <td>23.939</td>
      <td>-97.680</td>
    </tr>
    <tr>
      <th>5</th>
      <td>60202</td>
      <td>60202</td>
      <td>4676</td>
      <td>0.669</td>
      <td>28.786</td>
      <td>-97.677</td>
    </tr>
    <tr>
      <th>6</th>
      <td>60203</td>
      <td>60203</td>
      <td>6145</td>
      <td>0.315</td>
      <td>25.989</td>
      <td>-98.788</td>
    </tr>
    <tr>
      <th>7</th>
      <td>60204</td>
      <td>60204</td>
      <td>5454</td>
      <td>0.056</td>
      <td>14.562</td>
      <td>-99.615</td>
    </tr>
    <tr>
      <th>8</th>
      <td>60205</td>
      <td>60205</td>
      <td>2123</td>
      <td>0.130</td>
      <td>13.410</td>
      <td>-99.030</td>
    </tr>
    <tr>
      <th>9</th>
      <td>60206</td>
      <td>60206</td>
      <td>3147</td>
      <td>0.015</td>
      <td>12.891</td>
      <td>-99.886</td>
    </tr>
    <tr>
      <th>10</th>
      <td>60301</td>
      <td>60301</td>
      <td>7211</td>
      <td>0.941</td>
      <td>63.455</td>
      <td>-98.517</td>
    </tr>
    <tr>
      <th>11</th>
      <td>60302</td>
      <td>60302</td>
      <td>15306</td>
      <td>2.888</td>
      <td>74.634</td>
      <td>-96.130</td>
    </tr>
    <tr>
      <th>12</th>
      <td>60303</td>
      <td>60303</td>
      <td>13337</td>
      <td>0.083</td>
      <td>22.885</td>
      <td>-99.636</td>
    </tr>
    <tr>
      <th>13</th>
      <td>60304</td>
      <td>60304</td>
      <td>23796</td>
      <td>0.237</td>
      <td>15.038</td>
      <td>-98.424</td>
    </tr>
    <tr>
      <th>14</th>
      <td>60401</td>
      <td>60401</td>
      <td>9180</td>
      <td>0.898</td>
      <td>33.610</td>
      <td>-97.329</td>
    </tr>
    <tr>
      <th>15</th>
      <td>60402</td>
      <td>60402</td>
      <td>4660</td>
      <td>2.310</td>
      <td>32.768</td>
      <td>-92.950</td>
    </tr>
    <tr>
      <th>16</th>
      <td>60403</td>
      <td>60403</td>
      <td>6829</td>
      <td>1.455</td>
      <td>22.934</td>
      <td>-93.654</td>
    </tr>
    <tr>
      <th>17</th>
      <td>60404</td>
      <td>60404</td>
      <td>14364</td>
      <td>1.097</td>
      <td>21.666</td>
      <td>-94.938</td>
    </tr>
    <tr>
      <th>18</th>
      <td>60501</td>
      <td>60501</td>
      <td>3552</td>
      <td>3.092</td>
      <td>25.984</td>
      <td>-88.102</td>
    </tr>
    <tr>
      <th>19</th>
      <td>60502</td>
      <td>60502</td>
      <td>8362</td>
      <td>1.654</td>
      <td>32.919</td>
      <td>-94.976</td>
    </tr>
    <tr>
      <th>20</th>
      <td>60503</td>
      <td>60503</td>
      <td>3807</td>
      <td>1.039</td>
      <td>31.670</td>
      <td>-96.721</td>
    </tr>
    <tr>
      <th>21</th>
      <td>60504</td>
      <td>60504</td>
      <td>7817</td>
      <td>0.612</td>
      <td>26.212</td>
      <td>-97.665</td>
    </tr>
    <tr>
      <th>22</th>
      <td>60601</td>
      <td>60601</td>
      <td>5308</td>
      <td>0.016</td>
      <td>20.898</td>
      <td>-99.925</td>
    </tr>
    <tr>
      <th>23</th>
      <td>60602</td>
      <td>60602</td>
      <td>7895</td>
      <td>0.022</td>
      <td>19.312</td>
      <td>-99.884</td>
    </tr>
    <tr>
      <th>24</th>
      <td>60603</td>
      <td>60603</td>
      <td>16518</td>
      <td>0.129</td>
      <td>14.678</td>
      <td>-99.118</td>
    </tr>
  </tbody>
</table>
</div>



    


    
