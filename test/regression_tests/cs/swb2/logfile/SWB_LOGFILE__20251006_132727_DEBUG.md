# USGS Soil Water Balance Code run log #
--------------------------------------------------------------------------------

## Model run started on October 06 2025 13:27:27 ##

## SWB version 2.3.4, build 7 compiled on Oct  6 2025  12:44:21 ##
Git branch and commit hash: jjh  8d286fa4


                     Opened file "central_sands_swb2.ctl"  
                                         Comment characters: "#%!+=|[{(-*$"  
                                    Number of lines in file: 128  
     Number of lines excluding blanks, headers and comments: 68  

ASCII grids will be written to subdirectory "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/"  

> GRID 400 346 545300 432200 90.0  

> BASE_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m  

> START_DATE 01/01/2012  

> END_DATE 12/31/2013  
   Model run start date set to: 2012-01-01  
   Model run end date set to:   2013-12-31  

> LAND_USE_LOOKUP_TABLE Landuse_lookup_CDL.txt  

> IRRIGATION_LOOKUP_TABLE Irrigation_lookup_CDL.txt  

> HARGREAVES_ET_LOOKUP_TABLE Hargreaves_ET_parameters.txt  

                     Opened file "/Users/hellyj/src/swb2-jjh/test/test_data/tables/Landuse_lookup_CDL.txt"  
                                         Comment characters: "#!%"  
                                    Number of lines in file: 60  
     Number of lines excluding blanks, headers and comments: 59  
                                  Number of columns in file: 25  

                     Opened file "/Users/hellyj/src/swb2-jjh/test/test_data/tables/Irrigation_lookup_CDL.txt"  
                                         Comment characters: "#!%"  
                                    Number of lines in file: 63  
     Number of lines excluding blanks, headers and comments: 59  
                                  Number of columns in file: 41  

                     Opened file "/Users/hellyj/src/swb2-jjh/test/test_data/tables/Hargreaves_ET_parameters.txt"  
                                         Comment characters: "#!%"  
                                    Number of lines in file: 3  
     Number of lines excluding blanks, headers and comments: 2  
                                  Number of columns in file: 1  
### Summary of all items stored in LOOKUP TABLE dictionary  

 1)  KEY: "LU_Code"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |1                                                 |
|         2         |4                                                 |
|         3         |5                                                 |
|         4         |6                                                 |
|         5         |12                                                |
|         6         |21                                                |
|         7         |23                                                |
|         8         |24                                                |
|         9         |26                                                |
|        10         |27                                                |
|        11         |28                                                |
|        12         |29                                                |
|        13         |30                                                |
|        14         |32                                                |
|        15         |36                                                |
|        16         |37                                                |
|        17         |38                                                |
|        18         |41                                                |
|        19         |42                                                |
|        20         |43                                                |
|        21         |47                                                |
|        22         |49                                                |
|        23         |50                                                |
|        24         |53                                                |
|        25         |57                                                |
|        26         |58                                                |
|        27         |59                                                |
|        28         |61                                                |
|        29         |70                                                |
|        30         |92                                                |
|        31         |111                                               |
|        32         |121                                               |
|        33         |122                                               |
|        34         |123                                               |
|        35         |124                                               |
|        36         |131                                               |
|        37         |141                                               |
|        38         |142                                               |
|        39         |143                                               |
|        40         |151                                               |
|        41         |152                                               |
|        42         |171                                               |
|        43         |176                                               |
|        44         |181                                               |
|        45         |182                                               |
|        46         |190                                               |
|        47         |195                                               |
|        48         |205                                               |
|        49         |206                                               |
|        50         |207                                               |
|        51         |221                                               |
|        52         |225                                               |
|        53         |226                                               |
|        54         |241                                               |
|        55         |242                                               |
|        56         |243                                               |
|        57         |250                                               |
|        58         |251                                               |
|        59         |252                                               |

 2)  KEY: "Description"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |Corn                                              |
|         2         |Sorghum                                           |
|         3         |Soybeans                                          |
|         4         |Sunflowers                                        |
|         5         |Sweet Corn                                        |
|         6         |Barley                                            |
|         7         |Spring Wheat                                      |
|         8         |Winter Wheat                                      |
|         9         |Dbl Crop WinWht/Soybeans                          |
|        10         |Rye                                               |
|        11         |Oats                                              |
|        12         |Millet                                            |
|        13         |Speltz                                            |
|        14         |Flaxseed                                          |
|        15         |Alfalfa                                           |
|        16         |Other Hay/Non Alfalfa                             |
|        17         |Camelina                                          |
|        18         |Sugarbeets                                        |
|        19         |Dry Beans                                         |
|        20         |Potatoes                                          |
|        21         |Misc Vegs and Fruits                              |
|        22         |Onions                                            |
|        23         |Cucumbers                                         |
|        24         |Peas                                              |
|        25         |Herbs                                             |
|        26         |Clover/Wildflowers                                |
|        27         |Sod/Grass Seed                                    |
|        28         |Fallow/Idle Cropland                              |
|        29         |Christmas Trees                                   |
|        30         |Aquaculture                                       |
|        31         |Open Water                                        |
|        32         |Developed/Open Space                              |
|        33         |Developed/Low Intensity                           |
|        34         |Developed/Medium Intensity                        |
|        35         |Developed/High Intensity                          |
|        36         |Barren                                            |
|        37         |Deciduous Forest                                  |
|        38         |Evergreen Forest                                  |
|        39         |Mixed Forest                                      |
|        40         |Dwarf Scrub                                       |
|        41         |Shrubland                                         |
|        42         |Grassland/Herbaceous (arid)                       |
|        43         |Grass/Pasture                                     |
|        44         |Pasture/Hay (fair)                                |
|        45         |Cultivated Crops (SR+CR poor)                     |
|        46         |Woody Wetlands                                    |
|        47         |Herbaceous Wetlands                               |
|        48         |Triticale                                         |
|        49         |Carrots                                           |
|        50         |Asparagus                                         |
|        51         |Strawberries                                      |
|        52         |Dbl Crop WinWht/Corn                              |
|        53         |Dbl Crop Oats/Corn                                |
|        54         |Dbl Crop Corn/Soybeans                            |
|        55         |Blueberries                                       |
|        56         |Cabbage                                           |
|        57         |Cranberries                                       |
|        58         |Waste_disposal_fallow                             |
|        59         |Waste_disposal_grass                              |

 3)  KEY: "CN_1"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |65                                                |
|         2         |30                                                |
|         3         |67                                                |
|         4         |61                                                |
|         5         |65                                                |
|         6         |30                                                |
|         7         |30                                                |
|         8         |30                                                |
|         9         |67                                                |
|        10         |30                                                |
|        11         |30                                                |
|        12         |30                                                |
|        13         |30                                                |
|        14         |30                                                |
|        15         |30                                                |
|        16         |30                                                |
|        17         |55                                                |
|        18         |67                                                |
|        19         |67                                                |
|        20         |67                                                |
|        21         |67                                                |
|        22         |67                                                |
|        23         |67                                                |
|        24         |67                                                |
|        25         |67                                                |
|        26         |30                                                |
|        27         |30                                                |
|        28         |77                                                |
|        29         |43                                                |
|        30         |92                                                |
|        31         |100                                               |
|        32         |49                                                |
|        33         |67                                                |
|        34         |77                                                |
|        35         |89                                                |
|        36         |74                                                |
|        37         |30                                                |
|        38         |30                                                |
|        39         |30                                                |
|        40         |35                                                |
|        41         |35                                                |
|        42         |39                                                |
|        43         |39                                                |
|        44         |39                                                |
|        45         |39                                                |
|        46         |30                                                |
|        47         |30                                                |
|        48         |30                                                |
|        49         |67                                                |
|        50         |67                                                |
|        51         |67                                                |
|        52         |65                                                |
|        53         |65                                                |
|        54         |65                                                |
|        55         |67                                                |
|        56         |67                                                |
|        57         |51                                                |
|        58         |77                                                |
|        59         |30                                                |

 4)  KEY: "CN_2"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |75                                                |
|         2         |58                                                |
|         3         |78                                                |
|         4         |73                                                |
|         5         |75                                                |
|         6         |58                                                |
|         7         |58                                                |
|         8         |58                                                |
|         9         |78                                                |
|        10         |58                                                |
|        11         |58                                                |
|        12         |58                                                |
|        13         |58                                                |
|        14         |58                                                |
|        15         |58                                                |
|        16         |58                                                |
|        17         |69                                                |
|        18         |78                                                |
|        19         |78                                                |
|        20         |78                                                |
|        21         |78                                                |
|        22         |78                                                |
|        23         |78                                                |
|        24         |78                                                |
|        25         |78                                                |
|        26         |58                                                |
|        27         |58                                                |
|        28         |86                                                |
|        29         |65                                                |
|        30         |92                                                |
|        31         |100                                               |
|        32         |69                                                |
|        33         |78                                                |
|        34         |85                                                |
|        35         |92                                                |
|        36         |83                                                |
|        37         |55                                                |
|        38         |55                                                |
|        39         |55                                                |
|        40         |56                                                |
|        41         |56                                                |
|        42         |61                                                |
|        43         |61                                                |
|        44         |61                                                |
|        45         |61                                                |
|        46         |48                                                |
|        47         |58                                                |
|        48         |58                                                |
|        49         |78                                                |
|        50         |78                                                |
|        51         |78                                                |
|        52         |75                                                |
|        53         |75                                                |
|        54         |75                                                |
|        55         |78                                                |
|        56         |78                                                |
|        57         |67                                                |
|        58         |86                                                |
|        59         |58                                                |

 5)  KEY: "CN_3"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |82                                                |
|         2         |71                                                |
|         3         |85                                                |
|         4         |81                                                |
|         5         |82                                                |
|         6         |71                                                |
|         7         |71                                                |
|         8         |71                                                |
|         9         |85                                                |
|        10         |71                                                |
|        11         |71                                                |
|        12         |71                                                |
|        13         |71                                                |
|        14         |71                                                |
|        15         |71                                                |
|        16         |71                                                |
|        17         |78                                                |
|        18         |85                                                |
|        19         |85                                                |
|        20         |85                                                |
|        21         |85                                                |
|        22         |85                                                |
|        23         |85                                                |
|        24         |85                                                |
|        25         |85                                                |
|        26         |71                                                |
|        27         |71                                                |
|        28         |91                                                |
|        29         |76                                                |
|        30         |94                                                |
|        31         |100                                               |
|        32         |79                                                |
|        33         |85                                                |
|        34         |90                                                |
|        35         |94                                                |
|        36         |88                                                |
|        37         |70                                                |
|        38         |70                                                |
|        39         |70                                                |
|        40         |70                                                |
|        41         |70                                                |
|        42         |74                                                |
|        43         |74                                                |
|        44         |74                                                |
|        45         |74                                                |
|        46         |65                                                |
|        47         |71                                                |
|        48         |71                                                |
|        49         |85                                                |
|        50         |85                                                |
|        51         |85                                                |
|        52         |82                                                |
|        53         |82                                                |
|        54         |82                                                |
|        55         |85                                                |
|        56         |85                                                |
|        57         |76                                                |
|        58         |91                                                |
|        59         |71                                                |

 6)  KEY: "CN_4"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |86                                                |
|         2         |78                                                |
|         3         |89                                                |
|         4         |84                                                |
|         5         |86                                                |
|         6         |78                                                |
|         7         |78                                                |
|         8         |78                                                |
|         9         |89                                                |
|        10         |78                                                |
|        11         |78                                                |
|        12         |78                                                |
|        13         |78                                                |
|        14         |78                                                |
|        15         |78                                                |
|        16         |78                                                |
|        17         |83                                                |
|        18         |89                                                |
|        19         |89                                                |
|        20         |89                                                |
|        21         |89                                                |
|        22         |89                                                |
|        23         |89                                                |
|        24         |89                                                |
|        25         |89                                                |
|        26         |78                                                |
|        27         |78                                                |
|        28         |93                                                |
|        29         |82                                                |
|        30         |95                                                |
|        31         |100                                               |
|        32         |84                                                |
|        33         |89                                                |
|        34         |92                                                |
|        35         |95                                                |
|        36         |90                                                |
|        37         |77                                                |
|        38         |77                                                |
|        39         |77                                                |
|        40         |77                                                |
|        41         |77                                                |
|        42         |80                                                |
|        43         |80                                                |
|        44         |80                                                |
|        45         |80                                                |
|        46         |73                                                |
|        47         |78                                                |
|        48         |78                                                |
|        49         |89                                                |
|        50         |89                                                |
|        51         |89                                                |
|        52         |86                                                |
|        53         |86                                                |
|        54         |86                                                |
|        55         |89                                                |
|        56         |89                                                |
|        57         |80                                                |
|        58         |93                                                |
|        59         |78                                                |

 7)  KEY: "CN_5"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |65                                                |
|         2         |30                                                |
|         3         |67                                                |
|         4         |61                                                |
|         5         |65                                                |
|         6         |30                                                |
|         7         |30                                                |
|         8         |30                                                |
|         9         |67                                                |
|        10         |30                                                |
|        11         |30                                                |
|        12         |30                                                |
|        13         |30                                                |
|        14         |30                                                |
|        15         |30                                                |
|        16         |30                                                |
|        17         |55                                                |
|        18         |67                                                |
|        19         |67                                                |
|        20         |67                                                |
|        21         |67                                                |
|        22         |67                                                |
|        23         |67                                                |
|        24         |67                                                |
|        25         |67                                                |
|        26         |30                                                |
|        27         |30                                                |
|        28         |77                                                |
|        29         |43                                                |
|        30         |92                                                |
|        31         |100                                               |
|        32         |49                                                |
|        33         |67                                                |
|        34         |77                                                |
|        35         |89                                                |
|        36         |74                                                |
|        37         |30                                                |
|        38         |30                                                |
|        39         |30                                                |
|        40         |35                                                |
|        41         |35                                                |
|        42         |39                                                |
|        43         |39                                                |
|        44         |39                                                |
|        45         |39                                                |
|        46         |30                                                |
|        47         |30                                                |
|        48         |30                                                |
|        49         |67                                                |
|        50         |67                                                |
|        51         |67                                                |
|        52         |65                                                |
|        53         |65                                                |
|        54         |65                                                |
|        55         |67                                                |
|        56         |67                                                |
|        57         |51                                                |
|        58         |77                                                |
|        59         |30                                                |

 8)  KEY: "CN_6"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |86                                                |
|         2         |78                                                |
|         3         |89                                                |
|         4         |84                                                |
|         5         |86                                                |
|         6         |78                                                |
|         7         |78                                                |
|         8         |78                                                |
|         9         |89                                                |
|        10         |78                                                |
|        11         |78                                                |
|        12         |78                                                |
|        13         |78                                                |
|        14         |78                                                |
|        15         |78                                                |
|        16         |78                                                |
|        17         |83                                                |
|        18         |89                                                |
|        19         |89                                                |
|        20         |89                                                |
|        21         |89                                                |
|        22         |89                                                |
|        23         |89                                                |
|        24         |89                                                |
|        25         |89                                                |
|        26         |78                                                |
|        27         |78                                                |
|        28         |93                                                |
|        29         |82                                                |
|        30         |95                                                |
|        31         |100                                               |
|        32         |84                                                |
|        33         |89                                                |
|        34         |92                                                |
|        35         |95                                                |
|        36         |90                                                |
|        37         |77                                                |
|        38         |77                                                |
|        39         |77                                                |
|        40         |77                                                |
|        41         |77                                                |
|        42         |80                                                |
|        43         |80                                                |
|        44         |80                                                |
|        45         |80                                                |
|        46         |73                                                |
|        47         |78                                                |
|        48         |78                                                |
|        49         |89                                                |
|        50         |89                                                |
|        51         |89                                                |
|        52         |86                                                |
|        53         |86                                                |
|        54         |86                                                |
|        55         |89                                                |
|        56         |89                                                |
|        57         |80                                                |
|        58         |93                                                |
|        59         |78                                                |

 9)  KEY: "CN_7"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |100                                               |
|         2         |100                                               |
|         3         |100                                               |
|         4         |100                                               |
|         5         |100                                               |
|         6         |100                                               |
|         7         |100                                               |
|         8         |100                                               |
|         9         |100                                               |
|        10         |100                                               |
|        11         |100                                               |
|        12         |100                                               |
|        13         |100                                               |
|        14         |100                                               |
|        15         |100                                               |
|        16         |100                                               |
|        17         |100                                               |
|        18         |100                                               |
|        19         |100                                               |
|        20         |100                                               |
|        21         |100                                               |
|        22         |100                                               |
|        23         |100                                               |
|        24         |100                                               |
|        25         |100                                               |
|        26         |100                                               |
|        27         |100                                               |
|        28         |100                                               |
|        29         |100                                               |
|        30         |100                                               |
|        31         |100                                               |
|        32         |100                                               |
|        33         |100                                               |
|        34         |100                                               |
|        35         |100                                               |
|        36         |100                                               |
|        37         |100                                               |
|        38         |100                                               |
|        39         |100                                               |
|        40         |100                                               |
|        41         |100                                               |
|        42         |100                                               |
|        43         |100                                               |
|        44         |100                                               |
|        45         |100                                               |
|        46         |100                                               |
|        47         |100                                               |
|        48         |100                                               |
|        49         |100                                               |
|        50         |100                                               |
|        51         |100                                               |
|        52         |100                                               |
|        53         |100                                               |
|        54         |100                                               |
|        55         |100                                               |
|        56         |100                                               |
|        57         |100                                               |
|        58         |100                                               |
|        59         |100                                               |

 10)  KEY: "max_net_infil_1"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |4                                                 |
|         2         |4                                                 |
|         3         |4                                                 |
|         4         |4                                                 |
|         5         |4                                                 |
|         6         |4                                                 |
|         7         |4                                                 |
|         8         |4                                                 |
|         9         |4                                                 |
|        10         |4                                                 |
|        11         |4                                                 |
|        12         |4                                                 |
|        13         |4                                                 |
|        14         |4                                                 |
|        15         |4                                                 |
|        16         |4                                                 |
|        17         |4                                                 |
|        18         |4                                                 |
|        19         |4                                                 |
|        20         |4                                                 |
|        21         |4                                                 |
|        22         |4                                                 |
|        23         |4                                                 |
|        24         |4                                                 |
|        25         |4                                                 |
|        26         |4                                                 |
|        27         |4                                                 |
|        28         |4                                                 |
|        29         |4                                                 |
|        30         |4                                                 |
|        31         |4                                                 |
|        32         |4                                                 |
|        33         |4                                                 |
|        34         |4                                                 |
|        35         |4                                                 |
|        36         |4                                                 |
|        37         |4                                                 |
|        38         |4                                                 |
|        39         |4                                                 |
|        40         |4                                                 |
|        41         |4                                                 |
|        42         |4                                                 |
|        43         |4                                                 |
|        44         |4                                                 |
|        45         |4                                                 |
|        46         |4                                                 |
|        47         |4                                                 |
|        48         |4                                                 |
|        49         |4                                                 |
|        50         |4                                                 |
|        51         |4                                                 |
|        52         |4                                                 |
|        53         |4                                                 |
|        54         |4                                                 |
|        55         |4                                                 |
|        56         |4                                                 |
|        57         |4                                                 |
|        58         |4                                                 |
|        59         |4                                                 |

 11)  KEY: "max_net_infil_2"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.6                                               |
|         2         |0.6                                               |
|         3         |0.6                                               |
|         4         |0.6                                               |
|         5         |0.6                                               |
|         6         |0.6                                               |
|         7         |0.6                                               |
|         8         |0.6                                               |
|         9         |0.6                                               |
|        10         |0.6                                               |
|        11         |0.6                                               |
|        12         |0.6                                               |
|        13         |0.6                                               |
|        14         |0.6                                               |
|        15         |0.6                                               |
|        16         |0.6                                               |
|        17         |0.6                                               |
|        18         |0.6                                               |
|        19         |0.6                                               |
|        20         |0.6                                               |
|        21         |0.6                                               |
|        22         |0.6                                               |
|        23         |0.6                                               |
|        24         |0.6                                               |
|        25         |0.6                                               |
|        26         |0.6                                               |
|        27         |0.6                                               |
|        28         |0.6                                               |
|        29         |0.6                                               |
|        30         |0.6                                               |
|        31         |0.6                                               |
|        32         |0.6                                               |
|        33         |0.6                                               |
|        34         |0.6                                               |
|        35         |0.6                                               |
|        36         |0.6                                               |
|        37         |0.6                                               |
|        38         |0.6                                               |
|        39         |0.6                                               |
|        40         |0.6                                               |
|        41         |0.6                                               |
|        42         |0.6                                               |
|        43         |0.6                                               |
|        44         |0.6                                               |
|        45         |0.6                                               |
|        46         |0.6                                               |
|        47         |0.6                                               |
|        48         |0.6                                               |
|        49         |0.6                                               |
|        50         |0.6                                               |
|        51         |0.6                                               |
|        52         |0.6                                               |
|        53         |0.6                                               |
|        54         |0.6                                               |
|        55         |0.6                                               |
|        56         |0.6                                               |
|        57         |0.6                                               |
|        58         |0.6                                               |
|        59         |0.6                                               |

 12)  KEY: "max_net_infil_3"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.24                                              |
|         2         |0.24                                              |
|         3         |0.24                                              |
|         4         |0.24                                              |
|         5         |0.24                                              |
|         6         |0.24                                              |
|         7         |0.24                                              |
|         8         |0.24                                              |
|         9         |0.24                                              |
|        10         |0.24                                              |
|        11         |0.24                                              |
|        12         |0.24                                              |
|        13         |0.24                                              |
|        14         |0.24                                              |
|        15         |0.24                                              |
|        16         |0.24                                              |
|        17         |0.24                                              |
|        18         |0.24                                              |
|        19         |0.24                                              |
|        20         |0.24                                              |
|        21         |0.24                                              |
|        22         |0.24                                              |
|        23         |0.24                                              |
|        24         |0.24                                              |
|        25         |0.24                                              |
|        26         |0.24                                              |
|        27         |0.24                                              |
|        28         |0.24                                              |
|        29         |0.24                                              |
|        30         |0.24                                              |
|        31         |0.24                                              |
|        32         |0.24                                              |
|        33         |0.24                                              |
|        34         |0.24                                              |
|        35         |0.24                                              |
|        36         |0.24                                              |
|        37         |0.24                                              |
|        38         |0.24                                              |
|        39         |0.24                                              |
|        40         |0.24                                              |
|        41         |0.24                                              |
|        42         |0.24                                              |
|        43         |0.24                                              |
|        44         |0.24                                              |
|        45         |0.24                                              |
|        46         |0.24                                              |
|        47         |0.24                                              |
|        48         |0.24                                              |
|        49         |0.24                                              |
|        50         |0.24                                              |
|        51         |0.24                                              |
|        52         |0.24                                              |
|        53         |0.24                                              |
|        54         |0.24                                              |
|        55         |0.24                                              |
|        56         |0.24                                              |
|        57         |0.24                                              |
|        58         |0.24                                              |
|        59         |0.24                                              |

 13)  KEY: "max_net_infil_4"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.12                                              |
|         2         |0.12                                              |
|         3         |0.12                                              |
|         4         |0.12                                              |
|         5         |0.12                                              |
|         6         |0.12                                              |
|         7         |0.12                                              |
|         8         |0.12                                              |
|         9         |0.12                                              |
|        10         |0.12                                              |
|        11         |0.12                                              |
|        12         |0.12                                              |
|        13         |0.12                                              |
|        14         |0.12                                              |
|        15         |0.12                                              |
|        16         |0.12                                              |
|        17         |0.12                                              |
|        18         |0.12                                              |
|        19         |0.12                                              |
|        20         |0.12                                              |
|        21         |0.12                                              |
|        22         |0.12                                              |
|        23         |0.12                                              |
|        24         |0.12                                              |
|        25         |0.12                                              |
|        26         |0.12                                              |
|        27         |0.12                                              |
|        28         |0.12                                              |
|        29         |0.12                                              |
|        30         |0.12                                              |
|        31         |0.12                                              |
|        32         |0.12                                              |
|        33         |0.12                                              |
|        34         |0.12                                              |
|        35         |0.12                                              |
|        36         |0.12                                              |
|        37         |0.12                                              |
|        38         |0.12                                              |
|        39         |0.12                                              |
|        40         |0.12                                              |
|        41         |0.12                                              |
|        42         |0.12                                              |
|        43         |0.12                                              |
|        44         |0.12                                              |
|        45         |0.12                                              |
|        46         |0.12                                              |
|        47         |0.12                                              |
|        48         |0.12                                              |
|        49         |0.12                                              |
|        50         |0.12                                              |
|        51         |0.12                                              |
|        52         |0.12                                              |
|        53         |0.12                                              |
|        54         |0.12                                              |
|        55         |0.12                                              |
|        56         |0.12                                              |
|        57         |0.12                                              |
|        58         |0.12                                              |
|        59         |0.12                                              |

 14)  KEY: "max_net_infil_5"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |4                                                 |
|         2         |4                                                 |
|         3         |4                                                 |
|         4         |4                                                 |
|         5         |4                                                 |
|         6         |4                                                 |
|         7         |4                                                 |
|         8         |4                                                 |
|         9         |4                                                 |
|        10         |4                                                 |
|        11         |4                                                 |
|        12         |4                                                 |
|        13         |4                                                 |
|        14         |4                                                 |
|        15         |4                                                 |
|        16         |4                                                 |
|        17         |4                                                 |
|        18         |4                                                 |
|        19         |4                                                 |
|        20         |4                                                 |
|        21         |4                                                 |
|        22         |4                                                 |
|        23         |4                                                 |
|        24         |4                                                 |
|        25         |4                                                 |
|        26         |4                                                 |
|        27         |4                                                 |
|        28         |4                                                 |
|        29         |4                                                 |
|        30         |4                                                 |
|        31         |4                                                 |
|        32         |4                                                 |
|        33         |4                                                 |
|        34         |4                                                 |
|        35         |4                                                 |
|        36         |4                                                 |
|        37         |4                                                 |
|        38         |4                                                 |
|        39         |4                                                 |
|        40         |4                                                 |
|        41         |4                                                 |
|        42         |4                                                 |
|        43         |4                                                 |
|        44         |4                                                 |
|        45         |4                                                 |
|        46         |4                                                 |
|        47         |4                                                 |
|        48         |4                                                 |
|        49         |4                                                 |
|        50         |4                                                 |
|        51         |4                                                 |
|        52         |4                                                 |
|        53         |4                                                 |
|        54         |4                                                 |
|        55         |4                                                 |
|        56         |4                                                 |
|        57         |4                                                 |
|        58         |4                                                 |
|        59         |4                                                 |

 15)  KEY: "max_net_infil_6"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.12                                              |
|         2         |0.12                                              |
|         3         |0.12                                              |
|         4         |0.12                                              |
|         5         |0.12                                              |
|         6         |0.12                                              |
|         7         |0.12                                              |
|         8         |0.12                                              |
|         9         |0.12                                              |
|        10         |0.12                                              |
|        11         |0.12                                              |
|        12         |0.12                                              |
|        13         |0.12                                              |
|        14         |0.12                                              |
|        15         |0.12                                              |
|        16         |0.12                                              |
|        17         |0.12                                              |
|        18         |0.12                                              |
|        19         |0.12                                              |
|        20         |0.12                                              |
|        21         |0.12                                              |
|        22         |0.12                                              |
|        23         |0.12                                              |
|        24         |0.12                                              |
|        25         |0.12                                              |
|        26         |0.12                                              |
|        27         |0.12                                              |
|        28         |0.12                                              |
|        29         |0.12                                              |
|        30         |0.12                                              |
|        31         |0.12                                              |
|        32         |0.12                                              |
|        33         |0.12                                              |
|        34         |0.12                                              |
|        35         |0.12                                              |
|        36         |0.12                                              |
|        37         |0.12                                              |
|        38         |0.12                                              |
|        39         |0.12                                              |
|        40         |0.12                                              |
|        41         |0.12                                              |
|        42         |0.12                                              |
|        43         |0.12                                              |
|        44         |0.12                                              |
|        45         |0.12                                              |
|        46         |0.12                                              |
|        47         |0.12                                              |
|        48         |0.12                                              |
|        49         |0.12                                              |
|        50         |0.12                                              |
|        51         |0.12                                              |
|        52         |0.12                                              |
|        53         |0.12                                              |
|        54         |0.12                                              |
|        55         |0.12                                              |
|        56         |0.12                                              |
|        57         |0.12                                              |
|        58         |0.12                                              |
|        59         |0.12                                              |

 16)  KEY: "max_net_infil_7"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.12                                              |
|         2         |0.12                                              |
|         3         |0.12                                              |
|         4         |0.12                                              |
|         5         |0.12                                              |
|         6         |0.12                                              |
|         7         |0.12                                              |
|         8         |0.12                                              |
|         9         |0.12                                              |
|        10         |0.12                                              |
|        11         |0.12                                              |
|        12         |0.12                                              |
|        13         |0.12                                              |
|        14         |0.12                                              |
|        15         |0.12                                              |
|        16         |0.12                                              |
|        17         |0.12                                              |
|        18         |0.12                                              |
|        19         |0.12                                              |
|        20         |0.12                                              |
|        21         |0.12                                              |
|        22         |0.12                                              |
|        23         |0.12                                              |
|        24         |0.12                                              |
|        25         |0.12                                              |
|        26         |0.12                                              |
|        27         |0.12                                              |
|        28         |0.12                                              |
|        29         |0.12                                              |
|        30         |0.12                                              |
|        31         |0.12                                              |
|        32         |0.12                                              |
|        33         |0.12                                              |
|        34         |0.12                                              |
|        35         |0.12                                              |
|        36         |0.12                                              |
|        37         |0.12                                              |
|        38         |0.12                                              |
|        39         |0.12                                              |
|        40         |0.12                                              |
|        41         |0.12                                              |
|        42         |0.12                                              |
|        43         |0.12                                              |
|        44         |0.12                                              |
|        45         |0.12                                              |
|        46         |0.12                                              |
|        47         |0.12                                              |
|        48         |0.12                                              |
|        49         |0.12                                              |
|        50         |0.12                                              |
|        51         |0.12                                              |
|        52         |0.12                                              |
|        53         |0.12                                              |
|        54         |0.12                                              |
|        55         |0.12                                              |
|        56         |0.12                                              |
|        57         |0.12                                              |
|        58         |0.12                                              |
|        59         |0.12                                              |

 17)  KEY: "Interception_Growing"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0                                                 |
|         2         |0                                                 |
|         3         |0                                                 |
|         4         |0                                                 |
|         5         |0                                                 |
|         6         |0                                                 |
|         7         |0                                                 |
|         8         |0                                                 |
|         9         |0                                                 |
|        10         |0                                                 |
|        11         |0                                                 |
|        12         |0                                                 |
|        13         |0                                                 |
|        14         |0                                                 |
|        15         |0                                                 |
|        16         |0                                                 |
|        17         |0                                                 |
|        18         |0                                                 |
|        19         |0                                                 |
|        20         |0                                                 |
|        21         |0                                                 |
|        22         |0                                                 |
|        23         |0                                                 |
|        24         |0                                                 |
|        25         |0                                                 |
|        26         |0                                                 |
|        27         |0                                                 |
|        28         |0                                                 |
|        29         |0.05                                              |
|        30         |0                                                 |
|        31         |0                                                 |
|        32         |0                                                 |
|        33         |0                                                 |
|        34         |0                                                 |
|        35         |0                                                 |
|        36         |0                                                 |
|        37         |0.05                                              |
|        38         |0.1                                               |
|        39         |0.06                                              |
|        40         |0                                                 |
|        41         |0                                                 |
|        42         |0                                                 |
|        43         |0                                                 |
|        44         |0                                                 |
|        45         |0                                                 |
|        46         |0                                                 |
|        47         |0                                                 |
|        48         |0                                                 |
|        49         |0                                                 |
|        50         |0                                                 |
|        51         |0                                                 |
|        52         |0                                                 |
|        53         |0                                                 |
|        54         |0                                                 |
|        55         |0                                                 |
|        56         |0                                                 |
|        57         |0                                                 |
|        58         |0                                                 |
|        59         |0                                                 |

 18)  KEY: "Interception_Nongrowing"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0                                                 |
|         2         |0                                                 |
|         3         |0                                                 |
|         4         |0                                                 |
|         5         |0                                                 |
|         6         |0                                                 |
|         7         |0                                                 |
|         8         |0                                                 |
|         9         |0                                                 |
|        10         |0                                                 |
|        11         |0                                                 |
|        12         |0                                                 |
|        13         |0                                                 |
|        14         |0                                                 |
|        15         |0                                                 |
|        16         |0                                                 |
|        17         |0                                                 |
|        18         |0                                                 |
|        19         |0                                                 |
|        20         |0                                                 |
|        21         |0                                                 |
|        22         |0                                                 |
|        23         |0                                                 |
|        24         |0                                                 |
|        25         |0                                                 |
|        26         |0                                                 |
|        27         |0                                                 |
|        28         |0                                                 |
|        29         |0                                                 |
|        30         |0                                                 |
|        31         |0                                                 |
|        32         |0                                                 |
|        33         |0                                                 |
|        34         |0                                                 |
|        35         |0                                                 |
|        36         |0                                                 |
|        37         |0                                                 |
|        38         |0.1                                               |
|        39         |0.03                                              |
|        40         |0                                                 |
|        41         |0                                                 |
|        42         |0                                                 |
|        43         |0                                                 |
|        44         |0                                                 |
|        45         |0                                                 |
|        46         |0                                                 |
|        47         |0                                                 |
|        48         |0                                                 |
|        49         |0                                                 |
|        50         |0                                                 |
|        51         |0                                                 |
|        52         |0                                                 |
|        53         |0                                                 |
|        54         |0                                                 |
|        55         |0                                                 |
|        56         |0                                                 |
|        57         |0                                                 |
|        58         |0                                                 |
|        59         |0                                                 |

 19)  KEY: "RZ_1"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |2                                                 |
|         2         |2.4975                                            |
|         3         |1.875                                             |
|         4         |2.4975                                            |
|         5         |1.875                                             |
|         6         |2.4975                                            |
|         7         |2.4975                                            |
|         8         |2.4975                                            |
|         9         |1.875                                             |
|        10         |2.4975                                            |
|        11         |2.4975                                            |
|        12         |2.4975                                            |
|        13         |2.4975                                            |
|        14         |2.4975                                            |
|        15         |2.25                                              |
|        16         |2.4975                                            |
|        17         |2.4975                                            |
|        18         |2.4975                                            |
|        19         |1.5                                               |
|        20         |1                                                 |
|        21         |2.4975                                            |
|        22         |2.4975                                            |
|        23         |2.4975                                            |
|        24         |1.5                                               |
|        25         |2.4975                                            |
|        26         |2.4975                                            |
|        27         |2.4975                                            |
|        28         |1.5                                               |
|        29         |1.125                                             |
|        30         |2.4975                                            |
|        31         |0                                                 |
|        32         |2.4975                                            |
|        33         |1.5                                               |
|        34         |1.5                                               |
|        35         |1.5                                               |
|        36         |1.5                                               |
|        37         |3.12375                                           |
|        38         |1.561875                                          |
|        39         |3.12375                                           |
|        40         |2.4975                                            |
|        41         |2.4975                                            |
|        42         |2.4975                                            |
|        43         |2.4975                                            |
|        44         |2.4975                                            |
|        45         |2.4975                                            |
|        46         |3.375                                             |
|        47         |3.375                                             |
|        48         |2.4975                                            |
|        49         |2.4975                                            |
|        50         |2.4975                                            |
|        51         |2.4975                                            |
|        52         |2.4975                                            |
|        53         |2.4975                                            |
|        54         |2                                                 |
|        55         |2.4975                                            |
|        56         |2.4975                                            |
|        57         |2.4975                                            |
|        58         |1.5                                               |
|        59         |2.4975                                            |

 20)  KEY: "RZ_2"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |2                                                 |
|         2         |2.7075                                            |
|         3         |1.875                                             |
|         4         |2.7075                                            |
|         5         |1.875                                             |
|         6         |2.7075                                            |
|         7         |2.7075                                            |
|         8         |2.7075                                            |
|         9         |1.875                                             |
|        10         |2.7075                                            |
|        11         |2.7075                                            |
|        12         |2.7075                                            |
|        13         |2.7075                                            |
|        14         |2.7075                                            |
|        15         |2.25                                              |
|        16         |2.4975                                            |
|        17         |2.7075                                            |
|        18         |2.7075                                            |
|        19         |1.5                                               |
|        20         |1                                                 |
|        21         |2.7075                                            |
|        22         |2.7075                                            |
|        23         |2.7075                                            |
|        24         |1.5                                               |
|        25         |2.7075                                            |
|        26         |2.7075                                            |
|        27         |2.7075                                            |
|        28         |1.5                                               |
|        29         |1.125                                             |
|        30         |2.4975                                            |
|        31         |0                                                 |
|        32         |2.4975                                            |
|        33         |1.5                                               |
|        34         |1.5                                               |
|        35         |1.5                                               |
|        36         |1.5                                               |
|        37         |2.4975                                            |
|        38         |1.24875                                           |
|        39         |2.4975                                            |
|        40         |2.4975                                            |
|        41         |2.4975                                            |
|        42         |2.7075                                            |
|        43         |2.4975                                            |
|        44         |2.7075                                            |
|        45         |2.7075                                            |
|        46         |3.375                                             |
|        47         |3.375                                             |
|        48         |2.4975                                            |
|        49         |2.7075                                            |
|        50         |2.7075                                            |
|        51         |2.7075                                            |
|        52         |2.7075                                            |
|        53         |2.7075                                            |
|        54         |2                                                 |
|        55         |2.7075                                            |
|        56         |2.7075                                            |
|        57         |2.7075                                            |
|        58         |1.5                                               |
|        59         |2.7075                                            |

 21)  KEY: "RZ_3"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |2                                                 |
|         2         |2.55                                              |
|         3         |1.875                                             |
|         4         |2.55                                              |
|         5         |1.875                                             |
|         6         |2.55                                              |
|         7         |2.55                                              |
|         8         |2.55                                              |
|         9         |1.875                                             |
|        10         |2.55                                              |
|        11         |2.55                                              |
|        12         |2.55                                              |
|        13         |2.55                                              |
|        14         |2.55                                              |
|        15         |2.25                                              |
|        16         |3.1275                                            |
|        17         |2.55                                              |
|        18         |2.55                                              |
|        19         |1.5                                               |
|        20         |1                                                 |
|        21         |2.55                                              |
|        22         |2.55                                              |
|        23         |2.55                                              |
|        24         |1.5                                               |
|        25         |2.55                                              |
|        26         |2.55                                              |
|        27         |2.55                                              |
|        28         |1.5                                               |
|        29         |1.125                                             |
|        30         |2.4975                                            |
|        31         |0                                                 |
|        32         |2.4975                                            |
|        33         |1.5                                               |
|        34         |1.5                                               |
|        35         |1.5                                               |
|        36         |1.5                                               |
|        37         |2.4975                                            |
|        38         |1.24875                                           |
|        39         |2.4975                                            |
|        40         |2.4975                                            |
|        41         |2.4975                                            |
|        42         |2.55                                              |
|        43         |3.1275                                            |
|        44         |2.55                                              |
|        45         |2.55                                              |
|        46         |3.375                                             |
|        47         |3.375                                             |
|        48         |3.1275                                            |
|        49         |2.55                                              |
|        50         |2.55                                              |
|        51         |2.55                                              |
|        52         |2.55                                              |
|        53         |2.55                                              |
|        54         |2                                                 |
|        55         |2.55                                              |
|        56         |2.55                                              |
|        57         |2.55                                              |
|        58         |1.5                                               |
|        59         |2.55                                              |

 22)  KEY: "RZ_4"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |2                                                 |
|         2         |1.5825                                            |
|         3         |1.875                                             |
|         4         |1.5825                                            |
|         5         |1.875                                             |
|         6         |1.5825                                            |
|         7         |1.5825                                            |
|         8         |1.5825                                            |
|         9         |1.875                                             |
|        10         |1.5825                                            |
|        11         |1.5825                                            |
|        12         |1.5825                                            |
|        13         |1.5825                                            |
|        14         |1.5825                                            |
|        15         |2.25                                              |
|        16         |2.4975                                            |
|        17         |1.5825                                            |
|        18         |1.5825                                            |
|        19         |1.5                                               |
|        20         |1                                                 |
|        21         |1.5825                                            |
|        22         |1.5825                                            |
|        23         |1.5825                                            |
|        24         |1.5                                               |
|        25         |1.5825                                            |
|        26         |1.5825                                            |
|        27         |1.5825                                            |
|        28         |1.5                                               |
|        29         |1.125                                             |
|        30         |1.665                                             |
|        31         |0                                                 |
|        32         |2.4975                                            |
|        33         |1.5                                               |
|        34         |1.5                                               |
|        35         |1.5                                               |
|        36         |1.5                                               |
|        37         |1.99875                                           |
|        38         |0.999375                                          |
|        39         |1.99875                                           |
|        40         |1.665                                             |
|        41         |1.665                                             |
|        42         |1.5825                                            |
|        43         |2.4975                                            |
|        44         |1.5825                                            |
|        45         |1.5825                                            |
|        46         |3.375                                             |
|        47         |3.375                                             |
|        48         |2.4975                                            |
|        49         |1.5825                                            |
|        50         |1.5825                                            |
|        51         |1.5825                                            |
|        52         |1.5825                                            |
|        53         |1.5825                                            |
|        54         |2                                                 |
|        55         |1.5825                                            |
|        56         |1.5825                                            |
|        57         |1.5825                                            |
|        58         |1.5                                               |
|        59         |1.5825                                            |

 23)  KEY: "RZ_5"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |2                                                 |
|         2         |2.4975                                            |
|         3         |1.875                                             |
|         4         |2.4975                                            |
|         5         |1.875                                             |
|         6         |2.4975                                            |
|         7         |2.4975                                            |
|         8         |2.4975                                            |
|         9         |1.875                                             |
|        10         |2.4975                                            |
|        11         |2.4975                                            |
|        12         |2.4975                                            |
|        13         |2.4975                                            |
|        14         |2.4975                                            |
|        15         |2.25                                              |
|        16         |2.4975                                            |
|        17         |2.4975                                            |
|        18         |2.4975                                            |
|        19         |1.5                                               |
|        20         |1                                                 |
|        21         |2.4975                                            |
|        22         |2.4975                                            |
|        23         |2.4975                                            |
|        24         |1.5                                               |
|        25         |2.4975                                            |
|        26         |2.4975                                            |
|        27         |2.4975                                            |
|        28         |1.5                                               |
|        29         |1.125                                             |
|        30         |2.4975                                            |
|        31         |0                                                 |
|        32         |2.4975                                            |
|        33         |1.5                                               |
|        34         |1.5                                               |
|        35         |1.5                                               |
|        36         |1.5                                               |
|        37         |3.12375                                           |
|        38         |1.561875                                          |
|        39         |3.12375                                           |
|        40         |2.4975                                            |
|        41         |2.4975                                            |
|        42         |2.4975                                            |
|        43         |2.4975                                            |
|        44         |2.4975                                            |
|        45         |2.4975                                            |
|        46         |3.375                                             |
|        47         |3.375                                             |
|        48         |2.4975                                            |
|        49         |2.4975                                            |
|        50         |2.4975                                            |
|        51         |2.4975                                            |
|        52         |2.4975                                            |
|        53         |2.4975                                            |
|        54         |2                                                 |
|        55         |2.4975                                            |
|        56         |2.4975                                            |
|        57         |2.4975                                            |
|        58         |1.5                                               |
|        59         |2.4975                                            |

 24)  KEY: "RZ_6"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |2                                                 |
|         2         |1.5825                                            |
|         3         |1.875                                             |
|         4         |1.5825                                            |
|         5         |1.875                                             |
|         6         |1.5825                                            |
|         7         |1.5825                                            |
|         8         |1.5825                                            |
|         9         |1.875                                             |
|        10         |1.5825                                            |
|        11         |1.5825                                            |
|        12         |1.5825                                            |
|        13         |1.5825                                            |
|        14         |1.5825                                            |
|        15         |2.25                                              |
|        16         |2.4975                                            |
|        17         |1.5825                                            |
|        18         |1.5825                                            |
|        19         |1.5                                               |
|        20         |1                                                 |
|        21         |1.5825                                            |
|        22         |1.5825                                            |
|        23         |1.5825                                            |
|        24         |1.5                                               |
|        25         |1.5825                                            |
|        26         |1.5825                                            |
|        27         |1.5825                                            |
|        28         |1.5                                               |
|        29         |1.125                                             |
|        30         |1.665                                             |
|        31         |0                                                 |
|        32         |2.4975                                            |
|        33         |1.5                                               |
|        34         |1.5                                               |
|        35         |1.5                                               |
|        36         |1.5                                               |
|        37         |1.99875                                           |
|        38         |0.999375                                          |
|        39         |1.99875                                           |
|        40         |1.665                                             |
|        41         |1.665                                             |
|        42         |1.5825                                            |
|        43         |2.4975                                            |
|        44         |1.5825                                            |
|        45         |1.5825                                            |
|        46         |3.375                                             |
|        47         |3.375                                             |
|        48         |2.4975                                            |
|        49         |1.5825                                            |
|        50         |1.5825                                            |
|        51         |1.5825                                            |
|        52         |1.5825                                            |
|        53         |1.5825                                            |
|        54         |2                                                 |
|        55         |1.5825                                            |
|        56         |1.5825                                            |
|        57         |1.5825                                            |
|        58         |1.5                                               |
|        59         |1.5825                                            |

 25)  KEY: "RZ_7"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0                                                 |
|         2         |0                                                 |
|         3         |0                                                 |
|         4         |0                                                 |
|         5         |0                                                 |
|         6         |0                                                 |
|         7         |0                                                 |
|         8         |0                                                 |
|         9         |0                                                 |
|        10         |0                                                 |
|        11         |0                                                 |
|        12         |0                                                 |
|        13         |0                                                 |
|        14         |0                                                 |
|        15         |0                                                 |
|        16         |0                                                 |
|        17         |0                                                 |
|        18         |0                                                 |
|        19         |0                                                 |
|        20         |0                                                 |
|        21         |0                                                 |
|        22         |0                                                 |
|        23         |0                                                 |
|        24         |0                                                 |
|        25         |0                                                 |
|        26         |0                                                 |
|        27         |0                                                 |
|        28         |0                                                 |
|        29         |0                                                 |
|        30         |0                                                 |
|        31         |0                                                 |
|        32         |0                                                 |
|        33         |0                                                 |
|        34         |0                                                 |
|        35         |0                                                 |
|        36         |0                                                 |
|        37         |0                                                 |
|        38         |0                                                 |
|        39         |0                                                 |
|        40         |0                                                 |
|        41         |0                                                 |
|        42         |0                                                 |
|        43         |0                                                 |
|        44         |0                                                 |
|        45         |0                                                 |
|        46         |0                                                 |
|        47         |0                                                 |
|        48         |0                                                 |
|        49         |0                                                 |
|        50         |0                                                 |
|        51         |0                                                 |
|        52         |0                                                 |
|        53         |0                                                 |
|        54         |0                                                 |
|        55         |0                                                 |
|        56         |0                                                 |
|        57         |0                                                 |
|        58         |0                                                 |
|        59         |0                                                 |

 26)  KEY: "LU_Code_2"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |1                                                 |
|         2         |4                                                 |
|         3         |5                                                 |
|         4         |6                                                 |
|         5         |12                                                |
|         6         |21                                                |
|         7         |23                                                |
|         8         |24                                                |
|         9         |26                                                |
|        10         |27                                                |
|        11         |28                                                |
|        12         |29                                                |
|        13         |30                                                |
|        14         |32                                                |
|        15         |36                                                |
|        16         |37                                                |
|        17         |38                                                |
|        18         |41                                                |
|        19         |42                                                |
|        20         |43                                                |
|        21         |47                                                |
|        22         |49                                                |
|        23         |50                                                |
|        24         |53                                                |
|        25         |57                                                |
|        26         |58                                                |
|        27         |59                                                |
|        28         |61                                                |
|        29         |70                                                |
|        30         |92                                                |
|        31         |111                                               |
|        32         |121                                               |
|        33         |122                                               |
|        34         |123                                               |
|        35         |124                                               |
|        36         |131                                               |
|        37         |141                                               |
|        38         |142                                               |
|        39         |143                                               |
|        40         |151                                               |
|        41         |152                                               |
|        42         |171                                               |
|        43         |176                                               |
|        44         |181                                               |
|        45         |182                                               |
|        46         |190                                               |
|        47         |195                                               |
|        48         |205                                               |
|        49         |206                                               |
|        50         |207                                               |
|        51         |221                                               |
|        52         |225                                               |
|        53         |226                                               |
|        54         |241                                               |
|        55         |242                                               |
|        56         |243                                               |
|        57         |250                                               |
|        58         |251                                               |
|        59         |252                                               |

 27)  KEY: "Description_2"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |Corn                                              |
|         2         |Sorghum                                           |
|         3         |Soybeans                                          |
|         4         |Sunflower                                         |
|         5         |Sweet Corn                                        |
|         6         |Barley                                            |
|         7         |Spring Wheat                                      |
|         8         |Winter Wheat                                      |
|         9         |Dbl Crop WinWht/Soybeans                          |
|        10         |Rye                                               |
|        11         |Oats                                              |
|        12         |Millet                                            |
|        13         |Speltz                                            |
|        14         |Flaxseed                                          |
|        15         |Alfalfa                                           |
|        16         |Other Hay/Non Alfalfa                             |
|        17         |Camelina                                          |
|        18         |Sugarbeets                                        |
|        19         |Snap beans                                        |
|        20         |Potatoes                                          |
|        21         |Misc Vegs and Fruits                              |
|        22         |Onions                                            |
|        23         |Cucumbers                                         |
|        24         |Peas                                              |
|        25         |Herbs                                             |
|        26         |Clover/Wildflowers                                |
|        27         |Sod/Grass Seed                                    |
|        28         |Fallow / Idle                                     |
|        29         |Christmas Trees                                   |
|        30         |Aquaculture                                       |
|        31         |OPEN WATER                                        |
|        32         |"Developed, Open Space"                           |
|        33         |"Developed, Low Intensity"                        |
|        34         |"Developed, Medium Intensity"                     |
|        35         |"Developed, High Intensity"                       |
|        36         |Barren Land                                       |
|        37         |Deciduous Forest                                  |
|        38         |Evergreen Forest                                  |
|        39         |Mixed Forest                                      |
|        40         |Dwarf Scrub                                       |
|        41         |Shrub/Scrub                                       |
|        42         |Grassland/Herbaceous (arid)                       |
|        43         |Grass/Pasture                                     |
|        44         |Pasture/Hay (fair)                                |
|        45         |Cultivated Crops (SR+CR poor)                     |
|        46         |Woody Wetlands                                    |
|        47         |Emergent Herbaceous Wetlands                      |
|        48         |Triticale                                         |
|        49         |Carrots                                           |
|        50         |Asparagus                                         |
|        51         |Strawberries                                      |
|        52         |Dbl Crop WinWht/Corn                              |
|        53         |Dbl Crop Oats/Corn                                |
|        54         |Dbl Crop Corn/Soybeans                            |
|        55         |Blueberries                                       |
|        56         |Cabbage                                           |
|        57         |Cranberries                                       |
|        58         |Waste_disposal_fallow                             |
|        59         |Waste_disposal_grass                              |

 28)  KEY: "Mean_Plant_Height"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |8                                                 |
|         2         |3.28                                              |
|         3         |2.75                                              |
|         4         |6.56                                              |
|         5         |6                                                 |
|         6         |3.28                                              |
|         7         |3.28                                              |
|         8         |3.28                                              |
|         9         |2.46                                              |
|        10         |0.984                                             |
|        11         |3.28                                              |
|        12         |4.92                                              |
|        13         |3.28                                              |
|        14         |3.936                                             |
|        15         |2.296                                             |
|        16         |1.968                                             |
|        17         |1.968                                             |
|        18         |1.312                                             |
|        19         |1.312                                             |
|        20         |1.968                                             |
|        21         |1.64                                              |
|        22         |0.984                                             |
|        23         |0.984                                             |
|        24         |1.64                                              |
|        25         |1.968                                             |
|        26         |1.968                                             |
|        27         |0.328                                             |
|        28         |0.5                                               |
|        29         |6.56                                              |
|        30         |1                                                 |
|        31         |1                                                 |
|        32         |5                                                 |
|        33         |5                                                 |
|        34         |5                                                 |
|        35         |5                                                 |
|        36         |2                                                 |
|        37         |32.8                                              |
|        38         |32.8                                              |
|        39         |26.24                                             |
|        40         |4.92                                              |
|        41         |3.772                                             |
|        42         |0.492                                             |
|        43         |0.492                                             |
|        44         |0.328                                             |
|        45         |3.28                                              |
|        46         |3.28                                              |
|        47         |3.28                                              |
|        48         |3.28                                              |
|        49         |0.984                                             |
|        50         |0.656                                             |
|        51         |0.656                                             |
|        52         |4.1                                               |
|        53         |4.1                                               |
|        54         |3.28                                              |
|        55         |4.92                                              |
|        56         |1.312                                             |
|        57         |0.492                                             |
|        58         |0.5                                               |
|        59         |0.328                                             |

 29)  KEY: "KCB_ini"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.2                                               |
|         2         |0.15                                              |
|         3         |0.2                                               |
|         4         |0.15                                              |
|         5         |0.2                                               |
|         6         |0.15                                              |
|         7         |0.15                                              |
|         8         |0.4                                               |
|         9         |0.2                                               |
|        10         |0.95                                              |
|        11         |0.15                                              |
|        12         |0.15                                              |
|        13         |0.15                                              |
|        14         |0.15                                              |
|        15         |0.4                                               |
|        16         |0.4                                               |
|        17         |0.15                                              |
|        18         |0.35                                              |
|        19         |0.5                                               |
|        20         |0.2                                               |
|        21         |0.15                                              |
|        22         |0.15                                              |
|        23         |0.6                                               |
|        24         |0.5                                               |
|        25         |0.6                                               |
|        26         |0.4                                               |
|        27         |0.8                                               |
|        28         |0.3                                               |
|        29         |0.95                                              |
|        30         |0.15                                              |
|        31         |1                                                 |
|        32         |0.3                                               |
|        33         |0.3                                               |
|        34         |0.3                                               |
|        35         |0.3                                               |
|        36         |0.3                                               |
|        37         |0.45                                              |
|        38         |0.45                                              |
|        39         |0.6                                               |
|        40         |0.6                                               |
|        41         |0.6                                               |
|        42         |0.3                                               |
|        43         |0.4                                               |
|        44         |0.4                                               |
|        45         |0.15                                              |
|        46         |0.3                                               |
|        47         |0.3                                               |
|        48         |0.15                                              |
|        49         |0.15                                              |
|        50         |0.5                                               |
|        51         |0.4                                               |
|        52         |0.2                                               |
|        53         |0.2                                               |
|        54         |0.2                                               |
|        55         |0.3                                               |
|        56         |0.15                                              |
|        57         |0.3                                               |
|        58         |0.3                                               |
|        59         |0.8                                               |

 30)  KEY: "KCB_mid"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.96                                              |
|         2         |1                                                 |
|         3         |1.05                                              |
|         4         |1                                                 |
|         5         |0.96                                              |
|         6         |1.15                                              |
|         7         |1.15                                              |
|         8         |1.15                                              |
|         9         |1.05                                              |
|        10         |1.05                                              |
|        11         |1.15                                              |
|        12         |1                                                 |
|        13         |1.15                                              |
|        14         |1.1                                               |
|        15         |0.95                                              |
|        16         |0.9                                               |
|        17         |1.15                                              |
|        18         |1.2                                               |
|        19         |1.05                                              |
|        20         |0.9                                               |
|        21         |1.15                                              |
|        22         |0.95                                              |
|        23         |1                                                 |
|        24         |1.15                                              |
|        25         |1.15                                              |
|        26         |0.9                                               |
|        27         |0.85                                              |
|        28         |1                                                 |
|        29         |0.95                                              |
|        30         |1                                                 |
|        31         |1                                                 |
|        32         |1                                                 |
|        33         |1                                                 |
|        34         |1                                                 |
|        35         |1                                                 |
|        36         |1                                                 |
|        37         |1.15                                              |
|        38         |1.15                                              |
|        39         |1                                                 |
|        40         |1                                                 |
|        41         |1                                                 |
|        42         |0.9                                               |
|        43         |0.85                                              |
|        44         |0.85                                              |
|        45         |1.15                                              |
|        46         |1.2                                               |
|        47         |1.2                                               |
|        48         |1.15                                              |
|        49         |0.95                                              |
|        50         |0.95                                              |
|        51         |0.85                                              |
|        52         |0.96                                              |
|        53         |0.96                                              |
|        54         |0.96                                              |
|        55         |1.05                                              |
|        56         |0.95                                              |
|        57         |1.05                                              |
|        58         |1                                                 |
|        59         |0.85                                              |

 31)  KEY: "KCB_end"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.6                                               |
|         2         |0.55                                              |
|         3         |0.3                                               |
|         4         |0.35                                              |
|         5         |0.6                                               |
|         6         |0.25                                              |
|         7         |0.25                                              |
|         8         |0.25                                              |
|         9         |0.3                                               |
|        10         |1                                                 |
|        11         |0.25                                              |
|        12         |0.3                                               |
|        13         |0.25                                              |
|        14         |0.25                                              |
|        15         |0.9                                               |
|        16         |0.85                                              |
|        17         |0.25                                              |
|        18         |0.7                                               |
|        19         |0.95                                              |
|        20         |1.05                                              |
|        21         |0.25                                              |
|        22         |0.65                                              |
|        23         |0.75                                              |
|        24         |0.3                                               |
|        25         |1.1                                               |
|        26         |0.85                                              |
|        27         |0.85                                              |
|        28         |0.8                                               |
|        29         |0.95                                              |
|        30         |0.5                                               |
|        31         |1                                                 |
|        32         |0.8                                               |
|        33         |0.8                                               |
|        34         |0.8                                               |
|        35         |0.8                                               |
|        36         |0.8                                               |
|        37         |0.9                                               |
|        38         |0.9                                               |
|        39         |0.9                                               |
|        40         |0.9                                               |
|        41         |0.9                                               |
|        42         |0.8                                               |
|        43         |0.85                                              |
|        44         |0.85                                              |
|        45         |0.5                                               |
|        46         |0.3                                               |
|        47         |0.3                                               |
|        48         |0.25                                              |
|        49         |0.85                                              |
|        50         |0.3                                               |
|        51         |0.75                                              |
|        52         |0.86                                              |
|        53         |0.86                                              |
|        54         |0.86                                              |
|        55         |0.5                                               |
|        56         |0.85                                              |
|        57         |0.5                                               |
|        58         |0.8                                               |
|        59         |0.85                                              |

 32)  KEY: "KCB_min"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.2                                               |
|         2         |0.15                                              |
|         3         |0.2                                               |
|         4         |0.15                                              |
|         5         |0.2                                               |
|         6         |0.15                                              |
|         7         |0.15                                              |
|         8         |0.25                                              |
|         9         |0.2                                               |
|        10         |0.15                                              |
|        11         |0.15                                              |
|        12         |0.15                                              |
|        13         |0.15                                              |
|        14         |0.15                                              |
|        15         |0.4                                               |
|        16         |0.4                                               |
|        17         |0.15                                              |
|        18         |0.7                                               |
|        19         |0.5                                               |
|        20         |0.2                                               |
|        21         |0.15                                              |
|        22         |0.15                                              |
|        23         |0.6                                               |
|        24         |0.15                                              |
|        25         |0.6                                               |
|        26         |0.4                                               |
|        27         |0.8                                               |
|        28         |0.15                                              |
|        29         |0.95                                              |
|        30         |0.15                                              |
|        31         |1                                                 |
|        32         |0.15                                              |
|        33         |0.15                                              |
|        34         |0.15                                              |
|        35         |0.15                                              |
|        36         |0.15                                              |
|        37         |0.45                                              |
|        38         |0.45                                              |
|        39         |0.6                                               |
|        40         |0.6                                               |
|        41         |0.6                                               |
|        42         |0.3                                               |
|        43         |0.3                                               |
|        44         |0.3                                               |
|        45         |0.15                                              |
|        46         |0.3                                               |
|        47         |0.3                                               |
|        48         |0.15                                              |
|        49         |0.15                                              |
|        50         |0.3                                               |
|        51         |0.4                                               |
|        52         |0.2                                               |
|        53         |0.2                                               |
|        54         |0.2                                               |
|        55         |0.3                                               |
|        56         |0.15                                              |
|        57         |0.3                                               |
|        58         |0.15                                              |
|        59         |0.8                                               |

 33)  KEY: "First_day_of_growing_season"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |133                                               |
|         2         |133                                               |
|         3         |133                                               |
|         4         |133                                               |
|         5         |133                                               |
|         6         |133                                               |
|         7         |133                                               |
|         8         |133                                               |
|         9         |133                                               |
|        10         |133                                               |
|        11         |133                                               |
|        12         |133                                               |
|        13         |133                                               |
|        14         |133                                               |
|        15         |133                                               |
|        16         |133                                               |
|        17         |133                                               |
|        18         |133                                               |
|        19         |133                                               |
|        20         |133                                               |
|        21         |133                                               |
|        22         |133                                               |
|        23         |133                                               |
|        24         |133                                               |
|        25         |133                                               |
|        26         |133                                               |
|        27         |133                                               |
|        28         |133                                               |
|        29         |133                                               |
|        30         |133                                               |
|        31         |133                                               |
|        32         |133                                               |
|        33         |133                                               |
|        34         |133                                               |
|        35         |133                                               |
|        36         |133                                               |
|        37         |133                                               |
|        38         |133                                               |
|        39         |133                                               |
|        40         |133                                               |
|        41         |133                                               |
|        42         |133                                               |
|        43         |133                                               |
|        44         |133                                               |
|        45         |133                                               |
|        46         |133                                               |
|        47         |133                                               |
|        48         |133                                               |
|        49         |133                                               |
|        50         |133                                               |
|        51         |133                                               |
|        52         |133                                               |
|        53         |133                                               |
|        54         |133                                               |
|        55         |133                                               |
|        56         |133                                               |
|        57         |133                                               |
|        58         |133                                               |
|        59         |133                                               |

 34)  KEY: "Last_day_of_growing_season"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |268                                               |
|         2         |268                                               |
|         3         |268                                               |
|         4         |268                                               |
|         5         |268                                               |
|         6         |268                                               |
|         7         |268                                               |
|         8         |268                                               |
|         9         |268                                               |
|        10         |268                                               |
|        11         |268                                               |
|        12         |268                                               |
|        13         |268                                               |
|        14         |268                                               |
|        15         |268                                               |
|        16         |268                                               |
|        17         |268                                               |
|        18         |268                                               |
|        19         |268                                               |
|        20         |268                                               |
|        21         |268                                               |
|        22         |268                                               |
|        23         |268                                               |
|        24         |268                                               |
|        25         |268                                               |
|        26         |268                                               |
|        27         |268                                               |
|        28         |268                                               |
|        29         |268                                               |
|        30         |268                                               |
|        31         |268                                               |
|        32         |268                                               |
|        33         |268                                               |
|        34         |268                                               |
|        35         |268                                               |
|        36         |268                                               |
|        37         |268                                               |
|        38         |268                                               |
|        39         |268                                               |
|        40         |268                                               |
|        41         |268                                               |
|        42         |268                                               |
|        43         |268                                               |
|        44         |268                                               |
|        45         |268                                               |
|        46         |268                                               |
|        47         |268                                               |
|        48         |268                                               |
|        49         |268                                               |
|        50         |268                                               |
|        51         |268                                               |
|        52         |268                                               |
|        53         |268                                               |
|        54         |268                                               |
|        55         |268                                               |
|        56         |268                                               |
|        57         |268                                               |
|        58         |268                                               |
|        59         |268                                               |

 35)  KEY: "Planting_date"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |05/01                                             |
|         2         |05/01                                             |
|         3         |05/20                                             |
|         4         |05/01                                             |
|         5         |05/05                                             |
|         6         |04/20                                             |
|         7         |04/20                                             |
|         8         |10/01                                             |
|         9         |05/15                                             |
|        10         |04/20                                             |
|        11         |04/20                                             |
|        12         |04/20                                             |
|        13         |04/20                                             |
|        14         |04/20                                             |
|        15         |04/15                                             |
|        16         |05/01                                             |
|        17         |05/01                                             |
|        18         |04/20                                             |
|        19         |05/20                                             |
|        20         |05/10                                             |
|        21         |05/01                                             |
|        22         |04/15                                             |
|        23         |04/20                                             |
|        24         |04/15                                             |
|        25         |03/30                                             |
|        26         |03/30                                             |
|        27         |03/30                                             |
|        28         |03/30                                             |
|        29         |05/01                                             |
|        30         |03/30                                             |
|        31         |03/30                                             |
|        32         |03/30                                             |
|        33         |03/30                                             |
|        34         |03/30                                             |
|        35         |03/30                                             |
|        36         |03/30                                             |
|        37         |03/30                                             |
|        38         |03/30                                             |
|        39         |03/30                                             |
|        40         |03/30                                             |
|        41         |03/30                                             |
|        42         |04/15                                             |
|        43         |04/15                                             |
|        44         |04/15                                             |
|        45         |05/01                                             |
|        46         |05/01                                             |
|        47         |05/01                                             |
|        48         |04/15                                             |
|        49         |04/15                                             |
|        50         |02/15                                             |
|        51         |02/15                                             |
|        52         |05/01                                             |
|        53         |05/01                                             |
|        54         |05/01                                             |
|        55         |04/20                                             |
|        56         |04/20                                             |
|        57         |05/01                                             |
|        58         |03/30                                             |
|        59         |03/30                                             |

 36)  KEY: "L_ini"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |20                                                |
|         2         |20                                                |
|         3         |20                                                |
|         4         |25                                                |
|         5         |20                                                |
|         6         |40                                                |
|         7         |20                                                |
|         8         |160                                               |
|         9         |20                                                |
|        10         |20                                                |
|        11         |20                                                |
|        12         |20                                                |
|        13         |20                                                |
|        14         |20                                                |
|        15         |10                                                |
|        16         |10                                                |
|        17         |10                                                |
|        18         |50                                                |
|        19         |20                                                |
|        20         |20                                                |
|        21         |20                                                |
|        22         |15                                                |
|        23         |35                                                |
|        24         |35                                                |
|        25         |20                                                |
|        26         |20                                                |
|        27         |20                                                |
|        28         |20                                                |
|        29         |10                                                |
|        30         |20                                                |
|        31         |20                                                |
|        32         |20                                                |
|        33         |20                                                |
|        34         |20                                                |
|        35         |20                                                |
|        36         |20                                                |
|        37         |20                                                |
|        38         |20                                                |
|        39         |20                                                |
|        40         |20                                                |
|        41         |20                                                |
|        42         |10                                                |
|        43         |10                                                |
|        44         |10                                                |
|        45         |10                                                |
|        46         |10                                                |
|        47         |10                                                |
|        48         |30                                                |
|        49         |30                                                |
|        50         |90                                                |
|        51         |90                                                |
|        52         |10                                                |
|        53         |10                                                |
|        54         |30                                                |
|        55         |20                                                |
|        56         |20                                                |
|        57         |10                                                |
|        58         |20                                                |
|        59         |20                                                |

 37)  KEY: "L_dev"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |50                                                |
|         2         |35                                                |
|         3         |30                                                |
|         4         |35                                                |
|         5         |50                                                |
|         6         |40                                                |
|         7         |25                                                |
|         8         |75                                                |
|         9         |30                                                |
|        10         |25                                                |
|        11         |25                                                |
|        12         |30                                                |
|        13         |25                                                |
|        14         |25                                                |
|        15         |30                                                |
|        16         |30                                                |
|        17         |30                                                |
|        18         |40                                                |
|        19         |30                                                |
|        20         |70                                                |
|        21         |30                                                |
|        22         |25                                                |
|        23         |25                                                |
|        24         |25                                                |
|        25         |70                                                |
|        26         |70                                                |
|        27         |70                                                |
|        28         |70                                                |
|        29         |30                                                |
|        30         |70                                                |
|        31         |70                                                |
|        32         |70                                                |
|        33         |70                                                |
|        34         |70                                                |
|        35         |70                                                |
|        36         |70                                                |
|        37         |70                                                |
|        38         |70                                                |
|        39         |70                                                |
|        40         |70                                                |
|        41         |70                                                |
|        42         |25                                                |
|        43         |30                                                |
|        44         |25                                                |
|        45         |30                                                |
|        46         |30                                                |
|        47         |30                                                |
|        48         |40                                                |
|        49         |40                                                |
|        50         |30                                                |
|        51         |30                                                |
|        52         |30                                                |
|        53         |30                                                |
|        54         |50                                                |
|        55         |50                                                |
|        56         |30                                                |
|        57         |30                                                |
|        58         |70                                                |
|        59         |70                                                |

 38)  KEY: "L_mid"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |40                                                |
|         2         |40                                                |
|         3         |60                                                |
|         4         |45                                                |
|         5         |40                                                |
|         6         |50                                                |
|         7         |60                                                |
|         8         |75                                                |
|         9         |60                                                |
|        10         |60                                                |
|        11         |60                                                |
|        12         |55                                                |
|        13         |60                                                |
|        14         |60                                                |
|        15         |90                                                |
|        16         |25                                                |
|        17         |25                                                |
|        18         |50                                                |
|        19         |40                                                |
|        20         |30                                                |
|        21         |45                                                |
|        22         |70                                                |
|        23         |30                                                |
|        24         |30                                                |
|        25         |90                                                |
|        26         |90                                                |
|        27         |90                                                |
|        28         |90                                                |
|        29         |80                                                |
|        30         |90                                                |
|        31         |90                                                |
|        32         |90                                                |
|        33         |90                                                |
|        34         |90                                                |
|        35         |90                                                |
|        36         |90                                                |
|        37         |90                                                |
|        38         |90                                                |
|        39         |90                                                |
|        40         |90                                                |
|        41         |90                                                |
|        42         |35                                                |
|        43         |35                                                |
|        44         |35                                                |
|        45         |80                                                |
|        46         |80                                                |
|        47         |80                                                |
|        48         |60                                                |
|        49         |60                                                |
|        50         |200                                               |
|        51         |200                                               |
|        52         |80                                                |
|        53         |80                                                |
|        54         |40                                                |
|        55         |90                                                |
|        56         |20                                                |
|        57         |80                                                |
|        58         |90                                                |
|        59         |90                                                |

 39)  KEY: "L_late"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |30                                                |
|         2         |30                                                |
|         3         |25                                                |
|         4         |25                                                |
|         5         |20                                                |
|         6         |50                                                |
|         7         |30                                                |
|         8         |25                                                |
|         9         |25                                                |
|        10         |30                                                |
|        11         |30                                                |
|        12         |35                                                |
|        13         |30                                                |
|        14         |30                                                |
|        15         |30                                                |
|        16         |10                                                |
|        17         |10                                                |
|        18         |40                                                |
|        19         |20                                                |
|        20         |10                                                |
|        21         |30                                                |
|        22         |40                                                |
|        23         |20                                                |
|        24         |20                                                |
|        25         |30                                                |
|        26         |30                                                |
|        27         |30                                                |
|        28         |30                                                |
|        29         |20                                                |
|        30         |30                                                |
|        31         |30                                                |
|        32         |30                                                |
|        33         |30                                                |
|        34         |30                                                |
|        35         |30                                                |
|        36         |30                                                |
|        37         |30                                                |
|        38         |30                                                |
|        39         |30                                                |
|        40         |30                                                |
|        41         |30                                                |
|        42         |35                                                |
|        43         |35                                                |
|        44         |35                                                |
|        45         |20                                                |
|        46         |20                                                |
|        47         |20                                                |
|        48         |20                                                |
|        49         |20                                                |
|        50         |45                                                |
|        51         |45                                                |
|        52         |20                                                |
|        53         |20                                                |
|        54         |20                                                |
|        55         |20                                                |
|        56         |10                                                |
|        57         |20                                                |
|        58         |30                                                |
|        59         |30                                                |

 40)  KEY: "L_fallow"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |1                                                 |
|         2         |1                                                 |
|         3         |1                                                 |
|         4         |1                                                 |
|         5         |1                                                 |
|         6         |1                                                 |
|         7         |1                                                 |
|         8         |1                                                 |
|         9         |1                                                 |
|        10         |1                                                 |
|        11         |1                                                 |
|        12         |1                                                 |
|        13         |1                                                 |
|        14         |1                                                 |
|        15         |1                                                 |
|        16         |1                                                 |
|        17         |1                                                 |
|        18         |1                                                 |
|        19         |1                                                 |
|        20         |1                                                 |
|        21         |1                                                 |
|        22         |1                                                 |
|        23         |1                                                 |
|        24         |1                                                 |
|        25         |1                                                 |
|        26         |1                                                 |
|        27         |1                                                 |
|        28         |1                                                 |
|        29         |1                                                 |
|        30         |1                                                 |
|        31         |1                                                 |
|        32         |1                                                 |
|        33         |1                                                 |
|        34         |1                                                 |
|        35         |1                                                 |
|        36         |1                                                 |
|        37         |1                                                 |
|        38         |1                                                 |
|        39         |1                                                 |
|        40         |1                                                 |
|        41         |1                                                 |
|        42         |1                                                 |
|        43         |1                                                 |
|        44         |1                                                 |
|        45         |1                                                 |
|        46         |1                                                 |
|        47         |1                                                 |
|        48         |1                                                 |
|        49         |1                                                 |
|        50         |1                                                 |
|        51         |1                                                 |
|        52         |1                                                 |
|        53         |1                                                 |
|        54         |1                                                 |
|        55         |1                                                 |
|        56         |1                                                 |
|        57         |1                                                 |
|        58         |1                                                 |
|        59         |1                                                 |

 41)  KEY: "Irrigation_length"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |9999                                              |
|         2         |9999                                              |
|         3         |9999                                              |
|         4         |9999                                              |
|         5         |9999                                              |
|         6         |9999                                              |
|         7         |9999                                              |
|         8         |9999                                              |
|         9         |9999                                              |
|        10         |9999                                              |
|        11         |9999                                              |
|        12         |9999                                              |
|        13         |9999                                              |
|        14         |9999                                              |
|        15         |9999                                              |
|        16         |9999                                              |
|        17         |9999                                              |
|        18         |9999                                              |
|        19         |9999                                              |
|        20         |9999                                              |
|        21         |9999                                              |
|        22         |9999                                              |
|        23         |9999                                              |
|        24         |9999                                              |
|        25         |9999                                              |
|        26         |9999                                              |
|        27         |9999                                              |
|        28         |9999                                              |
|        29         |9999                                              |
|        30         |9999                                              |
|        31         |9999                                              |
|        32         |9999                                              |
|        33         |9999                                              |
|        34         |9999                                              |
|        35         |9999                                              |
|        36         |9999                                              |
|        37         |9999                                              |
|        38         |9999                                              |
|        39         |9999                                              |
|        40         |9999                                              |
|        41         |9999                                              |
|        42         |9999                                              |
|        43         |9999                                              |
|        44         |9999                                              |
|        45         |9999                                              |
|        46         |9999                                              |
|        47         |9999                                              |
|        48         |9999                                              |
|        49         |9999                                              |
|        50         |9999                                              |
|        51         |9999                                              |
|        52         |9999                                              |
|        53         |9999                                              |
|        54         |9999                                              |
|        55         |9999                                              |
|        56         |9999                                              |
|        57         |9999                                              |
|        58         |9999                                              |
|        59         |9999                                              |

 42)  KEY: "NA"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |DOY                                               |
|         2         |DOY                                               |
|         3         |DOY                                               |
|         4         |DOY                                               |
|         5         |DOY                                               |
|         6         |DOY                                               |
|         7         |DOY                                               |
|         8         |DOY                                               |
|         9         |DOY                                               |
|        10         |DOY                                               |
|        11         |DOY                                               |
|        12         |DOY                                               |
|        13         |DOY                                               |
|        14         |DOY                                               |
|        15         |DOY                                               |
|        16         |DOY                                               |
|        17         |DOY                                               |
|        18         |DOY                                               |
|        19         |DOY                                               |
|        20         |DOY                                               |
|        21         |DOY                                               |
|        22         |DOY                                               |
|        23         |DOY                                               |
|        24         |DOY                                               |
|        25         |DOY                                               |
|        26         |DOY                                               |
|        27         |DOY                                               |
|        28         |DOY                                               |
|        29         |DOY                                               |
|        30         |DOY                                               |
|        31         |DOY                                               |
|        32         |DOY                                               |
|        33         |DOY                                               |
|        34         |DOY                                               |
|        35         |DOY                                               |
|        36         |DOY                                               |
|        37         |DOY                                               |
|        38         |DOY                                               |
|        39         |DOY                                               |
|        40         |DOY                                               |
|        41         |DOY                                               |
|        42         |DOY                                               |
|        43         |DOY                                               |
|        44         |DOY                                               |
|        45         |DOY                                               |
|        46         |DOY                                               |
|        47         |DOY                                               |
|        48         |DOY                                               |
|        49         |DOY                                               |
|        50         |DOY                                               |
|        51         |DOY                                               |
|        52         |DOY                                               |
|        53         |DOY                                               |
|        54         |DOY                                               |
|        55         |DOY                                               |
|        56         |DOY                                               |
|        57         |DOY                                               |
|        58         |DOY                                               |
|        59         |DOY                                               |

 43)  KEY: "REW_1"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.196                                             |
|         2         |0.196                                             |
|         3         |0.196                                             |
|         4         |0.196                                             |
|         5         |0.196                                             |
|         6         |0.196                                             |
|         7         |0.196                                             |
|         8         |0.196                                             |
|         9         |0.196                                             |
|        10         |0.196                                             |
|        11         |0.196                                             |
|        12         |0.196                                             |
|        13         |0.196                                             |
|        14         |0.196                                             |
|        15         |0.196                                             |
|        16         |0.196                                             |
|        17         |0.196                                             |
|        18         |0.196                                             |
|        19         |0.196                                             |
|        20         |0.196                                             |
|        21         |0.196                                             |
|        22         |0.196                                             |
|        23         |0.196                                             |
|        24         |0.196                                             |
|        25         |0.196                                             |
|        26         |0.196                                             |
|        27         |0.196                                             |
|        28         |0.196                                             |
|        29         |0.196                                             |
|        30         |0.196                                             |
|        31         |1                                                 |
|        32         |0.196                                             |
|        33         |0.196                                             |
|        34         |0.196                                             |
|        35         |0.196                                             |
|        36         |0.196                                             |
|        37         |0.196                                             |
|        38         |0.196                                             |
|        39         |0.196                                             |
|        40         |0.196                                             |
|        41         |0.196                                             |
|        42         |0.196                                             |
|        43         |0.196                                             |
|        44         |0.196                                             |
|        45         |0.196                                             |
|        46         |0.196                                             |
|        47         |0.196                                             |
|        48         |0.196                                             |
|        49         |0.196                                             |
|        50         |0.196                                             |
|        51         |0.196                                             |
|        52         |0.196                                             |
|        53         |0.196                                             |
|        54         |0.196                                             |
|        55         |0.196                                             |
|        56         |0.196                                             |
|        57         |0.196                                             |
|        58         |0.196                                             |
|        59         |0.196                                             |

 44)  KEY: "REW_2"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.295                                             |
|         2         |0.295                                             |
|         3         |0.295                                             |
|         4         |0.295                                             |
|         5         |0.295                                             |
|         6         |0.295                                             |
|         7         |0.295                                             |
|         8         |0.295                                             |
|         9         |0.295                                             |
|        10         |0.295                                             |
|        11         |0.295                                             |
|        12         |0.295                                             |
|        13         |0.295                                             |
|        14         |0.295                                             |
|        15         |0.295                                             |
|        16         |0.295                                             |
|        17         |0.295                                             |
|        18         |0.295                                             |
|        19         |0.295                                             |
|        20         |0.295                                             |
|        21         |0.295                                             |
|        22         |0.295                                             |
|        23         |0.295                                             |
|        24         |0.295                                             |
|        25         |0.295                                             |
|        26         |0.295                                             |
|        27         |0.295                                             |
|        28         |0.295                                             |
|        29         |0.295                                             |
|        30         |0.295                                             |
|        31         |1                                                 |
|        32         |0.295                                             |
|        33         |0.295                                             |
|        34         |0.295                                             |
|        35         |0.295                                             |
|        36         |0.295                                             |
|        37         |0.295                                             |
|        38         |0.295                                             |
|        39         |0.295                                             |
|        40         |0.295                                             |
|        41         |0.295                                             |
|        42         |0.295                                             |
|        43         |0.295                                             |
|        44         |0.295                                             |
|        45         |0.295                                             |
|        46         |0.295                                             |
|        47         |0.295                                             |
|        48         |0.295                                             |
|        49         |0.295                                             |
|        50         |0.295                                             |
|        51         |0.295                                             |
|        52         |0.295                                             |
|        53         |0.295                                             |
|        54         |0.295                                             |
|        55         |0.295                                             |
|        56         |0.295                                             |
|        57         |0.295                                             |
|        58         |0.295                                             |
|        59         |0.295                                             |

 45)  KEY: "REW_3"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.393                                             |
|         2         |0.393                                             |
|         3         |0.393                                             |
|         4         |0.393                                             |
|         5         |0.393                                             |
|         6         |0.393                                             |
|         7         |0.393                                             |
|         8         |0.393                                             |
|         9         |0.393                                             |
|        10         |0.393                                             |
|        11         |0.393                                             |
|        12         |0.393                                             |
|        13         |0.393                                             |
|        14         |0.393                                             |
|        15         |0.393                                             |
|        16         |0.393                                             |
|        17         |0.393                                             |
|        18         |0.393                                             |
|        19         |0.393                                             |
|        20         |0.393                                             |
|        21         |0.393                                             |
|        22         |0.393                                             |
|        23         |0.393                                             |
|        24         |0.393                                             |
|        25         |0.393                                             |
|        26         |0.393                                             |
|        27         |0.393                                             |
|        28         |0.393                                             |
|        29         |0.393                                             |
|        30         |0.393                                             |
|        31         |1                                                 |
|        32         |0.393                                             |
|        33         |0.393                                             |
|        34         |0.393                                             |
|        35         |0.393                                             |
|        36         |0.393                                             |
|        37         |0.393                                             |
|        38         |0.393                                             |
|        39         |0.393                                             |
|        40         |0.393                                             |
|        41         |0.393                                             |
|        42         |0.393                                             |
|        43         |0.393                                             |
|        44         |0.393                                             |
|        45         |0.393                                             |
|        46         |0.393                                             |
|        47         |0.393                                             |
|        48         |0.393                                             |
|        49         |0.393                                             |
|        50         |0.393                                             |
|        51         |0.393                                             |
|        52         |0.393                                             |
|        53         |0.393                                             |
|        54         |0.393                                             |
|        55         |0.393                                             |
|        56         |0.393                                             |
|        57         |0.393                                             |
|        58         |0.393                                             |
|        59         |0.393                                             |

 46)  KEY: "REW_4"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.472                                             |
|         2         |0.472                                             |
|         3         |0.472                                             |
|         4         |0.472                                             |
|         5         |0.472                                             |
|         6         |0.472                                             |
|         7         |0.472                                             |
|         8         |0.472                                             |
|         9         |0.472                                             |
|        10         |0.472                                             |
|        11         |0.472                                             |
|        12         |0.472                                             |
|        13         |0.472                                             |
|        14         |0.472                                             |
|        15         |0.472                                             |
|        16         |0.472                                             |
|        17         |0.472                                             |
|        18         |0.472                                             |
|        19         |0.472                                             |
|        20         |0.472                                             |
|        21         |0.472                                             |
|        22         |0.472                                             |
|        23         |0.472                                             |
|        24         |0.472                                             |
|        25         |0.472                                             |
|        26         |0.472                                             |
|        27         |0.472                                             |
|        28         |0.472                                             |
|        29         |0.472                                             |
|        30         |0.472                                             |
|        31         |1                                                 |
|        32         |0.472                                             |
|        33         |0.472                                             |
|        34         |0.472                                             |
|        35         |0.472                                             |
|        36         |0.472                                             |
|        37         |0.472                                             |
|        38         |0.472                                             |
|        39         |0.472                                             |
|        40         |0.472                                             |
|        41         |0.472                                             |
|        42         |0.472                                             |
|        43         |0.472                                             |
|        44         |0.472                                             |
|        45         |0.472                                             |
|        46         |0.472                                             |
|        47         |0.472                                             |
|        48         |0.472                                             |
|        49         |0.472                                             |
|        50         |0.472                                             |
|        51         |0.472                                             |
|        52         |0.472                                             |
|        53         |0.472                                             |
|        54         |0.472                                             |
|        55         |0.472                                             |
|        56         |0.472                                             |
|        57         |0.472                                             |
|        58         |0.472                                             |
|        59         |0.472                                             |

 47)  KEY: "REW_5"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.196                                             |
|         2         |0.196                                             |
|         3         |0.196                                             |
|         4         |0.196                                             |
|         5         |0.196                                             |
|         6         |0.196                                             |
|         7         |0.196                                             |
|         8         |0.196                                             |
|         9         |0.196                                             |
|        10         |0.196                                             |
|        11         |0.196                                             |
|        12         |0.196                                             |
|        13         |0.196                                             |
|        14         |0.196                                             |
|        15         |0.196                                             |
|        16         |0.196                                             |
|        17         |0.196                                             |
|        18         |0.196                                             |
|        19         |0.196                                             |
|        20         |0.196                                             |
|        21         |0.196                                             |
|        22         |0.196                                             |
|        23         |0.196                                             |
|        24         |0.196                                             |
|        25         |0.196                                             |
|        26         |0.196                                             |
|        27         |0.196                                             |
|        28         |0.196                                             |
|        29         |0.196                                             |
|        30         |0.196                                             |
|        31         |1                                                 |
|        32         |0.196                                             |
|        33         |0.196                                             |
|        34         |0.196                                             |
|        35         |0.196                                             |
|        36         |0.196                                             |
|        37         |0.196                                             |
|        38         |0.196                                             |
|        39         |0.196                                             |
|        40         |0.196                                             |
|        41         |0.196                                             |
|        42         |0.196                                             |
|        43         |0.196                                             |
|        44         |0.196                                             |
|        45         |0.196                                             |
|        46         |0.196                                             |
|        47         |0.196                                             |
|        48         |0.196                                             |
|        49         |0.196                                             |
|        50         |0.196                                             |
|        51         |0.196                                             |
|        52         |0.196                                             |
|        53         |0.196                                             |
|        54         |0.196                                             |
|        55         |0.196                                             |
|        56         |0.196                                             |
|        57         |0.196                                             |
|        58         |0.196                                             |
|        59         |0.196                                             |

 48)  KEY: "REW_6"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.472                                             |
|         2         |0.472                                             |
|         3         |0.472                                             |
|         4         |0.472                                             |
|         5         |0.472                                             |
|         6         |0.472                                             |
|         7         |0.472                                             |
|         8         |0.472                                             |
|         9         |0.472                                             |
|        10         |0.472                                             |
|        11         |0.472                                             |
|        12         |0.472                                             |
|        13         |0.472                                             |
|        14         |0.472                                             |
|        15         |0.472                                             |
|        16         |0.472                                             |
|        17         |0.472                                             |
|        18         |0.472                                             |
|        19         |0.472                                             |
|        20         |0.472                                             |
|        21         |0.472                                             |
|        22         |0.472                                             |
|        23         |0.472                                             |
|        24         |0.472                                             |
|        25         |0.472                                             |
|        26         |0.472                                             |
|        27         |0.472                                             |
|        28         |0.472                                             |
|        29         |0.472                                             |
|        30         |0.472                                             |
|        31         |1                                                 |
|        32         |0.472                                             |
|        33         |0.472                                             |
|        34         |0.472                                             |
|        35         |0.472                                             |
|        36         |0.472                                             |
|        37         |0.472                                             |
|        38         |0.472                                             |
|        39         |0.472                                             |
|        40         |0.472                                             |
|        41         |0.472                                             |
|        42         |0.472                                             |
|        43         |0.472                                             |
|        44         |0.472                                             |
|        45         |0.472                                             |
|        46         |0.472                                             |
|        47         |0.472                                             |
|        48         |0.472                                             |
|        49         |0.472                                             |
|        50         |0.472                                             |
|        51         |0.472                                             |
|        52         |0.472                                             |
|        53         |0.472                                             |
|        54         |0.472                                             |
|        55         |0.472                                             |
|        56         |0.472                                             |
|        57         |0.472                                             |
|        58         |0.472                                             |
|        59         |0.472                                             |

 49)  KEY: "REW_7"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.472                                             |
|         2         |0.472                                             |
|         3         |0.472                                             |
|         4         |0.472                                             |
|         5         |0.472                                             |
|         6         |0.472                                             |
|         7         |0.472                                             |
|         8         |0.472                                             |
|         9         |0.472                                             |
|        10         |0.472                                             |
|        11         |0.472                                             |
|        12         |0.472                                             |
|        13         |0.472                                             |
|        14         |0.472                                             |
|        15         |0.472                                             |
|        16         |0.472                                             |
|        17         |0.472                                             |
|        18         |0.472                                             |
|        19         |0.472                                             |
|        20         |0.472                                             |
|        21         |0.472                                             |
|        22         |0.472                                             |
|        23         |0.472                                             |
|        24         |0.472                                             |
|        25         |0.472                                             |
|        26         |0.472                                             |
|        27         |0.472                                             |
|        28         |0.472                                             |
|        29         |0.472                                             |
|        30         |0.472                                             |
|        31         |1                                                 |
|        32         |0.472                                             |
|        33         |0.472                                             |
|        34         |0.472                                             |
|        35         |0.472                                             |
|        36         |0.472                                             |
|        37         |0.472                                             |
|        38         |0.472                                             |
|        39         |0.472                                             |
|        40         |0.472                                             |
|        41         |0.472                                             |
|        42         |0.472                                             |
|        43         |0.472                                             |
|        44         |0.472                                             |
|        45         |0.472                                             |
|        46         |0.472                                             |
|        47         |0.472                                             |
|        48         |0.472                                             |
|        49         |0.472                                             |
|        50         |0.472                                             |
|        51         |0.472                                             |
|        52         |0.472                                             |
|        53         |0.472                                             |
|        54         |0.472                                             |
|        55         |0.472                                             |
|        56         |0.472                                             |
|        57         |0.472                                             |
|        58         |0.472                                             |
|        59         |0.472                                             |

 50)  KEY: "TEW_1"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.354                                             |
|         2         |0.354                                             |
|         3         |0.354                                             |
|         4         |0.354                                             |
|         5         |0.354                                             |
|         6         |0.354                                             |
|         7         |0.354                                             |
|         8         |0.354                                             |
|         9         |0.354                                             |
|        10         |0.354                                             |
|        11         |0.354                                             |
|        12         |0.354                                             |
|        13         |0.354                                             |
|        14         |0.354                                             |
|        15         |0.354                                             |
|        16         |0.354                                             |
|        17         |0.354                                             |
|        18         |0.354                                             |
|        19         |0.354                                             |
|        20         |0.354                                             |
|        21         |0.354                                             |
|        22         |0.354                                             |
|        23         |0.354                                             |
|        24         |0.354                                             |
|        25         |0.354                                             |
|        26         |0.354                                             |
|        27         |0.354                                             |
|        28         |0.354                                             |
|        29         |0.354                                             |
|        30         |0.354                                             |
|        31         |1                                                 |
|        32         |0.354                                             |
|        33         |0.354                                             |
|        34         |0.354                                             |
|        35         |0.354                                             |
|        36         |0.354                                             |
|        37         |0.354                                             |
|        38         |0.354                                             |
|        39         |0.354                                             |
|        40         |0.354                                             |
|        41         |0.354                                             |
|        42         |0.354                                             |
|        43         |0.354                                             |
|        44         |0.354                                             |
|        45         |0.354                                             |
|        46         |0.354                                             |
|        47         |0.354                                             |
|        48         |0.354                                             |
|        49         |0.354                                             |
|        50         |0.354                                             |
|        51         |0.354                                             |
|        52         |0.354                                             |
|        53         |0.354                                             |
|        54         |0.354                                             |
|        55         |0.354                                             |
|        56         |0.354                                             |
|        57         |0.354                                             |
|        58         |0.354                                             |
|        59         |0.354                                             |

 51)  KEY: "TEW_2"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.669                                             |
|         2         |0.669                                             |
|         3         |0.669                                             |
|         4         |0.669                                             |
|         5         |0.669                                             |
|         6         |0.669                                             |
|         7         |0.669                                             |
|         8         |0.669                                             |
|         9         |0.669                                             |
|        10         |0.669                                             |
|        11         |0.669                                             |
|        12         |0.669                                             |
|        13         |0.669                                             |
|        14         |0.669                                             |
|        15         |0.669                                             |
|        16         |0.669                                             |
|        17         |0.669                                             |
|        18         |0.669                                             |
|        19         |0.669                                             |
|        20         |0.669                                             |
|        21         |0.669                                             |
|        22         |0.669                                             |
|        23         |0.669                                             |
|        24         |0.669                                             |
|        25         |0.669                                             |
|        26         |0.669                                             |
|        27         |0.669                                             |
|        28         |0.669                                             |
|        29         |0.669                                             |
|        30         |0.669                                             |
|        31         |1                                                 |
|        32         |0.669                                             |
|        33         |0.669                                             |
|        34         |0.669                                             |
|        35         |0.669                                             |
|        36         |0.669                                             |
|        37         |0.669                                             |
|        38         |0.669                                             |
|        39         |0.669                                             |
|        40         |0.669                                             |
|        41         |0.669                                             |
|        42         |0.669                                             |
|        43         |0.669                                             |
|        44         |0.669                                             |
|        45         |0.669                                             |
|        46         |0.669                                             |
|        47         |0.669                                             |
|        48         |0.669                                             |
|        49         |0.669                                             |
|        50         |0.669                                             |
|        51         |0.669                                             |
|        52         |0.669                                             |
|        53         |0.669                                             |
|        54         |0.669                                             |
|        55         |0.669                                             |
|        56         |0.669                                             |
|        57         |0.669                                             |
|        58         |0.669                                             |
|        59         |0.669                                             |

 52)  KEY: "TEW_3"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.906                                             |
|         2         |0.906                                             |
|         3         |0.906                                             |
|         4         |0.906                                             |
|         5         |0.906                                             |
|         6         |0.906                                             |
|         7         |0.906                                             |
|         8         |0.906                                             |
|         9         |0.906                                             |
|        10         |0.906                                             |
|        11         |0.906                                             |
|        12         |0.906                                             |
|        13         |0.906                                             |
|        14         |0.906                                             |
|        15         |0.906                                             |
|        16         |0.906                                             |
|        17         |0.906                                             |
|        18         |0.906                                             |
|        19         |0.906                                             |
|        20         |0.906                                             |
|        21         |0.906                                             |
|        22         |0.906                                             |
|        23         |0.906                                             |
|        24         |0.906                                             |
|        25         |0.906                                             |
|        26         |0.906                                             |
|        27         |0.906                                             |
|        28         |0.906                                             |
|        29         |0.906                                             |
|        30         |0.906                                             |
|        31         |1                                                 |
|        32         |0.906                                             |
|        33         |0.906                                             |
|        34         |0.906                                             |
|        35         |0.906                                             |
|        36         |0.906                                             |
|        37         |0.906                                             |
|        38         |0.906                                             |
|        39         |0.906                                             |
|        40         |0.906                                             |
|        41         |0.906                                             |
|        42         |0.906                                             |
|        43         |0.906                                             |
|        44         |0.906                                             |
|        45         |0.906                                             |
|        46         |0.906                                             |
|        47         |0.906                                             |
|        48         |0.906                                             |
|        49         |0.906                                             |
|        50         |0.906                                             |
|        51         |0.906                                             |
|        52         |0.906                                             |
|        53         |0.906                                             |
|        54         |0.906                                             |
|        55         |0.906                                             |
|        56         |0.906                                             |
|        57         |0.906                                             |
|        58         |0.906                                             |
|        59         |0.906                                             |

 53)  KEY: "TEW_4"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |1.063                                             |
|         2         |1.063                                             |
|         3         |1.063                                             |
|         4         |1.063                                             |
|         5         |1.063                                             |
|         6         |1.063                                             |
|         7         |1.063                                             |
|         8         |1.063                                             |
|         9         |1.063                                             |
|        10         |1.063                                             |
|        11         |1.063                                             |
|        12         |1.063                                             |
|        13         |1.063                                             |
|        14         |1.063                                             |
|        15         |1.063                                             |
|        16         |1.063                                             |
|        17         |1.063                                             |
|        18         |1.063                                             |
|        19         |1.063                                             |
|        20         |1.063                                             |
|        21         |1.063                                             |
|        22         |1.063                                             |
|        23         |1.063                                             |
|        24         |1.063                                             |
|        25         |1.063                                             |
|        26         |1.063                                             |
|        27         |1.063                                             |
|        28         |1.063                                             |
|        29         |1.063                                             |
|        30         |1.063                                             |
|        31         |1                                                 |
|        32         |1.063                                             |
|        33         |1.063                                             |
|        34         |1.063                                             |
|        35         |1.063                                             |
|        36         |1.063                                             |
|        37         |1.063                                             |
|        38         |1.063                                             |
|        39         |1.063                                             |
|        40         |1.063                                             |
|        41         |1.063                                             |
|        42         |1.063                                             |
|        43         |1.063                                             |
|        44         |1.063                                             |
|        45         |1.063                                             |
|        46         |1.063                                             |
|        47         |1.063                                             |
|        48         |1.063                                             |
|        49         |1.063                                             |
|        50         |1.063                                             |
|        51         |1.063                                             |
|        52         |1.063                                             |
|        53         |1.063                                             |
|        54         |1.063                                             |
|        55         |1.063                                             |
|        56         |1.063                                             |
|        57         |1.063                                             |
|        58         |1.063                                             |
|        59         |1.063                                             |

 54)  KEY: "TEW_5"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.354                                             |
|         2         |0.354                                             |
|         3         |0.354                                             |
|         4         |0.354                                             |
|         5         |0.354                                             |
|         6         |0.354                                             |
|         7         |0.354                                             |
|         8         |0.354                                             |
|         9         |0.354                                             |
|        10         |0.354                                             |
|        11         |0.354                                             |
|        12         |0.354                                             |
|        13         |0.354                                             |
|        14         |0.354                                             |
|        15         |0.354                                             |
|        16         |0.354                                             |
|        17         |0.354                                             |
|        18         |0.354                                             |
|        19         |0.354                                             |
|        20         |0.354                                             |
|        21         |0.354                                             |
|        22         |0.354                                             |
|        23         |0.354                                             |
|        24         |0.354                                             |
|        25         |0.354                                             |
|        26         |0.354                                             |
|        27         |0.354                                             |
|        28         |0.354                                             |
|        29         |0.354                                             |
|        30         |0.354                                             |
|        31         |1                                                 |
|        32         |0.354                                             |
|        33         |0.354                                             |
|        34         |0.354                                             |
|        35         |0.354                                             |
|        36         |0.354                                             |
|        37         |0.354                                             |
|        38         |0.354                                             |
|        39         |0.354                                             |
|        40         |0.354                                             |
|        41         |0.354                                             |
|        42         |0.354                                             |
|        43         |0.354                                             |
|        44         |0.354                                             |
|        45         |0.354                                             |
|        46         |0.354                                             |
|        47         |0.354                                             |
|        48         |0.354                                             |
|        49         |0.354                                             |
|        50         |0.354                                             |
|        51         |0.354                                             |
|        52         |0.354                                             |
|        53         |0.354                                             |
|        54         |0.354                                             |
|        55         |0.354                                             |
|        56         |0.354                                             |
|        57         |0.354                                             |
|        58         |0.354                                             |
|        59         |0.354                                             |

 55)  KEY: "TEW_6"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |1.063                                             |
|         2         |1.063                                             |
|         3         |1.063                                             |
|         4         |1.063                                             |
|         5         |1.063                                             |
|         6         |1.063                                             |
|         7         |1.063                                             |
|         8         |1.063                                             |
|         9         |1.063                                             |
|        10         |1.063                                             |
|        11         |1.063                                             |
|        12         |1.063                                             |
|        13         |1.063                                             |
|        14         |1.063                                             |
|        15         |1.063                                             |
|        16         |1.063                                             |
|        17         |1.063                                             |
|        18         |1.063                                             |
|        19         |1.063                                             |
|        20         |1.063                                             |
|        21         |1.063                                             |
|        22         |1.063                                             |
|        23         |1.063                                             |
|        24         |1.063                                             |
|        25         |1.063                                             |
|        26         |1.063                                             |
|        27         |1.063                                             |
|        28         |1.063                                             |
|        29         |1.063                                             |
|        30         |1.063                                             |
|        31         |1                                                 |
|        32         |1.063                                             |
|        33         |1.063                                             |
|        34         |1.063                                             |
|        35         |1.063                                             |
|        36         |1.063                                             |
|        37         |1.063                                             |
|        38         |1.063                                             |
|        39         |1.063                                             |
|        40         |1.063                                             |
|        41         |1.063                                             |
|        42         |1.063                                             |
|        43         |1.063                                             |
|        44         |1.063                                             |
|        45         |1.063                                             |
|        46         |1.063                                             |
|        47         |1.063                                             |
|        48         |1.063                                             |
|        49         |1.063                                             |
|        50         |1.063                                             |
|        51         |1.063                                             |
|        52         |1.063                                             |
|        53         |1.063                                             |
|        54         |1.063                                             |
|        55         |1.063                                             |
|        56         |1.063                                             |
|        57         |1.063                                             |
|        58         |1.063                                             |
|        59         |1.063                                             |

 56)  KEY: "TEW_7"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |1.063                                             |
|         2         |1.063                                             |
|         3         |1.063                                             |
|         4         |1.063                                             |
|         5         |1.063                                             |
|         6         |1.063                                             |
|         7         |1.063                                             |
|         8         |1.063                                             |
|         9         |1.063                                             |
|        10         |1.063                                             |
|        11         |1.063                                             |
|        12         |1.063                                             |
|        13         |1.063                                             |
|        14         |1.063                                             |
|        15         |1.063                                             |
|        16         |1.063                                             |
|        17         |1.063                                             |
|        18         |1.063                                             |
|        19         |1.063                                             |
|        20         |1.063                                             |
|        21         |1.063                                             |
|        22         |1.063                                             |
|        23         |1.063                                             |
|        24         |1.063                                             |
|        25         |1.063                                             |
|        26         |1.063                                             |
|        27         |1.063                                             |
|        28         |1.063                                             |
|        29         |1.063                                             |
|        30         |1.063                                             |
|        31         |1                                                 |
|        32         |1.063                                             |
|        33         |1.063                                             |
|        34         |1.063                                             |
|        35         |1.063                                             |
|        36         |1.063                                             |
|        37         |1.063                                             |
|        38         |1.063                                             |
|        39         |1.063                                             |
|        40         |1.063                                             |
|        41         |1.063                                             |
|        42         |1.063                                             |
|        43         |1.063                                             |
|        44         |1.063                                             |
|        45         |1.063                                             |
|        46         |1.063                                             |
|        47         |1.063                                             |
|        48         |1.063                                             |
|        49         |1.063                                             |
|        50         |1.063                                             |
|        51         |1.063                                             |
|        52         |1.063                                             |
|        53         |1.063                                             |
|        54         |1.063                                             |
|        55         |1.063                                             |
|        56         |1.063                                             |
|        57         |1.063                                             |
|        58         |1.063                                             |
|        59         |1.063                                             |

 57)  KEY: "Depletion_Fraction"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.55                                              |
|         2         |0.5                                               |
|         3         |0.5                                               |
|         4         |0.45                                              |
|         5         |0.55                                              |
|         6         |0.55                                              |
|         7         |0.55                                              |
|         8         |0.55                                              |
|         9         |0.55                                              |
|        10         |0.55                                              |
|        11         |0.55                                              |
|        12         |0.55                                              |
|        13         |0.55                                              |
|        14         |0.55                                              |
|        15         |0.6                                               |
|        16         |0.55                                              |
|        17         |0.55                                              |
|        18         |0.55                                              |
|        19         |0.45                                              |
|        20         |0.45                                              |
|        21         |0.35                                              |
|        22         |0.3                                               |
|        23         |0.35                                              |
|        24         |0.35                                              |
|        25         |0.4                                               |
|        26         |0.55                                              |
|        27         |0.5                                               |
|        28         |0.5                                               |
|        29         |0.7                                               |
|        30         |0.5                                               |
|        31         |1                                                 |
|        32         |0.1                                               |
|        33         |0.5                                               |
|        34         |0.5                                               |
|        35         |0.5                                               |
|        36         |0.5                                               |
|        37         |0.5                                               |
|        38         |0.7                                               |
|        39         |0.6                                               |
|        40         |0.5                                               |
|        41         |0.5                                               |
|        42         |0.55                                              |
|        43         |0.55                                              |
|        44         |0.55                                              |
|        45         |0.55                                              |
|        46         |0.6                                               |
|        47         |0.5                                               |
|        48         |0.35                                              |
|        49         |0.35                                              |
|        50         |0.45                                              |
|        51         |0.45                                              |
|        52         |0.55                                              |
|        53         |0.55                                              |
|        54         |0.55                                              |
|        55         |0.45                                              |
|        56         |0.45                                              |
|        57         |0.5                                               |
|        58         |0.5                                               |
|        59         |0.5                                               |

 58)  KEY: "GDD_Base"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |50                                                |
|         2         |50                                                |
|         3         |50                                                |
|         4         |50                                                |
|         5         |50                                                |
|         6         |50                                                |
|         7         |50                                                |
|         8         |50                                                |
|         9         |50                                                |
|        10         |50                                                |
|        11         |50                                                |
|        12         |50                                                |
|        13         |50                                                |
|        14         |50                                                |
|        15         |50                                                |
|        16         |50                                                |
|        17         |50                                                |
|        18         |50                                                |
|        19         |50                                                |
|        20         |50                                                |
|        21         |50                                                |
|        22         |50                                                |
|        23         |50                                                |
|        24         |50                                                |
|        25         |50                                                |
|        26         |50                                                |
|        27         |50                                                |
|        28         |50                                                |
|        29         |50                                                |
|        30         |50                                                |
|        31         |50                                                |
|        32         |50                                                |
|        33         |50                                                |
|        34         |50                                                |
|        35         |50                                                |
|        36         |50                                                |
|        37         |50                                                |
|        38         |50                                                |
|        39         |50                                                |
|        40         |50                                                |
|        41         |50                                                |
|        42         |50                                                |
|        43         |50                                                |
|        44         |50                                                |
|        45         |50                                                |
|        46         |50                                                |
|        47         |50                                                |
|        48         |50                                                |
|        49         |50                                                |
|        50         |50                                                |
|        51         |50                                                |
|        52         |50                                                |
|        53         |50                                                |
|        54         |50                                                |
|        55         |50                                                |
|        56         |50                                                |
|        57         |50                                                |
|        58         |50                                                |
|        59         |50                                                |

 59)  KEY: "GDD_Max"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |86                                                |
|         2         |86                                                |
|         3         |86                                                |
|         4         |86                                                |
|         5         |86                                                |
|         6         |86                                                |
|         7         |86                                                |
|         8         |86                                                |
|         9         |86                                                |
|        10         |86                                                |
|        11         |86                                                |
|        12         |86                                                |
|        13         |86                                                |
|        14         |86                                                |
|        15         |86                                                |
|        16         |86                                                |
|        17         |86                                                |
|        18         |86                                                |
|        19         |86                                                |
|        20         |86                                                |
|        21         |86                                                |
|        22         |86                                                |
|        23         |86                                                |
|        24         |86                                                |
|        25         |86                                                |
|        26         |86                                                |
|        27         |86                                                |
|        28         |86                                                |
|        29         |86                                                |
|        30         |86                                                |
|        31         |86                                                |
|        32         |86                                                |
|        33         |86                                                |
|        34         |86                                                |
|        35         |86                                                |
|        36         |86                                                |
|        37         |86                                                |
|        38         |86                                                |
|        39         |86                                                |
|        40         |86                                                |
|        41         |86                                                |
|        42         |86                                                |
|        43         |86                                                |
|        44         |86                                                |
|        45         |86                                                |
|        46         |86                                                |
|        47         |86                                                |
|        48         |86                                                |
|        49         |86                                                |
|        50         |86                                                |
|        51         |86                                                |
|        52         |86                                                |
|        53         |86                                                |
|        54         |86                                                |
|        55         |86                                                |
|        56         |86                                                |
|        57         |86                                                |
|        58         |86                                                |
|        59         |86                                                |

 60)  KEY: "MAD"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.5                                               |
|         2         |0.5                                               |
|         3         |0.5                                               |
|         4         |0.45                                              |
|         5         |0.5                                               |
|         6         |0.55                                              |
|         7         |0.55                                              |
|         8         |0.55                                              |
|         9         |0.55                                              |
|        10         |0.55                                              |
|        11         |0.55                                              |
|        12         |0.55                                              |
|        13         |0.55                                              |
|        14         |0.55                                              |
|        15         |0.6                                               |
|        16         |0.55                                              |
|        17         |0.55                                              |
|        18         |0.55                                              |
|        19         |0.55                                              |
|        20         |0.55                                              |
|        21         |0.55                                              |
|        22         |0.55                                              |
|        23         |0.35                                              |
|        24         |0.35                                              |
|        25         |0.4                                               |
|        26         |0.55                                              |
|        27         |1                                                 |
|        28         |1                                                 |
|        29         |1                                                 |
|        30         |1                                                 |
|        31         |1                                                 |
|        32         |1                                                 |
|        33         |1                                                 |
|        34         |1                                                 |
|        35         |1                                                 |
|        36         |1                                                 |
|        37         |1                                                 |
|        38         |1                                                 |
|        39         |1                                                 |
|        40         |1                                                 |
|        41         |1                                                 |
|        42         |1                                                 |
|        43         |1                                                 |
|        44         |1                                                 |
|        45         |0.55                                              |
|        46         |1                                                 |
|        47         |1                                                 |
|        48         |0.35                                              |
|        49         |0.35                                              |
|        50         |0.45                                              |
|        51         |0.45                                              |
|        52         |0.55                                              |
|        53         |0.55                                              |
|        54         |0.55                                              |
|        55         |0.45                                              |
|        56         |0.45                                              |
|        57         |0.5                                               |
|        58         |0.1                                               |
|        59         |0.1                                               |

 61)  KEY: "Irrigation_Start"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |06/15                                             |
|         2         |06/15                                             |
|         3         |08/01                                             |
|         4         |06/15                                             |
|         5         |06/15                                             |
|         6         |06/15                                             |
|         7         |06/15                                             |
|         8         |06/15                                             |
|         9         |06/15                                             |
|        10         |06/15                                             |
|        11         |06/15                                             |
|        12         |06/15                                             |
|        13         |06/15                                             |
|        14         |06/15                                             |
|        15         |04/01                                             |
|        16         |06/15                                             |
|        17         |06/15                                             |
|        18         |06/15                                             |
|        19         |06/15                                             |
|        20         |06/15                                             |
|        21         |06/15                                             |
|        22         |06/15                                             |
|        23         |06/15                                             |
|        24         |06/15                                             |
|        25         |06/15                                             |
|        26         |06/15                                             |
|        27         |06/15                                             |
|        28         |06/15                                             |
|        29         |06/15                                             |
|        30         |06/15                                             |
|        31         |06/15                                             |
|        32         |06/15                                             |
|        33         |06/15                                             |
|        34         |06/15                                             |
|        35         |06/15                                             |
|        36         |06/15                                             |
|        37         |06/15                                             |
|        38         |06/15                                             |
|        39         |06/15                                             |
|        40         |06/15                                             |
|        41         |06/15                                             |
|        42         |06/15                                             |
|        43         |06/15                                             |
|        44         |06/15                                             |
|        45         |06/15                                             |
|        46         |06/15                                             |
|        47         |06/15                                             |
|        48         |06/15                                             |
|        49         |06/15                                             |
|        50         |06/15                                             |
|        51         |06/15                                             |
|        52         |06/15                                             |
|        53         |06/15                                             |
|        54         |06/15                                             |
|        55         |06/15                                             |
|        56         |06/15                                             |
|        57         |06/15                                             |
|        58         |07/01                                             |
|        59         |07/01                                             |

 62)  KEY: "Irrigation_End"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |09/10                                             |
|         2         |09/01                                             |
|         3         |09/10                                             |
|         4         |09/01                                             |
|         5         |09/01                                             |
|         6         |09/01                                             |
|         7         |09/01                                             |
|         8         |09/01                                             |
|         9         |09/01                                             |
|        10         |09/01                                             |
|        11         |09/01                                             |
|        12         |09/01                                             |
|        13         |09/01                                             |
|        14         |09/01                                             |
|        15         |09/01                                             |
|        16         |09/01                                             |
|        17         |09/01                                             |
|        18         |09/01                                             |
|        19         |09/01                                             |
|        20         |09/01                                             |
|        21         |09/01                                             |
|        22         |09/01                                             |
|        23         |09/01                                             |
|        24         |09/01                                             |
|        25         |09/01                                             |
|        26         |09/01                                             |
|        27         |09/01                                             |
|        28         |09/01                                             |
|        29         |09/01                                             |
|        30         |09/01                                             |
|        31         |09/01                                             |
|        32         |09/01                                             |
|        33         |09/01                                             |
|        34         |09/01                                             |
|        35         |09/01                                             |
|        36         |09/01                                             |
|        37         |09/01                                             |
|        38         |09/01                                             |
|        39         |09/01                                             |
|        40         |09/01                                             |
|        41         |09/01                                             |
|        42         |09/01                                             |
|        43         |09/01                                             |
|        44         |09/01                                             |
|        45         |09/01                                             |
|        46         |09/01                                             |
|        47         |09/01                                             |
|        48         |09/01                                             |
|        49         |09/01                                             |
|        50         |09/01                                             |
|        51         |09/01                                             |
|        52         |09/01                                             |
|        53         |09/01                                             |
|        54         |09/01                                             |
|        55         |09/01                                             |
|        56         |09/01                                             |
|        57         |09/01                                             |
|        58         |11/01                                             |
|        59         |12/01                                             |

 63)  KEY: "Application_amount"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.5                                               |
|         2         |0.5                                               |
|         3         |0.5                                               |
|         4         |0.5                                               |
|         5         |0.5                                               |
|         6         |0.5                                               |
|         7         |0.5                                               |
|         8         |0.5                                               |
|         9         |0.5                                               |
|        10         |0.5                                               |
|        11         |0.5                                               |
|        12         |0.5                                               |
|        13         |0.5                                               |
|        14         |0.5                                               |
|        15         |0.2                                               |
|        16         |0.5                                               |
|        17         |0.5                                               |
|        18         |0.5                                               |
|        19         |0.5                                               |
|        20         |0.5                                               |
|        21         |0.5                                               |
|        22         |0.5                                               |
|        23         |0.5                                               |
|        24         |0.5                                               |
|        25         |0.5                                               |
|        26         |0.5                                               |
|        27         |0.5                                               |
|        28         |0.5                                               |
|        29         |0.5                                               |
|        30         |0.5                                               |
|        31         |0                                                 |
|        32         |0                                                 |
|        33         |0                                                 |
|        34         |0                                                 |
|        35         |0                                                 |
|        36         |0                                                 |
|        37         |0                                                 |
|        38         |0                                                 |
|        39         |0                                                 |
|        40         |0                                                 |
|        41         |0                                                 |
|        42         |0                                                 |
|        43         |0                                                 |
|        44         |0                                                 |
|        45         |0.5                                               |
|        46         |0                                                 |
|        47         |0                                                 |
|        48         |0.5                                               |
|        49         |0.5                                               |
|        50         |0.5                                               |
|        51         |0.5                                               |
|        52         |0.5                                               |
|        53         |0.5                                               |
|        54         |0.5                                               |
|        55         |0.5                                               |
|        56         |0.5                                               |
|        57         |0.5                                               |
|        58         |0.17                                              |
|        59         |1.25                                              |

 64)  KEY: "Fraction_irrigation_from_GW"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |1                                                 |
|         2         |1                                                 |
|         3         |1                                                 |
|         4         |1                                                 |
|         5         |1                                                 |
|         6         |1                                                 |
|         7         |1                                                 |
|         8         |1                                                 |
|         9         |1                                                 |
|        10         |1                                                 |
|        11         |1                                                 |
|        12         |1                                                 |
|        13         |1                                                 |
|        14         |1                                                 |
|        15         |1                                                 |
|        16         |1                                                 |
|        17         |1                                                 |
|        18         |1                                                 |
|        19         |1                                                 |
|        20         |1                                                 |
|        21         |1                                                 |
|        22         |1                                                 |
|        23         |1                                                 |
|        24         |1                                                 |
|        25         |1                                                 |
|        26         |1                                                 |
|        27         |1                                                 |
|        28         |1                                                 |
|        29         |1                                                 |
|        30         |1                                                 |
|        31         |1                                                 |
|        32         |1                                                 |
|        33         |1                                                 |
|        34         |1                                                 |
|        35         |1                                                 |
|        36         |1                                                 |
|        37         |1                                                 |
|        38         |1                                                 |
|        39         |1                                                 |
|        40         |1                                                 |
|        41         |1                                                 |
|        42         |1                                                 |
|        43         |1                                                 |
|        44         |1                                                 |
|        45         |1                                                 |
|        46         |1                                                 |
|        47         |1                                                 |
|        48         |1                                                 |
|        49         |1                                                 |
|        50         |1                                                 |
|        51         |1                                                 |
|        52         |1                                                 |
|        53         |1                                                 |
|        54         |1                                                 |
|        55         |1                                                 |
|        56         |1                                                 |
|        57         |1                                                 |
|        58         |1                                                 |
|        59         |1                                                 |

 65)  KEY: "Application_scheme"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |constant                                          |
|         2         |constant                                          |
|         3         |constant                                          |
|         4         |constant                                          |
|         5         |constant                                          |
|         6         |constant                                          |
|         7         |constant                                          |
|         8         |constant                                          |
|         9         |constant                                          |
|        10         |constant                                          |
|        11         |constant                                          |
|        12         |constant                                          |
|        13         |constant                                          |
|        14         |constant                                          |
|        15         |constant                                          |
|        16         |constant                                          |
|        17         |constant                                          |
|        18         |constant                                          |
|        19         |constant                                          |
|        20         |constant                                          |
|        21         |constant                                          |
|        22         |constant                                          |
|        23         |constant                                          |
|        24         |constant                                          |
|        25         |constant                                          |
|        26         |constant                                          |
|        27         |constant                                          |
|        28         |constant                                          |
|        29         |constant                                          |
|        30         |constant                                          |
|        31         |none                                              |
|        32         |none                                              |
|        33         |none                                              |
|        34         |none                                              |
|        35         |none                                              |
|        36         |none                                              |
|        37         |none                                              |
|        38         |none                                              |
|        39         |none                                              |
|        40         |none                                              |
|        41         |none                                              |
|        42         |none                                              |
|        43         |none                                              |
|        44         |none                                              |
|        45         |constant                                          |
|        46         |none                                              |
|        47         |none                                              |
|        48         |constant                                          |
|        49         |constant                                          |
|        50         |constant                                          |
|        51         |constant                                          |
|        52         |constant                                          |
|        53         |constant                                          |
|        54         |constant                                          |
|        55         |constant                                          |
|        56         |constant                                          |
|        57         |constant                                          |
|        58         |constant                                          |
|        59         |constant                                          |

 66)  KEY: "Irrigation_application_efficiency"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |1                                                 |
|         2         |1                                                 |
|         3         |1                                                 |
|         4         |1                                                 |
|         5         |1                                                 |
|         6         |1                                                 |
|         7         |1                                                 |
|         8         |1                                                 |
|         9         |1                                                 |
|        10         |1                                                 |
|        11         |1                                                 |
|        12         |1                                                 |
|        13         |1                                                 |
|        14         |1                                                 |
|        15         |1                                                 |
|        16         |1                                                 |
|        17         |1                                                 |
|        18         |1                                                 |
|        19         |1                                                 |
|        20         |1                                                 |
|        21         |1                                                 |
|        22         |1                                                 |
|        23         |1                                                 |
|        24         |1                                                 |
|        25         |1                                                 |
|        26         |1                                                 |
|        27         |1                                                 |
|        28         |1                                                 |
|        29         |1                                                 |
|        30         |1                                                 |
|        31         |1                                                 |
|        32         |1                                                 |
|        33         |1                                                 |
|        34         |1                                                 |
|        35         |1                                                 |
|        36         |1                                                 |
|        37         |1                                                 |
|        38         |1                                                 |
|        39         |1                                                 |
|        40         |1                                                 |
|        41         |1                                                 |
|        42         |1                                                 |
|        43         |1                                                 |
|        44         |1                                                 |
|        45         |1                                                 |
|        46         |1                                                 |
|        47         |1                                                 |
|        48         |1                                                 |
|        49         |1                                                 |
|        50         |1                                                 |
|        51         |1                                                 |
|        52         |1                                                 |
|        53         |1                                                 |
|        54         |1                                                 |
|        55         |1                                                 |
|        56         |1                                                 |
|        57         |1                                                 |
|        58         |1                                                 |
|        59         |1                                                 |

 67)  KEY: "Hargreaves_ET_slope______0_0023"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |Hargreaves_ET_exponent   0.5                      |
|         2         |Hargreaves_ET_constant   17.8                     |

> PRECIPITATION_METHOD GRIDDED  

> PRECIPITATION NETCDF prcp_Daymet_v3_%y.nc  

> PRECIPITATION_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs  

> PRECIPITATION_NETCDF_Z_VAR prcp  

> PRECIPITATION_SCALE_FACTOR 0.03937008  

> PRECIPITATION_MISSING_VALUES_CODE -9999.0  

> PRECIPITATION_MISSING_VALUES_OPERATOR <=  

> PRECIPITATION_MISSING_VALUES_ACTION zero  

> TMIN NETCDF tmin_Daymet_v3_%y.nc  

> TMIN_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs  

> TMIN_SCALE_FACTOR 1.8  

> TMIN_ADD_OFFSET 32.0  

> TMIN_MISSING_VALUES_CODE -9999.0  

> TMIN_MISSING_VALUES_OPERATOR <=  

> TMIN_MISSING_VALUES_ACTION mean  

> TMAX NETCDF tmax_Daymet_v3_%y.nc  

> TMAX_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs  

> TMAX_SCALE_FACTOR 1.8  

> TMAX_ADD_OFFSET 32.0  

> TMAX_MISSING_VALUES_CODE -9999.0  

> TMAX_MISSING_VALUES_OPERATOR <=  

> TMAX_MISSING_VALUES_ACTION mean  

> AVAILABLE_WATER_CONTENT_METHOD GRIDDED  

> AVAILABLE_WATER_CONTENT ARC_GRID available_water_capacity.asc  

> AVAILABLE_WATER_CONTENT_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "REFERENCE_ET0".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "REFERENCE_ET0".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "POTENTIAL_ET".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "POTENTIAL_ET".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "ACTUAL_ET".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "ACTUAL_ET".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "SOLAR_RADIATION".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "SOLAR_RADIATION".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "WIND_SPEED".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "WIND_SPEED".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "RAINFALL_ZONE".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "RAINFALL_ZONE".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "REFERENCE_ET_ZONE".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "REFERENCE_ET_ZONE".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "POTENTIAL_ET_ZONE".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "POTENTIAL_ET_ZONE".  

> FLOW_DIRECTION ARC_GRID d8_flow_direction.asc  

> FLOW_DIRECTION_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "FOG_RATIO".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "FOG_RATIO".  

> LAND_USE ARC_GRID landuse.asc  

> LAND_USE_LOOKUP_TABLE Landuse_lookup_CDL.txt  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "SOILS_CODE".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "SOILS_CODE".  

> HYDROLOGIC_SOILS_GROUP ARC_GRID hydrologic_soils_group.asc  

> HYDROLOGIC_SOILS_GROUP_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m  

> INITIAL_PERCENT_SOIL_MOISTURE CONSTANT 100.0  

> INITIAL_SNOW_COVER_STORAGE CONSTANT 2.0  

> INITIAL_CONTINUOUS_FROZEN_GROUND_INDEX CONSTANT 100.0  

> CFGI_LOWER_LIMIT CONSTANT 40.  

> CFGI_UPPER_LIMIT CONSTANT 100.  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "PERCENT_CANOPY_COVER".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "PERCENT_CANOPY_COVER".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "PERCENT_PERVIOUS_COVER".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "PERCENT_PERVIOUS_COVER".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "PERCENT_IMPERVIOUS_COVER".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "PERCENT_IMPERVIOUS_COVER".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "FRACTION_CANOPY_COVER".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "FRACTION_CANOPY_COVER".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "FRACTION_PERVIOUS_COVER".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "FRACTION_PERVIOUS_COVER".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "FRACTION_IMPERVIOUS_COVER".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "FRACTION_IMPERVIOUS_COVER".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "STEMFLOW_FRACTION".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "STEMFLOW_FRACTION".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "EVAPORATION_TO_RAINFALL_RATIO".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "EVAPORATION_TO_RAINFALL_RATIO".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "RAINFALL_ADJUST_FACTOR".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "RAINFALL_ADJUST_FACTOR".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "CESSPOOL_LEAKAGE".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "CESSPOOL_LEAKAGE".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "STORM_DRAIN_CAPTURE_FRACTION".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "STORM_DRAIN_CAPTURE_FRACTION".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "WATER_BODY_LEAKAGE".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "WATER_BODY_LEAKAGE".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "WATER_MAIN_LEAKAGE".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "WATER_MAIN_LEAKAGE".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "DISPOSAL_WELL_DISCHARGE".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "DISPOSAL_WELL_DISCHARGE".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "ANNUAL_DIRECT_NET_INFILTRATION_RATE".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "ANNUAL_DIRECT_NET_INFILTRATION_RATE".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "ANNUAL_SEPTIC_DISCHARGE".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "ANNUAL_SEPTIC_DISCHARGE".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "SEPTIC_DISCHARGE".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "SEPTIC_DISCHARGE".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "RUNOFF_ZONE".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "RUNOFF_ZONE".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "POLYGON_ID".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "POLYGON_ID".  

> SOIL_STORAGE_MAX_METHOD CALCULATED  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "PLANT_AVAILABLE_WATER".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "PLANT_AVAILABLE_WATER".  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "MAXIMUM_NET_INFILTRATION".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "MAXIMUM_NET_INFILTRATION".  

> IRRIGATION_MASK ARC_GRID irrigation_mask_from_cdl.asc  

> IRRIGATION_MASK_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "RELATIVE_HUMIDITY".  
                 module:  ../src/dictionary.F90  
                 line no:  318  

Your control file is missing gridded data relating to "RELATIVE_HUMIDITY".  

> INTERCEPTION_METHOD BUCKET  
==> BUCKET/HORTON INTERCEPTION submodel selected.  

> EVAPOTRANSPIRATION_METHOD HARGREAVES  
==> HARGREAVES-SAMANI EVAPOTRANSPIRATION submodel selected.  

> RUNOFF_METHOD CURVE_NUMBER  
==> CURVE NUMBER RUNOFF submodel selected.  

> PRECIPITATION_METHOD GRIDDED  
==> STANDARD PRECIPITATION submodel selected.  

> PRECIPITATION NETCDF prcp_Daymet_v3_%y.nc  

> PRECIPITATION_GRID_PROJECTION_DEFINITION +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs  

> PRECIPITATION_NETCDF_Z_VAR prcp  

> PRECIPITATION_SCALE_FACTOR 0.03937008  

> PRECIPITATION_MISSING_VALUES_CODE -9999.0  

> PRECIPITATION_MISSING_VALUES_OPERATOR <=  

> PRECIPITATION_MISSING_VALUES_ACTION zero  

> FOG_METHOD NONE  
==> NULL FOG submodel selected (i.e. no fog term).  

> AVAILABLE_WATER_CONTENT_METHOD GRIDDED  
==> GRIDDED VALUES method for populating AVAILABLE_WATER_CONTENT/AVAILABLE_WATER_CAPACITY selected.  

> AVAILABLE_WATER_CONTENT ARC_GRID available_water_capacity.asc  

> AVAILABLE_WATER_CONTENT_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m  

> SOIL_STORAGE_MAX_METHOD CALCULATED  
==> SOIL_STORAGE_MAX will be internally calculated from the given AWC and rooting depth values.  

> SOIL_MOISTURE_METHOD FAO-56_TWO_STAGE  
==> **TWO-STAGE** FAO-56 SOIL MOISTURE RETENTION submodel selected.  

> INITIAL_PERCENT_SOIL_MOISTURE CONSTANT 100.0  

> IRRIGATION_METHOD FAO-56  
==> IRRIGATION will be calculated and applied as needed.  

> IRRIGATION_MASK ARC_GRID irrigation_mask_from_cdl.asc  

> IRRIGATION_MASK_PROJECTION_DEFINITION +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m  

> IRRIGATION_LOOKUP_TABLE Irrigation_lookup_CDL.txt  

> CROP_COEFFICIENT_METHOD FAO-56  
==> FAO-56 crop coefficient calculation method selected.  

> GROWING_DEGREE_DAY_METHOD BASKERVILLE_EMIN  
==> Growing degree-day (GDD) will be calculated as described in Baskerville and Emin (1969)  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "DYNAMIC_LANDUSE".  
                 module:  ../src/dictionary.F90  
                 line no:  318  


         ** WARNING **  
               Your control file is missing any of the required directives relating to "DYNAMIC_LANDUSE" method.  


> DIRECT_RECHARGE_METHOD NONE  
==> GRIDDED or TABULAR values for water main leakage and other direct net infiltration will be used.  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "DIRECT_NET_INFILTRATION".  
                 module:  ../src/dictionary.F90  
                 line no:  318  


         ** WARNING **  
               Your control file is missing any of the required directives relating to "DIRECT_NET_INFILTRATION" method.  


         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "DIRECT_SOIL_MOISTURE".  
                 module:  ../src/dictionary.F90  
                 line no:  318  


         ** WARNING **  
               Your control file is missing any of the required directives relating to "DIRECT_SOIL_MOISTURE" method.  


> FLOW_ROUTING_METHOD NONE  
==> NULL FLOW ROUTING submodel selected -- NO routing will be performed.  

> ROOTING_DEPTH_METHOD DYNAMIC  
==> DYNAMIC rooting depth submodel selected.  

> DUMP_VARIABLES_1 COORDINATES 561167. 445224.  
==> SWB will dump variables for cell (177,202).  

> DUMP_VARIABLES_2 COORDINATES 546094., 438492.  
==> SWB will dump variables for cell (9,277).  

> DUMP_VARIABLES_3 COORDINATES 556791. 457569.  
==> SWB will dump variables for cell (128,65).  

> DUMP_VARIABLES_4 COORDINATES 568129. 458340.  
==> SWB will dump variables for cell (254,56).  

> DUMP_VARIABLES_5 COORDINATES 553927. 459454.  
==> SWB will dump variables for cell (96,44).  

> DUMP_VARIABLES_6 COORDINATES 555602. 434644.  
==> SWB will dump variables for cell (115,319).  

> DUMP_VARIABLES_7 COORDINATES 558663. 432949.  
==> SWB will dump variables for cell (149,338).  

> DUMP_VARIABLES_8 COORDINATES 570741. 445112.  
==> SWB will dump variables for cell (283,203).  
Opening file "/Users/hellyj/src/swb2-jjh/test/test_data/cs/landuse.asc" for LAND_USE data.  
Opening file "/Users/hellyj/src/swb2-jjh/test/test_data/cs/hydrologic_soils_group.asc" for HYDROLOGIC_SOILS_GROUP data.  
Landuse Code |  Soils Code  | Number of Matches | Rooting Depth (ft)  
-------------|--------------|-------------------|------------------  
1 | 1 | 4389 | 2.00000000  
4 | 1 | 12 | 2.49749994  
5 | 1 | 599 | 1.87500000  
6 | 1 | 0 | 2.49749994  
12 | 1 | 2002 | 1.87500000  
21 | 1 | 12 | 2.49749994  
23 | 1 | 1 | 2.49749994  
24 | 1 | 46 | 2.49749994  
26 | 1 | 2 | 1.87500000  
27 | 1 | 177 | 2.49749994  
28 | 1 | 51 | 2.49749994  
29 | 1 | 0 | 2.49749994  
30 | 1 | 0 | 2.49749994  
32 | 1 | 0 | 2.49749994  
36 | 1 | 1917 | 2.25000000  
37 | 1 | 359 | 2.49749994  
38 | 1 | 3 | 2.49749994  
41 | 1 | 1 | 2.49749994  
42 | 1 | 1529 | 1.50000000  
43 | 1 | 1832 | 1.00000000  
47 | 1 | 0 | 2.49749994  
49 | 1 | 1 | 2.49749994  
50 | 1 | 0 | 2.49749994  
53 | 1 | 208 | 1.50000000  
57 | 1 | 0 | 2.49749994  
58 | 1 | 5 | 2.49749994  
59 | 1 | 0 | 2.49749994  
61 | 1 | 0 | 1.50000000  
70 | 1 | 9 | 1.12500000  
92 | 1 | 0 | 2.49749994  
111 | 1 | 115 | 0.00000000  
121 | 1 | 3420 | 2.49749994  
122 | 1 | 2918 | 1.50000000  
123 | 1 | 763 | 1.50000000  
124 | 1 | 383 | 1.50000000  
131 | 1 | 48 | 1.50000000  
141 | 1 | 6598 | 3.12374997  
142 | 1 | 1798 | 1.56187499  
143 | 1 | 246 | 3.12374997  
151 | 1 | 0 | 2.49749994  
152 | 1 | 47 | 2.49749994  
171 | 1 | 0 | 2.49749994  
176 | 1 | 2445 | 2.49749994  
181 | 1 | 0 | 2.49749994  
182 | 1 | 0 | 2.49749994  
190 | 1 | 923 | 3.37500000  
195 | 1 | 437 | 3.37500000  
205 | 1 | 0 | 2.49749994  
206 | 1 | 15 | 2.49749994  
207 | 1 | 0 | 2.49749994  
221 | 1 | 0 | 2.49749994  
225 | 1 | 0 | 2.49749994  
226 | 1 | 0 | 2.49749994  
241 | 1 | 0 | 2.00000000  
242 | 1 | 0 | 2.49749994  
243 | 1 | 1 | 2.49749994  
250 | 1 | 0 | 2.49749994  
251 | 1 | 31 | 1.50000000  
252 | 1 | 80 | 2.49749994  
1 | 2 | 13496 | 2.00000000  
4 | 2 | 9 | 2.70749998  
5 | 2 | 1453 | 1.87500000  
6 | 2 | 0 | 2.70749998  
12 | 2 | 2346 | 1.87500000  
21 | 2 | 21 | 2.70749998  
23 | 2 | 2 | 2.70749998  
24 | 2 | 248 | 2.70749998  
26 | 2 | 2 | 1.87500000  
27 | 2 | 158 | 2.70749998  
28 | 2 | 92 | 2.70749998  
29 | 2 | 2 | 2.70749998  
30 | 2 | 0 | 2.70749998  
32 | 2 | 0 | 2.70749998  
36 | 2 | 7541 | 2.25000000  
37 | 2 | 749 | 2.49749994  
38 | 2 | 36 | 2.70749998  
41 | 2 | 7 | 2.70749998  
42 | 2 | 1239 | 1.50000000  
43 | 2 | 1925 | 1.00000000  
47 | 2 | 0 | 2.70749998  
49 | 2 | 0 | 2.70749998  
50 | 2 | 1 | 2.70749998  
53 | 2 | 454 | 1.50000000  
57 | 2 | 4 | 2.70749998  
58 | 2 | 21 | 2.70749998  
59 | 2 | 0 | 2.70749998  
61 | 2 | 5 | 1.50000000  
70 | 2 | 16 | 1.12500000  
92 | 2 | 0 | 2.49749994  
111 | 2 | 178 | 0.00000000  
121 | 2 | 2935 | 2.49749994  
122 | 2 | 796 | 1.50000000  
123 | 2 | 116 | 1.50000000  
124 | 2 | 26 | 1.50000000  
131 | 2 | 45 | 1.50000000  
141 | 2 | 22710 | 2.49749994  
142 | 2 | 1733 | 1.24874997  
143 | 2 | 570 | 2.49749994  
151 | 2 | 0 | 2.49749994  
152 | 2 | 82 | 2.49749994  
171 | 2 | 0 | 2.70749998  
176 | 2 | 4687 | 2.49749994  
181 | 2 | 0 | 2.70749998  
182 | 2 | 0 | 2.70749998  
190 | 2 | 2096 | 3.37500000  
195 | 2 | 579 | 3.37500000  
205 | 2 | 0 | 2.49749994  
206 | 2 | 30 | 2.70749998  
207 | 2 | 0 | 2.70749998  
221 | 2 | 0 | 2.70749998  
225 | 2 | 9 | 2.70749998  
226 | 2 | 0 | 2.70749998  
241 | 2 | 0 | 2.00000000  
242 | 2 | 0 | 2.70749998  
243 | 2 | 1 | 2.70749998  
250 | 2 | 2 | 2.70749998  
251 | 2 | 0 | 1.50000000  
252 | 2 | 31 | 2.70749998  
1 | 3 | 641 | 2.00000000  
4 | 3 | 1 | 2.54999995  
5 | 3 | 97 | 1.87500000  
6 | 3 | 0 | 2.54999995  
12 | 3 | 43 | 1.87500000  
21 | 3 | 1 | 2.54999995  
23 | 3 | 3 | 2.54999995  
24 | 3 | 1 | 2.54999995  
26 | 3 | 0 | 1.87500000  
27 | 3 | 7 | 2.54999995  
28 | 3 | 3 | 2.54999995  
29 | 3 | 0 | 2.54999995  
30 | 3 | 0 | 2.54999995  
32 | 3 | 0 | 2.54999995  
36 | 3 | 397 | 2.25000000  
37 | 3 | 95 | 3.12750006  
38 | 3 | 0 | 2.54999995  
41 | 3 | 0 | 2.54999995  
42 | 3 | 12 | 1.50000000  
43 | 3 | 40 | 1.00000000  
47 | 3 | 0 | 2.54999995  
49 | 3 | 0 | 2.54999995  
50 | 3 | 0 | 2.54999995  
53 | 3 | 16 | 1.50000000  
57 | 3 | 0 | 2.54999995  
58 | 3 | 5 | 2.54999995  
59 | 3 | 0 | 2.54999995  
61 | 3 | 0 | 1.50000000  
70 | 3 | 1 | 1.12500000  
92 | 3 | 0 | 2.49749994  
111 | 3 | 14 | 0.00000000  
121 | 3 | 450 | 2.49749994  
122 | 3 | 256 | 1.50000000  
123 | 3 | 84 | 1.50000000  
124 | 3 | 28 | 1.50000000  
131 | 3 | 0 | 1.50000000  
141 | 3 | 4922 | 2.49749994  
142 | 3 | 117 | 1.24874997  
143 | 3 | 125 | 2.49749994  
151 | 3 | 0 | 2.49749994  
152 | 3 | 12 | 2.49749994  
171 | 3 | 0 | 2.54999995  
176 | 3 | 326 | 3.12750006  
181 | 3 | 0 | 2.54999995  
182 | 3 | 0 | 2.54999995  
190 | 3 | 505 | 3.37500000  
195 | 3 | 176 | 3.37500000  
205 | 3 | 0 | 3.12750006  
206 | 3 | 0 | 2.54999995  
207 | 3 | 0 | 2.54999995  
221 | 3 | 0 | 2.54999995  
225 | 3 | 0 | 2.54999995  
226 | 3 | 0 | 2.54999995  
241 | 3 | 0 | 2.00000000  
242 | 3 | 0 | 2.54999995  
243 | 3 | 0 | 2.54999995  
250 | 3 | 0 | 2.54999995  
251 | 3 | 0 | 1.50000000  
252 | 3 | 0 | 2.54999995  
1 | 4 | 17 | 2.00000000  
4 | 4 | 0 | 1.58249998  
5 | 4 | 0 | 1.87500000  
6 | 4 | 0 | 1.58249998  
12 | 4 | 0 | 1.87500000  
21 | 4 | 0 | 1.58249998  
23 | 4 | 0 | 1.58249998  
24 | 4 | 2 | 1.58249998  
26 | 4 | 0 | 1.87500000  
27 | 4 | 0 | 1.58249998  
28 | 4 | 1 | 1.58249998  
29 | 4 | 0 | 1.58249998  
30 | 4 | 0 | 1.58249998  
32 | 4 | 0 | 1.58249998  
36 | 4 | 10 | 2.25000000  
37 | 4 | 7 | 2.49749994  
38 | 4 | 0 | 1.58249998  
41 | 4 | 0 | 1.58249998  
42 | 4 | 0 | 1.50000000  
43 | 4 | 0 | 1.00000000  
47 | 4 | 0 | 1.58249998  
49 | 4 | 0 | 1.58249998  
50 | 4 | 0 | 1.58249998  
53 | 4 | 0 | 1.50000000  
57 | 4 | 0 | 1.58249998  
58 | 4 | 0 | 1.58249998  
59 | 4 | 0 | 1.58249998  
61 | 4 | 0 | 1.50000000  
70 | 4 | 0 | 1.12500000  
92 | 4 | 0 | 1.66499996  
111 | 4 | 168 | 0.00000000  
121 | 4 | 122 | 2.49749994  
122 | 4 | 23 | 1.50000000  
123 | 4 | 6 | 1.50000000  
124 | 4 | 0 | 1.50000000  
131 | 4 | 0 | 1.50000000  
141 | 4 | 1494 | 1.99874997  
142 | 4 | 24 | 0.999374986  
143 | 4 | 22 | 1.99874997  
151 | 4 | 0 | 1.66499996  
152 | 4 | 3 | 1.66499996  
171 | 4 | 0 | 1.58249998  
176 | 4 | 47 | 2.49749994  
181 | 4 | 0 | 1.58249998  
182 | 4 | 0 | 1.58249998  
190 | 4 | 573 | 3.37500000  
195 | 4 | 157 | 3.37500000  
205 | 4 | 0 | 2.49749994  
206 | 4 | 0 | 1.58249998  
207 | 4 | 0 | 1.58249998  
221 | 4 | 0 | 1.58249998  
225 | 4 | 0 | 1.58249998  
226 | 4 | 0 | 1.58249998  
241 | 4 | 0 | 2.00000000  
242 | 4 | 0 | 1.58249998  
243 | 4 | 0 | 1.58249998  
250 | 4 | 0 | 1.58249998  
251 | 4 | 0 | 1.50000000  
252 | 4 | 0 | 1.58249998  
1 | 5 | 1509 | 2.00000000  
4 | 5 | 1 | 2.49749994  
5 | 5 | 376 | 1.87500000  
6 | 5 | 0 | 2.49749994  
12 | 5 | 1551 | 1.87500000  
21 | 5 | 0 | 2.49749994  
23 | 5 | 1 | 2.49749994  
24 | 5 | 4 | 2.49749994  
26 | 5 | 1 | 1.87500000  
27 | 5 | 15 | 2.49749994  
28 | 5 | 24 | 2.49749994  
29 | 5 | 0 | 2.49749994  
30 | 5 | 0 | 2.49749994  
32 | 5 | 0 | 2.49749994  
36 | 5 | 447 | 2.25000000  
37 | 5 | 163 | 2.49749994  
38 | 5 | 0 | 2.49749994  
41 | 5 | 0 | 2.49749994  
42 | 5 | 313 | 1.50000000  
43 | 5 | 1137 | 1.00000000  
47 | 5 | 0 | 2.49749994  
49 | 5 | 0 | 2.49749994  
50 | 5 | 0 | 2.49749994  
53 | 5 | 79 | 1.50000000  
57 | 5 | 0 | 2.49749994  
58 | 5 | 2 | 2.49749994  
59 | 5 | 0 | 2.49749994  
61 | 5 | 7 | 1.50000000  
70 | 5 | 0 | 1.12500000  
92 | 5 | 0 | 2.49749994  
111 | 5 | 83 | 0.00000000  
121 | 5 | 411 | 2.49749994  
122 | 5 | 90 | 1.50000000  
123 | 5 | 30 | 1.50000000  
124 | 5 | 27 | 1.50000000  
131 | 5 | 0 | 1.50000000  
141 | 5 | 7024 | 3.12374997  
142 | 5 | 161 | 1.56187499  
143 | 5 | 159 | 3.12374997  
151 | 5 | 0 | 2.49749994  
152 | 5 | 34 | 2.49749994  
171 | 5 | 0 | 2.49749994  
176 | 5 | 1830 | 2.49749994  
181 | 5 | 0 | 2.49749994  
182 | 5 | 0 | 2.49749994  
190 | 5 | 3296 | 3.37500000  
195 | 5 | 1440 | 3.37500000  
205 | 5 | 0 | 2.49749994  
206 | 5 | 26 | 2.49749994  
207 | 5 | 0 | 2.49749994  
221 | 5 | 0 | 2.49749994  
225 | 5 | 0 | 2.49749994  
226 | 5 | 0 | 2.49749994  
241 | 5 | 0 | 2.00000000  
242 | 5 | 0 | 2.49749994  
243 | 5 | 0 | 2.49749994  
250 | 5 | 1 | 2.49749994  
251 | 5 | 0 | 1.50000000  
252 | 5 | 0 | 2.49749994  
1 | 6 | 120 | 2.00000000  
4 | 6 | 0 | 1.58249998  
5 | 6 | 9 | 1.87500000  
6 | 6 | 0 | 1.58249998  
12 | 6 | 15 | 1.87500000  
21 | 6 | 0 | 1.58249998  
23 | 6 | 0 | 1.58249998  
24 | 6 | 1 | 1.58249998  
26 | 6 | 0 | 1.87500000  
27 | 6 | 1 | 1.58249998  
28 | 6 | 0 | 1.58249998  
29 | 6 | 0 | 1.58249998  
30 | 6 | 0 | 1.58249998  
32 | 6 | 0 | 1.58249998  
36 | 6 | 123 | 2.25000000  
37 | 6 | 38 | 2.49749994  
38 | 6 | 0 | 1.58249998  
41 | 6 | 0 | 1.58249998  
42 | 6 | 2 | 1.50000000  
43 | 6 | 8 | 1.00000000  
47 | 6 | 0 | 1.58249998  
49 | 6 | 0 | 1.58249998  
50 | 6 | 0 | 1.58249998  
53 | 6 | 0 | 1.50000000  
57 | 6 | 0 | 1.58249998  
58 | 6 | 1 | 1.58249998  
59 | 6 | 0 | 1.58249998  
61 | 6 | 0 | 1.50000000  
70 | 6 | 0 | 1.12500000  
92 | 6 | 0 | 1.66499996  
111 | 6 | 6 | 0.00000000  
121 | 6 | 111 | 2.49749994  
122 | 6 | 19 | 1.50000000  
123 | 6 | 9 | 1.50000000  
124 | 6 | 2 | 1.50000000  
131 | 6 | 0 | 1.50000000  
141 | 6 | 2315 | 1.99874997  
142 | 6 | 38 | 0.999374986  
143 | 6 | 68 | 1.99874997  
151 | 6 | 0 | 1.66499996  
152 | 6 | 10 | 1.66499996  
171 | 6 | 0 | 1.58249998  
176 | 6 | 119 | 2.49749994  
181 | 6 | 0 | 1.58249998  
182 | 6 | 0 | 1.58249998  
190 | 6 | 521 | 3.37500000  
195 | 6 | 119 | 3.37500000  
205 | 6 | 0 | 2.49749994  
206 | 6 | 0 | 1.58249998  
207 | 6 | 0 | 1.58249998  
221 | 6 | 0 | 1.58249998  
225 | 6 | 0 | 1.58249998  
226 | 6 | 0 | 1.58249998  
241 | 6 | 0 | 2.00000000  
242 | 6 | 0 | 1.58249998  
243 | 6 | 0 | 1.58249998  
250 | 6 | 0 | 1.58249998  
251 | 6 | 0 | 1.50000000  
252 | 6 | 0 | 1.58249998  
1 | 7 | 9 | 0.00000000  
4 | 7 | 0 | 0.00000000  
5 | 7 | 0 | 0.00000000  
6 | 7 | 0 | 0.00000000  
12 | 7 | 0 | 0.00000000  
21 | 7 | 0 | 0.00000000  
23 | 7 | 0 | 0.00000000  
24 | 7 | 0 | 0.00000000  
26 | 7 | 0 | 0.00000000  
27 | 7 | 0 | 0.00000000  
28 | 7 | 1 | 0.00000000  
29 | 7 | 0 | 0.00000000  
30 | 7 | 0 | 0.00000000  
32 | 7 | 0 | 0.00000000  
36 | 7 | 2 | 0.00000000  
37 | 7 | 2 | 0.00000000  
38 | 7 | 0 | 0.00000000  
41 | 7 | 0 | 0.00000000  
42 | 7 | 1 | 0.00000000  
43 | 7 | 1 | 0.00000000  
47 | 7 | 0 | 0.00000000  
49 | 7 | 0 | 0.00000000  
50 | 7 | 0 | 0.00000000  
53 | 7 | 0 | 0.00000000  
57 | 7 | 0 | 0.00000000  
58 | 7 | 0 | 0.00000000  
59 | 7 | 0 | 0.00000000  
61 | 7 | 0 | 0.00000000  
70 | 7 | 0 | 0.00000000  
92 | 7 | 0 | 0.00000000  
111 | 7 | 2883 | 0.00000000  
121 | 7 | 33 | 0.00000000  
122 | 7 | 12 | 0.00000000  
123 | 7 | 10 | 0.00000000  
124 | 7 | 6 | 0.00000000  
131 | 7 | 0 | 0.00000000  
141 | 7 | 188 | 0.00000000  
142 | 7 | 8 | 0.00000000  
143 | 7 | 7 | 0.00000000  
151 | 7 | 0 | 0.00000000  
152 | 7 | 1 | 0.00000000  
171 | 7 | 0 | 0.00000000  
176 | 7 | 26 | 0.00000000  
181 | 7 | 0 | 0.00000000  
182 | 7 | 0 | 0.00000000  
190 | 7 | 266 | 0.00000000  
195 | 7 | 117 | 0.00000000  
205 | 7 | 0 | 0.00000000  
206 | 7 | 0 | 0.00000000  
207 | 7 | 0 | 0.00000000  
221 | 7 | 0 | 0.00000000  
225 | 7 | 0 | 0.00000000  
226 | 7 | 0 | 0.00000000  
241 | 7 | 0 | 0.00000000  
242 | 7 | 0 | 0.00000000  
243 | 7 | 0 | 0.00000000  
250 | 7 | 0 | 0.00000000  
251 | 7 | 0 | 0.00000000  
252 | 7 | 0 | 0.00000000  
Opening file "/Users/hellyj/src/swb2-jjh/test/test_data/cs/available_water_capacity.asc" for AVAILABLE_WATER_CONTENT data.  
Opening file "/Users/hellyj/src/swb2-jjh/test/test_data/cs/d8_flow_direction.asc" for FLOW_DIRECTION data.  

138400 cells are currently active out of a total of 138400  


Hydrologic soils groups as read into SWB data structure  

33423 cells belong to soils group 1  
66453 cells belong to soils group 2  
8378 cells belong to soils group 3  
2676 cells belong to soils group 4  
20242 cells belong to soils group 5  
3655 cells belong to soils group 6  
3573 cells belong to soils group 7  


Matches were found between landuse grid value and table value for 138400 cells out of a total of 138400 active cells.  


         ** WARNING **  
               Could not find a grid or constant value for the canopy cover fraction. Using a value of 1.0 for the entire model domain.  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Surface_Storage_Max"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Surface_Storage_Maximum"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Surface_Storage_Max (2) Surface_Storage_Maximum"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Surface_Storage_Max (2) Surface_Storage_Maximum".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Storm_drain_capture"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Storm_drain_capture_fraction"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Storm_drain_capture (2) Storm_drain_capture_fraction"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Storm_drain_capture (2) Storm_drain_capture_fraction".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "GDD_first_day_of_growing_season"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "GDD_start_of_growing_season"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "GDD_growing_season_start"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) GDD_first_day_of_growing_season (2) GDD_start_of_growing_season (3) GDD_growing_season_start"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) GDD_first_day_of_growing_season (2) GDD_start_of_growing_season (3) GDD_growing_season_start".  


         ** WARNING **  
               The number of landuses does not match the number of GDD values specified for defining the beginning of the growing season.  
                 module:  ../src/growing_season.F90  
                 line no:  143  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Killing_frost_temperature"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Air_temperature_end_of_growing_season"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Air_temperature_growing_season_end"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Killing_frost_temperature (2) Air_temperature_end_of_growing_season (3) Air_temperature_growing_season_end"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Killing_frost_temperature (2) Air_temperature_end_of_growing_season (3) Air_temperature_growing_season_end".  


         ** WARNING **  
               The number of landuses does not match the number of killing frost values specified to define the end of the growing season.  
                 module:  ../src/growing_season.F90  
                 line no:  172  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Growing_season_interception"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Growing_season_interception_a"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Growing_season_interception_b"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Interception_growing_b_term"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Interception_b_term_growing_season"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Growing_season_interception_b (2) Interception_growing_b_term (3) Interception_b_term_growing_season"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Growing_season_interception_b (2) Interception_growing_b_term (3) Interception_b_term_growing_season".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Growing_season_interception_n"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Interception_growing_n_term"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Interception_n_term_growing_season"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Growing_season_interception_n (2) Interception_growing_n_term (3) Interception_n_term_growing_season"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Growing_season_interception_n (2) Interception_growing_n_term (3) Interception_n_term_growing_season".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Nongrowing_season_interception"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Nongrowing_season_interception_a"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Nongrowing_season_interception_b"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Interception_nongrowing_b_term"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Interception_b_term_nongrowing_season"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Nongrowing_season_interception_b (2) Interception_nongrowing_b_term (3) Interception_b_term_nongrowing_season"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Nongrowing_season_interception_b (2) Interception_nongrowing_b_term (3) Interception_b_term_nongrowing_season".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Nongrowing_season_interception_n"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Interception_nongrowing_n_term"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Interception_n_term_nongrowing_season"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Nongrowing_season_interception_n (2) Interception_nongrowing_n_term (3) Interception_n_term_nongrowing_season"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Nongrowing_season_interception_n (2) Interception_nongrowing_n_term (3) Interception_n_term_nongrowing_season".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Interception_storage_max_nongrowing"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Interception_storage_max_nongrowing_season"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Interception_Storage_Maximum_nongrowing"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Interception_storage_max_nongrowing (2) Interception_storage_max_nongrowing_season (3) Interception_Storage_Maximum_nongrowing"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Interception_storage_max_nongrowing (2) Interception_storage_max_nongrowing_season (3) Interception_Storage_Maximum_nongrowing".  


         ** WARNING **  
               The number of landuses does not match the number of interception storage maximum values for the NONGROWING season ('interception_storage_max_nongrowing').  
                 module:  ../src/interception__bucket.F90  
                 line no:  242  

        ==> A default value equal to the 'Growing_season_interception_a' was assigned for the maximum interception storage  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Interception_storage_max_growing"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Interception_storage_max_growing_season"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Interception_Storage_Maximum_growing"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Interception_storage_max_growing (2) Interception_storage_max_growing_season (3) Interception_Storage_Maximum_growing"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Interception_storage_max_growing (2) Interception_storage_max_growing_season (3) Interception_Storage_Maximum_growing".  


         ** WARNING **  
               The number of landuses does not match the number of interception storage maximum values for the GROWING season ('interception_storage_max_growing').  
                 module:  ../src/interception__bucket.F90  
                 line no:  266  

        ==> A default value equal to the 'Nongrowing_season_interception_a' was assigned for the maximum interception storage  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "allow_variable_rooting_depth"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "variable_rooting_depth"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) allow_variable_rooting_depth (2) variable_rooting_depth"  
                 module:  ../src/dictionary.F90  
                 line no:  719  


         ** WARNING **  
               Failed to find a lookup table column named "(1) allow_variable_rooting_depth (2) variable_rooting_depth".  


         ** WARNING **  
               The number of landuses does not match the number of values supplied for the 'allow_variable_rooting_depth' parameter.  
                 module:  ../src/rooting_depth__FAO56.F90  
                 line no:  66  

        ==> By default, all rooting depths will be allowed to vary, using the FAO-56 methodology.  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Minimum_fraction_vegetative_cover"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Min_fraction_covered_soil"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Min_vegetative_cover_fraction"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Minimum_fraction_vegetative_cover (2) Min_fraction_covered_soil (3) Min_vegetative_cover_fraction"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Minimum_fraction_vegetative_cover (2) Min_fraction_covered_soil (3) Min_vegetative_cover_fraction".  


         ** WARNING **  
               The number of landuses does not match the number of values supplied for the minimum fraction of soil covered by vegetation.  
                 module:  ../src/actual_et__fao56__two_stage.F90  
                 line no:  103  

        ==> A default value of 0.05 was assigned for the minimum fraction of covered soil.  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Hargreaves_ET_slope"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "Hargreaves_ET_slope"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "Hargreaves_ET_slope".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Hargreaves_ET_exponent"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "Hargreaves_ET_exponent"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "Hargreaves_ET_exponent".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Hargreaves_ET_constant"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "Hargreaves_ET_constant"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "Hargreaves_ET_constant".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "GDD_Base_Temp"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "GDD_Base_Temperature"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "GDD_Max_Temp"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "GDD_Maximum_Temperature"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "GDD_Maximum_Temp"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "GDD_Reset_Date"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "GDD_Reset"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) GDD_Reset_Date (2) GDD_Reset"  
                 module:  ../src/dictionary.F90  
                 line no:  719  


         ** WARNING **  
               Failed to find a lookup table column named "(1) GDD_Reset_Date (2) GDD_Reset".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Monthly_Irrigation_Schedule"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Monthly_Irr_Schedule"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Irrigation_Application_Schedule"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Monthly_Application_Schedule"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Monthly_Irrigation_Schedule (2) Monthly_Irr_Schedule (3) Irrigation_Application_Schedule (4) Monthly_Application_Schedule"  
                 module:  ../src/dictionary.F90  
                 line no:  719  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Monthly_Irrigation_Schedule (2) Monthly_Irr_Schedule (3) Irrigation_Application_Schedule (4) Monthly_Application_Schedule".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Max_allowable_depletion"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Maximum_allowable_depletion"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "First_day_of_irrigation"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "First_DOY_irrigation"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Last_day_of_irrigation"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Last_DOY_irrigation"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Irrigation_efficiency"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Irrigation_application_method"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Irrigation_application_scheme"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Irrigation_application_option"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Application_method"  


         ** WARNING **  
               The number of values defining monthly irrigation application timing (1)  
               does not match the number of landuse codes (59).  
               Assuming that irrigation is applied *every* day [default].  
                 module:  ../src/irrigation.F90  
                 line no:  362  


## Initializing irrigation application rules and application schedules ##  

landuse 1: Apply constant amount  
landuse 4: Apply constant amount  
landuse 5: Apply constant amount  
landuse 6: Apply constant amount  
landuse 12: Apply constant amount  
landuse 21: Apply constant amount  
landuse 23: Apply constant amount  
landuse 24: Apply constant amount  
landuse 26: Apply constant amount  
landuse 27: Apply constant amount  
landuse 28: Apply constant amount  
landuse 29: Apply constant amount  
landuse 30: Apply constant amount  
landuse 32: Apply constant amount  
landuse 36: Apply constant amount  
landuse 37: Apply constant amount  
landuse 38: Apply constant amount  
landuse 41: Apply constant amount  
landuse 42: Apply constant amount  
landuse 43: Apply constant amount  
landuse 47: Apply constant amount  
landuse 49: Apply constant amount  
landuse 50: Apply constant amount  
landuse 53: Apply constant amount  
landuse 57: Apply constant amount  
landuse 58: Apply constant amount  
landuse 59: Apply constant amount  
landuse 61: Apply constant amount  
landuse 70: Apply constant amount  
landuse 92: Apply constant amount  
landuse 111: Apply nothing  
landuse 121: Apply nothing  
landuse 122: Apply nothing  
landuse 123: Apply nothing  
landuse 124: Apply nothing  
landuse 131: Apply nothing  
landuse 141: Apply nothing  
landuse 142: Apply nothing  
landuse 143: Apply nothing  
landuse 151: Apply nothing  
landuse 152: Apply nothing  
landuse 171: Apply nothing  
landuse 176: Apply nothing  
landuse 181: Apply nothing  
landuse 182: Apply constant amount  
landuse 190: Apply nothing  
landuse 195: Apply nothing  
landuse 205: Apply constant amount  
landuse 206: Apply constant amount  
landuse 207: Apply constant amount  
landuse 221: Apply constant amount  
landuse 225: Apply constant amount  
landuse 226: Apply constant amount  
landuse 241: Apply constant amount  
landuse 242: Apply constant amount  
landuse 243: Apply constant amount  
landuse 250: Apply constant amount  
landuse 251: Apply constant amount  
landuse 252: Apply constant amount  

         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Annual_direct_net_infiltration_rate"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Annual_direct_recharge_rate"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Annual_direct_recharge"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Annual_direct_net_infiltration"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Annual_direct_net_infiltration_rate (2) Annual_direct_recharge_rate (3) Annual_direct_recharge (4) Annual_direct_net_infiltration"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Annual_direct_net_infiltration_rate (2) Annual_direct_recharge_rate (3) Annual_direct_recharge (4) Annual_direct_net_infiltration".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Cesspool_direct_net_infiltration"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Cesspool_recharge"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Cesspool_discharge"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Cesspool_leakage"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Cesspool_direct_net_infiltration (2) Cesspool_recharge (3) Cesspool_discharge (4) Cesspool_leakage"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Cesspool_direct_net_infiltration (2) Cesspool_recharge (3) Cesspool_discharge (4) Cesspool_leakage".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Water_body_recharge"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Water_body_discharge"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Water_body_leakage"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Water_body_recharge (2) Water_body_discharge (3) Water_body_leakage"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Water_body_recharge (2) Water_body_discharge (3) Water_body_leakage".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Water_main_recharge"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Water_main_discharge"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Water_main_leakage"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Water_main_recharge (2) Water_main_discharge (3) Water_main_leakage"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Water_main_recharge (2) Water_main_discharge (3) Water_main_leakage".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Disposal_well_recharge"  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Disposal_well_discharge"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value(s) of: "(1) Disposal_well_recharge (2) Disposal_well_discharge"  
                 module:  ../src/dictionary.F90  
                 line no:  947  


         ** WARNING **  
               Failed to find a lookup table column named "(1) Disposal_well_recharge (2) Disposal_well_discharge".  

| Landuse Code |  Soils Code  | Number of Matches | Maximum net infiltration (in) |  
|-------------|--------------|-------------------|--------------------------------|  
| 1 | 1 | 4389 | 4.00000000 |  
| 4 | 1 | 12 | 4.00000000 |  
| 5 | 1 | 599 | 4.00000000 |  
| 6 | 1 | 0 | 4.00000000 |  
| 12 | 1 | 2002 | 4.00000000 |  
| 21 | 1 | 12 | 4.00000000 |  
| 23 | 1 | 1 | 4.00000000 |  
| 24 | 1 | 46 | 4.00000000 |  
| 26 | 1 | 2 | 4.00000000 |  
| 27 | 1 | 177 | 4.00000000 |  
| 28 | 1 | 51 | 4.00000000 |  
| 29 | 1 | 0 | 4.00000000 |  
| 30 | 1 | 0 | 4.00000000 |  
| 32 | 1 | 0 | 4.00000000 |  
| 36 | 1 | 1917 | 4.00000000 |  
| 37 | 1 | 359 | 4.00000000 |  
| 38 | 1 | 3 | 4.00000000 |  
| 41 | 1 | 1 | 4.00000000 |  
| 42 | 1 | 1529 | 4.00000000 |  
| 43 | 1 | 1832 | 4.00000000 |  
| 47 | 1 | 0 | 4.00000000 |  
| 49 | 1 | 1 | 4.00000000 |  
| 50 | 1 | 0 | 4.00000000 |  
| 53 | 1 | 208 | 4.00000000 |  
| 57 | 1 | 0 | 4.00000000 |  
| 58 | 1 | 5 | 4.00000000 |  
| 59 | 1 | 0 | 4.00000000 |  
| 61 | 1 | 0 | 4.00000000 |  
| 70 | 1 | 9 | 4.00000000 |  
| 92 | 1 | 0 | 4.00000000 |  
| 111 | 1 | 115 | 4.00000000 |  
| 121 | 1 | 3420 | 4.00000000 |  
| 122 | 1 | 2918 | 4.00000000 |  
| 123 | 1 | 763 | 4.00000000 |  
| 124 | 1 | 383 | 4.00000000 |  
| 131 | 1 | 48 | 4.00000000 |  
| 141 | 1 | 6598 | 4.00000000 |  
| 142 | 1 | 1798 | 4.00000000 |  
| 143 | 1 | 246 | 4.00000000 |  
| 151 | 1 | 0 | 4.00000000 |  
| 152 | 1 | 47 | 4.00000000 |  
| 171 | 1 | 0 | 4.00000000 |  
| 176 | 1 | 2445 | 4.00000000 |  
| 181 | 1 | 0 | 4.00000000 |  
| 182 | 1 | 0 | 4.00000000 |  
| 190 | 1 | 923 | 4.00000000 |  
| 195 | 1 | 437 | 4.00000000 |  
| 205 | 1 | 0 | 4.00000000 |  
| 206 | 1 | 15 | 4.00000000 |  
| 207 | 1 | 0 | 4.00000000 |  
| 221 | 1 | 0 | 4.00000000 |  
| 225 | 1 | 0 | 4.00000000 |  
| 226 | 1 | 0 | 4.00000000 |  
| 241 | 1 | 0 | 4.00000000 |  
| 242 | 1 | 0 | 4.00000000 |  
| 243 | 1 | 1 | 4.00000000 |  
| 250 | 1 | 0 | 4.00000000 |  
| 251 | 1 | 31 | 4.00000000 |  
| 252 | 1 | 80 | 4.00000000 |  
| 1 | 2 | 13496 | 0.600000024 |  
| 4 | 2 | 9 | 0.600000024 |  
| 5 | 2 | 1453 | 0.600000024 |  
| 6 | 2 | 0 | 0.600000024 |  
| 12 | 2 | 2346 | 0.600000024 |  
| 21 | 2 | 21 | 0.600000024 |  
| 23 | 2 | 2 | 0.600000024 |  
| 24 | 2 | 248 | 0.600000024 |  
| 26 | 2 | 2 | 0.600000024 |  
| 27 | 2 | 158 | 0.600000024 |  
| 28 | 2 | 92 | 0.600000024 |  
| 29 | 2 | 2 | 0.600000024 |  
| 30 | 2 | 0 | 0.600000024 |  
| 32 | 2 | 0 | 0.600000024 |  
| 36 | 2 | 7541 | 0.600000024 |  
| 37 | 2 | 749 | 0.600000024 |  
| 38 | 2 | 36 | 0.600000024 |  
| 41 | 2 | 7 | 0.600000024 |  
| 42 | 2 | 1239 | 0.600000024 |  
| 43 | 2 | 1925 | 0.600000024 |  
| 47 | 2 | 0 | 0.600000024 |  
| 49 | 2 | 0 | 0.600000024 |  
| 50 | 2 | 1 | 0.600000024 |  
| 53 | 2 | 454 | 0.600000024 |  
| 57 | 2 | 4 | 0.600000024 |  
| 58 | 2 | 21 | 0.600000024 |  
| 59 | 2 | 0 | 0.600000024 |  
| 61 | 2 | 5 | 0.600000024 |  
| 70 | 2 | 16 | 0.600000024 |  
| 92 | 2 | 0 | 0.600000024 |  
| 111 | 2 | 178 | 0.600000024 |  
| 121 | 2 | 2935 | 0.600000024 |  
| 122 | 2 | 796 | 0.600000024 |  
| 123 | 2 | 116 | 0.600000024 |  
| 124 | 2 | 26 | 0.600000024 |  
| 131 | 2 | 45 | 0.600000024 |  
| 141 | 2 | 22710 | 0.600000024 |  
| 142 | 2 | 1733 | 0.600000024 |  
| 143 | 2 | 570 | 0.600000024 |  
| 151 | 2 | 0 | 0.600000024 |  
| 152 | 2 | 82 | 0.600000024 |  
| 171 | 2 | 0 | 0.600000024 |  
| 176 | 2 | 4687 | 0.600000024 |  
| 181 | 2 | 0 | 0.600000024 |  
| 182 | 2 | 0 | 0.600000024 |  
| 190 | 2 | 2096 | 0.600000024 |  
| 195 | 2 | 579 | 0.600000024 |  
| 205 | 2 | 0 | 0.600000024 |  
| 206 | 2 | 30 | 0.600000024 |  
| 207 | 2 | 0 | 0.600000024 |  
| 221 | 2 | 0 | 0.600000024 |  
| 225 | 2 | 9 | 0.600000024 |  
| 226 | 2 | 0 | 0.600000024 |  
| 241 | 2 | 0 | 0.600000024 |  
| 242 | 2 | 0 | 0.600000024 |  
| 243 | 2 | 1 | 0.600000024 |  
| 250 | 2 | 2 | 0.600000024 |  
| 251 | 2 | 0 | 0.600000024 |  
| 252 | 2 | 31 | 0.600000024 |  
| 1 | 3 | 641 | 0.239999995 |  
| 4 | 3 | 1 | 0.239999995 |  
| 5 | 3 | 97 | 0.239999995 |  
| 6 | 3 | 0 | 0.239999995 |  
| 12 | 3 | 43 | 0.239999995 |  
| 21 | 3 | 1 | 0.239999995 |  
| 23 | 3 | 3 | 0.239999995 |  
| 24 | 3 | 1 | 0.239999995 |  
| 26 | 3 | 0 | 0.239999995 |  
| 27 | 3 | 7 | 0.239999995 |  
| 28 | 3 | 3 | 0.239999995 |  
| 29 | 3 | 0 | 0.239999995 |  
| 30 | 3 | 0 | 0.239999995 |  
| 32 | 3 | 0 | 0.239999995 |  
| 36 | 3 | 397 | 0.239999995 |  
| 37 | 3 | 95 | 0.239999995 |  
| 38 | 3 | 0 | 0.239999995 |  
| 41 | 3 | 0 | 0.239999995 |  
| 42 | 3 | 12 | 0.239999995 |  
| 43 | 3 | 40 | 0.239999995 |  
| 47 | 3 | 0 | 0.239999995 |  
| 49 | 3 | 0 | 0.239999995 |  
| 50 | 3 | 0 | 0.239999995 |  
| 53 | 3 | 16 | 0.239999995 |  
| 57 | 3 | 0 | 0.239999995 |  
| 58 | 3 | 5 | 0.239999995 |  
| 59 | 3 | 0 | 0.239999995 |  
| 61 | 3 | 0 | 0.239999995 |  
| 70 | 3 | 1 | 0.239999995 |  
| 92 | 3 | 0 | 0.239999995 |  
| 111 | 3 | 14 | 0.239999995 |  
| 121 | 3 | 450 | 0.239999995 |  
| 122 | 3 | 256 | 0.239999995 |  
| 123 | 3 | 84 | 0.239999995 |  
| 124 | 3 | 28 | 0.239999995 |  
| 131 | 3 | 0 | 0.239999995 |  
| 141 | 3 | 4922 | 0.239999995 |  
| 142 | 3 | 117 | 0.239999995 |  
| 143 | 3 | 125 | 0.239999995 |  
| 151 | 3 | 0 | 0.239999995 |  
| 152 | 3 | 12 | 0.239999995 |  
| 171 | 3 | 0 | 0.239999995 |  
| 176 | 3 | 326 | 0.239999995 |  
| 181 | 3 | 0 | 0.239999995 |  
| 182 | 3 | 0 | 0.239999995 |  
| 190 | 3 | 505 | 0.239999995 |  
| 195 | 3 | 176 | 0.239999995 |  
| 205 | 3 | 0 | 0.239999995 |  
| 206 | 3 | 0 | 0.239999995 |  
| 207 | 3 | 0 | 0.239999995 |  
| 221 | 3 | 0 | 0.239999995 |  
| 225 | 3 | 0 | 0.239999995 |  
| 226 | 3 | 0 | 0.239999995 |  
| 241 | 3 | 0 | 0.239999995 |  
| 242 | 3 | 0 | 0.239999995 |  
| 243 | 3 | 0 | 0.239999995 |  
| 250 | 3 | 0 | 0.239999995 |  
| 251 | 3 | 0 | 0.239999995 |  
| 252 | 3 | 0 | 0.239999995 |  
| 1 | 4 | 17 | 0.119999997 |  
| 4 | 4 | 0 | 0.119999997 |  
| 5 | 4 | 0 | 0.119999997 |  
| 6 | 4 | 0 | 0.119999997 |  
| 12 | 4 | 0 | 0.119999997 |  
| 21 | 4 | 0 | 0.119999997 |  
| 23 | 4 | 0 | 0.119999997 |  
| 24 | 4 | 2 | 0.119999997 |  
| 26 | 4 | 0 | 0.119999997 |  
| 27 | 4 | 0 | 0.119999997 |  
| 28 | 4 | 1 | 0.119999997 |  
| 29 | 4 | 0 | 0.119999997 |  
| 30 | 4 | 0 | 0.119999997 |  
| 32 | 4 | 0 | 0.119999997 |  
| 36 | 4 | 10 | 0.119999997 |  
| 37 | 4 | 7 | 0.119999997 |  
| 38 | 4 | 0 | 0.119999997 |  
| 41 | 4 | 0 | 0.119999997 |  
| 42 | 4 | 0 | 0.119999997 |  
| 43 | 4 | 0 | 0.119999997 |  
| 47 | 4 | 0 | 0.119999997 |  
| 49 | 4 | 0 | 0.119999997 |  
| 50 | 4 | 0 | 0.119999997 |  
| 53 | 4 | 0 | 0.119999997 |  
| 57 | 4 | 0 | 0.119999997 |  
| 58 | 4 | 0 | 0.119999997 |  
| 59 | 4 | 0 | 0.119999997 |  
| 61 | 4 | 0 | 0.119999997 |  
| 70 | 4 | 0 | 0.119999997 |  
| 92 | 4 | 0 | 0.119999997 |  
| 111 | 4 | 168 | 0.119999997 |  
| 121 | 4 | 122 | 0.119999997 |  
| 122 | 4 | 23 | 0.119999997 |  
| 123 | 4 | 6 | 0.119999997 |  
| 124 | 4 | 0 | 0.119999997 |  
| 131 | 4 | 0 | 0.119999997 |  
| 141 | 4 | 1494 | 0.119999997 |  
| 142 | 4 | 24 | 0.119999997 |  
| 143 | 4 | 22 | 0.119999997 |  
| 151 | 4 | 0 | 0.119999997 |  
| 152 | 4 | 3 | 0.119999997 |  
| 171 | 4 | 0 | 0.119999997 |  
| 176 | 4 | 47 | 0.119999997 |  
| 181 | 4 | 0 | 0.119999997 |  
| 182 | 4 | 0 | 0.119999997 |  
| 190 | 4 | 573 | 0.119999997 |  
| 195 | 4 | 157 | 0.119999997 |  
| 205 | 4 | 0 | 0.119999997 |  
| 206 | 4 | 0 | 0.119999997 |  
| 207 | 4 | 0 | 0.119999997 |  
| 221 | 4 | 0 | 0.119999997 |  
| 225 | 4 | 0 | 0.119999997 |  
| 226 | 4 | 0 | 0.119999997 |  
| 241 | 4 | 0 | 0.119999997 |  
| 242 | 4 | 0 | 0.119999997 |  
| 243 | 4 | 0 | 0.119999997 |  
| 250 | 4 | 0 | 0.119999997 |  
| 251 | 4 | 0 | 0.119999997 |  
| 252 | 4 | 0 | 0.119999997 |  
| 1 | 5 | 1509 | 4.00000000 |  
| 4 | 5 | 1 | 4.00000000 |  
| 5 | 5 | 376 | 4.00000000 |  
| 6 | 5 | 0 | 4.00000000 |  
| 12 | 5 | 1551 | 4.00000000 |  
| 21 | 5 | 0 | 4.00000000 |  
| 23 | 5 | 1 | 4.00000000 |  
| 24 | 5 | 4 | 4.00000000 |  
| 26 | 5 | 1 | 4.00000000 |  
| 27 | 5 | 15 | 4.00000000 |  
| 28 | 5 | 24 | 4.00000000 |  
| 29 | 5 | 0 | 4.00000000 |  
| 30 | 5 | 0 | 4.00000000 |  
| 32 | 5 | 0 | 4.00000000 |  
| 36 | 5 | 447 | 4.00000000 |  
| 37 | 5 | 163 | 4.00000000 |  
| 38 | 5 | 0 | 4.00000000 |  
| 41 | 5 | 0 | 4.00000000 |  
| 42 | 5 | 313 | 4.00000000 |  
| 43 | 5 | 1137 | 4.00000000 |  
| 47 | 5 | 0 | 4.00000000 |  
| 49 | 5 | 0 | 4.00000000 |  
| 50 | 5 | 0 | 4.00000000 |  
| 53 | 5 | 79 | 4.00000000 |  
| 57 | 5 | 0 | 4.00000000 |  
| 58 | 5 | 2 | 4.00000000 |  
| 59 | 5 | 0 | 4.00000000 |  
| 61 | 5 | 7 | 4.00000000 |  
| 70 | 5 | 0 | 4.00000000 |  
| 92 | 5 | 0 | 4.00000000 |  
| 111 | 5 | 83 | 4.00000000 |  
| 121 | 5 | 411 | 4.00000000 |  
| 122 | 5 | 90 | 4.00000000 |  
| 123 | 5 | 30 | 4.00000000 |  
| 124 | 5 | 27 | 4.00000000 |  
| 131 | 5 | 0 | 4.00000000 |  
| 141 | 5 | 7024 | 4.00000000 |  
| 142 | 5 | 161 | 4.00000000 |  
| 143 | 5 | 159 | 4.00000000 |  
| 151 | 5 | 0 | 4.00000000 |  
| 152 | 5 | 34 | 4.00000000 |  
| 171 | 5 | 0 | 4.00000000 |  
| 176 | 5 | 1830 | 4.00000000 |  
| 181 | 5 | 0 | 4.00000000 |  
| 182 | 5 | 0 | 4.00000000 |  
| 190 | 5 | 3296 | 4.00000000 |  
| 195 | 5 | 1440 | 4.00000000 |  
| 205 | 5 | 0 | 4.00000000 |  
| 206 | 5 | 26 | 4.00000000 |  
| 207 | 5 | 0 | 4.00000000 |  
| 221 | 5 | 0 | 4.00000000 |  
| 225 | 5 | 0 | 4.00000000 |  
| 226 | 5 | 0 | 4.00000000 |  
| 241 | 5 | 0 | 4.00000000 |  
| 242 | 5 | 0 | 4.00000000 |  
| 243 | 5 | 0 | 4.00000000 |  
| 250 | 5 | 1 | 4.00000000 |  
| 251 | 5 | 0 | 4.00000000 |  
| 252 | 5 | 0 | 4.00000000 |  
| 1 | 6 | 120 | 0.119999997 |  
| 4 | 6 | 0 | 0.119999997 |  
| 5 | 6 | 9 | 0.119999997 |  
| 6 | 6 | 0 | 0.119999997 |  
| 12 | 6 | 15 | 0.119999997 |  
| 21 | 6 | 0 | 0.119999997 |  
| 23 | 6 | 0 | 0.119999997 |  
| 24 | 6 | 1 | 0.119999997 |  
| 26 | 6 | 0 | 0.119999997 |  
| 27 | 6 | 1 | 0.119999997 |  
| 28 | 6 | 0 | 0.119999997 |  
| 29 | 6 | 0 | 0.119999997 |  
| 30 | 6 | 0 | 0.119999997 |  
| 32 | 6 | 0 | 0.119999997 |  
| 36 | 6 | 123 | 0.119999997 |  
| 37 | 6 | 38 | 0.119999997 |  
| 38 | 6 | 0 | 0.119999997 |  
| 41 | 6 | 0 | 0.119999997 |  
| 42 | 6 | 2 | 0.119999997 |  
| 43 | 6 | 8 | 0.119999997 |  
| 47 | 6 | 0 | 0.119999997 |  
| 49 | 6 | 0 | 0.119999997 |  
| 50 | 6 | 0 | 0.119999997 |  
| 53 | 6 | 0 | 0.119999997 |  
| 57 | 6 | 0 | 0.119999997 |  
| 58 | 6 | 1 | 0.119999997 |  
| 59 | 6 | 0 | 0.119999997 |  
| 61 | 6 | 0 | 0.119999997 |  
| 70 | 6 | 0 | 0.119999997 |  
| 92 | 6 | 0 | 0.119999997 |  
| 111 | 6 | 6 | 0.119999997 |  
| 121 | 6 | 111 | 0.119999997 |  
| 122 | 6 | 19 | 0.119999997 |  
| 123 | 6 | 9 | 0.119999997 |  
| 124 | 6 | 2 | 0.119999997 |  
| 131 | 6 | 0 | 0.119999997 |  
| 141 | 6 | 2315 | 0.119999997 |  
| 142 | 6 | 38 | 0.119999997 |  
| 143 | 6 | 68 | 0.119999997 |  
| 151 | 6 | 0 | 0.119999997 |  
| 152 | 6 | 10 | 0.119999997 |  
| 171 | 6 | 0 | 0.119999997 |  
| 176 | 6 | 119 | 0.119999997 |  
| 181 | 6 | 0 | 0.119999997 |  
| 182 | 6 | 0 | 0.119999997 |  
| 190 | 6 | 521 | 0.119999997 |  
| 195 | 6 | 119 | 0.119999997 |  
| 205 | 6 | 0 | 0.119999997 |  
| 206 | 6 | 0 | 0.119999997 |  
| 207 | 6 | 0 | 0.119999997 |  
| 221 | 6 | 0 | 0.119999997 |  
| 225 | 6 | 0 | 0.119999997 |  
| 226 | 6 | 0 | 0.119999997 |  
| 241 | 6 | 0 | 0.119999997 |  
| 242 | 6 | 0 | 0.119999997 |  
| 243 | 6 | 0 | 0.119999997 |  
| 250 | 6 | 0 | 0.119999997 |  
| 251 | 6 | 0 | 0.119999997 |  
| 252 | 6 | 0 | 0.119999997 |  
| 1 | 7 | 9 | 0.119999997 |  
| 4 | 7 | 0 | 0.119999997 |  
| 5 | 7 | 0 | 0.119999997 |  
| 6 | 7 | 0 | 0.119999997 |  
| 12 | 7 | 0 | 0.119999997 |  
| 21 | 7 | 0 | 0.119999997 |  
| 23 | 7 | 0 | 0.119999997 |  
| 24 | 7 | 0 | 0.119999997 |  
| 26 | 7 | 0 | 0.119999997 |  
| 27 | 7 | 0 | 0.119999997 |  
| 28 | 7 | 1 | 0.119999997 |  
| 29 | 7 | 0 | 0.119999997 |  
| 30 | 7 | 0 | 0.119999997 |  
| 32 | 7 | 0 | 0.119999997 |  
| 36 | 7 | 2 | 0.119999997 |  
| 37 | 7 | 2 | 0.119999997 |  
| 38 | 7 | 0 | 0.119999997 |  
| 41 | 7 | 0 | 0.119999997 |  
| 42 | 7 | 1 | 0.119999997 |  
| 43 | 7 | 1 | 0.119999997 |  
| 47 | 7 | 0 | 0.119999997 |  
| 49 | 7 | 0 | 0.119999997 |  
| 50 | 7 | 0 | 0.119999997 |  
| 53 | 7 | 0 | 0.119999997 |  
| 57 | 7 | 0 | 0.119999997 |  
| 58 | 7 | 0 | 0.119999997 |  
| 59 | 7 | 0 | 0.119999997 |  
| 61 | 7 | 0 | 0.119999997 |  
| 70 | 7 | 0 | 0.119999997 |  
| 92 | 7 | 0 | 0.119999997 |  
| 111 | 7 | 2883 | 0.119999997 |  
| 121 | 7 | 33 | 0.119999997 |  
| 122 | 7 | 12 | 0.119999997 |  
| 123 | 7 | 10 | 0.119999997 |  
| 124 | 7 | 6 | 0.119999997 |  
| 131 | 7 | 0 | 0.119999997 |  
| 141 | 7 | 188 | 0.119999997 |  
| 142 | 7 | 8 | 0.119999997 |  
| 143 | 7 | 7 | 0.119999997 |  
| 151 | 7 | 0 | 0.119999997 |  
| 152 | 7 | 1 | 0.119999997 |  
| 171 | 7 | 0 | 0.119999997 |  
| 176 | 7 | 26 | 0.119999997 |  
| 181 | 7 | 0 | 0.119999997 |  
| 182 | 7 | 0 | 0.119999997 |  
| 190 | 7 | 266 | 0.119999997 |  
| 195 | 7 | 117 | 0.119999997 |  
| 205 | 7 | 0 | 0.119999997 |  
| 206 | 7 | 0 | 0.119999997 |  
| 207 | 7 | 0 | 0.119999997 |  
| 221 | 7 | 0 | 0.119999997 |  
| 225 | 7 | 0 | 0.119999997 |  
| 226 | 7 | 0 | 0.119999997 |  
| 241 | 7 | 0 | 0.119999997 |  
| 242 | 7 | 0 | 0.119999997 |  
| 243 | 7 | 0 | 0.119999997 |  
| 250 | 7 | 0 | 0.119999997 |  
| 251 | 7 | 0 | 0.119999997 |  
| 252 | 7 | 0 | 0.119999997 |  

         ** WARNING **  
               Failed to find a dictionary entry with a key value of "GDD_plant"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "GDD_plant"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "GDD_plant".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "GDD_ini"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "GDD_ini"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "GDD_ini".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "GDD_dev"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "GDD_dev"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "GDD_dev".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "GDD_mid"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "GDD_mid"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "GDD_mid".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "GDD_late"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "GDD_late"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "GDD_late".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Kcb_Jan"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "Kcb_Jan"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "Kcb_Jan".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Kcb_Feb"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "Kcb_Feb"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "Kcb_Feb".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Kcb_Mar"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "Kcb_Mar"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "Kcb_Mar".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Kcb_Apr"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "Kcb_Apr"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "Kcb_Apr".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Kcb_May"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "Kcb_May"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "Kcb_May".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Kcb_Jun"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "Kcb_Jun"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "Kcb_Jun".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Kcb_Jul"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "Kcb_Jul"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "Kcb_Jul".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Kcb_Aug"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "Kcb_Aug"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "Kcb_Aug".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Kcb_Sep"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "Kcb_Sep"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "Kcb_Sep".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Kcb_Oct"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "Kcb_Oct"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "Kcb_Oct".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Kcb_Nov"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "Kcb_Nov"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "Kcb_Nov".  


         ** WARNING **  
               Failed to find a dictionary entry with a key value of "Kcb_Dec"  


         ** WARNING **  
               Failed to find a dictionary entry associated with key value of "Kcb_Dec"  
                 module:  ../src/dictionary.F90  
                 line no:  998  


         ** WARNING **  
               Failed to find a lookup table column named "Kcb_Dec".  

## Crop Kcb Curve Summary ##  

_only meaningful for landuses where the Kcb curve is defined in terms of days _  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
| 1 | 2012-05-01 (doy:122) | 2012-05-21 |  (doy:142) | 2012-07-10 |  (doy:192) | 2012-08-19 |  (doy:232) | 2012-09-18 |  (doy:262) | 2012-09-19 (doy:263) |  
| 4 | 2012-05-01 (doy:122) | 2012-05-21 |  (doy:142) | 2012-06-25 |  (doy:177) | 2012-08-04 |  (doy:217) | 2012-09-03 |  (doy:247) | 2012-09-04 (doy:248) |  
| 5 | 2012-05-20 (doy:141) | 2012-06-09 |  (doy:161) | 2012-07-09 |  (doy:191) | 2012-09-07 |  (doy:251) | 2012-10-02 |  (doy:276) | 2012-10-03 (doy:277) |  
| 6 | 2012-05-01 (doy:122) | 2012-05-26 |  (doy:147) | 2012-06-30 |  (doy:182) | 2012-08-14 |  (doy:227) | 2012-09-08 |  (doy:252) | 2012-09-09 (doy:253) |  
| 12 | 2012-05-05 (doy:126) | 2012-05-25 |  (doy:146) | 2012-07-14 |  (doy:196) | 2012-08-23 |  (doy:236) | 2012-09-12 |  (doy:256) | 2012-09-13 (doy:257) |  
| 21 | 2012-04-20 (doy:111) | 2012-05-30 |  (doy:151) | 2012-07-09 |  (doy:191) | 2012-08-28 |  (doy:241) | 2012-10-17 |  (doy:291) | 2012-10-18 (doy:292) |  
| 23 | 2012-04-20 (doy:111) | 2012-05-10 |  (doy:131) | 2012-06-04 |  (doy:156) | 2012-08-03 |  (doy:216) | 2012-09-02 |  (doy:246) | 2012-09-03 (doy:247) |  
| 24 | 2012-10-01 (doy:275) | 2013-03-10 |  (doy:69) | 2013-05-24 |  (doy:144) | 2013-08-07 |  (doy:219) | 2013-09-01 |  (doy:244) | 2013-09-02 (doy:245) |  
| 26 | 2012-05-15 (doy:136) | 2012-06-04 |  (doy:156) | 2012-07-04 |  (doy:186) | 2012-09-02 |  (doy:246) | 2012-09-27 |  (doy:271) | 2012-09-28 (doy:272) |  
| 27 | 2012-04-20 (doy:111) | 2012-05-10 |  (doy:131) | 2012-06-04 |  (doy:156) | 2012-08-03 |  (doy:216) | 2012-09-02 |  (doy:246) | 2012-09-03 (doy:247) |  
| 28 | 2012-04-20 (doy:111) | 2012-05-10 |  (doy:131) | 2012-06-04 |  (doy:156) | 2012-08-03 |  (doy:216) | 2012-09-02 |  (doy:246) | 2012-09-03 (doy:247) |  
| 29 | 2012-04-20 (doy:111) | 2012-05-10 |  (doy:131) | 2012-06-09 |  (doy:161) | 2012-08-03 |  (doy:216) | 2012-09-07 |  (doy:251) | 2012-09-08 (doy:252) |  
| 30 | 2012-04-20 (doy:111) | 2012-05-10 |  (doy:131) | 2012-06-04 |  (doy:156) | 2012-08-03 |  (doy:216) | 2012-09-02 |  (doy:246) | 2012-09-03 (doy:247) |  
| 32 | 2012-04-20 (doy:111) | 2012-05-10 |  (doy:131) | 2012-06-04 |  (doy:156) | 2012-08-03 |  (doy:216) | 2012-09-02 |  (doy:246) | 2012-09-03 (doy:247) |  
| 36 | 2012-04-15 (doy:106) | 2012-04-25 |  (doy:116) | 2012-05-25 |  (doy:146) | 2012-08-23 |  (doy:236) | 2012-09-22 |  (doy:266) | 2012-09-23 (doy:267) |  
| 37 | 2012-05-01 (doy:122) | 2012-05-11 |  (doy:132) | 2012-06-10 |  (doy:162) | 2012-07-05 |  (doy:187) | 2012-07-15 |  (doy:197) | 2012-07-16 (doy:198) |  
| 38 | 2012-05-01 (doy:122) | 2012-05-11 |  (doy:132) | 2012-06-10 |  (doy:162) | 2012-07-05 |  (doy:187) | 2012-07-15 |  (doy:197) | 2012-07-16 (doy:198) |  
| 41 | 2012-04-20 (doy:111) | 2012-06-09 |  (doy:161) | 2012-07-19 |  (doy:201) | 2012-09-07 |  (doy:251) | 2012-10-17 |  (doy:291) | 2012-10-18 (doy:292) |  
| 42 | 2012-05-20 (doy:141) | 2012-06-09 |  (doy:161) | 2012-07-09 |  (doy:191) | 2012-08-18 |  (doy:231) | 2012-09-07 |  (doy:251) | 2012-09-08 (doy:252) |  
| 43 | 2012-05-10 (doy:131) | 2012-05-30 |  (doy:151) | 2012-08-08 |  (doy:221) | 2012-09-07 |  (doy:251) | 2012-09-17 |  (doy:261) | 2012-09-18 (doy:262) |  
| 47 | 2012-05-01 (doy:122) | 2012-05-21 |  (doy:142) | 2012-06-20 |  (doy:172) | 2012-08-04 |  (doy:217) | 2012-09-03 |  (doy:247) | 2012-09-04 (doy:248) |  
| 49 | 2012-04-15 (doy:106) | 2012-04-30 |  (doy:121) | 2012-05-25 |  (doy:146) | 2012-08-03 |  (doy:216) | 2012-09-12 |  (doy:256) | 2012-09-13 (doy:257) |  
| 50 | 2012-04-20 (doy:111) | 2012-05-25 |  (doy:146) | 2012-06-19 |  (doy:171) | 2012-07-19 |  (doy:201) | 2012-08-08 |  (doy:221) | 2012-08-09 (doy:222) |  
| 53 | 2012-04-15 (doy:106) | 2012-05-20 |  (doy:141) | 2012-06-14 |  (doy:166) | 2012-07-14 |  (doy:196) | 2012-08-03 |  (doy:216) | 2012-08-04 (doy:217) |  
| 57 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 58 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 59 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 61 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 70 | 2012-05-01 (doy:122) | 2012-05-11 |  (doy:132) | 2012-06-10 |  (doy:162) | 2012-08-29 |  (doy:242) | 2012-09-18 |  (doy:262) | 2012-09-19 (doy:263) |  
| 92 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 111 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 121 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 122 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 123 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 124 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 131 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 141 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 142 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 143 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 151 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 152 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 171 | 2012-04-15 (doy:106) | 2012-04-25 |  (doy:116) | 2012-05-20 |  (doy:141) | 2012-06-24 |  (doy:176) | 2012-07-29 |  (doy:211) | 2012-07-30 (doy:212) |  
| 176 | 2012-04-15 (doy:106) | 2012-04-25 |  (doy:116) | 2012-05-25 |  (doy:146) | 2012-06-29 |  (doy:181) | 2012-08-03 |  (doy:216) | 2012-08-04 (doy:217) |  
| 181 | 2012-04-15 (doy:106) | 2012-04-25 |  (doy:116) | 2012-05-20 |  (doy:141) | 2012-06-24 |  (doy:176) | 2012-07-29 |  (doy:211) | 2012-07-30 (doy:212) |  
| 182 | 2012-05-01 (doy:122) | 2012-05-11 |  (doy:132) | 2012-06-10 |  (doy:162) | 2012-08-29 |  (doy:242) | 2012-09-18 |  (doy:262) | 2012-09-19 (doy:263) |  
| 190 | 2012-05-01 (doy:122) | 2012-05-11 |  (doy:132) | 2012-06-10 |  (doy:162) | 2012-08-29 |  (doy:242) | 2012-09-18 |  (doy:262) | 2012-09-19 (doy:263) |  
| 195 | 2012-05-01 (doy:122) | 2012-05-11 |  (doy:132) | 2012-06-10 |  (doy:162) | 2012-08-29 |  (doy:242) | 2012-09-18 |  (doy:262) | 2012-09-19 (doy:263) |  
| 205 | 2012-04-15 (doy:106) | 2012-05-15 |  (doy:136) | 2012-06-24 |  (doy:176) | 2012-08-23 |  (doy:236) | 2012-09-12 |  (doy:256) | 2012-09-13 (doy:257) |  
| 206 | 2012-04-15 (doy:106) | 2012-05-15 |  (doy:136) | 2012-06-24 |  (doy:176) | 2012-08-23 |  (doy:236) | 2012-09-12 |  (doy:256) | 2012-09-13 (doy:257) |  
| 207 | 2012-02-15 (doy:46) | 2012-05-15 |  (doy:136) | 2012-06-14 |  (doy:166) | 2012-12-31 |  (doy:366) | 2013-02-14 |  (doy:45) | 2013-02-15 (doy:46) |  
| 221 | 2012-02-15 (doy:46) | 2012-05-15 |  (doy:136) | 2012-06-14 |  (doy:166) | 2012-12-31 |  (doy:366) | 2013-02-14 |  (doy:45) | 2013-02-15 (doy:46) |  
| 225 | 2012-05-01 (doy:122) | 2012-05-11 |  (doy:132) | 2012-06-10 |  (doy:162) | 2012-08-29 |  (doy:242) | 2012-09-18 |  (doy:262) | 2012-09-19 (doy:263) |  
| 226 | 2012-05-01 (doy:122) | 2012-05-11 |  (doy:132) | 2012-06-10 |  (doy:162) | 2012-08-29 |  (doy:242) | 2012-09-18 |  (doy:262) | 2012-09-19 (doy:263) |  
| 241 | 2012-05-01 (doy:122) | 2012-05-31 |  (doy:152) | 2012-07-20 |  (doy:202) | 2012-08-29 |  (doy:242) | 2012-09-18 |  (doy:262) | 2012-09-19 (doy:263) |  
| 242 | 2012-04-20 (doy:111) | 2012-05-10 |  (doy:131) | 2012-06-29 |  (doy:181) | 2012-09-27 |  (doy:271) | 2012-10-17 |  (doy:291) | 2012-10-18 (doy:292) |  
| 243 | 2012-04-20 (doy:111) | 2012-05-10 |  (doy:131) | 2012-06-09 |  (doy:161) | 2012-06-29 |  (doy:181) | 2012-07-09 |  (doy:191) | 2012-07-10 (doy:192) |  
| 250 | 2012-05-01 (doy:122) | 2012-05-11 |  (doy:132) | 2012-06-10 |  (doy:162) | 2012-08-29 |  (doy:242) | 2012-09-18 |  (doy:262) | 2012-09-19 (doy:263) |  
| 251 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  
| 252 | 2012-03-30 (doy:90) | 2012-04-19 |  (doy:110) | 2012-06-28 |  (doy:180) | 2012-09-26 |  (doy:270) | 2012-10-26 |  (doy:300) | 2012-10-27 (doy:301) |  

         ** WARNING **  
               Failed to find a dictionary entry associated with a key value of "OPTION".  
                 module:  ../src/dictionary.F90  
                 line no:  318  


> OUTPUTENABLE bare_soil_evaporation crop_et snowmelt soil_storage delta_soil_storage ENABLE growing_season growing_degree_day  

> Enabling output for 'bare_soil_evaporation'  

> Enabling output for 'crop_et'  

> Enabling output for 'snowmelt'  

> Enabling output for 'soil_storage'  

> Enabling output for 'delta_soil_storage'  

> Enabling output for 'growing_season'  

> Enabling output for 'growing_degree_day'  

Catalog key: "PRECIPITATION"  

---------------------------------------------------  
DATA STRUCTURE DETAILS:  
---------------------------------------------------  
catalog key word: "PRECIPITATION"  
source data form: 4  
source data type: -9999  
source file type: 2  
description:  
source PROJ4 string: +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs  
source file type: NETCDF  
filename template: /Users/hellyj/src/swb2-jjh/test/test_data/cs/prcp_Daymet_v3_%y.nc  
source filename: /Users/hellyj/src/swb2-jjh/test/test_data/cs/prcp_Daymet_v3_%y.nc  
---------------------------------------------------  
GRID DETAILS:  
---------------------------------------------------  
file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/prcp_Daymet_v3_%y.nc"  
nx: 400  
ny: 346  
cellsize: 90.000000000000000  
X0: 545300.00000000000  
Y0: 432200.00000000000  
X1: 581300.00000000000  
Y1: 463340.00000000000  
Type: 1  
PROJ4 string: "+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m"  
---------------------------------------------------  

Catalog key: "TMIN"  

---------------------------------------------------  
DATA STRUCTURE DETAILS:  
---------------------------------------------------  
catalog key word: "TMIN"  
source data form: 4  
source data type: -9999  
source file type: 2  
description:  
source PROJ4 string: +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs  
source file type: NETCDF  
filename template: /Users/hellyj/src/swb2-jjh/test/test_data/cs/tmin_Daymet_v3_%y.nc  
source filename: /Users/hellyj/src/swb2-jjh/test/test_data/cs/tmin_Daymet_v3_%y.nc  
---------------------------------------------------  
GRID DETAILS:  
---------------------------------------------------  
file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/tmin_Daymet_v3_%y.nc"  
nx: 400  
ny: 346  
cellsize: 90.000000000000000  
X0: 545300.00000000000  
Y0: 432200.00000000000  
X1: 581300.00000000000  
Y1: 463340.00000000000  
Type: 1  
PROJ4 string: "+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m"  
---------------------------------------------------  

Catalog key: "TMAX"  

---------------------------------------------------  
DATA STRUCTURE DETAILS:  
---------------------------------------------------  
catalog key word: "TMAX"  
source data form: 4  
source data type: -9999  
source file type: 2  
description:  
source PROJ4 string: +proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs  
source file type: NETCDF  
filename template: /Users/hellyj/src/swb2-jjh/test/test_data/cs/tmax_Daymet_v3_%y.nc  
source filename: /Users/hellyj/src/swb2-jjh/test/test_data/cs/tmax_Daymet_v3_%y.nc  
---------------------------------------------------  
GRID DETAILS:  
---------------------------------------------------  
file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/tmax_Daymet_v3_%y.nc"  
nx: 400  
ny: 346  
cellsize: 90.000000000000000  
X0: 545300.00000000000  
Y0: 432200.00000000000  
X1: 581300.00000000000  
Y1: 463340.00000000000  
Type: 1  
PROJ4 string: "+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m"  
---------------------------------------------------  

Catalog key: "AVAILABLE_WATER_CONTENT"  

---------------------------------------------------  
DATA STRUCTURE DETAILS:  
---------------------------------------------------  
catalog key word: "AVAILABLE_WATER_CONTENT"  
source data form: 1  
source data type: 1  
source file type: 0  
description: AVAILABLE_WATER_CONTENT  
source PROJ4 string: +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m  
source file type: ARC_GRID  
filename template:  
source filename: /Users/hellyj/src/swb2-jjh/test/test_data/cs/available_water_capacity.asc  
---------------------------------------------------  
GRID DETAILS:  
---------------------------------------------------  
file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/available_water_capacity.asc"  
nx: 400  
ny: 346  
cellsize: 90.000000000000000  
X0: 545300.00000000000  
Y0: 432200.00000000000  
X1: 581300.00000000000  
Y1: 463340.00000000000  
Type: 1  
PROJ4 string: "+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m"  
---------------------------------------------------  

Catalog key: "FLOW_DIRECTION"  

---------------------------------------------------  
DATA STRUCTURE DETAILS:  
---------------------------------------------------  
catalog key word: "FLOW_DIRECTION"  
source data form: 1  
source data type: 0  
source file type: 0  
description: FLOW_DIRECTION  
source PROJ4 string: +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m  
source file type: ARC_GRID  
filename template:  
source filename: /Users/hellyj/src/swb2-jjh/test/test_data/cs/d8_flow_direction.asc  
---------------------------------------------------  
GRID DETAILS:  
---------------------------------------------------  
file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/d8_flow_direction.asc"  
nx: 400  
ny: 346  
cellsize: 90.000000000000000  
X0: 545300.00000000000  
Y0: 432200.00000000000  
X1: 581300.00000000000  
Y1: 463340.00000000000  
Type: 0  
PROJ4 string: "+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m"  
---------------------------------------------------  

Catalog key: "LAND_USE"  

---------------------------------------------------  
DATA STRUCTURE DETAILS:  
---------------------------------------------------  
catalog key word: "LAND_USE"  
source data form: 1  
source data type: 0  
source file type: 0  
description: LAND_USE  
source PROJ4 string: +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m  
source file type: ARC_GRID  
filename template:  
source filename: /Users/hellyj/src/swb2-jjh/test/test_data/cs/landuse.asc  
---------------------------------------------------  
GRID DETAILS:  
---------------------------------------------------  
file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/landuse.asc"  
nx: 400  
ny: 346  
cellsize: 90.000000000000000  
X0: 545300.00000000000  
Y0: 432200.00000000000  
X1: 581300.00000000000  
Y1: 463340.00000000000  
Type: 0  
PROJ4 string: "+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m"  
---------------------------------------------------  

Catalog key: "HYDROLOGIC_SOILS_GROUP"  

---------------------------------------------------  
DATA STRUCTURE DETAILS:  
---------------------------------------------------  
catalog key word: "HYDROLOGIC_SOILS_GROUP"  
source data form: 1  
source data type: 0  
source file type: 0  
description: HYDROLOGIC_SOILS_GROUP  
source PROJ4 string: +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m  
source file type: ARC_GRID  
filename template:  
source filename: /Users/hellyj/src/swb2-jjh/test/test_data/cs/hydrologic_soils_group.asc  
---------------------------------------------------  
GRID DETAILS:  
---------------------------------------------------  
file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/hydrologic_soils_group.asc"  
nx: 400  
ny: 346  
cellsize: 90.000000000000000  
X0: 545300.00000000000  
Y0: 432200.00000000000  
X1: 581300.00000000000  
Y1: 463340.00000000000  
Type: 0  
PROJ4 string: "+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m"  
---------------------------------------------------  

Catalog key: "INITIAL_PERCENT_SOIL_MOISTURE"  

---------------------------------------------------  
DATA STRUCTURE DETAILS:  
---------------------------------------------------  
catalog key word: "INITIAL_PERCENT_SOIL_MOISTURE"  
source data form: 0  
source data type: 1  
source file type: 4  
description: INITIAL_PERCENT_SOIL_MOISTURE  
source PROJ4 string:  
source file type:  
filename template:  
source filename:  
---------------------------------------------------  
GRID DETAILS:  
---------------------------------------------------  
file: "None: constant value entered from control file."  
nx: 400  
ny: 346  
cellsize: 90.000000000000000  
X0: 545300.00000000000  
Y0: 432200.00000000000  
X1: 581300.00000000000  
Y1: 463340.00000000000  
Type: 1  
PROJ4 string: "+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m"  
---------------------------------------------------  

Catalog key: "INITIAL_SNOW_COVER_STORAGE"  

---------------------------------------------------  
DATA STRUCTURE DETAILS:  
---------------------------------------------------  
catalog key word: "INITIAL_SNOW_COVER_STORAGE"  
source data form: 0  
source data type: 1  
source file type: 4  
description: INITIAL_SNOW_COVER_STORAGE  
source PROJ4 string:  
source file type:  
filename template:  
source filename:  
---------------------------------------------------  
GRID DETAILS:  
---------------------------------------------------  
file: "None: constant value entered from control file."  
nx: 400  
ny: 346  
cellsize: 90.000000000000000  
X0: 545300.00000000000  
Y0: 432200.00000000000  
X1: 581300.00000000000  
Y1: 463340.00000000000  
Type: 1  
PROJ4 string: "+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m"  
---------------------------------------------------  

Catalog key: "INITIAL_CONTINUOUS_FROZEN_GROUND_INDEX"  

---------------------------------------------------  
DATA STRUCTURE DETAILS:  
---------------------------------------------------  
catalog key word: "INITIAL_CONTINUOUS_FROZEN_GROUND_INDEX"  
source data form: 0  
source data type: 1  
source file type: 4  
description: INITIAL_CONTINUOUS_FROZEN_GROUND_INDEX  
source PROJ4 string:  
source file type:  
filename template:  
source filename:  
---------------------------------------------------  
GRID DETAILS:  
---------------------------------------------------  
file: "None: constant value entered from control file."  
nx: 400  
ny: 346  
cellsize: 90.000000000000000  
X0: 545300.00000000000  
Y0: 432200.00000000000  
X1: 581300.00000000000  
Y1: 463340.00000000000  
Type: 1  
PROJ4 string: "+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m"  
---------------------------------------------------  

Catalog key: "CFGI_LOWER_LIMIT"  

---------------------------------------------------  
DATA STRUCTURE DETAILS:  
---------------------------------------------------  
catalog key word: "CFGI_LOWER_LIMIT"  
source data form: 0  
source data type: 1  
source file type: 4  
description: CFGI_LOWER_LIMIT  
source PROJ4 string:  
source file type:  
filename template:  
source filename:  
---------------------------------------------------  
GRID DETAILS:  
---------------------------------------------------  
file: "None: constant value entered from control file."  
nx: 400  
ny: 346  
cellsize: 90.000000000000000  
X0: 545300.00000000000  
Y0: 432200.00000000000  
X1: 581300.00000000000  
Y1: 463340.00000000000  
Type: 1  
PROJ4 string: "+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m"  
---------------------------------------------------  

Catalog key: "CFGI_UPPER_LIMIT"  

---------------------------------------------------  
DATA STRUCTURE DETAILS:  
---------------------------------------------------  
catalog key word: "CFGI_UPPER_LIMIT"  
source data form: 0  
source data type: 1  
source file type: 4  
description: CFGI_UPPER_LIMIT  
source PROJ4 string:  
source file type:  
filename template:  
source filename:  
---------------------------------------------------  
GRID DETAILS:  
---------------------------------------------------  
file: "None: constant value entered from control file."  
nx: 400  
ny: 346  
cellsize: 90.000000000000000  
X0: 545300.00000000000  
Y0: 432200.00000000000  
X1: 581300.00000000000  
Y1: 463340.00000000000  
Type: 1  
PROJ4 string: "+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m"  
---------------------------------------------------  

Catalog key: "IRRIGATION_MASK"  

---------------------------------------------------  
DATA STRUCTURE DETAILS:  
---------------------------------------------------  
catalog key word: "IRRIGATION_MASK"  
source data form: 1  
source data type: 0  
source file type: 0  
description: IRRIGATION_MASK  
source PROJ4 string: +proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m  
source file type: ARC_GRID  
filename template:  
source filename: /Users/hellyj/src/swb2-jjh/test/test_data/cs/irrigation_mask_from_cdl.asc  
---------------------------------------------------  
GRID DETAILS:  
---------------------------------------------------  
file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/irrigation_mask_from_cdl.asc"  
nx: 400  
ny: 346  
cellsize: 90.000000000000000  
X0: 545300.00000000000  
Y0: 432200.00000000000  
X1: 581300.00000000000  
Y1: 463340.00000000000  
Type: 0  
PROJ4 string: "+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m"  
---------------------------------------------------  
### Summary of all items stored in dictionary data structure  

 1)  KEY: "GRID"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |400                                               |
|         2         |346                                               |
|         3         |545300                                            |
|         4         |432200                                            |
|         5         |90.0                                              |

 2)  KEY: "BASE_PROJECTION_DEFINITION"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |+proj=tmerc                                       |
|         2         |+lat_0=0.0                                        |
|         3         |+lon_0=-90.0                                      |
|         4         |+k=0.9996                                         |
|         5         |+x_0=520000                                       |
|         6         |+y_0=-4480000                                     |
|         7         |+datum=NAD83                                      |
|         8         |+units=m                                          |

 3)  KEY: "INTERCEPTION_METHOD"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |BUCKET                                            |

 4)  KEY: "EVAPOTRANSPIRATION_METHOD"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |HARGREAVES                                        |

 5)  KEY: "RUNOFF_METHOD"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |CURVE_NUMBER                                      |

 6)  KEY: "SOIL_MOISTURE_METHOD"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |FAO-56_TWO_STAGE                                  |

 7)  KEY: "PRECIPITATION_METHOD"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |GRIDDED                                           |

 8)  KEY: "GROWING_DEGREE_DAY_METHOD"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |BASKERVILLE_EMIN                                  |

 9)  KEY: "FOG_METHOD"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |NONE                                              |

 10)  KEY: "FLOW_ROUTING_METHOD"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |NONE                                              |

 11)  KEY: "IRRIGATION_METHOD"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |FAO-56                                            |

 12)  KEY: "ROOTING_DEPTH_METHOD"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |DYNAMIC                                           |

 13)  KEY: "CROP_COEFFICIENT_METHOD"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |FAO-56                                            |

 14)  KEY: "DIRECT_RECHARGE_METHOD"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |NONE                                              |

 15)  KEY: "SOIL_STORAGE_MAX_METHOD"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |CALCULATED                                        |

 16)  KEY: "AVAILABLE_WATER_CONTENT_METHOD"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |GRIDDED                                           |

 17)  KEY: "GROWING_SEASON"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |133                                               |
|         2         |268                                               |
|         3         |TRUE                                              |

 18)  KEY: "PRECIPITATION"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |NETCDF                                            |
|         2         |prcp_Daymet_v3_%y.nc                              |

 19)  KEY: "PRECIPITATION_GRID_PROJECTION_DEFINITION"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |+proj=lcc                                         |
|         2         |+lat_1=25.0                                       |
|         3         |+lat_2=60.0                                       |
|         4         |+lat_0=42.5                                       |
|         5         |+lon_0=-100.0                                     |
|         6         |+x_0=0.0                                          |
|         7         |+y_0=0.0                                          |
|         8         |+ellps=GRS80                                      |
|         9         |+datum=NAD83                                      |
|        10         |+units=m                                          |
|        11         |+no_defs                                          |

 20)  KEY: "PRECIPITATION_NETCDF_Z_VAR"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |prcp                                              |

 21)  KEY: "PRECIPITATION_SCALE_FACTOR"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |0.03937008                                        |

 22)  KEY: "PRECIPITATION_MISSING_VALUES_CODE"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |-9999.0                                           |

 23)  KEY: "PRECIPITATION_MISSING_VALUES_OPERATOR"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |<=                                                |

 24)  KEY: "PRECIPITATION_MISSING_VALUES_ACTION"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |zero                                              |

 25)  KEY: "TMAX"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |NETCDF                                            |
|         2         |tmax_Daymet_v3_%y.nc                              |

 26)  KEY: "TMAX_GRID_PROJECTION_DEFINITION"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |+proj=lcc                                         |
|         2         |+lat_1=25.0                                       |
|         3         |+lat_2=60.0                                       |
|         4         |+lat_0=42.5                                       |
|         5         |+lon_0=-100.0                                     |
|         6         |+x_0=0.0                                          |
|         7         |+y_0=0.0                                          |
|         8         |+ellps=GRS80                                      |
|         9         |+datum=NAD83                                      |
|        10         |+units=m                                          |
|        11         |+no_defs                                          |

 27)  KEY: "TMAX_SCALE_FACTOR"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |1.8                                               |

 28)  KEY: "TMAX_ADD_OFFSET"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |32.0                                              |

 29)  KEY: "TMAX_MISSING_VALUES_CODE"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |-9999.0                                           |

 30)  KEY: "TMAX_MISSING_VALUES_OPERATOR"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |<=                                                |

 31)  KEY: "TMAX_MISSING_VALUES_ACTION"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |mean                                              |

 32)  KEY: "TMIN"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |NETCDF                                            |
|         2         |tmin_Daymet_v3_%y.nc                              |

 33)  KEY: "TMIN_GRID_PROJECTION_DEFINITION"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |+proj=lcc                                         |
|         2         |+lat_1=25.0                                       |
|         3         |+lat_2=60.0                                       |
|         4         |+lat_0=42.5                                       |
|         5         |+lon_0=-100.0                                     |
|         6         |+x_0=0.0                                          |
|         7         |+y_0=0.0                                          |
|         8         |+ellps=GRS80                                      |
|         9         |+datum=NAD83                                      |
|        10         |+units=m                                          |
|        11         |+no_defs                                          |

 34)  KEY: "TMIN_SCALE_FACTOR"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |1.8                                               |

 35)  KEY: "TMIN_ADD_OFFSET"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |32.0                                              |

 36)  KEY: "TMIN_MISSING_VALUES_CODE"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |-9999.0                                           |

 37)  KEY: "TMIN_MISSING_VALUES_OPERATOR"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |<=                                                |

 38)  KEY: "TMIN_MISSING_VALUES_ACTION"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |mean                                              |

 39)  KEY: "INITIAL_CONTINUOUS_FROZEN_GROUND_INDEX"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |CONSTANT                                          |
|         2         |100.0                                             |

 40)  KEY: "CFGI_UPPER_LIMIT"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |CONSTANT                                          |
|         2         |100.                                              |

 41)  KEY: "CFGI_LOWER_LIMIT"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |CONSTANT                                          |
|         2         |40.                                               |

 42)  KEY: "FLOW_DIRECTION"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |ARC_GRID                                          |
|         2         |d8_flow_direction.asc                             |

 43)  KEY: "FLOW_DIRECTION_PROJECTION_DEFINITION"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |+proj=tmerc                                       |
|         2         |+lat_0=0.0                                        |
|         3         |+lon_0=-90.0                                      |
|         4         |+k=0.9996                                         |
|         5         |+x_0=520000                                       |
|         6         |+y_0=-4480000                                     |
|         7         |+datum=NAD83                                      |
|         8         |+units=m                                          |

 44)  KEY: "HYDROLOGIC_SOILS_GROUP"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |ARC_GRID                                          |
|         2         |hydrologic_soils_group.asc                        |

 45)  KEY: "HYDROLOGIC_SOILS_GROUP_PROJECTION_DEFINITION"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |+proj=tmerc                                       |
|         2         |+lat_0=0.0                                        |
|         3         |+lon_0=-90.0                                      |
|         4         |+k=0.9996                                         |
|         5         |+x_0=520000                                       |
|         6         |+y_0=-4480000                                     |
|         7         |+datum=NAD83                                      |
|         8         |+units=m                                          |

 46)  KEY: "LAND_USE"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |ARC_GRID                                          |
|         2         |landuse.asc                                       |

 47)  KEY: "LANDUSE_PROJECTION_DEFINITION"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |+proj=tmerc                                       |
|         2         |+lat_0=0.0                                        |
|         3         |+lon_0=-90.0                                      |
|         4         |+k=0.9996                                         |
|         5         |+x_0=520000                                       |
|         6         |+y_0=-4480000                                     |
|         7         |+datum=NAD83                                      |
|         8         |+units=m                                          |

 48)  KEY: "AVAILABLE_WATER_CONTENT"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |ARC_GRID                                          |
|         2         |available_water_capacity.asc                      |

 49)  KEY: "AVAILABLE_WATER_CONTENT_PROJECTION_DEFINITION"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |+proj=tmerc                                       |
|         2         |+lat_0=0.0                                        |
|         3         |+lon_0=-90.0                                      |
|         4         |+k=0.9996                                         |
|         5         |+x_0=520000                                       |
|         6         |+y_0=-4480000                                     |
|         7         |+datum=NAD83                                      |
|         8         |+units=m                                          |

 50)  KEY: "IRRIGATION_MASK"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |ARC_GRID                                          |
|         2         |irrigation_mask_from_cdl.asc                      |

 51)  KEY: "IRRIGATION_MASK_PROJECTION_DEFINITION"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |+proj=tmerc                                       |
|         2         |+lat_0=0.0                                        |
|         3         |+lon_0=-90.0                                      |
|         4         |+k=0.9996                                         |
|         5         |+x_0=520000                                       |
|         6         |+y_0=-4480000                                     |
|         7         |+datum=NAD83                                      |
|         8         |+units=m                                          |

 52)  KEY: "LAND_USE_LOOKUP_TABLE"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |Landuse_lookup_CDL.txt                            |

 53)  KEY: "IRRIGATION_LOOKUP_TABLE"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |Irrigation_lookup_CDL.txt                         |

 54)  KEY: "HARGREAVES_ET_LOOKUP_TABLE"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |Hargreaves_ET_parameters.txt                      |

 55)  KEY: "INITIAL_PERCENT_SOIL_MOISTURE"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |CONSTANT                                          |
|         2         |100.0                                             |

 56)  KEY: "INITIAL_SNOW_COVER_STORAGE"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |CONSTANT                                          |
|         2         |2.0                                               |

 57)  KEY: "DUMP_VARIABLES_1"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |COORDINATES                                       |
|         2         |561167.                                           |
|         3         |445224.                                           |

 58)  KEY: "DUMP_VARIABLES_2"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |COORDINATES                                       |
|         2         |546094.,                                          |
|         3         |438492.                                           |

 59)  KEY: "DUMP_VARIABLES_3"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |COORDINATES                                       |
|         2         |556791.                                           |
|         3         |457569.                                           |

 60)  KEY: "DUMP_VARIABLES_4"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |COORDINATES                                       |
|         2         |568129.                                           |
|         3         |458340.                                           |

 61)  KEY: "DUMP_VARIABLES_5"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |COORDINATES                                       |
|         2         |553927.                                           |
|         3         |459454.                                           |

 62)  KEY: "DUMP_VARIABLES_6"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |COORDINATES                                       |
|         2         |555602.                                           |
|         3         |434644.                                           |

 63)  KEY: "DUMP_VARIABLES_7"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |COORDINATES                                       |
|         2         |558663.                                           |
|         3         |432949.                                           |

 64)  KEY: "DUMP_VARIABLES_8"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |COORDINATES                                       |
|         2         |570741.                                           |
|         3         |445112.                                           |

 65)  KEY: "OUTPUT"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |ENABLE                                            |
|         2         |bare_soil_evaporation                             |
|         3         |crop_et                                           |
|         4         |snowmelt                                          |
|         5         |soil_storage                                      |
|         6         |delta_soil_storage                                |
|         7         |ENABLE                                            |
|         8         |growing_season                                    |
|         9         |growing_degree_day                                |

 66)  KEY: "START_DATE"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |01/01/2012                                        |

 67)  KEY: "END_DATE"  

|Index              |Value                                             |
|------------------:|-------------------------------------------------:|
|         1         |12/31/2013                                        |

Time spent initializing simulation: 2 seconds.  

Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_gross_precipitation__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_rainfall__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_snowfall__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_interception__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_runon__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_runoff__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_soil_storage__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_delta_soil_storage__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_reference_ET0__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_actual_et__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_snowmelt__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_tmin__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_tmax__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_net_infiltration__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_rejected_net_infiltration__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_irrigation__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_runoff_outside__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_crop_et__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_bare_soil_evaporation__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_growing_degree_day__2012-01-01_to_2013-12-31__346_by_400.nc"  
Attempting to open netCDF file for writing with filename "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_growing_season__2012-01-01_to_2013-12-31__346_by_400.nc"  
Calculating: 2012-01-01  
Opening file "/Users/hellyj/src/swb2-jjh/test/test_data/cs/irrigation_mask_from_cdl.asc" for IRRIGATION_MASK data.  
Attempting to open READONLY netCDF file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/prcp_Daymet_v3_2012.nc"  
Succeeded.  ncid: 1441792  format: NC_FORMAT_CLASSIC  

Transforming gridded data in file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/prcp_Daymet_v3_2012.nc"  
 FROM: '+proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'  
 TO:   '+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m'  
Attempting to open READONLY netCDF file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/tmin_Daymet_v3_2012.nc"  
Succeeded.  ncid: 1507328  format: NC_FORMAT_CLASSIC  

Transforming gridded data in file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/tmin_Daymet_v3_2012.nc"  
 FROM: '+proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'  
 TO:   '+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m'  
Attempting to open READONLY netCDF file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/tmax_Daymet_v3_2012.nc"  
Succeeded.  ncid: 1572864  format: NC_FORMAT_CLASSIC  

Transforming gridded data in file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/tmax_Daymet_v3_2012.nc"  
 FROM: '+proj=lcc +lat_1=25.0 +lat_2=60.0 +lat_0=42.5 +lon_0=-100.0 +x_0=0.0 +y_0=0.0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs'  
 TO:   '+proj=tmerc +lat_0=0.0 +lon_0=-90.0 +k=0.9996 +x_0=520000 +y_0=-4480000 +datum=NAD83 +units=m'  
Calculating: 2012-01-02  
Calculating: 2012-01-03  
Calculating: 2012-01-04  
Calculating: 2012-01-05  
Calculating: 2012-01-06  
Calculating: 2012-01-07  
Calculating: 2012-01-08  
Calculating: 2012-01-09  
Calculating: 2012-01-10  
Calculating: 2012-01-11  
Calculating: 2012-01-12  
Calculating: 2012-01-13  
Calculating: 2012-01-14  
Calculating: 2012-01-15  
Calculating: 2012-01-16  
Calculating: 2012-01-17  
Calculating: 2012-01-18  
Calculating: 2012-01-19  
Calculating: 2012-01-20  
Calculating: 2012-01-21  
Calculating: 2012-01-22  
Calculating: 2012-01-23  
Calculating: 2012-01-24  
Calculating: 2012-01-25  
Calculating: 2012-01-26  
Calculating: 2012-01-27  
Calculating: 2012-01-28  
Calculating: 2012-01-29  
Calculating: 2012-01-30  
Calculating: 2012-01-31  
Calculating: 2012-02-01  
Calculating: 2012-02-02  
Calculating: 2012-02-03  
Calculating: 2012-02-04  
Calculating: 2012-02-05  
Calculating: 2012-02-06  
Calculating: 2012-02-07  
Calculating: 2012-02-08  
Calculating: 2012-02-09  
Calculating: 2012-02-10  
Calculating: 2012-02-11  
Calculating: 2012-02-12  
Calculating: 2012-02-13  
Calculating: 2012-02-14  
Calculating: 2012-02-15  
Calculating: 2012-02-16  
Calculating: 2012-02-17  
Calculating: 2012-02-18  
Calculating: 2012-02-19  
Calculating: 2012-02-20  
Calculating: 2012-02-21  
Calculating: 2012-02-22  
Calculating: 2012-02-23  
Calculating: 2012-02-24  
Calculating: 2012-02-25  
Calculating: 2012-02-26  
Calculating: 2012-02-27  
Calculating: 2012-02-28  
Calculating: 2012-02-29  
Calculating: 2012-03-01  
Calculating: 2012-03-02  
Calculating: 2012-03-03  
Calculating: 2012-03-04  
Calculating: 2012-03-05  
Calculating: 2012-03-06  
Calculating: 2012-03-07  
Calculating: 2012-03-08  
Calculating: 2012-03-09  
Calculating: 2012-03-10  
Calculating: 2012-03-11  
Calculating: 2012-03-12  
Calculating: 2012-03-13  
Calculating: 2012-03-14  
Calculating: 2012-03-15  
Calculating: 2012-03-16  
Calculating: 2012-03-17  
Calculating: 2012-03-18  
Calculating: 2012-03-19  
Calculating: 2012-03-20  
Calculating: 2012-03-21  
Calculating: 2012-03-22  
Calculating: 2012-03-23  
Calculating: 2012-03-24  
Calculating: 2012-03-25  
Calculating: 2012-03-26  
Calculating: 2012-03-27  
Calculating: 2012-03-28  
Calculating: 2012-03-29  
Calculating: 2012-03-30  
Calculating: 2012-03-31  
Calculating: 2012-04-01  
Calculating: 2012-04-02  
Calculating: 2012-04-03  
Calculating: 2012-04-04  
Calculating: 2012-04-05  
Calculating: 2012-04-06  
Calculating: 2012-04-07  
Calculating: 2012-04-08  
Calculating: 2012-04-09  
Calculating: 2012-04-10  
Calculating: 2012-04-11  
Calculating: 2012-04-12  
Calculating: 2012-04-13  
Calculating: 2012-04-14  
Calculating: 2012-04-15  
Calculating: 2012-04-16  
Calculating: 2012-04-17  
Calculating: 2012-04-18  
Calculating: 2012-04-19  
Calculating: 2012-04-20  
Calculating: 2012-04-21  
Calculating: 2012-04-22  
Calculating: 2012-04-23  
Calculating: 2012-04-24  
Calculating: 2012-04-25  
Calculating: 2012-04-26  
Calculating: 2012-04-27  
Calculating: 2012-04-28  
Calculating: 2012-04-29  
Calculating: 2012-04-30  
Calculating: 2012-05-01  
Calculating: 2012-05-02  
Calculating: 2012-05-03  
Calculating: 2012-05-04  
Calculating: 2012-05-05  
Calculating: 2012-05-06  
Calculating: 2012-05-07  
Calculating: 2012-05-08  
Calculating: 2012-05-09  
Calculating: 2012-05-10  
Calculating: 2012-05-11  
Calculating: 2012-05-12  
Calculating: 2012-05-13  
Calculating: 2012-05-14  
Calculating: 2012-05-15  
Calculating: 2012-05-16  
Calculating: 2012-05-17  
Calculating: 2012-05-18  
Calculating: 2012-05-19  
Calculating: 2012-05-20  
Calculating: 2012-05-21  
Calculating: 2012-05-22  
Calculating: 2012-05-23  
Calculating: 2012-05-24  
Calculating: 2012-05-25  
Calculating: 2012-05-26  
Calculating: 2012-05-27  
Calculating: 2012-05-28  
Calculating: 2012-05-29  
Calculating: 2012-05-30  
Calculating: 2012-05-31  
Calculating: 2012-06-01  
Calculating: 2012-06-02  
Calculating: 2012-06-03  
Calculating: 2012-06-04  
Calculating: 2012-06-05  
Calculating: 2012-06-06  
Calculating: 2012-06-07  
Calculating: 2012-06-08  
Calculating: 2012-06-09  
Calculating: 2012-06-10  
Calculating: 2012-06-11  
Calculating: 2012-06-12  
Calculating: 2012-06-13  
Calculating: 2012-06-14  
Calculating: 2012-06-15  
Calculating: 2012-06-16  
Calculating: 2012-06-17  
Calculating: 2012-06-18  
Calculating: 2012-06-19  
Calculating: 2012-06-20  
Calculating: 2012-06-21  
Calculating: 2012-06-22  
Calculating: 2012-06-23  
Calculating: 2012-06-24  
Calculating: 2012-06-25  
Calculating: 2012-06-26  
Calculating: 2012-06-27  
Calculating: 2012-06-28  
Calculating: 2012-06-29  
Calculating: 2012-06-30  
Calculating: 2012-07-01  
Calculating: 2012-07-02  
Calculating: 2012-07-03  
Calculating: 2012-07-04  
Calculating: 2012-07-05  
Calculating: 2012-07-06  
Calculating: 2012-07-07  
Calculating: 2012-07-08  
Calculating: 2012-07-09  
Calculating: 2012-07-10  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
243 | 2013-04-20 | 2013-05-10 | 2013-06-09 | 2013-06-29 | 2013-07-09 | 2013-07-10  
Calculating: 2012-07-11  
Calculating: 2012-07-12  
Calculating: 2012-07-13  
Calculating: 2012-07-14  
Calculating: 2012-07-15  
Calculating: 2012-07-16  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
37 | 2013-05-01 | 2013-05-11 | 2013-06-10 | 2013-07-05 | 2013-07-15 | 2013-07-16  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
38 | 2013-05-01 | 2013-05-11 | 2013-06-10 | 2013-07-05 | 2013-07-15 | 2013-07-16  
Calculating: 2012-07-17  
Calculating: 2012-07-18  
Calculating: 2012-07-19  
Calculating: 2012-07-20  
Calculating: 2012-07-21  
Calculating: 2012-07-22  
Calculating: 2012-07-23  
Calculating: 2012-07-24  
Calculating: 2012-07-25  
Calculating: 2012-07-26  
Calculating: 2012-07-27  
Calculating: 2012-07-28  
Calculating: 2012-07-29  
Calculating: 2012-07-30  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
171 | 2013-04-15 | 2013-04-25 | 2013-05-20 | 2013-06-24 | 2013-07-29 | 2013-07-30  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
181 | 2013-04-15 | 2013-04-25 | 2013-05-20 | 2013-06-24 | 2013-07-29 | 2013-07-30  
Calculating: 2012-07-31  
Calculating: 2012-08-01  
Calculating: 2012-08-02  
Calculating: 2012-08-03  
Calculating: 2012-08-04  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
53 | 2013-04-15 | 2013-05-20 | 2013-06-14 | 2013-07-14 | 2013-08-03 | 2013-08-04  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
176 | 2013-04-15 | 2013-04-25 | 2013-05-25 | 2013-06-29 | 2013-08-03 | 2013-08-04  
Calculating: 2012-08-05  
Calculating: 2012-08-06  
Calculating: 2012-08-07  
Calculating: 2012-08-08  
Calculating: 2012-08-09  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
50 | 2013-04-20 | 2013-05-25 | 2013-06-19 | 2013-07-19 | 2013-08-08 | 2013-08-09  
Calculating: 2012-08-10  
Calculating: 2012-08-11  
Calculating: 2012-08-12  
Calculating: 2012-08-13  
Calculating: 2012-08-14  
Calculating: 2012-08-15  
Calculating: 2012-08-16  
Calculating: 2012-08-17  
Calculating: 2012-08-18  
Calculating: 2012-08-19  
Calculating: 2012-08-20  
Calculating: 2012-08-21  
Calculating: 2012-08-22  
Calculating: 2012-08-23  
Calculating: 2012-08-24  
Calculating: 2012-08-25  
Calculating: 2012-08-26  
Calculating: 2012-08-27  
Calculating: 2012-08-28  
Calculating: 2012-08-29  
Calculating: 2012-08-30  
Calculating: 2012-08-31  
Calculating: 2012-09-01  
Calculating: 2012-09-02  
Calculating: 2012-09-03  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
23 | 2013-04-20 | 2013-05-10 | 2013-06-04 | 2013-08-03 | 2013-09-02 | 2013-09-03  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
27 | 2013-04-20 | 2013-05-10 | 2013-06-04 | 2013-08-03 | 2013-09-02 | 2013-09-03  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
28 | 2013-04-20 | 2013-05-10 | 2013-06-04 | 2013-08-03 | 2013-09-02 | 2013-09-03  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
30 | 2013-04-20 | 2013-05-10 | 2013-06-04 | 2013-08-03 | 2013-09-02 | 2013-09-03  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
32 | 2013-04-20 | 2013-05-10 | 2013-06-04 | 2013-08-03 | 2013-09-02 | 2013-09-03  
Calculating: 2012-09-04  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
4 | 2013-05-01 | 2013-05-21 | 2013-06-25 | 2013-08-04 | 2013-09-03 | 2013-09-04  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
47 | 2013-05-01 | 2013-05-21 | 2013-06-20 | 2013-08-04 | 2013-09-03 | 2013-09-04  
Calculating: 2012-09-05  
Calculating: 2012-09-06  
Calculating: 2012-09-07  
Calculating: 2012-09-08  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
29 | 2013-04-20 | 2013-05-10 | 2013-06-09 | 2013-08-03 | 2013-09-07 | 2013-09-08  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
42 | 2013-05-20 | 2013-06-09 | 2013-07-09 | 2013-08-18 | 2013-09-07 | 2013-09-08  
Calculating: 2012-09-09  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
6 | 2013-05-01 | 2013-05-26 | 2013-06-30 | 2013-08-14 | 2013-09-08 | 2013-09-09  
Calculating: 2012-09-10  
Calculating: 2012-09-11  
Calculating: 2012-09-12  
Calculating: 2012-09-13  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
12 | 2013-05-05 | 2013-05-25 | 2013-07-14 | 2013-08-23 | 2013-09-12 | 2013-09-13  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
49 | 2013-04-15 | 2013-04-30 | 2013-05-25 | 2013-08-03 | 2013-09-12 | 2013-09-13  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
205 | 2013-04-15 | 2013-05-15 | 2013-06-24 | 2013-08-23 | 2013-09-12 | 2013-09-13  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
206 | 2013-04-15 | 2013-05-15 | 2013-06-24 | 2013-08-23 | 2013-09-12 | 2013-09-13  
Calculating: 2012-09-14  
Calculating: 2012-09-15  
Calculating: 2012-09-16  
Calculating: 2012-09-17  
Calculating: 2012-09-18  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
43 | 2013-05-10 | 2013-05-30 | 2013-08-08 | 2013-09-07 | 2013-09-17 | 2013-09-18  
Calculating: 2012-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
1 | 2013-05-01 | 2013-05-21 | 2013-07-10 | 2013-08-19 | 2013-09-18 | 2013-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
70 | 2013-05-01 | 2013-05-11 | 2013-06-10 | 2013-08-29 | 2013-09-18 | 2013-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
182 | 2013-05-01 | 2013-05-11 | 2013-06-10 | 2013-08-29 | 2013-09-18 | 2013-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
190 | 2013-05-01 | 2013-05-11 | 2013-06-10 | 2013-08-29 | 2013-09-18 | 2013-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
195 | 2013-05-01 | 2013-05-11 | 2013-06-10 | 2013-08-29 | 2013-09-18 | 2013-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
225 | 2013-05-01 | 2013-05-11 | 2013-06-10 | 2013-08-29 | 2013-09-18 | 2013-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
226 | 2013-05-01 | 2013-05-11 | 2013-06-10 | 2013-08-29 | 2013-09-18 | 2013-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
241 | 2013-05-01 | 2013-05-31 | 2013-07-20 | 2013-08-29 | 2013-09-18 | 2013-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
250 | 2013-05-01 | 2013-05-11 | 2013-06-10 | 2013-08-29 | 2013-09-18 | 2013-09-19  
Calculating: 2012-09-20  
Calculating: 2012-09-21  
Calculating: 2012-09-22  
Calculating: 2012-09-23  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
36 | 2013-04-15 | 2013-04-25 | 2013-05-25 | 2013-08-23 | 2013-09-22 | 2013-09-23  
Calculating: 2012-09-24  
Calculating: 2012-09-25  
Calculating: 2012-09-26  
Calculating: 2012-09-27  
Calculating: 2012-09-28  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
26 | 2013-05-15 | 2013-06-04 | 2013-07-04 | 2013-09-02 | 2013-09-27 | 2013-09-28  
Calculating: 2012-09-29  
Calculating: 2012-09-30  
Calculating: 2012-10-01  
Calculating: 2012-10-02  
Calculating: 2012-10-03  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
5 | 2013-05-20 | 2013-06-09 | 2013-07-09 | 2013-09-07 | 2013-10-02 | 2013-10-03  
Calculating: 2012-10-04  
Calculating: 2012-10-05  
Calculating: 2012-10-06  
Calculating: 2012-10-07  
Calculating: 2012-10-08  
Calculating: 2012-10-09  
Calculating: 2012-10-10  
Calculating: 2012-10-11  
Calculating: 2012-10-12  
Calculating: 2012-10-13  
Calculating: 2012-10-14  
Calculating: 2012-10-15  
Calculating: 2012-10-16  
Calculating: 2012-10-17  
Calculating: 2012-10-18  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
21 | 2013-04-20 | 2013-05-30 | 2013-07-09 | 2013-08-28 | 2013-10-17 | 2013-10-18  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
41 | 2013-04-20 | 2013-06-09 | 2013-07-19 | 2013-09-07 | 2013-10-17 | 2013-10-18  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
242 | 2013-04-20 | 2013-05-10 | 2013-06-29 | 2013-09-27 | 2013-10-17 | 2013-10-18  
Calculating: 2012-10-19  
Calculating: 2012-10-20  
Calculating: 2012-10-21  
Calculating: 2012-10-22  
Calculating: 2012-10-23  
Calculating: 2012-10-24  
Calculating: 2012-10-25  
Calculating: 2012-10-26  
Calculating: 2012-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
57 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
58 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
59 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
61 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
92 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
111 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
121 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
122 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
123 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
124 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
131 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
141 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
142 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
143 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
151 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
152 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
251 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
252 | 2013-03-30 | 2013-04-19 | 2013-06-28 | 2013-09-26 | 2013-10-26 | 2013-10-27  
Calculating: 2012-10-28  
Calculating: 2012-10-29  
Calculating: 2012-10-30  
Calculating: 2012-10-31  
Calculating: 2012-11-01  
Calculating: 2012-11-02  
Calculating: 2012-11-03  
Calculating: 2012-11-04  
Calculating: 2012-11-05  
Calculating: 2012-11-06  
Calculating: 2012-11-07  
Calculating: 2012-11-08  
Calculating: 2012-11-09  
Calculating: 2012-11-10  
Calculating: 2012-11-11  
Calculating: 2012-11-12  
Calculating: 2012-11-13  
Calculating: 2012-11-14  
Calculating: 2012-11-15  
Calculating: 2012-11-16  
Calculating: 2012-11-17  
Calculating: 2012-11-18  
Calculating: 2012-11-19  
Calculating: 2012-11-20  
Calculating: 2012-11-21  
Calculating: 2012-11-22  
Calculating: 2012-11-23  
Calculating: 2012-11-24  
Calculating: 2012-11-25  
Calculating: 2012-11-26  
Calculating: 2012-11-27  
Calculating: 2012-11-28  
Calculating: 2012-11-29  
Calculating: 2012-11-30  
Calculating: 2012-12-01  
Calculating: 2012-12-02  
Calculating: 2012-12-03  
Calculating: 2012-12-04  
Calculating: 2012-12-05  
Calculating: 2012-12-06  
Calculating: 2012-12-07  
Calculating: 2012-12-08  
Calculating: 2012-12-09  
Calculating: 2012-12-10  
Calculating: 2012-12-11  
Calculating: 2012-12-12  
Calculating: 2012-12-13  
Calculating: 2012-12-14  
Calculating: 2012-12-15  
Calculating: 2012-12-16  
Calculating: 2012-12-17  
Calculating: 2012-12-18  
Calculating: 2012-12-19  
Calculating: 2012-12-20  
Calculating: 2012-12-21  
Calculating: 2012-12-22  
Calculating: 2012-12-23  
Calculating: 2012-12-24  
Calculating: 2012-12-25  
Calculating: 2012-12-26  
Calculating: 2012-12-27  
Calculating: 2012-12-28  
Calculating: 2012-12-29  
Calculating: 2012-12-30  
Calculating: 2012-12-31  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/prcp_Daymet_v3_2012.nc"  
============================================================  
Missing day found in NetCDF file - padding values  
============================================================  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/tmin_Daymet_v3_2012.nc"  
============================================================  
Missing day found in NetCDF file - padding values  
============================================================  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/tmax_Daymet_v3_2012.nc"  
============================================================  
Missing day found in NetCDF file - padding values  
============================================================  
Calculating: 2013-01-01  
Attempting to open READONLY netCDF file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/prcp_Daymet_v3_2013.nc"  
Succeeded.  ncid: 1441792  format: NC_FORMAT_CLASSIC  
Attempting to open READONLY netCDF file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/tmin_Daymet_v3_2013.nc"  
Succeeded.  ncid: 1507328  format: NC_FORMAT_CLASSIC  
Attempting to open READONLY netCDF file: "/Users/hellyj/src/swb2-jjh/test/test_data/cs/tmax_Daymet_v3_2013.nc"  
Succeeded.  ncid: 1572864  format: NC_FORMAT_CLASSIC  
Calculating: 2013-01-02  
Calculating: 2013-01-03  
Calculating: 2013-01-04  
Calculating: 2013-01-05  
Calculating: 2013-01-06  
Calculating: 2013-01-07  
Calculating: 2013-01-08  
Calculating: 2013-01-09  
Calculating: 2013-01-10  
Calculating: 2013-01-11  
Calculating: 2013-01-12  
Calculating: 2013-01-13  
Calculating: 2013-01-14  
Calculating: 2013-01-15  
Calculating: 2013-01-16  
Calculating: 2013-01-17  
Calculating: 2013-01-18  
Calculating: 2013-01-19  
Calculating: 2013-01-20  
Calculating: 2013-01-21  
Calculating: 2013-01-22  
Calculating: 2013-01-23  
Calculating: 2013-01-24  
Calculating: 2013-01-25  
Calculating: 2013-01-26  
Calculating: 2013-01-27  
Calculating: 2013-01-28  
Calculating: 2013-01-29  
Calculating: 2013-01-30  
Calculating: 2013-01-31  
Calculating: 2013-02-01  
Calculating: 2013-02-02  
Calculating: 2013-02-03  
Calculating: 2013-02-04  
Calculating: 2013-02-05  
Calculating: 2013-02-06  
Calculating: 2013-02-07  
Calculating: 2013-02-08  
Calculating: 2013-02-09  
Calculating: 2013-02-10  
Calculating: 2013-02-11  
Calculating: 2013-02-12  
Calculating: 2013-02-13  
Calculating: 2013-02-14  
Calculating: 2013-02-15  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
207 | 2013-02-15 | 2013-05-16 | 2013-06-15 | 2014-01-01 | 2014-02-15 | 2014-02-16  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
221 | 2013-02-15 | 2013-05-16 | 2013-06-15 | 2014-01-01 | 2014-02-15 | 2014-02-16  
Calculating: 2013-02-16  
Calculating: 2013-02-17  
Calculating: 2013-02-18  
Calculating: 2013-02-19  
Calculating: 2013-02-20  
Calculating: 2013-02-21  
Calculating: 2013-02-22  
Calculating: 2013-02-23  
Calculating: 2013-02-24  
Calculating: 2013-02-25  
Calculating: 2013-02-26  
Calculating: 2013-02-27  
Calculating: 2013-02-28  
Calculating: 2013-03-01  
Calculating: 2013-03-02  
Calculating: 2013-03-03  
Calculating: 2013-03-04  
Calculating: 2013-03-05  
Calculating: 2013-03-06  
Calculating: 2013-03-07  
Calculating: 2013-03-08  
Calculating: 2013-03-09  
Calculating: 2013-03-10  
Calculating: 2013-03-11  
Calculating: 2013-03-12  
Calculating: 2013-03-13  
Calculating: 2013-03-14  
Calculating: 2013-03-15  
Calculating: 2013-03-16  
Calculating: 2013-03-17  
Calculating: 2013-03-18  
Calculating: 2013-03-19  
Calculating: 2013-03-20  
Calculating: 2013-03-21  
Calculating: 2013-03-22  
Calculating: 2013-03-23  
Calculating: 2013-03-24  
Calculating: 2013-03-25  
Calculating: 2013-03-26  
Calculating: 2013-03-27  
Calculating: 2013-03-28  
Calculating: 2013-03-29  
Calculating: 2013-03-30  
Calculating: 2013-03-31  
Calculating: 2013-04-01  
Calculating: 2013-04-02  
Calculating: 2013-04-03  
Calculating: 2013-04-04  
Calculating: 2013-04-05  
Calculating: 2013-04-06  
Calculating: 2013-04-07  
Calculating: 2013-04-08  
Calculating: 2013-04-09  
Calculating: 2013-04-10  
Calculating: 2013-04-11  
Calculating: 2013-04-12  
Calculating: 2013-04-13  
Calculating: 2013-04-14  
Calculating: 2013-04-15  
Calculating: 2013-04-16  
Calculating: 2013-04-17  
Calculating: 2013-04-18  
Calculating: 2013-04-19  
Calculating: 2013-04-20  
Calculating: 2013-04-21  
Calculating: 2013-04-22  
Calculating: 2013-04-23  
Calculating: 2013-04-24  
Calculating: 2013-04-25  
Calculating: 2013-04-26  
Calculating: 2013-04-27  
Calculating: 2013-04-28  
Calculating: 2013-04-29  
Calculating: 2013-04-30  
Calculating: 2013-05-01  
Calculating: 2013-05-02  
Calculating: 2013-05-03  
Calculating: 2013-05-04  
Calculating: 2013-05-05  
Calculating: 2013-05-06  
Calculating: 2013-05-07  
Calculating: 2013-05-08  
Calculating: 2013-05-09  
Calculating: 2013-05-10  
Calculating: 2013-05-11  
Calculating: 2013-05-12  
Calculating: 2013-05-13  
Calculating: 2013-05-14  
Calculating: 2013-05-15  
Calculating: 2013-05-16  
Calculating: 2013-05-17  
Calculating: 2013-05-18  
Calculating: 2013-05-19  
Calculating: 2013-05-20  
Calculating: 2013-05-21  
Calculating: 2013-05-22  
Calculating: 2013-05-23  
Calculating: 2013-05-24  
Calculating: 2013-05-25  
Calculating: 2013-05-26  
Calculating: 2013-05-27  
Calculating: 2013-05-28  
Calculating: 2013-05-29  
Calculating: 2013-05-30  
Calculating: 2013-05-31  
Calculating: 2013-06-01  
Calculating: 2013-06-02  
Calculating: 2013-06-03  
Calculating: 2013-06-04  
Calculating: 2013-06-05  
Calculating: 2013-06-06  
Calculating: 2013-06-07  
Calculating: 2013-06-08  
Calculating: 2013-06-09  
Calculating: 2013-06-10  
Calculating: 2013-06-11  
Calculating: 2013-06-12  
Calculating: 2013-06-13  
Calculating: 2013-06-14  
Calculating: 2013-06-15  
Calculating: 2013-06-16  
Calculating: 2013-06-17  
Calculating: 2013-06-18  
Calculating: 2013-06-19  
Calculating: 2013-06-20  
Calculating: 2013-06-21  
Calculating: 2013-06-22  
Calculating: 2013-06-23  
Calculating: 2013-06-24  
Calculating: 2013-06-25  
Calculating: 2013-06-26  
Calculating: 2013-06-27  
Calculating: 2013-06-28  
Calculating: 2013-06-29  
Calculating: 2013-06-30  
Calculating: 2013-07-01  
Calculating: 2013-07-02  
Calculating: 2013-07-03  
Calculating: 2013-07-04  
Calculating: 2013-07-05  
Calculating: 2013-07-06  
Calculating: 2013-07-07  
Calculating: 2013-07-08  
Calculating: 2013-07-09  
Calculating: 2013-07-10  
Calculating: 2013-07-11  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
243 | 2014-04-20 | 2014-05-10 | 2014-06-09 | 2014-06-29 | 2014-07-09 | 2014-07-10  
Calculating: 2013-07-12  
Calculating: 2013-07-13  
Calculating: 2013-07-14  
Calculating: 2013-07-15  
Calculating: 2013-07-16  
Calculating: 2013-07-17  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
37 | 2014-05-01 | 2014-05-11 | 2014-06-10 | 2014-07-05 | 2014-07-15 | 2014-07-16  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
38 | 2014-05-01 | 2014-05-11 | 2014-06-10 | 2014-07-05 | 2014-07-15 | 2014-07-16  
Calculating: 2013-07-18  
Calculating: 2013-07-19  
Calculating: 2013-07-20  
Calculating: 2013-07-21  
Calculating: 2013-07-22  
Calculating: 2013-07-23  
Calculating: 2013-07-24  
Calculating: 2013-07-25  
Calculating: 2013-07-26  
Calculating: 2013-07-27  
Calculating: 2013-07-28  
Calculating: 2013-07-29  
Calculating: 2013-07-30  
Calculating: 2013-07-31  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
171 | 2014-04-15 | 2014-04-25 | 2014-05-20 | 2014-06-24 | 2014-07-29 | 2014-07-30  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
181 | 2014-04-15 | 2014-04-25 | 2014-05-20 | 2014-06-24 | 2014-07-29 | 2014-07-30  
Calculating: 2013-08-01  
Calculating: 2013-08-02  
Calculating: 2013-08-03  
Calculating: 2013-08-04  
Calculating: 2013-08-05  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
53 | 2014-04-15 | 2014-05-20 | 2014-06-14 | 2014-07-14 | 2014-08-03 | 2014-08-04  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
176 | 2014-04-15 | 2014-04-25 | 2014-05-25 | 2014-06-29 | 2014-08-03 | 2014-08-04  
Calculating: 2013-08-06  
Calculating: 2013-08-07  
Calculating: 2013-08-08  
Calculating: 2013-08-09  
Calculating: 2013-08-10  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
50 | 2014-04-20 | 2014-05-25 | 2014-06-19 | 2014-07-19 | 2014-08-08 | 2014-08-09  
Calculating: 2013-08-11  
Calculating: 2013-08-12  
Calculating: 2013-08-13  
Calculating: 2013-08-14  
Calculating: 2013-08-15  
Calculating: 2013-08-16  
Calculating: 2013-08-17  
Calculating: 2013-08-18  
Calculating: 2013-08-19  
Calculating: 2013-08-20  
Calculating: 2013-08-21  
Calculating: 2013-08-22  
Calculating: 2013-08-23  
Calculating: 2013-08-24  
Calculating: 2013-08-25  
Calculating: 2013-08-26  
Calculating: 2013-08-27  
Calculating: 2013-08-28  
Calculating: 2013-08-29  
Calculating: 2013-08-30  
Calculating: 2013-08-31  
Calculating: 2013-09-01  
Calculating: 2013-09-02  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
24 | 2013-10-01 | 2014-03-10 | 2014-05-24 | 2014-08-07 | 2014-09-01 | 2014-09-02  
Calculating: 2013-09-03  
Calculating: 2013-09-04  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
23 | 2014-04-20 | 2014-05-10 | 2014-06-04 | 2014-08-03 | 2014-09-02 | 2014-09-03  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
27 | 2014-04-20 | 2014-05-10 | 2014-06-04 | 2014-08-03 | 2014-09-02 | 2014-09-03  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
28 | 2014-04-20 | 2014-05-10 | 2014-06-04 | 2014-08-03 | 2014-09-02 | 2014-09-03  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
30 | 2014-04-20 | 2014-05-10 | 2014-06-04 | 2014-08-03 | 2014-09-02 | 2014-09-03  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
32 | 2014-04-20 | 2014-05-10 | 2014-06-04 | 2014-08-03 | 2014-09-02 | 2014-09-03  
Calculating: 2013-09-05  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
4 | 2014-05-01 | 2014-05-21 | 2014-06-25 | 2014-08-04 | 2014-09-03 | 2014-09-04  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
47 | 2014-05-01 | 2014-05-21 | 2014-06-20 | 2014-08-04 | 2014-09-03 | 2014-09-04  
Calculating: 2013-09-06  
Calculating: 2013-09-07  
Calculating: 2013-09-08  
Calculating: 2013-09-09  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
29 | 2014-04-20 | 2014-05-10 | 2014-06-09 | 2014-08-03 | 2014-09-07 | 2014-09-08  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
42 | 2014-05-20 | 2014-06-09 | 2014-07-09 | 2014-08-18 | 2014-09-07 | 2014-09-08  
Calculating: 2013-09-10  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
6 | 2014-05-01 | 2014-05-26 | 2014-06-30 | 2014-08-14 | 2014-09-08 | 2014-09-09  
Calculating: 2013-09-11  
Calculating: 2013-09-12  
Calculating: 2013-09-13  
Calculating: 2013-09-14  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
12 | 2014-05-05 | 2014-05-25 | 2014-07-14 | 2014-08-23 | 2014-09-12 | 2014-09-13  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
49 | 2014-04-15 | 2014-04-30 | 2014-05-25 | 2014-08-03 | 2014-09-12 | 2014-09-13  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
205 | 2014-04-15 | 2014-05-15 | 2014-06-24 | 2014-08-23 | 2014-09-12 | 2014-09-13  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
206 | 2014-04-15 | 2014-05-15 | 2014-06-24 | 2014-08-23 | 2014-09-12 | 2014-09-13  
Calculating: 2013-09-15  
Calculating: 2013-09-16  
Calculating: 2013-09-17  
Calculating: 2013-09-18  
Calculating: 2013-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
43 | 2014-05-10 | 2014-05-30 | 2014-08-08 | 2014-09-07 | 2014-09-17 | 2014-09-18  
Calculating: 2013-09-20  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
1 | 2014-05-01 | 2014-05-21 | 2014-07-10 | 2014-08-19 | 2014-09-18 | 2014-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
70 | 2014-05-01 | 2014-05-11 | 2014-06-10 | 2014-08-29 | 2014-09-18 | 2014-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
182 | 2014-05-01 | 2014-05-11 | 2014-06-10 | 2014-08-29 | 2014-09-18 | 2014-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
190 | 2014-05-01 | 2014-05-11 | 2014-06-10 | 2014-08-29 | 2014-09-18 | 2014-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
195 | 2014-05-01 | 2014-05-11 | 2014-06-10 | 2014-08-29 | 2014-09-18 | 2014-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
225 | 2014-05-01 | 2014-05-11 | 2014-06-10 | 2014-08-29 | 2014-09-18 | 2014-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
226 | 2014-05-01 | 2014-05-11 | 2014-06-10 | 2014-08-29 | 2014-09-18 | 2014-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
241 | 2014-05-01 | 2014-05-31 | 2014-07-20 | 2014-08-29 | 2014-09-18 | 2014-09-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
250 | 2014-05-01 | 2014-05-11 | 2014-06-10 | 2014-08-29 | 2014-09-18 | 2014-09-19  
Calculating: 2013-09-21  
Calculating: 2013-09-22  
Calculating: 2013-09-23  
Calculating: 2013-09-24  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
36 | 2014-04-15 | 2014-04-25 | 2014-05-25 | 2014-08-23 | 2014-09-22 | 2014-09-23  
Calculating: 2013-09-25  
Calculating: 2013-09-26  
Calculating: 2013-09-27  
Calculating: 2013-09-28  
Calculating: 2013-09-29  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
26 | 2014-05-15 | 2014-06-04 | 2014-07-04 | 2014-09-02 | 2014-09-27 | 2014-09-28  
Calculating: 2013-09-30  
Calculating: 2013-10-01  
Calculating: 2013-10-02  
Calculating: 2013-10-03  
Calculating: 2013-10-04  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
5 | 2014-05-20 | 2014-06-09 | 2014-07-09 | 2014-09-07 | 2014-10-02 | 2014-10-03  
Calculating: 2013-10-05  
Calculating: 2013-10-06  
Calculating: 2013-10-07  
Calculating: 2013-10-08  
Calculating: 2013-10-09  
Calculating: 2013-10-10  
Calculating: 2013-10-11  
Calculating: 2013-10-12  
Calculating: 2013-10-13  
Calculating: 2013-10-14  
Calculating: 2013-10-15  
Calculating: 2013-10-16  
Calculating: 2013-10-17  
Calculating: 2013-10-18  
Calculating: 2013-10-19  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
21 | 2014-04-20 | 2014-05-30 | 2014-07-09 | 2014-08-28 | 2014-10-17 | 2014-10-18  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
41 | 2014-04-20 | 2014-06-09 | 2014-07-19 | 2014-09-07 | 2014-10-17 | 2014-10-18  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
242 | 2014-04-20 | 2014-05-10 | 2014-06-29 | 2014-09-27 | 2014-10-17 | 2014-10-18  
Calculating: 2013-10-20  
Calculating: 2013-10-21  
Calculating: 2013-10-22  
Calculating: 2013-10-23  
Calculating: 2013-10-24  
Calculating: 2013-10-25  
Calculating: 2013-10-26  
Calculating: 2013-10-27  
Calculating: 2013-10-28  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
57 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
58 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
59 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
61 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
92 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
111 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
121 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
122 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
123 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
124 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
131 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
141 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
142 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
143 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
151 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
152 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
251 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
## Updating Kcb Date Values ##  

Landuse Code | Planting Date | End of 'ini' | End of 'dev' | End of 'mid' | End of 'late' | End of 'fallow'  
-------------|---------------|--------------|--------------|--------------|---------------|-----------------  
252 | 2014-03-30 | 2014-04-19 | 2014-06-28 | 2014-09-26 | 2014-10-26 | 2014-10-27  
Calculating: 2013-10-29  
Calculating: 2013-10-30  
Calculating: 2013-10-31  
Calculating: 2013-11-01  
Calculating: 2013-11-02  
Calculating: 2013-11-03  
Calculating: 2013-11-04  
Calculating: 2013-11-05  
Calculating: 2013-11-06  
Calculating: 2013-11-07  
Calculating: 2013-11-08  
Calculating: 2013-11-09  
Calculating: 2013-11-10  
Calculating: 2013-11-11  
Calculating: 2013-11-12  
Calculating: 2013-11-13  
Calculating: 2013-11-14  
Calculating: 2013-11-15  
Calculating: 2013-11-16  
Calculating: 2013-11-17  
Calculating: 2013-11-18  
Calculating: 2013-11-19  
Calculating: 2013-11-20  
Calculating: 2013-11-21  
Calculating: 2013-11-22  
Calculating: 2013-11-23  
Calculating: 2013-11-24  
Calculating: 2013-11-25  
Calculating: 2013-11-26  
Calculating: 2013-11-27  
Calculating: 2013-11-28  
Calculating: 2013-11-29  
Calculating: 2013-11-30  
Calculating: 2013-12-01  
Calculating: 2013-12-02  
Calculating: 2013-12-03  
Calculating: 2013-12-04  
Calculating: 2013-12-05  
Calculating: 2013-12-06  
Calculating: 2013-12-07  
Calculating: 2013-12-08  
Calculating: 2013-12-09  
Calculating: 2013-12-10  
Calculating: 2013-12-11  
Calculating: 2013-12-12  
Calculating: 2013-12-13  
Calculating: 2013-12-14  
Calculating: 2013-12-15  
Calculating: 2013-12-16  
Calculating: 2013-12-17  
Calculating: 2013-12-18  
Calculating: 2013-12-19  
Calculating: 2013-12-20  
Calculating: 2013-12-21  
Calculating: 2013-12-22  
Calculating: 2013-12-23  
Calculating: 2013-12-24  
Calculating: 2013-12-25  
Calculating: 2013-12-26  
Calculating: 2013-12-27  
Calculating: 2013-12-28  
Calculating: 2013-12-29  
Calculating: 2013-12-30  
Calculating: 2013-12-31  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_gross_precipitation__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_rainfall__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_snowfall__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_interception__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_runon__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_runoff__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_soil_storage__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_delta_soil_storage__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_reference_ET0__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_actual_et__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_snowmelt__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_tmin__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_tmax__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_net_infiltration__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_rejected_net_infiltration__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_irrigation__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_runoff_outside__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_crop_et__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_bare_soil_evaporation__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_growing_degree_day__2012-01-01_to_2013-12-31__346_by_400.nc"  
Closing netCDF file with name: "/Users/hellyj/src/swb2-jjh/test/regression_tests/cs/swb2/output/central_sands_growing_season__2012-01-01_to_2013-12-31__346_by_400.nc"  

Time spent running simulation: 1 minutes, 50 seconds.  
Total runtime                : 1 minutes, 53 seconds.  

