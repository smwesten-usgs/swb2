module model_initialize

  use iso_c_binding, only                : c_int, c_float, c_double, c_bool
  use constants_and_conversions
  use datetime
  use data_catalog
  use data_catalog_entry
  use dictionary
  use exceptions
  use file_operations
  use grid
!  use loop_iterate
  use logfiles
  use model_domain, only                 : MODEL,                                        &
                                           minmaxmean,                                   &
                                           initialize_landuse_codes,                     &
                                           read_landuse_codes

  use output, only                       : initialize_output, set_output_directory,      &
                                           set_output_prefix, set_output_latlon_option
  use parameters
  use precipitation__method_of_fragments
  use simulation_datetime, only          : SIM_DT
  use strings
  use string_list
  use storm_drain_capture, only          : storm_drain_capture_initialize
  implicit none

  private

  public :: read_control_file, write_control_file,initialize_all
  public :: check_for_fatal_warnings

  type GRIDDED_DATASETS_T
    character (len=38)     :: sName
    logical (kind=c_bool)  :: lOptional
    integer (kind=c_int)   :: iDataType
  end type GRIDDED_DATASETS_T

  type METHODS_LIST_T
    character (len=23)     :: sName
    logical (kind=c_bool)  :: lOptional
  end type METHODS_LIST_T

  integer (kind=c_int), parameter :: NUMBER_OF_KNOWN_GRIDS   = 41
  integer (kind=c_int), parameter :: NUMBER_OF_KNOWN_METHODS = 17

  type (GRIDDED_DATASETS_T), parameter  :: KNOWN_GRIDS( NUMBER_OF_KNOWN_GRIDS ) =       &

    [ GRIDDED_DATASETS_T("PRECIPITATION                         ", lFALSE, DATATYPE_FLOAT ),     &
      GRIDDED_DATASETS_T("TMIN                                  ", lFALSE, DATATYPE_FLOAT ),     &
      GRIDDED_DATASETS_T("TMAX                                  ", lFALSE, DATATYPE_FLOAT ),     &
      GRIDDED_DATASETS_T("AVAILABLE_WATER_CONTENT               ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("REFERENCE_ET0                         ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("POTENTIAL_ET                          ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("SOLAR_RADIATION                       ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("WIND_SPEED                            ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("RAINFALL_ZONE                         ", lTRUE, DATATYPE_INT ),        &
      GRIDDED_DATASETS_T("FLOW_DIRECTION                        ", lTRUE, DATATYPE_INT),         &
      GRIDDED_DATASETS_T("FOG_RATIO                             ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("LAND_USE                              ", lFALSE, DATATYPE_INT ),       &
      GRIDDED_DATASETS_T("SOILS_CODE                            ", lTRUE, DATATYPE_INT ),        &
      GRIDDED_DATASETS_T("HYDROLOGIC_SOILS_GROUP                ", lFALSE, DATATYPE_INT ),       &
      GRIDDED_DATASETS_T("INITIAL_PERCENT_SOIL_MOISTURE         ", lFALSE, DATATYPE_FLOAT),      &
      GRIDDED_DATASETS_T("INITIAL_SNOW_COVER_STORAGE            ", lTRUE, DATATYPE_FLOAT),       &
      GRIDDED_DATASETS_T("INITIAL_CONTINUOUS_FROZEN_GROUND_INDEX", lTRUE, DATATYPE_FLOAT),       &
      GRIDDED_DATASETS_T("PERCENT_CANOPY_COVER                  ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("PERCENT_PERVIOUS_COVER                ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("PERCENT_IMPERVIOUS_COVER              ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("FRACTION_CANOPY_COVER                 ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("FRACTION_PERVIOUS_COVER               ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("FRACTION_IMPERVIOUS_COVER             ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("STEMFLOW_FRACTION                     ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("EVAPORATION_TO_RAINFALL_RATIO         ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("RAINFALL_ADJUST_FACTOR                ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("CESSPOOL_LEAKAGE                      ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("STORM_DRAIN_LEAKAGE                   ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("WATER_BODY_LEAKAGE                    ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("WATER_MAIN_LEAKAGE                    ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("DISPOSAL_WELL_DISCHARGE               ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("ANNUAL_DIRECT_NET_INFILTRATION_RATE   ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("ANNUAL_SEPTIC_DISCHARGE               ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("SEPTIC_DISCHARGE                      ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("RUNOFF_ZONE                           ", lTRUE, DATATYPE_INT ),        &
      GRIDDED_DATASETS_T("POLYGON_ID                            ", lTRUE, DATATYPE_INT ),        &
      GRIDDED_DATASETS_T("SOIL_STORAGE_MAX                      ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("PLANT_AVAILABLE_WATER                 ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("MAXIMUM_NET_INFILTRATION              ", lTRUE, DATATYPE_FLOAT ),      &
      GRIDDED_DATASETS_T("IRRIGATION_MASK                       ", lTRUE, DATATYPE_INT),         &
      GRIDDED_DATASETS_T("RELATIVE_HUMIDITY                     ", lTRUE, DATATYPE_FLOAT )   ]

  type (METHODS_LIST_T), parameter  :: KNOWN_METHODS( NUMBER_OF_KNOWN_METHODS ) =   &

    [ METHODS_LIST_T("INTERCEPTION           ", lFALSE),                            &
      METHODS_LIST_T("EVAPOTRANSPIRATION     ", lFALSE),                            &
      METHODS_LIST_T("RUNOFF                 ", lFALSE),                            &
      METHODS_LIST_T("PRECIPITATION          ", lFALSE),                            &
      METHODS_LIST_T("FOG                    ", lTRUE),                             &
      METHODS_LIST_T("AVAILABLE_WATER_CONTENT", lTRUE),                             &
      METHODS_LIST_T("SOIL_STORAGE_MAX       ", lTRUE),                             &
      METHODS_LIST_T("SOIL_MOISTURE          ", lFALSE),                            &
      METHODS_LIST_T("IRRIGATION             ", lTRUE),                             &
      METHODS_LIST_T("CROP_COEFFICIENT       ", lTRUE),                             &
      METHODS_LIST_T("GROWING_DEGREE_DAY     ", lTRUE),                             &
      METHODS_LIST_T("DYNAMIC_LANDUSE        ", lTRUE),                             &
      METHODS_LIST_T("DIRECT_NET_INFILTRATION", lTRUE),                             &
      METHODS_LIST_T("DIRECT_SOIL_MOISTURE   ", lTRUE),                             &
      METHODS_LIST_T("FLOW_ROUTING           ", lTRUE),                             &
      METHODS_LIST_T("ROOTING_DEPTH          ", lTRUE),                             &
      METHODS_LIST_T("DUMP_VARIABLES         ", lTRUE)   ]

  ! grid that will be used in the calculation of cell latitudes
  type (GENERAL_GRID_T), pointer    :: pCOORD_GRD

contains

  subroutine initialize_all( output_prefix, output_dirname, data_dirname,         &
                             weather_data_dirname )

!    use polygon_summarize, only : initialize_polygon_summarize

    character (len=*), intent(in) :: output_prefix, output_dirname, data_dirname, &
                                     weather_data_dirname

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    ! set output directory names for NetCDF and Surfer/Arc ASCII grid output
    call grid_set_output_directory_name( output_dirname )
    call set_data_directory( data_dirname )
    call set_weather_data_directory( weather_data_dirname )
    call set_output_directory( output_dirname )
    call MODEL%set_output_directory( output_dirname )
    call set_output_prefix( output_prefix )

    ! define SWB project boundary and geographic projection
    call initialize_grid_options()

    ! define the start and end date for the simulation
    call initialize_start_and_end_dates()

    ! read in and munge all tables that have been defined in the control file as ***_LOOKUP_TABLE
    call initialize_lookup_tables()

    ! scan input file entries for keywords associated with known gridded datasets
    ! (e.g. PRECIPITATION, TMIN, TMAX, FOG_ZONE, etc.)

    ! if the grid is mentioned in the control file, call the bound "initialize" method that
    ! will wire in the type of data file. all associated methods will also be acted upon if
    ! present in the control file (e.g. TMAX_ADD_OFFSET, TMAX_NETCDF_X_VAR, etc.)
    do iIndex = 1, ubound(KNOWN_GRIDS, 1)

      call initialize_generic_grid( sKey=KNOWN_GRIDS(iIndex)%sName,            &
                                    lOptional=KNOWN_GRIDS(iIndex)%lOptional,   &
                                    iDataType=KNOWN_GRIDS(iIndex)%iDataType )

    enddo

    ! scan the control file input for method specifications
    ! (e.g. EVAPOTRANSPIRATION_METHOD HARGREAVES-SAMANI )
    do iIndex = 1, ubound(KNOWN_METHODS, 1)

      call initialize_generic_method( sKey=KNOWN_METHODS( iIndex )%sName,              &
                                      lOptional=KNOWN_METHODS( iIndex )%lOptional )

    enddo

    ! check to see that there are no NULL method pointers
    call MODEL%preflight_check_method_pointers()

    ! bring in soils (HSG, AWC), landuse, and flow direction data from native grids
    ! and pack that data into vectors for active grid cells only
    call initialize_soils_landuse_awc_flowdir_values()


    call initialize_ancillary_values()

    ! call each of the initialization routines associated with the chosen methods
    call MODEL%initialize_methods()

    ! find general program options and process
    call initialize_program_options( )

    ! open and prepare NetCDF files for output
    call initialize_output( MODEL )

!    call initialize_polygon_summarize()

    ! if any fatal warnings have been issued to this point, this
    ! is the end of the simulation!
    call check_for_fatal_warnings()

    ! dump details about all gridded datasets currently in the data_catalog
    call DAT%print()

    ! dump details about all dictionary keys and values to the debug logfile
    call CF_DICT%print_all( iLogLevel=LOG_DEBUG, lEcho=FALSE )

  end subroutine initialize_all

!--------------------------------------------------------------------------------------------------

  !> Initialize soils, landuse, and available water content values.
  !!
  !! @NOTE Rather than read the raw values and initialize the SWB
  !! variables in one step, we have two steps:
  !! 1. read raw;
  !! 2. initialize SWB variables.
  !!
  !! Two steps are needed because the call to "set_inactive_cells" requires access to the "raw"
  !! values, not the SWB variables. Basically any cell with negative values found in the landuse, soils groups,
  !! or awc grids results in that cell being inactivated. Once the complete collection of active cells is
  !! found, the SWB variables can be initialized.
  !!

  subroutine initialize_soils_landuse_awc_flowdir_values()

    use model_domain, only : ROOTING_DEPTH_MAX

    call read_landuse_codes()

    call read_hydrologic_soil_groups()

    ! call to following results in the assembly of a table of maximum rooting
    ! depths by hydrologic soil group and landuse code; max rooting depths
    ! are also assigned to individual cells
    call MODEL%update_rooting_depth_table()

    ! read in the 2D array of AWC values; needed so that we can use it to set inactive cells
    call MODEL%read_AWC_data()

    ! determine which grid cells should be inactive on the basis of available water content,
    ! hudrologic soils group, and/or soils group grids
    call MODEL%set_inactive_cells()

    ! allocate and zero out model arrays
    call MODEL%initialize_arrays()

    ! create 1D vectors containing row-column indices of unpacked grid
    call MODEL%initialize_row_column_indices()

    ! read_polygon matches HWB polygon attributes with the corresponding gridcells in SWB
!    call read_polygon_id()

    ! now that we know which cells are active, pack the 2D array in to 1D vector of AWC values
    call MODEL%init_AWC()

    ! to this point, rooting depths
    MODEL%rooting_depth_max = pack( ROOTING_DEPTH_MAX, MODEL%active )

    ! if the method selected reads these directly from a grid, the rooting depths previously
    ! calculated will be overwritten and recalculated using the gridded max soil moisture and awc values
    call MODEL%init_soil_storage_max()

    call initialize_hydrologic_soil_groups()

    call initialize_landuse_codes()

  end subroutine initialize_soils_landuse_awc_flowdir_values

!--------------------------------------------------------------------------------------------------

  subroutine initialize_ancillary_values()

    call initialize_percent_pervious()

    call initialize_percent_canopy_cover()

    call initialize_latitude()

    call initialize_soil_storage()

    call initialize_snow_storage()

    call MODEL%init_continuous_frozen_ground_index()

    call initialize_surface_storage_max()

    call storm_drain_capture_initialize()

    call MODEL%initialize_growing_season()

  end subroutine initialize_ancillary_values

!--------------------------------------------------------------------------------------------------

  subroutine set_data_directory( data_dirname )

    character(len=*), intent(in) :: data_dirname

    ! setting the MODULE variable DATA_DIRECTORY_NAME, module = file_operations
    DATA_DIRECTORY_NAME = data_dirname

  end subroutine set_data_directory

  !--------------------------------------------------------------------------------------------------

  subroutine set_weather_data_directory( weather_data_dirname )

    character(len=*), intent(in) :: weather_data_dirname

    ! setting the MODULE variable WEATHER_DATA_DIRECTORY_NAME, module = XXXX
    !WEATHER_DATA_DIRECTORY_NAME = data_dirname

  end subroutine set_weather_data_directory

!--------------------------------------------------------------------------------------------------

  subroutine initialize_snow_storage()

    type (DATA_CATALOG_ENTRY_T), pointer :: pINITIAL_SNOW_COVER_STORAGE

    ! [ LOCALS ]
    real (kind=c_float), allocatable  :: fInitial_Snow_Cover_Storage(:)
    integer (kind=c_int)              :: iStat

    allocate ( fInitial_Snow_Cover_Storage( count( MODEL%active ) ), stat=iStat )

    ! locate the data structure associated with the gridded initial_snow_cover_storage
    pINITIAL_SNOW_COVER_STORAGE => DAT%find("INITIAL_SNOW_COVER_STORAGE")

    ! OK, now try the old SWB 1.0 syntax
    if ( .not. associated( pINITIAL_SNOW_COVER_STORAGE ) )   &
    ! locate the data structure associated with the gridded initial_snow_cover_storage
      pINITIAL_SNOW_COVER_STORAGE => DAT%find("INITIAL_SNOW_COVER")

    if ( .not. associated( pINITIAL_SNOW_COVER_STORAGE ) ) then
        call warn(sMessage="An INITIAL_SNOW_COVER_STORAGE grid (or constant) was not found.",    &
        sHints="Check your control file to see that a valid INITIAL_SNOW_COVER_STORAGE grid or"  &
          //" constant is specified.", lFatal=lFALSE )

      MODEL%snow_storage = 0.0_c_float

    else

      call pINITIAL_SNOW_COVER_STORAGE%getvalues()

      ! map the 2D array of INITIAL_PERCENT_SOIL_MOISTURE values to the vector of active cells
      fInitial_Snow_Cover_Storage = pack( pINITIAL_SNOW_COVER_STORAGE%pGrdBase%rData, MODEL%active )

     if ( minval( fInitial_Snow_Cover_Storage ) < fZERO &
        .or. maxval( fInitial_Snow_Cover_Storage ) > 300.0_c_float )  &
       call warn(sMessage="One or more initial snow cover storage values outside of " &
         //"valid range (0 to 300)", lFatal=lTRUE )

     MODEL%snow_storage = fInitial_Snow_Cover_Storage

    endif

  end subroutine initialize_snow_storage

!--------------------------------------------------------------------------------------------------

  subroutine initialize_soil_storage()

    type (DATA_CATALOG_ENTRY_T), pointer :: pINITIAL_PERCENT_SOIL_MOISTURE

    ! [ LOCALS ]
    real (kind=c_float), allocatable  :: fInitial_Percent_Soil_Moisture(:)
    integer (kind=c_int)              :: iStat

    allocate ( fInitial_Percent_Soil_Moisture( count( MODEL%active ) ), stat=iStat )

    ! locate the data structure associated with the gridded initial_percent_soil_moisture entries
    pINITIAL_PERCENT_SOIL_MOISTURE => DAT%find("INITIAL_PERCENT_SOIL_MOISTURE")

    ! OK, now try the old SWB 1.0 syntax
    if ( .not. associated( pINITIAL_PERCENT_SOIL_MOISTURE ) )   &
      pINITIAL_PERCENT_SOIL_MOISTURE => DAT%find("INITIAL_SOIL_MOISTURE")

    if ( .not. associated( pINITIAL_PERCENT_SOIL_MOISTURE ) ) then
        call warn(sMessage="An INITIAL_PERCENT_SOIL_MOISTURE grid (or constant) was not found.",    &
        sHints="Check your control file to see that a valid INITIAL_PERCENT_SOIL_MOISTURE grid or"  &
          //" constant is specified.", lFatal=lTRUE )
    else

      call pINITIAL_PERCENT_SOIL_MOISTURE%getvalues()

      ! map the 2D array of INITIAL_PERCENT_SOIL_MOISTURE values to the vector of active cells
      fInitial_Percent_Soil_Moisture = pack( pINITIAL_PERCENT_SOIL_MOISTURE%pGrdBase%rData, MODEL%active )

     if ( minval( fInitial_Percent_Soil_Moisture ) < fZERO &
        .or. maxval( fInitial_Percent_Soil_Moisture ) > 100.0_c_float )  &
       call warn(sMessage="One or more initial percent soils moisture values outside of " &
         //"valid range (0% to 100%)", lFatal=lTRUE )

     MODEL%soil_storage = fInitial_Percent_Soil_Moisture / 100.0_c_float * MODEL%soil_storage_max

    endif

  end subroutine initialize_soil_storage

!--------------------------------------------------------------------------------------------------

  subroutine initialize_percent_pervious()

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iIndex
    type (DATA_CATALOG_ENTRY_T), pointer :: pPERCENT_IMPERVIOUS
    type (DATA_CATALOG_ENTRY_T), pointer :: pPERCENT_PERVIOUS
    type (DATA_CATALOG_ENTRY_T), pointer :: pFRACTION_IMPERVIOUS
    type (DATA_CATALOG_ENTRY_T), pointer :: pFRACTION_PERVIOUS
    type ( GENERAL_GRID_T ), pointer     :: pTempGrd

    pPERCENT_IMPERVIOUS => DAT%find("PERCENT_IMPERVIOUS_COVER")
    pPERCENT_PERVIOUS => DAT%find("PERCENT_PERVIOUS_COVER")
    pFRACTION_IMPERVIOUS => DAT%find("FRACTION_IMPERVIOUS_COVER")
    pFRACTION_PERVIOUS => DAT%find("FRACTION_PERVIOUS_COVER")

    if ( associated(pPERCENT_IMPERVIOUS) ) then

      call pPERCENT_IMPERVIOUS%getvalues()

      if (associated( pPERCENT_IMPERVIOUS%pGrdBase) ) then
        MODEL%pervious_fraction = pack( 1.0_c_float - pPERCENT_IMPERVIOUS%pGrdBase%rData/100.0_c_float, MODEL%active )
      else
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)
      endif

    elseif ( associated( pPERCENT_PERVIOUS ) ) then

      call pPERCENT_PERVIOUS%getvalues()

      if (associated( pPERCENT_PERVIOUS%pGrdBase) ) then
        MODEL%pervious_fraction = pack( (pPERCENT_PERVIOUS%pGrdBase%rData/100.0_c_float), MODEL%active )
      else
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)
      endif

    elseif ( associated(pFRACTION_IMPERVIOUS) ) then

      call pFRACTION_IMPERVIOUS%getvalues()

      if (associated( pFRACTION_IMPERVIOUS%pGrdBase) ) then
        MODEL%pervious_fraction = pack( 1.0_c_float - pFRACTION_IMPERVIOUS%pGrdBase%rData, MODEL%active )
      else
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)
      endif

    elseif ( associated( pFRACTION_PERVIOUS ) ) then

      call pFRACTION_PERVIOUS%getvalues()

      if (associated( pFRACTION_PERVIOUS%pGrdBase) ) then
        MODEL%pervious_fraction = pack( pFRACTION_PERVIOUS%pGrdBase%rData, MODEL%active )
      else
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)
      endif

    else

      MODEL%pervious_fraction = 1.0_c_float

    endif

     if ( minval( MODEL%pervious_fraction ) < fZERO &
        .or. maxval( MODEL%pervious_fraction ) > 1.0_c_float )  &
       call warn(sMessage="One or more percent (im)pervious cover percent/fraction values are outside of " &
         //"valid range (0% to 100% or 0.0 to 1.0)", lFatal=lTRUE )

     if ( all( MODEL%pervious_fraction < 0.01_c_float ) ) &
       call warn(sMessage="All (im)pervious cover percent/fraction values are suspiciously low " &
         //"(less than 1% or less than 0.01)", lFatal=lTRUE,                                      &
         sHints="Check to see whether (im)pervious cover is expressed as a fraction (0.0-1.0)"   &
              //" or a percentage (0-100%)." )

    pTempGrd => grid_Create( iNX=MODEL%number_of_columns, iNY=MODEL%number_of_rows, &
        rX0=MODEL%X_ll, rY0=MODEL%Y_ll, &
        rGridCellSize=MODEL%gridcellsize, iDataType=GRID_DATATYPE_REAL )

    pTempGrd%rData = unpack( MODEL%pervious_fraction, MODEL%active, MODEL%nodata_fill_value )

    call grid_WriteArcGrid( sFilename="Fraction_pervious_surface__as_read_in_unitless.asc", pGrd=pTempGrd )

    call grid_Destroy( pTempGrd )

  end subroutine initialize_percent_pervious

!--------------------------------------------------------------------------------------------------

  subroutine initialize_percent_canopy_cover

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iIndex
    type (DATA_CATALOG_ENTRY_T), pointer :: pPERCENT_CANOPY_COVER
    type (DATA_CATALOG_ENTRY_T), pointer :: pFRACTION_CANOPY_COVER
    type ( GENERAL_GRID_T ), pointer     :: pTempGrd

    pPERCENT_CANOPY_COVER => DAT%find("PERCENT_CANOPY_COVER")
    pFRACTION_CANOPY_COVER => DAT%find("FRACTION_CANOPY_COVER")

    if ( associated(pPERCENT_CANOPY_COVER) ) then

      call pPERCENT_CANOPY_COVER%getvalues()

      if (associated( pPERCENT_CANOPY_COVER%pGrdBase) ) then
        MODEL%canopy_cover_fraction = pack( pPERCENT_CANOPY_COVER%pGrdBase%rData/100.0_c_float, MODEL%active )
      else
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)
      endif

    elseif ( associated(pFRACTION_CANOPY_COVER) ) then

      call pFRACTION_CANOPY_COVER%getvalues()

      if (associated( pFRACTION_CANOPY_COVER%pGrdBase) ) then
        MODEL%canopy_cover_fraction = pack( pFRACTION_CANOPY_COVER%pGrdBase%rData, MODEL%active )
      else
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)
      endif

    else

      MODEL%canopy_cover_fraction = 1.0_c_float

      call warn("Could not find a grid or constant value for the canopy cover fraction. Using a" &
        //" value of 1.0 for the entire model domain." )

    endif

    if ( minval( MODEL%canopy_cover_fraction ) < fZERO &
       .or. maxval( MODEL%canopy_cover_fraction ) > 1.0_c_float )  &
      call warn(sMessage="One or more percent canopy cover percent/fraction values values are outside of " &
        //"valid range (0% to 100% or 0.0 to 1.0)", lFatal=lTRUE )

     if ( all( MODEL%canopy_cover_fraction < 0.01_c_float ) )                               &
       call warn(sMessage="All canopy cover percent/fraction values are suspiciously low " &
         //"(less than 1% or less than 0.01)", lFatal=lTRUE,                                &
         sHints="Check to see whether canopy cover is expressed as a fraction (0.0-1.0)"   &
              //" or a percentage (0-100%)." )

    pTempGrd => grid_Create( iNX=MODEL%number_of_columns, iNY=MODEL%number_of_rows, &
        rX0=MODEL%X_ll, rY0=MODEL%Y_ll, &
        rGridCellSize=MODEL%gridcellsize, iDataType=GRID_DATATYPE_REAL )

    pTempGrd%rData = unpack( MODEL%canopy_cover_fraction, MODEL%active, MODEL%nodata_fill_value )

    call grid_WriteArcGrid( sFilename="Fraction_canopy_cover__as_read_in_unitless.asc", pGrd=pTempGrd )

    call grid_Destroy( pTempGrd )

  end subroutine initialize_percent_canopy_cover

!--------------------------------------------------------------------------------------------------

  subroutine initialize_hydrologic_soil_groups

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    integer (kind=c_int)                 :: iIndex
    type (DATA_CATALOG_ENTRY_T), pointer :: pHSG

    pHSG => DAT%find("HYDROLOGIC_SOILS_GROUP")

    if ( associated(pHSG) ) then

      if (associated( pHSG%pGrdBase) ) then
        MODEL%soil_group = pack( pHSG%pGrdBase%iData, MODEL%active )
      else
        call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)
      endif

    else

      call die("Attempted use of NULL pointer. Failed to find HYDROLOGIC_SOILS_GROUP data element.", &
        __SRCNAME__, __LINE__)

    endif

    call LOGS%write("Hydrologic soils groups as read into SWB data structure", iLinesBefore=1, iLinesAfter=1, iLogLevel=LOG_DEBUG)

    do iIndex = 1, maxval(pHSG%pGrdBase%iData)

      call LOGS%write( asCharacter(count(MODEL%soil_group == iIndex) )//" cells belong to soils group " &
        //asCharacter(iIndex), iLogLevel=LOG_DEBUG )

    end do

    call LOGS%write("", iLinesBefore=1, iLogLevel=LOG_DEBUG)

  end subroutine initialize_hydrologic_soil_groups

!--------------------------------------------------------------------------------------------------

  subroutine read_hydrologic_soil_groups

    ! [ LOCALS ]
    type (DATA_CATALOG_ENTRY_T), pointer :: pHSG

    pHSG => DAT%find("HYDROLOGIC_SOILS_GROUP")

    if ( associated(pHSG) ) then

      call pHSG%getvalues()
      call grid_WriteArcGrid("Hydrologic_soil_groups__as_read_into_SWB.asc", pHSG%pGrdBase )

    else

      call warn(sMessage="HYDROLOGIC_SOILS_GROUP dataset is flawed or missing.", lFatal=lTRUE,     &
        iLogLevel = LOG_ALL, sHints="Check to see that a valid path and filename have"  &
        //" been ~included in the control file for the HYDROLOGIC_SOILS_GROUP dataset.",           &
        lEcho = lTRUE )

    endif

  end subroutine read_hydrologic_soil_groups

!--------------------------------------------------------------------------------------------------

  subroutine read_polygon_id()



    ! [ LOCALS ]
    type (DATA_CATALOG_ENTRY_T), pointer :: pPOLYGON_ID
    logical (kind=c_bool)                :: any_problems
    type (STRING_LIST_T)                 :: slList
    integer (kind=c_int), allocatable    :: polygon_id(:)
    real (kind=c_float), allocatable     :: rooting_depth_inches(:)
    real (kind=c_float), allocatable     :: soil_moisture_storage(:)
    integer (kind=c_int)                 :: iNumberOfPolygonIDs
    type (GENERAL_GRID_T), pointer       :: pTempGrd
    integer (kind=c_int)                 :: index



    pTempGrd => grid_Create( iNX=MODEL%number_of_columns, iNY=MODEL%number_of_rows, &
        rX0=MODEL%X_ll, rY0=MODEL%Y_ll, &
        rGridCellSize=MODEL%gridcellsize, iDataType=GRID_DATATYPE_REAL )

    any_problems = lTRUE

    do

      pPOLYGON_ID => DAT%find("POLYGON_ID")

      if ( .not. associated( pPOLYGON_ID ) ) exit

      call pPOLYGON_ID%getvalues()
      call grid_WriteArcGrid("Polygon_ID__as_read_into_SWB.asc", pPOLYGON_ID%pGrdBase )

      MODEL%polygon_id = pack( pPOLYGON_ID%pGrdBase%iData, MODEL%active )

      exit

    enddo

!       call slList%append("POLY_ID")

!       !> Determine how many POLYGON_IDS are present
!       call PARAMS%get_parameters( slKeys=slList, iValues=polygon_id )
!       iNumberOfPolygonIDs = count( polygon_id > 0 )
!       call slList%clear()

!       call slList%append("rooting_depth_inches")

!       ! retrieve rooting depths, in inches
!       call PARAMS%get_parameters( slKeys=slList, fValues=rooting_depth_inches )
!       call slList%clear()

!       call slList%append("SMC")

!       ! retrieve soil moisture maximum content, in inches
!       call PARAMS%get_parameters( slKeys=slList, fValues=soil_moisture_storage )
!       call slList%clear()

!       if ( ubound( soil_moisture_storage, 1) == ubound( polygon_id, 1 ) )  any_problems = lFALSE

!       exit

!     enddo

!     print *, any_problems, ubound( polygon_id, 1), ubound( soil_moisture_storage, 1), ubound( rooting_depth_inches, 1 )
!     call minmaxmean( soil_moisture_storage, "Max_soil_storage")
!     call minmaxmean( rooting_depth_inches, "Rooting_depth_inches")

!     call assert( .not. any_problems, "One or more steps failed while processing POLYGON_ID.", &
!       __SRCNAME__, __LINE__ )

! !$OMP PARALLEL DO

!     do index=1, ubound( polygon_id, 1)

!       if ( mod( index, 10 ) == 0 ) print *, "Processing polygon number "//asCharacter(index) &
!           //" of "//asCharacter( ubound( polygon_id, 1) )

!       where ( MODEL%polygon_id == polygon_id( index ) )

!         MODEL%hwb_rooting_depth = rooting_depth_inches( MODEL%landuse_index( index ) ) / 12.0_c_float

!         MODEL%hwb_soil_storage_max = soil_moisture_storage( index )

!       end where

!     enddo

! !$OMP END PARALLEL DO

!     where ( MODEL%hwb_rooting_depth > 0.0_c_float )

!       MODEL%hwb_awc_in_per_ft = MODEL%hwb_soil_storage_max / MODEL%hwb_rooting_depth

!     end where

!     pTempGrd%rData = unpack( MODEL%hwb_rooting_depth, MODEL%active, MODEL%nodata_fill_value )

!     call grid_WriteArcGrid("Maximum_rooting_depth__as_read_in_from_HWB_input__feet.asc", pTempGrd )


!     pTempGrd%rData = unpack( MODEL%hwb_soil_storage_max, MODEL%active, MODEL%nodata_fill_value )

!     call grid_WriteArcGrid("Maximum_soil_storage__as_read_in_from_HWB_input__inches.asc", pTempGrd )


!     pTempGrd%rData = unpack( MODEL%hwb_awc_in_per_ft, MODEL%active, MODEL%nodata_fill_value )

!     call grid_WriteArcGrid("Available_water_content__as_read_in_from_HWB_input__inches_per_foot.asc", pTempGrd )

     call grid_Destroy( pTempGrd )

  end subroutine read_polygon_id

!--------------------------------------------------------------------------------------------------

!   subroutine read_landuse_codes
!
!     ! [ LOCALS ]
!     type (DATA_CATALOG_ENTRY_T), pointer :: pLULC
!
!     pLULC => DAT%find("LAND_USE")
!
!     if ( associated(pLULC) ) then
!
!       call pLULC%getvalues()
!       call grid_WriteArcGrid("Landuse_land_cover__as_read_into_SWB.asc", pLULC%pGrdBase )
!
!     else
!
!       call warn(sMessage="LAND_USE dataset is flawed or missing.", lFatal=lTRUE,         &
!         iLogLevel = LOG_ALL, sHints="Check to see that a valid path and filename have"   &
!         //" been ~included in the control file for the LAND_USE dataset.",               &
!         lEcho = lTRUE )
!
!     endif
!
!   end subroutine read_landuse_codes
!
! !--------------------------------------------------------------------------------------------------

  subroutine write_control_file( sFilename, sGridSpecification, slExtraDirectives )

    character (len=*), intent(in)              :: sFilename
    character (len=*), intent(in), optional    :: sGridSpecification
    type (STRING_LIST_T), intent(in), optional :: slExtraDirectives

    ! [ LOCALS ]
    character (len=256)             :: sRecord, sSubstring
    character (len=:), allocatable  :: sText
    integer (kind=c_int)            :: iStat
    integer (kind=c_int)            :: iIndex
    integer (kind=c_int)            :: iCount
    type (ASCII_FILE_T)             :: CF
    type (DICT_ENTRY_T), pointer    :: pDict

    pDict => null()

    call CF%open( sFilename = sFilename )

    ! get GRID specification and move pointer past it; discard values
    if ( present( sGridSpecification ) ) then
      call CF_DICT%get_value(sText, "GRID")
      pDict => CF_DICT%get_next_entry()
      call assert( associated( pDict ), "INTERNAL PROGRAMMING ERROR -- Attempted use of null poitner", &
        __SRCNAME__, __LINE__ )
      call CF%writeLine( trim( sGridSpecification ) )

    else

      pDict => CF_DICT%get_entry(1)

    endif

    do while ( associated( pDict ) )

      call CF_DICT%get_value( sText )
      call CF%writeLine( trim(pDict%key)//" "//sText )
      pDict => CF_DICT%get_next_entry()

    enddo

    if ( present( slExtraDirectives ) ) then

      iCount = slExtraDirectives%count

      do iIndex=1, iCount

        call CF%writeLine( trim( slExtraDirectives%get( iIndex ) ) )

      enddo

    endif

    call CF%close()

  end subroutine write_control_file


  subroutine read_control_file( sFilename )

    character (len=*), intent(in)    :: sFilename

    ! [ LOCALS ]
    character (len=256)   :: sRecord, sSubstring
    integer (kind=c_int)  :: iStat
    type (ASCII_FILE_T)   :: CF
    integer (kind=c_int)  :: dumpfile_count

    dumpfile_count = 0

    ! open the control file and define the comment characters and delimiters to be used in
    ! parsing the ASCII text
    call CF%open( sFilename = sFilename,            &
                  sCommentChars = "#%!+=|[{(-*$",   &
                  sDelimiters = "WHITESPACE",       &
                  lHasHeader = .false._c_bool )

    do

      ! read in next line of the control file
      sRecord = CF%readLine()

      if ( CF%isEOF() )  exit

      ! create and allocate memory for a single dictionary entry
      CF_ENTRY => null()
      allocate( CF_ENTRY, stat=iStat )
      call assert(iStat == 0, "Failed to allocate memory for dictionary object", &
            __SRCNAME__, __LINE__ )

      ! break off key value for the current record
      call chomp(sRecord, sSubstring, CF%sDelimiters )

      if ( len_trim( sSubstring ) > 0 ) then

        ! there can be more than one instance of 'DUMP_VARIABLES'
        ! need to make the key unique; tack on an instance number
        if ( sSubstring .strequal. "DUMP_VARIABLES" ) then
          dumpfile_count = dumpfile_count + 1
          sSubstring = trim(sSubstring)//"_"//asCharacter(dumpfile_count)
        endif

        ! first add the key value to the directory entry data structure
        call CF_ENTRY%add_key( sSubstring )

        ! break off first directive for the current record
        call chomp( sRecord, sSubstring, CF%sDelimiters )

        do while ( len_trim( sSubString ) > 0 )

          ! add the next directive snippet to dictionary entry data structure
          call CF_ENTRY%add_entry( sSubstring )

          ! break off next directive for the current record
          call chomp( sRecord, sSubstring, CF%sDelimiters )

        enddo

        ! add the final entry to the dictionary data structure
        call CF_DICT%add_entry( CF_ENTRY )

      endif

    enddo

    ! close the control file
    call CF%close()

  end subroutine read_control_file

!--------------------------------------------------------------------------------------------------



  !> Generic routine to handle intake of gridded data.
  !!
  !! @param[in]  sKey        Name for the data type being processed.
  !! @param[in]  lOptional   If lOptional is TRUE, kill model run eventually IF sKey
  !!                         does not match a known data type.
  !! @param[in]  iDataType   Datatype as defined in @ref constants_and-conversion.F90
  !!
  !! This routine accepts a data grid type, for example, "PRECIPITATION", and attempts to
  !! handle all related control file directives associated with MODEL data type. In MODEL way,
  !! a new gridded data type may be added simply by extending the list of known data types
  !! to the list defined in the module variable @ref KNOWN_TYPES.
  !!

  subroutine initialize_generic_grid(sKey, lOptional, iDataType )

    character (len=*), intent(in)      :: sKey
    logical (kind=c_bool), intent(in)  :: lOptional
    integer (kind=c_int), intent(in)   :: iDataType

    ! [ LOCALS ]
    type (STRING_LIST_T)                 :: myDirectives
    type (STRING_LIST_T)                 :: myOptions
    integer (kind=c_int)                 :: iIndex
    character (len=:), allocatable       :: sCmdText
    character (len=:), allocatable       :: sArgText
    character (len=:), allocatable       :: sArgText_1
    character (len=:), allocatable       :: sArgText_2
    integer (kind=c_int)                 :: iStat
    type (DATA_CATALOG_ENTRY_T), pointer :: pENTRY
    logical (kind=c_bool)             :: lGridPresent

    pENTRY => null()
    lGridPresent = lFALSE

    ! obtain a string list of directives that contain the keyword
    ! (e.g. if the key is "TMIN", MODEL might return:
    ! "TMIN ARC_ASCII input/mygrid.asc"
    ! "TMIN_PROJECTION_DEFINITION =Proj=latlon +datum=WGS84"
    ! "TMIN_MINIMUM_ALLOWED_VALUE -60.0" )
    ! the loop below then handles the specific directives in turn
    myDirectives = CF_DICT%grep_keys( sKey )

!    call myDirectives%print

    if ( myDirectives%count == 0 ) then

      call LOGS%write("Your control file is missing gridded data relating to "//dquote(sKey)//".", &
        iLogLevel=LOG_ALL, lEcho=lFALSE )

      if (.not. lOptional) then
        call warn("Your control file is missing gridded data relating to "//dquote(sKey)//".", &
          lFatal = lTRUE )

      endif

    else

      ! allocate memory for a generic data_catalog_entry
      allocate(pENTRY, stat=iStat)
      call assert( iStat == 0, "Failed to allocate memory for the "//dquote(sKey)//" data structure", &
        __SRCNAME__, __LINE__ )

      ! process all known directives associated with key word
      do iIndex = 1, myDirectives%count

        ! myDirectives is a string list of all SWB directives that contain sKey
        ! sCmdText contains an individual directive
        sCmdText = myDirectives%get(iIndex)

        ! For MODEL directive, obtain the associated dictionary entries
        call CF_DICT%get_values(sCmdText, myOptions )

        ! dictionary entries are initially space-delimited; sArgText_1 contains
        ! all dictionary entries present, concatenated, with a space between entries
        sArgText = myOptions%get(1, myOptions%count )

        ! echo the original directive and dictionary entries to the logfile
        call LOGS%write("> "//sCmdText//" "//sArgText, iLinesBefore=1 )

        ! most of the time, we only care about the first dictionary entry, obtained below
        sArgText_1 = myOptions%get(1)
        sArgText_2 = myOptions%get(2)

        ! first option is that the key value and directive are the same
        ! (.e.g. "PRECIPITATION"; no trailing underscores or modifiers )
        ! MODEL is a grid definition directive
        if ( sCmdText .strapprox. sKey ) then

          pENTRY%sVariableName_z = asLowercase( sKey )

          ! determine the type of grid and act appropriately
          if (sArgText_1 .strapprox. "CONSTANT" ) then

            select case ( iDataType )

              case ( DATATYPE_FLOAT )

              call pENTRY%initialize(            &
                sDescription=trim(sCmdText),     &
                rConstant=asFloat(sArgText_2)  )
              lGridPresent = lTRUE

            case ( DATATYPE_INT )

              call pENTRY%initialize(            &
                sDescription=trim(sCmdText),     &
                iConstant=asInt(sArgText_2)  )
              lGridPresent = lTRUE

            case default

              call die( "INTERNAL PROGRAMMING ERROR: Unhandled data type selected.", &
                __SRCNAME__, __LINE__ )

            end select

          elseif ( (sArgText_1 .strapprox. "ARC_ASCII")              &
              .or. (sArgText_1 .strapprox. "SURFER")                 &
              .or. (sArgText_1 .strapprox. "ARC_GRID") ) then

            call pENTRY%initialize(           &
              sDescription=trim(sCmdText),    &
              sFileType=trim(sArgText_1),     &
              sFilename=trim(sArgText_2),     &
              iDataType=iDataType )
            lGridPresent = lTRUE

          elseif ( sArgText_1 .strapprox. "NETCDF" ) then

            call pENTRY%initialize_netcdf(    &
              sDescription=trim(sCmdText),    &
              sFilename = trim(sArgText_2),   &
              iDataType=iDataType )
            lGridPresent = lTRUE

          elseif ( sArgText_1 .strapprox. "TABLE" ) then

            ! add code to get the table header name and table values

          else

            call warn( "Did not find a valid "//dquote(sKey)//" option. Value supplied was: "//dquote(sArgText_1), &
              lFatal = lTRUE, sHints="Valid options include "//dquote("ARC_ASCII")//", "//dquote("ARC_GRID") &
              //", "//dquote("SURFER")//", or "//dquote("NETCDF") )

          endif


        elseif ( sCmdText .containssimilar. "_USE_MAJORITY_FILTER" ) then

          call pENTRY%set_majority_filter_flag( lTRUE )

        elseif ( sCmdText .containssimilar. "_MONTHNAMES_CAPITALIZED" ) then

          pENTRY%iFilename_Monthname_Capitalization_Rule = FILE_TEMPLATE_CAPITALIZED_MONTHNAME

        elseif ( sCmdText .containssimilar. "_MONTHNAMES_LOWERCASE" ) then

          pENTRY%iFilename_Monthname_Capitalization_Rule = FILE_TEMPLATE_LOWERCASE_MONTHNAME

        elseif ( sCmdText .containssimilar. "_MONTHNAMES_UPPERCASE" ) then

          pENTRY%iFilename_Monthname_Capitalization_Rule = FILE_TEMPLATE_UPPERCASE_MONTHNAME

        elseif ( sCmdText .containssimilar. "_CONVERSION_FACTOR" ) then

          call pENTRY%set_scale(asFloat(sArgText_1))

        elseif ( sCmdText .containssimilar. "_SCALE" ) then

          call pENTRY%set_scale(asFloat(sArgText_1))

        elseif ( sCmdText .containssimilar. "_OFFSET" ) then

          call pENTRY%set_offset(asFloat(sArgText_1))

        elseif ( sCmdText .containssimilar. "NETCDF_X_VAR" ) then

          pENTRY%sVariableName_x = trim(sArgText_1)

        elseif ( sCmdText .containssimilar. "NETCDF_Y_VAR" ) then

          pENTRY%sVariableName_y = trim(sArgText_1)

        elseif ( sCmdText .containssimilar. "NETCDF_Z_VAR" ) then

          pENTRY%sVariableName_z = trim(sArgText_1)

        elseif ( sCmdText .containssimilar. "NETCDF_X_VAR_ADD_OFFSET" ) then

          call pENTRY%set_X_offset( asDouble( sArgText_1 ) )

        elseif ( sCmdText .containssimilar. "NETCDF_Y_VAR_ADD_OFFSET" ) then

          call pENTRY%set_Y_offset( asDouble( sArgText_1 ) )

        elseif ( index( string=sCmdText, substring="NETCDF_TIME_VAR" ) > 0 ) then

          pENTRY%sVariableName_time = trim(sArgText_1)

        elseif ( index( string=sCmdText, substring="NETCDF_VARIABLE_ORDER" ) > 0 ) then

          call pENTRY%set_variable_order( asLowercase(sArgText_1) )

        elseif ( index( string=sCmdText, substring="NETCDF_FLIP_VERTICAL" ) > 0 ) then

          call pENTRY%set_grid_flip_vertical()

        elseif ( index( string=sCmdText, substring="NETCDF_FLIP_HORIZONTAL" ) > 0 ) then

          call pENTRY%set_grid_flip_horizontal()

        elseif ( index( string=sCmdText, substring="NETCDF_NO_AUTOMATIC_GRID_FLIPPING" ) > 0 ) then

          call pENTRY%do_not_allow_netcdf_grid_data_flipping()

        elseif ( index( string=sCmdText, substring="ALLOW_MISSING_FILES" ) > 0 ) then

          call pENTRY%allow_missing_files()

        elseif ( index( string=sCmdText, substring="NETCDF_MAKE_LOCAL_ARCHIVE" ) > 0 ) then

          call pENTRY%set_make_local_archive(lTRUE)

        elseif ( index( string=sCmdText, substring="_PROJECTION_DEFINITION" ) > 0 ) then

          call pENTRY%set_PROJ4( trim(sArgText) )

        elseif ( index( string=sCmdText, substring="_DATE_COLUMN" ) > 0 ) then

          pENTRY%sDateColumnName = trim( sArgText_1 )

        elseif ( index( string=sCmdText, substring="_VALUE_COLUMN" ) > 0 ) then

          pENTRY%sValueColumnName = trim( sArgText_1 )


        elseif ( index( string=sCmdText, substring="_MINIMUM_ALLOWED_VALUE" ) > 0 ) then

          pENTRY%rMinAllowedValue = asFloat(sArgText_1)

        elseif ( index( string=sCmdText, substring="_MAXIMUM_ALLOWED_VALUE" ) > 0 ) then

          pENTRY%rMaxAllowedValue = asFloat(sArgText_1)

        elseif ( index( string=sCmdText, substring="_MISSING_VALUES_CODE" ) > 0 ) then

          pENTRY%rMissingValuesCode = asFloat(sArgText_1)

        elseif ( index( string=sCmdText, substring="_MISSING_VALUES_OPERATOR" ) > 0 ) then

          pENTRY%sMissingValuesOperator = trim(sArgText_1)

        elseif ( index( string=sCmdText, substring= "_MISSING_VALUES_ACTION") > 0 ) then

          if (sArgText_1 .strequal. "ZERO") then

            pENTRY%iMissingValuesAction = MISSING_VALUES_ZERO_OUT

          elseif (sArgText_1 .strequal. "MEAN" ) then

            pENTRY%iMissingValuesAction = MISSING_VALUES_REPLACE_WITH_MEAN

          else

            call warn("Unknown missing value action supplied for " &
              //dquote(sKey)//" data: "//dquote(sArgText_1) )

          endif

        elseif ( index( string=sCmdText, substring="_METHOD") > 0 ) then

          ! no operation; just keep SWB quiet about METHOD and it will be included in the
          ! methods initialization section.

        else

          call warn("Unknown directive detected in code at line "//asCharacter(__LINE__)//", file "//__SRCNAME__ &
            //". ~Ignoring. Directive is: "//dquote(sCmdText), iLogLevel=LOG_DEBUG )

        endif

      enddo

      ! if an unadorned grid specification directive was processed, then we can add the key and
      ! the data_catalog_entry to the data_catalog
      if ( lGridPresent )call DAT%add( key=sKey, data=pENTRY )

      pENTRY => null()

    endif

    call myDirectives%clear()
    call myOptions%clear()

  end subroutine initialize_generic_grid

!--------------------------------------------------------------------------------------------------

  subroutine initialize_grid_options()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myOptions
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat
    real (kind=c_double)             :: rX0, rX1, rY0, rY1, rGridCellSize
    integer (kind=c_int)             :: iNX, iNY
    real (kind=c_float)              :: fTempVal


    ! For MODEL directive, obtain the associated dictionary entries
    call CF_DICT%get_values( "GRID", myOptions )

    ! dictionary entries are initially space-delimited; sArgText contains
    ! all dictionary entries present, concatenated, with a space between entries
    sArgText = myOptions%get(1, myOptions%count )

    ! echo the original directive and dictionary entries to the logfile
    call LOGS%write("> GRID "//sArgText, iLinesBefore=1 )

    iNX = asInt( myOptions%get(1) )
    iNY = asInt( myOptions%get(2) )
    rX0 = asDouble( myOptions%get(3) )
    rY0 = asDouble( myOptions%get(4) )

    if ( myOptions%count == 5 ) then

      rGridCellSize = asDouble( myOptions%get(5) )

      call MODEL%initialize_grid(iNX, iNY, rX0, rY0, rGridCellSize)

      rX1 = rX0 + rGridCellSize * real(iNX, kind=c_double)
      rY1 = rY0 + rGridCellSize * real(iNY, kind=c_double)

    elseif ( myOptions%count == 7 ) then

      rX1 = asDouble( myOptions%get(5) )
      rY1 = asDouble( myOptions%get(6) )
      rGridCellSize = asDouble( myOptions%get(7) )

      fTempVal = ( rX1 - rX0 ) / real(iNX, kind=c_double)

      call MODEL%initialize_grid(iNX, iNY, rX0, rY0, rGridCellSize)

    else

      call warn("Grid specification is flawed or missing.", lFatal=lTRUE, iLogLevel = LOG_ALL, lEcho = lTRUE )

    endif

    call myOptions%clear()

    ! For MODEL directive, obtain the associated dictionary entries
    call CF_DICT%get_values( "BASE_PROJECTION_DEFINITION", myOptions )

    if ( myOptions%get(1) .strequal. "<NA>" ) then
      call die(sMessage="Your control file is missing a BASE_PROJECTION_DEFINITION entry.",            &
         sHints="This version of SWB requires that you add a BASE_PROJECTION_DEFINITION entry "        &
         //"to your control file." )
    endif

    ! dictionary entries are initially space-delimited; sArgText contains
    ! all dictionary entries present, concatenated, with a space between entries
    sArgText = myOptions%get(1, myOptions%count )

    ! echo the original directive and dictionary entries to the logfile
    call LOGS%write("> BASE_PROJECTION_DEFINITION "//sArgText, iLinesBefore=1)

    ! BNDS is a module-level data structure that will be used in other modules to
    ! supply bounding box information for the SWB project area
    BNDS%iNumCols = iNX
    BNDS%iNumRows = iNY
    BNDS%fX_ll = rX0
    BNDS%fY_ll = rY0
    BNDS%fY_ur = rY1
    BNDS%fX_ur = rX1
    BNDS%fGridCellSize = rGridCellSize
    BNDS%sPROJ4_string = trim(sArgText)

    MODEL%PROJ4_string = trim(sArgText)

  end subroutine initialize_grid_options

!--------------------------------------------------------------------------------------------------

  subroutine initialize_start_and_end_dates()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myDirectives
    type (STRING_LIST_T)             :: myOptions
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sCmdText
    character (len=:), allocatable   :: sOptionText
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat
    logical (kind=c_bool)            :: lHaveStartDate
    logical (kind=c_bool)            :: lHaveEndDate

    lHaveStartDate = lFALSE
    lHaveEndDate = lFALSE

    myDirectives = CF_DICT%grep_keys("DATE")

!     if ( myDirectives%count < 2 ) then

!       call warn(sMessage="Your control file seems to be missing START_DATE and/or END_DATE", &
!         sHints="Add a START_DATE and/or END_DATE directive to your control file. Date "      &
!         //"~should be specified as mm/dd/yyyy.", lFatal = lTRUE, iLogLevel = LOG_ALL,        &
!         lEcho = lTRUE )


    call LOGS%set_loglevel( LOG_ALL )
    call LOGS%set_echo( lFALSE )

    do iIndex = 1, myDirectives%count

      ! myDirectives is a string list of all SWB directives that contain the string "DATE"
      ! sCmdText contains an individual directive
      sCmdText = myDirectives%get(iIndex)

      ! For MODEL directive, obtain the associated dictionary entries
      call CF_DICT%get_values(sCmdText, myOptions )

      ! dictionary entries are initially space-delimited; sArgText contains
      ! all dictionary entries present, concatenated, with a space between entries
      sArgText = myOptions%get(1, myOptions%count )

      ! echo the original directive and dictionary entries to the logfile
      call LOGS%write("> "//sCmdText//" "//sArgText, iLinesBefore=1 )

      ! most of the time, we only care about the first dictionary entry, obtained below
      sOptionText = myOptions%get(1)

      select case ( sCmdText )

        case ( "START_DATE", "STARTDATE", "BEGIN_DATE" )

          lHaveStartDate = lTRUE
          call SIM_DT%start%parseDate( sOptionText )
          call SIM_DT%start%calcJulianDay()

        case ( "END_DATE", "ENDDATE", "STOP_DATE" )

          lHaveEndDate = lTRUE
          call SIM_DT%end%parseDate( sOptionText )
          call SIM_DT%end%calcJulianDay()

        case default

          call warn("Unknown directive present, line "//asCharacter(__LINE__)//", file "//__SRCNAME__ &
            //". Ignoring. Directive is: "//dquote(sCmdText), iLogLevel=LOG_DEBUG )

      end select

    enddo

    if ( lHaveStartDate .and. lHaveEndDate ) then

      SIM_DT%curr = SIM_DT%start
      SIM_DT%iDOY = day_of_year( SIM_DT%curr%getJulianDay() )

      SIM_DT%iDaysInMonth = SIM_DT%curr%dayspermonth()
      SIM_DT%iDaysInYear = SIM_DT%curr%daysperyear()
      SIM_DT%lIsLeapYear = SIM_DT%curr%isLeapYear()

      call LOGS%write("Model run start date set to: "//SIM_DT%start%prettydate(), iTab=4, lEcho=lTRUE)
      call LOGS%write("Model run end date set to:   "//SIM_DT%end%prettydate(), iTab=4, lEcho=lTRUE)

    else

      call warn(sMessage="Your control file seems to be missing START_DATE and/or END_DATE", &
        sHints="Add a START_DATE and/or END_DATE directive to your control file. Date "      &
        //"~should be specified as mm/dd/yyyy.", lFatal = lTRUE, iLogLevel = LOG_ALL,        &
        lEcho = lTRUE )

    endif

  end subroutine initialize_start_and_end_dates

!--------------------------------------------------------------------------------------------------

  !> Find any parameter tables specified in the control file; process and store contents.
  !!
  !! Any control file entry that contains the text "LOOKUP_TABLE" is assumed to specify a parameter table
  !! that needs to be read in and processed.
  !!
  !! @note The entries given in the files are implicitly assumed to be in sorted order. For example, if the parameters
  !! pertain to landuse codes, it is assumed that all table values are given in order from lowest to highest
  !! landuse code.

  subroutine initialize_lookup_tables()

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myDirectives
    type (STRING_LIST_T)             :: myOptions
    type (STRING_LIST_T)             :: slString
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sCmdText
    character (len=:), allocatable   :: sOptionText
    character (len=:), allocatable   :: sArgText
    character (len=:), allocatable   :: sText
    character (len=256)              :: sBuf
    integer (kind=c_int)             :: iStat
    type (PARAMETERS_T)              :: PARAMS
    integer (kind=c_int)             :: iCount
    type (DICT_ENTRY_T), pointer     :: pDict1
    type (DICT_ENTRY_T), pointer     :: pDict2

    iCount = 0

    myDirectives = CF_DICT%grep_keys("LOOKUP_TABLE")

    if ( myDirectives%count == 0 ) then

      call warn("Your control file seems to be missing the required lookup table(s).", &
        lFatal = lTRUE )

    else

      call LOGS%set_loglevel( LOG_ALL )
      call LOGS%set_echo( lFALSE )

      ! iterate over list of all control file statements that contain the phrase "LOOKUP_TABLE"
      do iIndex = 1, myDirectives%count

        ! sCmdText contains an individual directive
        sCmdText = myDirectives%get(iIndex)

        ! for MODEL directive, obtain the associated dictionary entries
        call CF_DICT%get_values(sCmdText, myOptions )

        ! dictionary entries are initially space-delimited; sArgText contains
        ! all dictionary entries present, concatenated, with a space between entries
        sArgText = myOptions%get(1, myOptions%count )

        ! echo the original directive and dictionary entries to the logfile
        call LOGS%write("> "//sCmdText//" "//sArgText, iLinesBefore=1 )

        ! most of the time, we only care about the first dictionary entry, obtained below
        sOptionText = myOptions%get(1)

        if ( index(string=sCmdText, substring="LOOKUP_TABLE" ) > 0 ) then

            call PARAMS%add_file( sOptionText )
            iCount = iCount + 1

        else

            call warn("Unknown directive present, line "//asCharacter(__LINE__)//", file "//__SRCNAME__ &
              //". Ignoring. Directive is: "//dquote(sCmdText), iLogLevel=LOG_DEBUG )

        endif

      enddo

      if ( iCount > 0 ) then

        call PARAMS%munge_file()
        call PARAMS_DICT%print_all(iLogLevel=LOG_DEBUG, lEcho=lFALSE)

      endif

    endif

  end subroutine initialize_lookup_tables

!--------------------------------------------------------------------------------------------------

  subroutine initialize_generic_method( sKey, lOptional)

    character (len=*), intent(in)     :: sKey
    logical (kind=c_bool), intent(in) :: lOptional

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myDirectives
    type (STRING_LIST_T)             :: myOptions
    integer (kind=c_int)             :: iIndex
    integer (kind=c_int)             :: indx
    character (len=:), allocatable   :: sCmdText
!    character (len=:), allocatable   :: sOptionText
    type (STRING_LIST_T)             :: argv_list
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat
    integer (kind=c_int)             :: status
    logical (kind=c_bool)            :: lFatal
    integer (kind=c_int)             :: num_elements

    ! obtain a list of control file directives whose key values contain the string sKey
    myDirectives = CF_DICT%grep_keys( trim(sKey) )

    lFatal = .not. lOptional

    if ( myDirectives%count == 0 ) then

      call warn("Your control file is missing any of the required directives relating to "//dquote(sKey)//" method.", &
        lFatal = lFatal, iLogLevel = LOG_ALL, lEcho = lTRUE )

    else

      call LOGS%set_loglevel( LOG_ALL )
      call LOGS%set_echo( lFALSE )

      ! repeat MODEL process for each control file directive in list
      do iIndex = 1, myDirectives%count

        ! sCmdText contains an individual directive (e.g. TMAX NETCDF input/tmax_%0m_%0d_%0Y.nc )
        sCmdText = myDirectives%get(iIndex)

        ! For MODEL directive, obtain the associated dictionary entries
        call CF_DICT%get_values(sCmdText, myOptions )

        ! dictionary entries are initially space-delimited; sArgText contains
        ! all dictionary entries present, concatenated, with a space between entries
        sArgText = myOptions%get(1, myOptions%count )

        ! echo the original directive and dictionary entries to the logfile
        call LOGS%write("> "//sCmdText//" "//sArgText, iLinesBefore=1 )

        ! most of the time, we only care about the first dictionary entry, obtained below
!        sOptionText = myOptions%get(1)

        num_elements = myOptions%count

        call argv_list%clear()

        do indx=1, myOptions%count
          call argv_list%append( myOptions%get( indx ) )
        enddo

        ! Any entry in the control file that contains the substring "METHOD" will be
        ! handed to the "set_method_pointers" subroutine in an attempt to wire up the correct
        ! process modules
        if ( ( sCmdText .contains. "METHOD" ) .or. ( sCmdText .contains. "DUMP" ) ) then

          call MODEL%set_method_pointers( trim(sCmdText), argv_list )

        endif

      enddo

    endif

  end subroutine initialize_generic_method

!--------------------------------------------------------------------------------------------------

  subroutine initialize_program_options( )

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: myDirectives
    type (STRING_LIST_T)             :: myOptions
    integer (kind=c_int)             :: iIndex
    integer (kind=c_int)             :: indx
    character (len=:), allocatable   :: sCmdText
!    character (len=:), allocatable   :: sOptionText
    type (STRING_LIST_T)             :: argv_list
    character (len=:), allocatable   :: sArgText
    integer (kind=c_int)             :: iStat
    integer (kind=c_int)             :: status
    logical (kind=c_bool)            :: lFatal
    integer (kind=c_int)             :: num_elements
    character (len=:), allocatable   :: Option_Name

    ! obtain a list of control file directives whose key values contain the string sKey
    myDirectives = CF_DICT%grep_keys( "OPTION" )

    call LOGS%set_loglevel( LOG_ALL )
    call LOGS%set_echo( lFALSE )

    ! repeat MODEL process for each control file directive in list
    do iIndex = 1, myDirectives%count

      ! sCmdText contains an individual directive (e.g. TMAX NETCDF input/tmax_%0m_%0d_%0Y.nc )
      sCmdText = myDirectives%get(iIndex)

      ! For MODEL directive, obtain the associated dictionary entries
      call CF_DICT%get_values(sCmdText, myOptions )

      ! dictionary entries are initially space-delimited; sArgText contains
      ! all dictionary entries present, concatenated, with a space between entries
      sArgText = myOptions%get(1, myOptions%count )

      ! echo the original directive and dictionary entries to the logfile
      call LOGS%write("> "//sCmdText//" "//sArgText, iLinesBefore=1 )

      ! most of the time, we only care about the first dictionary entry, obtained below
!        sOptionText = myOptions%get(1)

      num_elements = myOptions%count

      call argv_list%clear()

      do indx=1, myOptions%count
        call argv_list%append( myOptions%get( indx ) )
      enddo

      ! Any entry in the control file that contains the substring "OPTION" will be
      ! processed directly
      if ( ( sCmdText .contains. "OPTION" ) ) then

        Option_Name = argv_list%get(1)

        select case ( Option_Name )

          case ( "NO_LATLON_IN_OUTPUT" )

            call set_output_latlon_option( lFALSE )
            call LOGS%write("==> LATITUDE and LONGITUDE will *not* be included in output NetCDF files.", iLinesBefore=1 )

        end select

      endif

    enddo

  end subroutine initialize_program_options


!--------------------------------------------------------------------------------------------------


  subroutine initialize_latitude()

    ! [ LOCALS ]
    integer (kind=c_int)  :: iIndex

    pCOORD_GRD => grid_Create( iNX=MODEL%number_of_columns, iNY=MODEL%number_of_rows, &
        rX0=MODEL%X_ll, rY0=MODEL%Y_ll, &
        rGridCellSize=MODEL%gridcellsize, iDataType=GRID_DATATYPE_REAL )

    allocate ( MODEL%X(MODEL%number_of_columns ) )
    allocate ( MODEL%Y(MODEL%number_of_rows ) )

    ! call the grid routine to populate the X and Y values
    call grid_PopulateXY( pCOORD_GRD )

    ! populating these in order to have them available later for use in writing results to NetCDF
    MODEL%X = pCOORD_GRD%rX( :, 1 )
    MODEL%Y = pCOORD_GRD%rY( 1, : )

    ! transform to unprojected (lat/lon) coordinate system
    call grid_Transform(pGrd=pCOORD_GRD, sFromPROJ4=MODEL%PROJ4_string, &
        sToPROJ4="+proj=lonlat +ellps=GRS80 +datum=WGS84 +no_defs" )

    MODEL%latitude = pack( pCOORD_GRD%rY, MODEL%active )

    MODEL%X_lon = pCOORD_GRD%rX
    MODEL%Y_lat = pCOORD_GRD%rY

    pCOORD_GRD%rData=pCOORD_GRD%rX
    call grid_WriteArcGrid( sFilename="Longitude__calculated.asc", pGrd=pCOORD_GRD )
    pCOORD_GRD%rData=pCOORD_GRD%rY
    call grid_WriteArcGrid( sFilename="Latitude__calculated.asc", pGrd=pCOORD_GRD )

    call grid_Destroy( pCOORD_GRD )

  end subroutine initialize_latitude

! !--------------------------------------------------------------------------------------------------
!
!   !> Match landuse codes from table with those contained in the gridded landuse.
!   !!
!   !! This routine loops through all known
!
!   subroutine initialize_landuse_codes()
!
!     ! [ LOCALS ]
!     integer (kind=c_int)                 :: iIndex
!     integer (kind=c_int), allocatable    :: iLandUseCodes(:)
!     type (DATA_CATALOG_ENTRY_T), pointer :: pLULC
!     integer (kind=c_int)                 :: iIndex2
!     integer (kind=c_int)                 :: iCount
!     integer (kind=c_int)                 :: iStat
!     logical (kind=c_bool)                :: lMatch
!     type (STRING_LIST_T)                 :: slList
!
!     call slList%append("LU_Code")
!     call slList%append("LU_code")
!     call slList%append("Landuse_Code")
!     call slList%append("LULC_Code")
!
!     !> Determine how many landuse codes are present
!     call PARAMS%get_parameters( slKeys=slList, iValues=iLanduseCodes, lFatal=TRUE )
!
!     ! obtain a pointer to the LAND_USE grid
!     pLULC => DAT%find("LAND_USE")
!
!     if ( associated(pLULC) ) then
!
!       if (associated( pLULC%pGrdBase) ) then
!         MODEL%landuse_code = pack( pLULC%pGrdBase%iData, MODEL%active )
!       else
!         call die("INTERNAL PROGRAMMING ERROR: attempted use of NULL pointer", __SRCNAME__, __LINE__)
!       endif
!     else
!       call die("Attempted use of NULL pointer. Failed to find LAND_USE data element.", &
!         __SRCNAME__, __LINE__)
!     endif
!
!     ! setting this to a value that is likely valid; if this is set to a negative value, a landuse
!     ! code that is present in the grid but not in the lookup table will trigger a fatal error, however,
!     ! processing will continue until a bounds error is triggered somewhere else in the code,
!     MODEL%landuse_index = -9999
!     iCount = 0
!
!     ! only run through matching process if we've found a LU_Code entry in the
!     ! parameter dictionary
!     if ( all( iLandUseCodes >= 0 ) ) then
!
!       do iIndex = 1, ubound(MODEL%landuse_code,1)
!
!         lMatch = lFALSE
!
!         do iIndex2=1, ubound(iLandUseCodes, 1)
!
!           if (MODEL%landuse_code(iIndex) == iLandUseCodes(iIndex2) ) then
!             MODEL%landuse_index(iIndex) = iIndex2
!             iCount = iCount + 1
!             lMatch = lTRUE
!             exit
!           endif
!
!         enddo
!
!         if ( .not. lMatch ) then
!           call warn(sMessage="Failed to match landuse code "//asCharacter(MODEL%landuse_code(iIndex) ) &
!             //" with a corresponding landuse code from lookup tables.",                                &
!             sHints="Make sure your lookup table(s) have landuse codes corresponding to all values in " &
!             //"the land-use grid.", lFatal=TRUE, iLogLevel=LOG_ALL, lEcho=TRUE)
!           ! we're setting this value to a valid value. this shouldn't cause problems with any
!           ! calculations because we've already thrown a fatal error
!           MODEL%landuse_index(iIndex) = 1
!         endif
!       enddo
!
!       call LOGS%write("Matches were found between landuse grid value and table value for " &
!         //asCharacter(iCount)//" cells out of a total of "//asCharacter(ubound(MODEL%landuse_code,1))//" active cells.", &
!         iLinesBefore=1, iLinesAfter=1, iLogLevel=LOG_ALL)
!
!       call slList%clear()
!
!     endif
!
!     ! if we have more than one cell for which an index value could not be assigned, trigger fatal error
!     if ( count(MODEL%landuse_index < 0) > 0 ) then
!       call warn(asCharacter(count(MODEL%landuse_index < 0))//" landuse codes could not be "        &
!       //" assigned a landuse index value.", lFatal=lTRUE, sHints="Make sure that you have an "        &
!       //"entry in the landuse lookup table for each unique code contained in your landuse grid." )
!     endif
!
!
!   end subroutine initialize_landuse_codes

!--------------------------------------------------------------------------------------------------

  subroutine initialize_surface_storage_max()

    integer (kind=c_int)               :: iIndex
    integer (kind=c_int)               :: iStat
    character (len=256)                :: sBuf
    type (STRING_LIST_T)               :: slList
    integer( kind=c_int), allocatable  :: iLanduseTableCodes(:)
    integer (kind=c_int)               :: iNumberOfLanduses
    real (kind=c_float), allocatable   :: SURFACE_STORAGE_MAXIMUM(:)
    real (kind=c_float)                :: current_surface_storage_max


    ! create list of possible table headings to look for...
    call slList%append( "LU_Code" )
    call slList%append( "Landuse_Lookup_Code" )

    !> Determine how many landuse codes are present
    call PARAMS%get_parameters( slKeys=slList, iValues=iLanduseTableCodes )
    iNumberOfLanduses = count( iLanduseTableCodes >= 0 )

    call slList%clear()
    call slList%append("Surface_Storage_Max")
    call slList%append("Surface_Storage_Maximum")

    MODEL%surface_storage_max = 0.0_c_float

    call PARAMS%get_parameters( slKeys=slList, fValues=SURFACE_STORAGE_MAXIMUM, lFatal=FALSE )

    if ( all( SURFACE_STORAGE_MAXIMUM > rTINYVAL ) ) then

      do iIndex=1, ubound( SURFACE_STORAGE_MAXIMUM, 1)

        current_surface_storage_max = SURFACE_STORAGE_MAXIMUM( iIndex )

        where( MODEL%landuse_index == iIndex )

          model%surface_storage_max = current_surface_storage_max

        end where

      enddo

    endif

  end subroutine initialize_surface_storage_max

end module model_initialize
