module swb_merge_initialize

  use iso_c_binding, only                : c_int, c_float, c_double, c_bool
  use constants_and_conversions
  use datetime
  use data_catalog
  use data_catalog_entry
  use dictionary
  use exceptions
  use logfiles, only                     : LOGS, LOG_ALL, LOG_DEBUG
  use file_operations
  use grid
  use netcdf4_support
  use output, only                       : initialize_output
  use swb_merge_domain
  use simulation_datetime, only : SIM_DT
  use strings
  use string_list  
  implicit none

  private

  public :: initialize_all

  type (GENERAL_GRID_T), pointer    :: pCOORD_GRD

  type :: BOUNDING_BOX_T
    integer (kind=c_int)           :: number_of_columns
    integer (kind=c_int)           :: number_of_rows    
    real (kind=c_double)           :: Xmin
    real (kind=c_double)           :: Xmax
    real (kind=c_double)           :: Ymin
    real (kind=c_double)           :: Ymax
    real (kind=c_double)           :: Xll
    real (kind=c_double)           :: Xur
    real (kind=c_double)           :: Yll
    real (kind=c_double)           :: Yur
    integer (kind=c_int)           :: origin_column
    integer (kind=c_int)           :: origin_row
    real (kind=c_double)           :: Xgridcellsize 
    real (kind=c_double)           :: Ygridcellsize 
    character (len=:), allocatable :: PROJ4_string
  end type BOUNDING_BOX_T
  
  type (BOUNDING_BOX_T), allocatable :: bbox(:)             

contains

  subroutine initialize_all( filelist )

    type (STRING_LIST_T), intent(in)    :: filelist

    ! [ LOCALS ]
    integer (kind=c_int)                :: indx, jndx
    type (STRING_LIST_T)                :: variable_list
    type (STRING_LIST_T)                :: name_list
    type (STRING_LIST_T)                :: value_list 
    character (len=:), allocatable      :: PROJ4_string     
    character (len=:), allocatable      :: sBuf
    type (T_NETCDF4_FILE ), allocatable :: NCFILES(:)
    integer (kind=c_int)                :: status
    integer (kind=c_int)                :: num_files
    real (kind=c_double)                :: cellsize_x, cellsize_y
    real (kind=c_double)                :: start_jd, end_jd
    type (BOUNDING_BOX_T)               :: merge_bnds

    num_files = filelist%count

    allocate( NCFILES( num_files ), stat=status )
    allocate( bbox( num_files ), stat=status )
    allocate( MODEL%NCFILE, stat=status )

    do indx=1, num_files-1

      call netcdf_open_and_prepare_for_merging(NCFILE=NCFILES( indx ),             &
                                               sFilename=filelist%get( indx ) )
      call netcdf_get_variable_list(NCFILE=NCFILES( indx ), variable_list=variable_list )

      call netcdf_get_attribute_list_for_variable( NCFILE=NCFILES( indx ),           &
                                                   variable_name="crs",              &
                                                   attribute_name_list=name_list,    &
                                                   attribute_value_list=value_list )
      MODEL%NCFILE%iVarID(NC_Z)=4

      do jndx=1,name_list%count
        if ( name_list%get( jndx ) .contains. "proj4" )     &
          bbox( indx )%PROJ4_string = value_list%get( jndx )
      enddo  

      ! capture bounding box for each netCDF file
      bbox( indx )%number_of_rows = NCFILES( indx )%iNY
      bbox( indx )%number_of_columns = NCFILES( indx )%iNX      
      bbox( indx )%Xgridcellsize = NCFILES( indx )%rGridCellSizeX
      bbox( indx )%Ygridcellsize = NCFILES( indx )%rGridCellSizeY
      cellsize_x = bbox( indx )%Xgridcellsize
      cellsize_y = bbox( indx )%Ygridcellsize

      ! Xmin, Ymin, etc. are coordinates at the center of gridcells
      bbox( indx )%Xmin = minval( NCFILES( indx )%rX_coords )
      bbox( indx )%Xmax = maxval( NCFILES( indx )%rX_coords )
      bbox( indx )%Ymin = minval( NCFILES( indx )%rY_coords )
      bbox( indx )%Ymax = maxval( NCFILES( indx )%rY_coords )

      ! Xll, Xur, etc. represent coordinates at the extreme corners of
      ! the respective gridcells
      bbox( indx )%Xll = bbox( indx )%Xmin - cellsize_x / 2.0_c_double
      bbox( indx )%Xur = bbox( indx )%Xmax + cellsize_x / 2.0_c_double
      bbox( indx )%Yll = bbox( indx )%Ymin - cellsize_y / 2.0_c_double
      bbox( indx )%Yur = bbox( indx )%Xmax + cellsize_y / 2.0_c_double      

      start_jd = NCFILES( indx )%iFirstDayJD
      end_jd = NCFILES( indx )%iLastDayJD      

    enddo

    ! at this point we should have the spatial information necessary to 
    ! create an all-encompassing bounding box

    merge_bnds%Xmin = minval( bbox(:)%Xmin )
    merge_bnds%Xmax = maxval( bbox(:)%Xmax )
    merge_bnds%Ymin = minval( bbox(:)%Ymin )
    merge_bnds%Ymax = maxval( bbox(:)%Ymax )
    merge_bnds%number_of_rows =                                               &
      ( ( merge_bnds%Ymax - merge_bnds%Ymin ) + cellsize_x ) / cellsize_x
    merge_bnds%number_of_columns =                                            &
      ( ( merge_bnds%Xmax - merge_bnds%Xmin ) + cellsize_y ) / cellsize_y

    merge_bnds%Xll = merge_bnds%Xmin - cellsize_x / 2.0_c_double
    merge_bnds%Xur = merge_bnds%Xmax + cellsize_x / 2.0_c_double
    merge_bnds%Yll = merge_bnds%Ymin - cellsize_y / 2.0_c_double
    merge_bnds%Yur = merge_bnds%Xmax + cellsize_y / 2.0_c_double  
    merge_bnds%Xgridcellsize = bbox(1)%Xgridcellsize
    merge_bnds%PROJ4_string = bbox(1)%PROJ4_string

  

    ! define SWB project boundary and geographic projection
    
    !call initialize_grid_options( xll, yll, xur, yur, resolution, proj4 )

    ! define the start and end date for the simulation
    call initialize_start_and_end_dates( start_jd, end_jd )
    print *, "Start date: ", SIM_DT%start%prettydate()
    print *, "End date: ", SIM_DT%end%prettydate()    

    call initialize_grid_options( xll=merge_bnds%Xll,                    &
                                  yll=merge_bnds%Yll,                    &
                                  xur=merge_bnds%Xur,                    &
                                  yur=merge_bnds%Yur,                    &
                                  nx=merge_bnds%number_of_columns,       &
                                  ny=merge_bnds%number_of_rows,          &
                                  gridcellsize=merge_bnds%Xgridcellsize, &
                                  proj4_string=merge_bnds%PROJ4_string )

    call initialize_latitude()

    call netcdf_open_and_prepare_as_output( NCFILE=MODEL%NCFILE,              &
                                            sVariableName="tmax",             &
                                            sVariableUnits="degrees F",       &
                                            iNX=merge_bnds%number_of_columns,    &
                                            iNY=merge_bnds%number_of_rows,       &
                                            fX=MODEL%X,                       &
                                            fY=MODEL%Y,                       &
                                            StartDate=SIM_DT%start,           &
                                            EndDate=SIM_DT%end,               &
                                            PROJ4_string=merge_bnds%PROJ4_string,&
                                            dpLat=MODEL%Y_lat,                &
                                            dpLon=MODEL%X_lon )

!    call netcdf_open_and_prepare_as_output()

  end subroutine initialize_all

!--------------------------------------------------------------------------------------------------

  subroutine initialize_grid_options( xll, yll, xur, yur, nx, ny, gridcellsize, proj4_string )

    real (kind=c_double), intent(inout) :: xll
    real (kind=c_double), intent(inout) :: yll
    real (kind=c_double), intent(inout) :: xur
    real (kind=c_double), intent(inout) :: yur
    integer (kind=c_int), intent(inout) :: nx
    integer (kind=c_int), intent(inout) :: ny
    real (kind=c_double), intent(inout) :: gridcellsize
    character (len=*), intent(inout)    :: proj4_string

    call MODEL%initialize_grid(iNumCols=nx, iNumRows=ny, dX_ll=xll, dY_ll=yll, &
      dGridCellSize=gridcellsize )

    ! BNDS is a module-level data structure that will be used in other modules to 
    ! supply bounding box information for the SWB project area
    BNDS%iNumCols = nx
    BNDS%iNumRows = ny
    BNDS%fX_ll = xll
    BNDS%fY_ll = yll
    BNDS%fY_ur = yur
    BNDS%fX_ur = xur
    BNDS%fGridCellSize = gridcellsize
    BNDS%sPROJ4_string = proj4_string

    MODEL%PROJ4_string = proj4_string

  end subroutine initialize_grid_options

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

!--------------------------------------------------------------------------------------------------

  subroutine initialize_start_and_end_dates( start_jd, end_jd )

    real (kind=c_double), intent(inout)  :: start_jd
    real (kind=c_double), intent(inout)  :: end_jd

    call SIM_DT%start%setJulianDate( start_jd )
    call SIM_DT%start%calcGregorianDate()

    call SIM_DT%end%setJulianDate( end_jd )
    call SIM_DT%end%calcGregorianDate()

  end subroutine initialize_start_and_end_dates

end module swb_merge_initialize