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

  public :: initialize_all, bbox, NCFILES

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
    integer (kind=c_int), allocatable :: merge_column(:,:)
    integer (kind=c_int), allocatable :: merge_row(:,:)
    real (kind=c_double)           :: Xgridcellsize 
    real (kind=c_double)           :: Ygridcellsize 
    character (len=:), allocatable :: PROJ4_string
    real (kind=c_float), allocatable :: data_grid(:,:)
  end type BOUNDING_BOX_T
  
  type (BOUNDING_BOX_T), allocatable :: bbox(:)       
  type (T_NETCDF4_FILE ), allocatable :: NCFILES(:)      

contains

  subroutine initialize_all( filelist )

    type (STRING_LIST_T), intent(in)    :: filelist

    ! [ LOCALS ]
    real (kind=c_double),parameter      :: num_cells_as_buffer = 2.0
    integer (kind=c_int)                :: indx, jndx, col, row
    type (STRING_LIST_T)                :: variable_list
    type (STRING_LIST_T)                :: crs_name_list, z_name_list
    type (STRING_LIST_T)                :: crs_value_list, z_value_list 
    character (len=:), allocatable      :: PROJ4_string     
    character (len=:), allocatable      :: sBuf
    integer (kind=c_int)                :: status
    integer (kind=c_int)                :: num_files
    real (kind=c_double)                :: cellsize_x, cellsize_y
    real (kind=c_double)                :: buffer_x, buffer_y
    real (kind=c_double)                :: start_jd, end_jd
    type (BOUNDING_BOX_T)               :: merge_bnds
    integer (kind=c_int)                :: col_row(2)
    integer (kind=c_int)                :: variable_id
    character (len=:), allocatable      :: z_variable_name
    character (len=:), allocatable      :: z_variable_units
    type (STRING_LIST_T), pointer       :: history_list
    type(DATETIME_T)                    :: DT
    character (len=:), allocatable      :: date_time_text

    num_files = filelist%count

    call DT%systime()
    date_time_text = DT%prettydatetime()

    allocate( NCFILES( num_files ), stat=status )
    allocate( bbox( num_files ), stat=status )
    allocate( MODEL%NCFILE, stat=status )
    allocate( history_list )

    call history_list%append(date_time_text//": Begin merge of "//asCharacter( num_files ) &
      //" netCDF files.")

    do indx=1, num_files

      call netcdf_open_and_prepare_for_merging(NCFILE=NCFILES( indx ),                  &
                                               sFilename=filelist%get( indx ) )

      call netcdf_get_variable_list(NCFILE=NCFILES( indx ), variable_list=variable_list )

      call netcdf_get_attribute_list_for_variable( NCFILE=NCFILES( indx ),              &
                                                   variable_name="crs",                 &
                                                   attribute_name_list=crs_name_list,   &
                                                   attribute_value_list=crs_value_list )

      call find_z_variable( variable_list, z_variable_name )

      call netcdf_get_variable_id_for_variable( NCFILE=NCFILES( indx ),                 &
                                                variable_name=z_variable_name,          &
                                                variable_id=variable_id )


      call netcdf_get_attribute_list_for_variable( NCFILE=NCFILES( indx ),              &
                                                   variable_name=z_variable_name,       &
                                                   attribute_name_list=z_name_list,   &
                                                   attribute_value_list=z_value_list ) 
      NCFILES( indx )%iVarID(NC_Z)=variable_id
      NCFILES( indx )%iVarType(NC_Z) = NC_FLOAT

      do jndx=1,z_name_list%count
        if ( z_name_list%get( jndx ) .strequal. "units" )     &
          z_variable_units = z_value_list%get( jndx )
      enddo  

      associate ( bbox => bbox( indx ) )

        do jndx=1,crs_name_list%count
          if ( crs_name_list%get( jndx ) .contains. "proj4" )     &
            bbox%PROJ4_string = crs_value_list%get( jndx )
        enddo  

        ! capture bounding box for each netCDF file
        bbox%number_of_rows = NCFILES( indx )%iNY
        bbox%number_of_columns = NCFILES( indx )%iNX      
        bbox%Xgridcellsize = NCFILES( indx )%rGridCellSizeX
        bbox%Ygridcellsize = NCFILES( indx )%rGridCellSizeY
        cellsize_x = bbox%Xgridcellsize
        cellsize_y = bbox%Ygridcellsize

        allocate( bbox%merge_column( bbox%number_of_columns, bbox%number_of_rows),  &
          stat=status )
        allocate( bbox%merge_row( bbox%number_of_columns, bbox%number_of_rows),  &
          stat=status )
        allocate( bbox%data_grid( bbox%number_of_columns, bbox%number_of_rows), stat=status )

        ! Xmin, Ymin, etc. are coordinates at the center of gridcells
        bbox%Xmin = minval( NCFILES( indx )%rX_coords )
        bbox%Xmax = maxval( NCFILES( indx )%rX_coords )
        bbox%Ymin = minval( NCFILES( indx )%rY_coords )
        bbox%Ymax = maxval( NCFILES( indx )%rY_coords )

        ! Xll, Xur, etc. represent coordinates at the extreme corners of
        ! the respective gridcells
        bbox%Xll = bbox%Xmin - cellsize_x / 2.0_c_double
        bbox%Xur = bbox%Xmax + cellsize_x / 2.0_c_double
        bbox%Yll = bbox%Ymin - cellsize_y / 2.0_c_double
        bbox%Yur = bbox%Xmax + cellsize_y / 2.0_c_double      

      end associate

      call history_list%append("Added "//trim(z_variable_name)//" data from file " &
        //filelist%get( indx )//"." )

      start_jd = NCFILES( indx )%iFirstDayJD
      end_jd = NCFILES( indx )%iLastDayJD      

    enddo

    ! at this point we should have the spatial information necessary to 
    ! create an all-encompassing bounding box

    buffer_x = real( num_cells_as_buffer, kind=c_double) * cellsize_x
    buffer_y = real( num_cells_as_buffer, kind=c_double) * cellsize_y    
    merge_bnds%Xmin = minval( bbox(:)%Xmin )
    merge_bnds%Xmax = maxval( bbox(:)%Xmax )
    merge_bnds%Ymin = minval( bbox(:)%Ymin )
    merge_bnds%Ymax = maxval( bbox(:)%Ymax )

    ! number of cells equals exact number needed PLUS 'num_cells_as_buffer' 
    ! added on either end
    merge_bnds%number_of_rows = nint(                                         &
      ( ( merge_bnds%Ymax - merge_bnds%Ymin ) + cellsize_x ) / cellsize_x )   &
      + 2_c_int * int( num_cells_as_buffer, kind=c_int)
    merge_bnds%number_of_columns = nint(                                      &
      ( ( merge_bnds%Xmax - merge_bnds%Xmin ) + cellsize_y ) / cellsize_y )   &
      + 2_c_int * int( num_cells_as_buffer, kind=c_int)

    ! account for 1/2 cell difference between gridcell coordinate and the
    ! bounding box coordinate; expand in all directions by 'num_cells_as_buffer'
    merge_bnds%Xll = merge_bnds%Xmin - cellsize_x * ( num_cells_as_buffer + 0.5_c_double )
    merge_bnds%Xur = merge_bnds%Xmax + cellsize_x * ( num_cells_as_buffer + 0.5_c_double )
    merge_bnds%Yll = merge_bnds%Ymin - cellsize_y * ( num_cells_as_buffer + 0.5_c_double )
    merge_bnds%Yur = merge_bnds%Xmax + cellsize_y * ( num_cells_as_buffer + 0.5_c_double )
    merge_bnds%Xgridcellsize = cellsize_x
    merge_bnds%Ygridcellsize = cellsize_y
    merge_bnds%PROJ4_string = bbox(1)%PROJ4_string

    ! define SWB project boundary and geographic projection
    
    !call initialize_grid_options( xll, yll, xur, yur, resolution, proj4 )

    ! define the start and end date for the simulation
    call initialize_start_and_end_dates( start_jd, end_jd )

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
                                            sVariableName=z_variable_name,    &
                                            sVariableUnits=z_variable_units,  &
                                            iNX=merge_bnds%number_of_columns, &
                                            iNY=merge_bnds%number_of_rows,    &
                                            fX=MODEL%X,                       &
                                            fY=MODEL%Y,                       &
                                            StartDate=SIM_DT%start,           &
                                            EndDate=SIM_DT%end,               &
                                            PROJ4_string=merge_bnds%PROJ4_string,&
                                            history_list=history_list,        &
                                            dpLat=MODEL%Y_lat,                &
                                            dpLon=MODEL%X_lon )

    ! now that we know the outer dimensions of the merged grids, go back and
    ! determine row, column numbers *in the merged grid* that correspond to the
    ! locations in each of the grids to be merged.  
    do indx=1, num_files
 
      associate( bbox => bbox( indx ),              &
                 nc => NCFILES( indx ),             &
                 x => NCFILES( indx )%rX_Coords,    &
                 y => NCFILES( indx )%rY_Coords ) 

        do col=1, ubound( bbox%merge_column, 1 )
          do row=1, ubound( bbox%merge_column, 2 )
            col_row = netcdf_coord_to_col_row(NCFILE=MODEL%NCFILE,  &
              rX=x( col ), rY=y( row ) )
              bbox%merge_column( col, row ) = col_row(1)
              bbox%merge_row( col, row ) = col_row(2)
          enddo
        enddo  
      end associate

    enddo

    call variable_list%set_autocleanup( FALSE )
    call crs_name_list%set_autocleanup( FALSE ) 
    call z_name_list%set_autocleanup( FALSE )
    call crs_value_list%set_autocleanup( FALSE ) 
    call z_value_list%set_autocleanup( FALSE )  
    call history_list%set_autocleanup( TRUE )  

    call history_list%print()

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

    SIM_DT%curr = SIM_DT%start
    SIM_DT%iDOY = day_of_year( SIM_DT%curr%getJulianDay() )

    SIM_DT%iDaysInMonth = SIM_DT%curr%dayspermonth()
    SIM_DT%iDaysInYear = SIM_DT%curr%daysperyear()
    SIM_DT%lIsLeapYear = SIM_DT%curr%isLeapYear()

  end subroutine initialize_start_and_end_dates

!--------------------------------------------------------------------------------------------------

  subroutine find_z_variable( variable_list, z_variable_name )

    type (STRING_LIST_T), intent(in)  :: variable_list  
    character (len=:), allocatable    :: z_variable_name
    
    ! [ LOCALS ]
    integer (kind=c_int) :: indx
    character (len=32)   :: variable_name

    do indx=1, variable_list%count

      variable_name = variable_list%get( indx ) 

      select case( variable_name )

        case ( "x", "y" )

        case ( "lon", "lat" )

        case ( "crs" )

        case ( "time")

        case default

          z_variable_name = variable_name
          exit

      end select

    enddo  

  end subroutine find_z_variable  

!--------------------------------------------------------------------------------------------------

end module swb_merge_initialize