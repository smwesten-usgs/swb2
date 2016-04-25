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
  use output, only                       : initialize_output
  use swb_merge_domain
  use simulation_datetime, only : SIM_DT
  use strings
  use string_list  
  implicit none

  private

  public :: initialize_all

contains

  subroutine initialize_all( filelist )

    type (STRING_LIST_T), intent(in)    :: filelist

    ! [ LOCALS ]
    integer (kind=c_int)            :: iIndex
    type (STRING_LIST_T)            :: slList
    character (len=:), allocatable  :: sBuf


!!  read metadata from each netcdf file

!! calculate bounding box around all netcdf files





    ! define SWB project boundary and geographic projection
    call initialize_grid_options( xll, yll, xur, yur, resolution, proj4 )

    ! define the start and end date for the simulation
    call initialize_start_and_end_dates()
        

    call initialize_latitude()

  end subroutine initialize_all

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

          call warn("Unknown directive present, line "//asCharacter(__LINE__)//", file "//__FILE__ &
            //". Ignoring. Directive is: "//dquote(sCmdText), iLogLevel=LOG_DEBUG )

      end select

    enddo

    if ( lHaveStartDate .and. lHaveEndDate ) then

      SIM_DT%curr = SIM_DT%start
      SIM_DT%iDOY = day_of_year( SIM_DT%curr%getJulianDay() )

      SIM_DT%iDaysInMonth = SIM_DT%curr%dayspermonth()
      SIM_DT%iDaysInYear = SIM_DT%curr%daysperyear()
      SIM_DT%lIsLeapYear = SIM_DT%curr%isLeapYear()

      call LOGS%write("Model run start date set to: "//SIM_DT%start%prettydate(), iTab=4)
      call LOGS%write("Model run end date set to:   "//SIM_DT%end%prettydate(), iTab=4)

    else
    
      call warn(sMessage="Your control file seems to be missing START_DATE and/or END_DATE", &
        sHints="Add a START_DATE and/or END_DATE directive to your control file. Date "      &
        //"~should be specified as mm/dd/yyyy.", lFatal = lTRUE, iLogLevel = LOG_ALL,        &
        lEcho = lTRUE )
  
    endif

  end subroutine initialize_start_and_end_dates 

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

end module swb_merge_initialize