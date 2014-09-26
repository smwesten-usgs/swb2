module fog__monthly_grid

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use dictionary
  use file_operations
  use netcdf4_support
  use simulation_datetime
  use strings
  use string_list

  implicit none

  private

  public :: fog_monthly_grid_initialize, fog_monthly_grid_calculate, pFOG_ZONE

  type (DATA_CATALOG_ENTRY_T), pointer :: pFOG_ZONE        ! data catalog object => FOG_ZONE grid
  integer (kind=c_int), allocatable    :: FOG_ZONE_ID(:)   ! local vector of FOG_ZONE ID values
  type (DATA_CATALOG_ENTRY_T), pointer :: pFOG_ELEV        ! data catalog object => FOG_ELEV grid
  integer (kind=c_int), allocatable    :: FOG_ELEV_ID(:)   ! local vector of FOG_ELEV_ID values
  type (T_NETCDF4_FILE), pointer       :: pNCFILE          ! pointer to OUTPUT NetCDF file
  real (kind=c_float), allocatable     :: FOG(:)           ! 

  type, public :: RATIOS_T
    integer (kind=c_int) :: iFogZoneID
    integer (kind=c_int) :: iFogElevID
    real (kind=c_float)  :: fRatioValue(12)
  end type RATIOS_T

  type (RATIOS_T), allocatable, target, public       :: RATIOS(:)


contains

  subroutine fog_monthly_grid_initialize( lActive, dX, dY, dX_lon, dY_lat )

    logical (kind=c_bool), intent(in)     :: lActive(:,:)
    real (kind=c_double), intent(in)      :: dX(:)
    real (kind=c_double), intent(in)      :: dY(:)
    real (kind=c_double), intent(in)      :: dX_lon(:,:)
    real (kind=c_double), intent(in)      :: dY_lat(:,:)

    ! [ LOCALS ]
    integer (kind=c_int)                 :: iStat
    type (STRING_LIST_T)                 :: slString
    integer (kind=c_int)                 :: iIndex 
    integer (kind=c_int)                 :: iNX
    integer (kind=c_int)                 :: iNY


    iNX = ubound(lActive, 1)
    iNY = ubound(lActive, 2)

    ! locate the data structure associated with the gridded rainfall zone entries
    pFOG_ZONE => DAT%find("FOG_ZONE")
    if ( .not. associated(pFOG_ZONE) ) &
        call die("A FOG_ZONE grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

    allocate( FOG_ZONE_ID( count (lActive ) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )


    ! locate the data structure associated with the gridded rainfall zone entries
    pFOG_ELEV => DAT%find("FOG_ELEVATION")
    if ( .not. associated(pFOG_ELEV) ) &
        call die("A FOG_ELEVATION grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

    allocate( FOG_ELEV_ID( count (lActive ) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )


    allocate( FOG( count (lActive ) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    call pFOG_ZONE%getvalues()
    call pFOG_ELEV%getvalues()

    FOG_ZONE_ID = pack( pFOG_ZONE%pGrdBase%iData, lActive )
    FOG_ELEV_ID = pack( pFOG_ELEV%pGrdBase%iData, lActive )

    ! look up the name of the fragments file in the control file dictionary
    call CF_DICT%get_values( sKey="FOG_RATIOS_MONTHLY_FILE", slString=slString )

    call read_monthly_fog_ratios( slString%get(1) )

    allocate ( pNCFILE, stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    call netcdf_open_and_prepare_as_output( NCFILE=pNCFILE, sVariableName="fog", &
      sVariableUnits="inches", iNX=iNX, iNY=iNY, &
      fX=dX, fY=dY, StartDate=SIM_DT%start, EndDate=SIM_DT%end, dpLat=dY_lat, dpLon=dX_lon  )


  end subroutine fog_monthly_grid_initialize

!--------------------------------------------------------------------------------------------------

  subroutine read_monthly_fog_ratios( sFilename )

    character (len=*), intent(in)    :: sFilename

    ! [ LOCALS ]
    character (len=512)   :: sRecord, sSubstring
    integer (kind=c_int)  :: iStat
    integer (kind=c_int)  :: iCount
    integer (kind=c_int)  :: iIndex
    integer (kind=c_int)  :: iNumLines  
    type (ASCII_FILE_T)   :: RATIOS_FILE


    call RATIOS_FILE%open( sFilename = sFilename, &
                  sCommentChars = "#%!", &
                  sDelimiters = "WHITESPACE", &
                  lHasHeader = .false._c_bool )

    iNumLines = RATIOS_FILE%numLines()

    allocate(  RATIOS( iNumLines ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory for fog ratios table", &
      __FILE__, __LINE__ )

    iCount = 0

    do 

      ! read in next line of file
      sRecord = RATIOS_FILE%readLine()

      if ( RATIOS_FILE%isEOF() ) exit 

      iCount = iCount + 1

      ! read in rain gage zone
      call chomp(sRecord, sSubstring, RATIOS_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 ) &
      call die( "Missing fog zone number in the monthly fog ratios file", &
        __FILE__, __LINE__, "Problem occured on line number "//asCharacter(RATIOS_FILE%currentLineNum() ) &
        //" of file "//dquote(sFilename) )

      RATIOS(iCount)%iFogZoneID = asInt(sSubString)

      ! read in elevation ID number for this zone
      call chomp(sRecord, sSubstring, RATIOS_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 ) &
      call die( "Missing elevation ID number in the monthly fog ratios file", &
        __FILE__, __LINE__, "Problem occured on line number "//asCharacter(RATIOS_FILE%currentLineNum() ) &
        //" of file "//dquote(sFilename) )

      RATIOS(iCount)%iFogElevID = asInt(sSubString)
      
      do iIndex = 1, 12

        ! read in fraction for given  month
        call chomp(sRecord, sSubstring, RATIOS_FILE%sDelimiters )

        if ( len_trim(sSubstring) == 0 ) &
        call die( "Missing fog fraction value in the monthly fog ratios file", &
          __FILE__, __LINE__, "Problem occured on line number "//asCharacter(RATIOS_FILE%currentLineNum() ) &
          //" of file "//dquote(sFilename) )

        RATIOS(iCount)%fRatioValue(iIndex) = asFloat( sSubstring )

      enddo
      
    enddo    

    call LOGS%write("Maximum fog zone number in for monthly fog ratios file: "  &
        //asCharacter(maxval(RATIOS%iFogZoneID)), &
        iTab=31, iLinesAfter=1, iLogLevel=LOG_ALL)

  end subroutine read_monthly_fog_ratios

!--------------------------------------------------------------------------------------------------

  subroutine fog_monthly_grid_calculate( fRainfall, lActive, fDont_Care )

    real (kind=c_float), intent(in)        :: fRainfall(:)
    logical (kind=c_bool), intent(in)      :: lActive(:,:)
    real (kind=c_float), intent(in)        :: fDont_Care(:,:)

    ! [ LOCALS ] 
    integer (kind=c_int) :: iJulianDay
    integer (kind=c_int) :: iMonth
    integer (kind=c_int) :: iDay
    integer (kind=c_int) :: iYear
    integer (kind=c_int) :: iDaysInMonth
    integer (kind=c_int) :: iNumDaysFromOrigin
    integer (kind=c_int)                 :: iNX
    integer (kind=c_int)                 :: iNY
    integer (kind=c_int) :: iIndex
    real (kind=c_float)  :: fFactor

    iNX = ubound(fDont_Care, 1)
    iNY = ubound(fDont_Care, 2)

    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear
      iDaysInMonth = SIM_DT%iDaysInMonth
      iNumDaysFromOrigin = SIM_DT%iNumDaysFromOrigin
  
!      call pMONTHLY_FOG_FRACTION%getvalues( iMonth, iDay, iYear, iJulianDay )

      ! write timestamp to NetCDF file
      call netcdf_put_variable_vector(NCFILE=pNCFILE, &
      iVarID=pNCFILE%iVarID(NC_TIME), &
      iStart=[int( iNumDaysFromOrigin, kind=c_size_t)], &
      iCount=[1_c_size_t], &
      iStride=[1_c_ptrdiff_t], &
      dpValues=[real( iNumDaysFromOrigin, kind=c_double)])

      FOG = 0.0_c_float

      do iIndex=1,ubound(RATIOS, 1)

        fFactor = RATIOS(iIndex)%fRatioValue( iMonth )

        where ( ( FOG_ZONE_ID == RATIOS(iIndex)%iFogZoneID )      &
          .and. ( FOG_ELEV_ID == RATIOS(iIndex)%iFogElevID ) )

          FOG = fRainfall * fFactor

        end where  

      enddo

      call netcdf_put_packed_variable_array(NCFILE=pNCFILE, &
                   iVarID=pNCFILE%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(iNY, kind=c_size_t), int(iNX, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=FOG, lMask=lActive, rField=fDont_Care )
     
    end associate


  end subroutine fog_monthly_grid_calculate

end module fog__monthly_grid
