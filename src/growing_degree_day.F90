module growing_degree_day

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_long_long,   &
                            c_size_t, c_ptrdiff_t

  use constants_and_conversions
  use datetime                   , only : mmdd2doy
  use exceptions
  use netcdf4_support
  use parameters
  use simulation_datetime
  use string_list
  implicit none

  private

  public :: GDD, GDD_BASE, GDD_MAX, GDD_RESET_DATE
  public growing_degree_day_calculate, growing_degree_day_initialize

  real (kind=c_float), allocatable  :: GDD(:)
  real (kind=c_float), allocatable  :: GDD_BASE(:)
  real (kind=c_float), allocatable  :: GDD_MAX(:) 
  integer (kind=c_int), allocatable :: GDD_RESET_DATE(:)
  type (T_NETCDF4_FILE), pointer    :: pNCFILE
 
contains

  subroutine growing_degree_day_initialize( lActive, iLanduseIndex,          & 
                                            PROJ4_string, dX, dY,            &
                                            dX_lon, dY_lat,                  &
                                            output_directory_name )

    logical (kind=c_bool), intent(in)     :: lActive(:,:)
    integer (kind=c_int), intent(in)      :: iLanduseIndex(:)
    character (len=*), intent(inout)      :: PROJ4_string
    real (kind=c_double), intent(in)      :: dX(:)
    real (kind=c_double), intent(in)      :: dY(:)
    real (kind=c_double), intent(in)      :: dX_lon(:,:)
    real (kind=c_double), intent(in)      :: dY_lat(:,:)
    character (len=*), intent(in)         :: output_directory_name

    ! [ LOCALS ]
    integer (kind=c_int)              :: iStat
    integer (kind=c_int)              :: iIndex
    type (STRING_LIST_T)              :: slList
    type (STRING_LIST_T)              :: slGDD_Reset
    character (len=32)                :: sBuf
    real (kind=c_float), allocatable  :: fGDD_Base_(:)
    real (kind=c_float), allocatable  :: fGDD_Max_(:)
    integer (kind=c_int)              :: iNumberOfLanduses
    integer (kind=c_int), allocatable :: iLanduseCode(:)

    allocate( GDD( count( lActive ) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( GDD_BASE( count( lActive ) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( GDD_MAX( count( lActive ) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    !> create string list that allows for alternate heading identifiers for the landuse code
    call slList%append("LU_Code")
    call slList%append("Landuse_Code") 
    call slList%append("Landuse_Lookup_Code")

    !> Determine how many landuse codes are present
    call PARAMS%get_parameters( slKeys=slList, iValues=iLanduseCode )
    iNumberOfLanduses = count( iLanduseCode >= 0 )
    call slList%clear()

    call slList%append("GDD_Base_Temp")
    call slList%append("GDD_Base_Temperature")
    call slList%append("GDD_Base")

    call PARAMS%get_parameters( slKeys=slList, fValues=fGDD_Base_ )
    call slList%clear()

    call slList%append("GDD_Max_Temp")
    call slList%append("GDD_Maximum_Temperature")
    call slList%append("GDD_Maximum_Temp")
    call slList%append("GDD_Max")

    call PARAMS%get_parameters( slKeys=slList, fValues=fGDD_Max_ )
    call slList%clear()

    call slList%append("GDD_Reset_Date")
    call slList%append("GDD_Reset")

    call PARAMS%get_parameters( slKeys=slList, slValues=slGDD_Reset ) 
    call slList%clear()

    allocate( GDD_RESET_DATE( iNumberOfLanduses ), stat=iStat )
    call assert( iStat==0, "Problem allocating memory.", __FILE__, __LINE__ )


    if ( slGDD_Reset%count == iNumberOfLanduses ) then

      do iIndex=1, slGDD_Reset%count
        sBuf = slGDD_Reset%get( iIndex )
        GDD_RESET_DATE( iIndex ) = mmdd2doy( sBuf )
      enddo 
      
    else
    
      ! if no GDD_RESET_DATE found in parameter tables, assign the default: reset GDD at end of calendar year
      GDD_RESET_DATE = 365_c_int  

    endif   

    
    if ( ubound( fGDD_Max_, 1 ) == iNumberOfLanduses ) then

      do iIndex=1, ubound( iLanduseIndex, 1)
        GDD_MAX( iIndex ) = fGDD_Max_( iLanduseIndex( iIndex ) )
      enddo 
      
    else
    
      ! if no GDD_MAX found in parameter tables, assign the default FAO-56 value
      GDD_MAX = 86.0_c_float  

    endif   


    if ( ubound( fGDD_Base_, 1 ) == iNumberOfLanduses ) then

      do iIndex=1, ubound( iLanduseIndex, 1)
        GDD_BASE( iIndex ) = fGDD_Base_( iLanduseIndex( iIndex ) )
      enddo 
      
    else
    
      ! if no GDD_Base found in parameter tables, assign the default FAO-56 value
      GDD_BASE = 50.0_c_float  

    endif   

    GDD = 0.0_c_float 

    allocate ( pNCFILE, stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    call netcdf_open_and_prepare_as_output( NCFILE=pNCFILE, sVariableName="growing_degree_day", &
      sVariableUnits="degree-days Fahrenheit", iNX=ubound(lActive, 1), iNY=ubound(lActive, 2),  &
      fX=dX, fY=dY,                                                                             &
      StartDate=SIM_DT%start, EndDate=SIM_DT%end, PROJ4_string=PROJ4_string,                    &
      dpLat=dY_lat, dpLon=dX_lon, fValidMin=0.0, fValidMax=9000.0,                              &
      sDirName=output_directory_name )

  end subroutine growing_degree_day_initialize

!--------------------------------------------------------------------------------------------------

  elemental subroutine growing_degree_day_calculate( fGDD, iGDD_Reset_DOY, fTMean, fT_GDD_Base, fT_GDD_Max )

    ! [ ARGUMENTS ]
    real (kind=c_float), intent(inout)       :: fGDD
    integer (kind=c_int), intent(in)         :: iGDD_Reset_DOY
    real (kind=c_float), intent(in)          :: fTMean
    real (kind=c_float), intent(in)          :: fT_GDD_Base
    real (kind=c_float), intent(in)          :: fT_GDD_Max

    ! [ LOCALS ]
    real (kind=c_float) :: fDelta

    if ( SIM_DT%iDOY == iGDD_Reset_DOY ) fGDD = 0.0_c_float

    fDelta = min(fTMean, fT_GDD_Max) - fT_GDD_Base

    if ( fDelta > 0.0_c_float ) fGDD = fGDD + fDelta

  end subroutine growing_degree_day_calculate 

!--------------------------------------------------------------------------------------------------

  subroutine growing_degree_day_output( lActive, nodata_fill_value )

    logical (kind=c_bool), intent(in)      :: lActive(:,:)
    real (kind=c_float), intent(in)        :: nodata_fill_value(:,:)

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

    iNX = ubound(nodata_fill_value, 1)
    iNY = ubound(nodata_fill_value, 2)

    associate ( dt => SIM_DT%curr )

!       iJulianDay = dt%getJulianDay()
!       iMonth = asInt( dt%iMonth )
!       iDay = asInt( dt%iDay )
!       iYear = dt%iYear
      iDaysInMonth = SIM_DT%iDaysInMonth
      iNumDaysFromOrigin = SIM_DT%iNumDaysFromOrigin

        ! write timestamp to NetCDF file
      call netcdf_put_variable_vector(NCFILE=pNCFILE, &
        iVarID=pNCFILE%iVarID(NC_TIME), &
        iStart=[int( iNumDaysFromOrigin, kind=c_size_t)], &
        iCount=[1_c_size_t], &
        iStride=[1_c_ptrdiff_t], &
        dpValues=[real( iNumDaysFromOrigin, kind=c_double)])

      call netcdf_put_packed_variable_array(NCFILE=pNCFILE, &
                   iVarID=pNCFILE%iVarID(NC_Z), &
                   iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t], &
                   iCount=[1_c_size_t, int(iNY, kind=c_size_t), int(iNX, kind=c_size_t)],              &
                   iStride=[1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t],                         &
                   rValues=GDD, lMask=lActive, rField=nodata_fill_value )

    end associate

  end subroutine growing_degree_day_output

end module growing_degree_day