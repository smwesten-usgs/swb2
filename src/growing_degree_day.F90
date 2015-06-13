module growing_degree_day

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_long_long,   &
                            c_size_t, c_ptrdiff_t

  use constants_and_conversions
  use exceptions
  use netcdf4_support
  use parameters
  use simulation_datetime
  use string_list
  implicit none

  private

  public :: GDD, GDD_BASE, GDD_MAX
  public growing_degree_day_calculate, growing_degree_day_initialize

  real (kind=c_float), allocatable  :: GDD(:)
  real (kind=c_float), allocatable  :: GDD_BASE(:)
  real (kind=c_float), allocatable  :: GDD_MAX(:) 
  type (T_NETCDF4_FILE), pointer    :: pNCFILE
 
contains

  subroutine growing_degree_day_initialize( lActive, dX, dY, dX_lon, dY_lat )

    logical (kind=c_bool), intent(in)     :: lActive(:,:)
    real (kind=c_double), intent(in)      :: dX(:)
    real (kind=c_double), intent(in)      :: dY(:)
    real (kind=c_double), intent(in)      :: dX_lon(:,:)
    real (kind=c_double), intent(in)      :: dY_lat(:,:)

    ! [ LOCALS ]
    integer (kind=c_int)  :: iStat
    type (STRING_LIST_T)  :: slList
    real (kind=c_float), allocatable  :: fGDD_Base_(:)
    real (kind=c_float), allocatable  :: fGDD_Max_(:)

    allocate( GDD( count( lActive ) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( GDD_BASE( count( lActive ) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( GDD_MAX( count( lActive ) ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    call slList%append("GDD_Base_Temp")
    call slList%append("GDD_Base_Temperature")
    call slList%append("GDD_Base")

    call PARAMS%get_parameters( slKeys=slList, fValues=fGDD_Base_, lFatal=lTRUE )

    call slList%clear()

    call slList%append("GDD_Max_Temp")
    call slList%append("GDD_Maximum_Temperature")
    call slList%append("GDD_Maximum_Temp")
    call slList%append("GDD_Max")

    call PARAMS%get_parameters( slKeys=slList, fValues=fGDD_Max_, lFatal=lTRUE )

    call slList%clear()

    GDD = 0.0_c_float 

    allocate ( pNCFILE, stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    call netcdf_open_and_prepare_as_output( NCFILE=pNCFILE, sVariableName="growing_degree_day", &
      sVariableUnits="degree-days Fahrenheit", iNX=ubound(lActive, 1), iNY=ubound(lActive, 2), &
      fX=dX, fY=dY, StartDate=SIM_DT%start, EndDate=SIM_DT%end, dpLat=dY_lat, dpLon=dX_lon, &
      fValidMin=0.0, fValidMax=7000.0   )

  end subroutine growing_degree_day_initialize

!--------------------------------------------------------------------------------------------------

  elemental subroutine growing_degree_day_calculate( fGDD, fTMean, fT_GDD_Base, fT_GDD_Max )

    ! [ ARGUMENTS ]
    real (kind=c_float), intent(inout)       :: fGDD
    real (kind=c_float), intent(in)          :: fTMean
    real (kind=c_float), intent(in)          :: fT_GDD_Base
    real (kind=c_float), intent(in)          :: fT_GDD_Max

    ! [ LOCALS ]
    real (kind=c_float) :: fDelta

    fDelta = min(fTMean, fT_GDD_Max) - fT_GDD_Base

    if ( fDelta > 0.0_c_float ) then

      fGDD = fGDD + fDelta

    else

      fGDD = 0.0_c_float

    end if

  end subroutine growing_degree_day_calculate 

!--------------------------------------------------------------------------------------------------

  subroutine growing_degree_day_output( lActive, fDont_Care )

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
                   rValues=GDD, lMask=lActive, rField=fDont_Care )

    end associate

  end subroutine growing_degree_day_output

end module growing_degree_day