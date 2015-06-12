module growing_degree_day

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_long_long
  use constants_and_conversions
  use exceptions
  use parameters
  use string_list
  implicit none

  private

  public :: GDD, GDD_BASE, GDD_MAX
  public growing_degree_day_calculate, growing_degree_day_initialize

  real (kind=c_float), allocatable  :: GDD(:)
  real (kind=c_float), allocatable  :: GDD_BASE(:)
  real (kind=c_float), allocatable  :: GDD_MAX(:)  

contains

  subroutine growing_degree_day_initialize( iNumActiveCells )

    integer (kind=c_int), intent(in)  :: iNumActiveCells

    ! [ LOCALS ]
    integer (kind=c_int)  :: iStat
    type (STRING_LIST_T)  :: slList
    real (kind=c_float), allocatable  :: fGDD_Base_(:)
    real (kind=c_float), allocatable  :: fGDD_Max_(:)

    allocate( GDD( iNumActiveCells ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( GDD_BASE( iNumActiveCells ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    allocate( GDD_MAX( iNumActiveCells ), stat=iStat )
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

end module growing_degree_day