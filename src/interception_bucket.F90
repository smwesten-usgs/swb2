module interception_bucket

  use iso_c_binding
  use exceptions
  use parameters
  use strings
  implicit none

  private

  public calculate_interception_bucket, initialize_interception_bucket

  logical (kind=c_bool) :: GROWING_SEASON = .true._c_bool

  integer (kind=c_int), allocatable :: iLanduseCodes(:)
  real (kind=c_float), allocatable  :: fInterceptionValue_GrowingSeason(:)
  real (kind=c_float), allocatable  :: fInterceptionValue_DormantSeason(:)

contains   

  subroutine initialize_interception_bucket()

    ! [ LOCALS ]
    integer (kind=c_int)        :: iNumberOfLanduses
    logical (kind=c_bool)       :: lAreLengthsEqual

    !> Determine how many landuse codes are present
    call PARAMS%get_values( sKey="LU_Code", iValues=iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes > 0 )
    
    call PARAMS%get_values( sKey="Interception_Growing" , fValues=fInterceptionValue_GrowingSeason )
    call PARAMS%get_values( sKey="Interception_Nongrowing", fValues=fInterceptionValue_DormantSeason )

    lAreLengthsEqual = ( ( ubound(fInterceptionValue_GrowingSeason,1) == ubound(iLanduseCodes,1) )  &
                  .and. ( ubound(fInterceptionValue_DormantSeason,1) == ubound(iLanduseCodes,1) )    )

    if ( .not. lAreLengthsEqual )     &
      call warn( sMessage="The number of landuses does not match the number of interception values.",   &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

  end subroutine initialize_interception_bucket


  function initialize_interception_bucket_python_sub( n, fInterception_GrowingSeason, &
      fInterception_DormantSeason )    result(iResCode) bind(c)

    integer (kind=c_int), intent(in)   :: n
    real (kind=c_float), intent(in)    :: fInterception_GrowingSeason(n)
    real (kind=c_float), intent(in)    :: fInterception_DormantSeason(n)
    integer (kind=c_int)               :: iResCode

    fInterceptionValue_GrowingSeason = fInterception_GrowingSeason
    fInterceptionValue_DormantSeason = fInterception_DormantSeason
    iResCode = 0

  end function initialize_interception_bucket_python_sub


  function calculate_interception_bucket( iLanduseIndex, fPrecip )   result( fInterception )  bind(c)

    integer (kind=c_int), intent(in) :: iLanduseIndex
    real (kind=c_float), intent(in)  :: fPrecip
    real (kind=c_float)              :: fInterception

    !!! Need to come up with a module that provides the current day of year, month, day, year, and growing season

    ! [ LOCALS ]
    real (kind=c_float) :: fPotentialInterception

    if ( iLanduseIndex > ubound( fInterceptionValue_GrowingSeason, 1) ) &
      call die( "Internal programming error -- index out of bounds", __FILE__, __LINE__ )

    if (GROWING_SEASON) then

      fPotentialInterception = fInterceptionValue_GrowingSeason(iLanduseIndex)

    else

      fPotentialInterception = fInterceptionValue_DormantSeason(iLanduseIndex)

    endif

    fInterception = min( fPotentialInterception, fPrecip )

 
  end function calculate_interception_bucket

end module interception_bucket