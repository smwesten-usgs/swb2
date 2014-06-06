module interception_bucket

  use iso_c_binding
  use cell_class
  use parameters
  implicit none

 !! error !! we have cell_class using interception_bucket, and interception_bucket using cell_class

  private

  type, public :: INTERCEPTION_BUCKET_T

    integer (kind=c_int), allocatable :: iLanduseCode(:)
    real (kind=c_float), allocatable  :: fInterceptionValue_GrowingSeason(:)
    real (kind=c_float), allocatable  :: fInterceptionValue_DormantSeason(:)

  end type INTERCEPTION_BUCKET_T

  type (INTERCEPTION_BUCKET_T) :: DAT

contains   

  subroutine initialize_interception_bucket_sub()

    ! [ LOCALS ]
    integer (kind=c_int)        :: iNumberOfLanduses
    logical (kind=c_bool)       :: lAreLengthsEqual

    !> Determine how many landuse codes are present
    call PARAMS%get_values( "LU_Code", DAT%iLanduseCode )
    iNumberOfLanduses = count( DAT%iLanduseCode > 0 )

    call PARAMS%get_values( "Interception_Growing", DAT%fInterceptionValue_GrowingSeason(:) )
    call PARAMS%get_values( "Interception_Nongrowing", DAT%fInterceptionValue_DormantSeason(:) )

    lAreLengthsEqual = ( ( ubound(DAT%fInterceptionValue_GrowingSeason,1) == ubound(DAT%iLanduseCode,1) )  &
                  .and. ( ubound(DAT%fInterceptionValue_DormantSeason,1) == ubound(DAT%iLanduseCode,1) )    )

    if ( .not. lAreLengthsEqual )     &
      call warn( "The number of landuses does not match the number of interception values.",   &
        __FILE__, __LINE__, lFatal=.true._c_bool )

  end subroutine initialize_interception_bucket_sub


  function calculate_interception_bucket( iLanduseIndex, fPrecip )   result( fInterception )

    integer (kind=c_int), intent(in) :: iLanduseIndex
    real (kind=c_float), intent(in)  :: fPrecip
    real (kind=c_float)              :: fInterception

    !!! Need to come up with a module that provides the current day of year, month, day, year, and growing season

    ! [ LOCALS ]
    feal (kind=c_float) :: fPotentialInterception

    if ( iLanduseIndex > ubound( DAT%fInterceptionValue_GrowingSeason, 1) &
      call die( "Internal programming error -- index out of bounds", __FILE__, __LINE__ )

    if (GROWING_SEASON) then

      fPotentialInterception = DAT%fInterceptionValue_GrowingSeason(iLanduseIndex)

    else

      fPotentialInterception = DAT%fInterceptionValue_DormantSeason(iLanduseIndex)

    endif

    fInterception = min( fPotentialInterception, fPrecip )

 
  end function calculate_interception_bucket

end module interception_bucket