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

  contains

    procedure :: initialize_interception_bucket_sub
    generic   :: initialize => initialize_bucket_sub

  end type INTERCEPTION_BUCKET_T

  type (INTERCEPTION_BUCKET_T) :: DAT

contains   

  subroutine initialize_interception_bucket_sub(this)

    class (CELL_NORMAL_T), intent(inout)  :: this

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

  end subroutine  


  subroutine calculate_interception_bucket_sub(this)

    class (CELL_NORMAL_T), intent(inout)   :: this

    !!! Need to come up with a module that provides the current day of year, month, day, year, and growing season

    if ( this%iLanduseIndex > ubound( DAT%fInterceptionValue_GrowingSeason, 1) &
      call die( "Internal programming error -- index out of bounds", __FILE__, __LINE__ )

    if (GROWING_SEASON) then

      this%fInterception = DAT%fInterceptionValue_GrowingSeason(this%iLanduseIndex)

    else

      this%fInterception = DAT%fInterceptionValue_DormantSeason(this%iLanduseIndex)

    endif
 

end module interception_bucket