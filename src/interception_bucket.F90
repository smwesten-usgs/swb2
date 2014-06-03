module interception_bucket

  use iso_c_binding
  use parameters
  implicit none

  type, public :: INTERCEPTION_BUCKET_T

    integer (kind=c_int), allocatable :: iLanduseCode(:)
    real (kind=c_float), allocatable  :: fInterceptionValue_GrowingSeason(:)
    real (kind=c_float), allocatable  :: fInterceptionValue_DormantSeason(:)

  contains

    procedure :: initialize_interception_bucket_sub
    generic   :: initialize => initialize_bucket_sub


  end type INTERCEPTION_BUCKET_T


  subroutine initialize_interception_bucket_sub(this)

  	class (INTERCEPTION_BUCKET_SUB_T), intent(inout)  :: this

    ! [ LOCALS ]
    type (STRING_LIST_T)             :: slREW, slTEW
    integer (kind=c_int)             :: iTEWSeqNums, iREWSeqNums
    integer (kind=c_int)             :: iNumberOfTEW, iNumberOfREW
    integer (kind=c_int)             :: iIndex
    character (len=:), allocatable   :: sText
    logical (kind=c_bool)            :: lAreLengthsEqual

   !> Determine how many landuse codes are present
   call PARAMS%get_values( "LU_Code", this%iLanduseCode )
   iNumberOfLanduses = count( this%iLanduseCode > 0 )

   call PARAMS%get_values( "Interception_Growing", this%fInterceptionValue_GrowingSeason(:) )
   call PARAMS%get_values( "Interception_Nongrowing", this%fInterceptionValue_DormantSeason(:) )

   lAreLengthsEqual = ( ( ubound(this%fInterceptionValue_GrowingSeason,1) == ubound(this%iLanduseCode,1) )  &
   	              .and. ( ubound(this%fInterceptionValue_DormantSeason,1) == ubound(this%iLanduseCode,1) )    )

   if ( .not. lAreLengthsEqual )     &
     call warn( "The number of landuses does not match the number of interception values.",   &
     	__FILE__, __LINE__, lFatal=.true._c_bool )

 end subroutine  

end module interception_bucket