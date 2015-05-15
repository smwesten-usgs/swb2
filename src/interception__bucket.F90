module interception__bucket

  use iso_c_binding
  use exceptions
  use parameters
  use strings
  implicit none

  private

  public interception_bucket_initialize
  public interception_bucket_calculate

  logical (kind=c_bool) :: GROWING_SEASON = .true._c_bool

  integer (kind=c_int), allocatable :: iLanduseCodes(:)
  real (kind=c_float), allocatable  :: fInterceptionValue_GrowingSeason(:)
  real (kind=c_float), allocatable  :: fInterceptionValue_DormantSeason(:)

contains   

  subroutine interception_bucket_initialize()

    ! [ LOCALS ]
    integer (kind=c_int)        :: iNumberOfLanduses
    logical (kind=c_bool)       :: lAreLengthsEqual

    !> Determine how many landuse codes are present
    call PARAM_DICT%get_values( sKey="LU_Code", iValues=iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes > 0 )
    
    call PARAM_DICT%get_values( sKey="Interception_Growing" , fValues=fInterceptionValue_GrowingSeason )
    call PARAM_DICT%get_values( sKey="Interception_Nongrowing", fValues=fInterceptionValue_DormantSeason )

    lAreLengthsEqual = ( ( ubound(fInterceptionValue_GrowingSeason,1) == ubound(iLanduseCodes,1) )  &
                  .and. ( ubound(fInterceptionValue_DormantSeason,1) == ubound(iLanduseCodes,1) )    )

    if ( .not. lAreLengthsEqual )     &
      call warn( sMessage="The number of landuses does not match the number of interception values.",   &
        sModule=__FILE__, iLine=__LINE__, lFatal=.true._c_bool )

  end subroutine interception_bucket_initialize


  elemental function interception_bucket_calculate( iLanduseIndex, fPrecip, fFog )   result( fInterception )

    integer (kind=c_int), intent(in) :: iLanduseIndex
    real (kind=c_float), intent(in)  :: fPrecip
    real (kind=c_float), intent(in)  :: fFog
    real (kind=c_float)              :: fInterception

    !!! Need to come up with a module that provides the current day of year, month, day, year, and growing season

    ! [ LOCALS ]
    real (kind=c_float) :: fPotentialInterception

    if (GROWING_SEASON) then

      fPotentialInterception = fInterceptionValue_GrowingSeason(iLanduseIndex)

    else

      fPotentialInterception = fInterceptionValue_DormantSeason(iLanduseIndex)

    endif

    fInterception = min( fPotentialInterception, fPrecip + fFog )

 
  end function interception_bucket_calculate

end module interception__bucket