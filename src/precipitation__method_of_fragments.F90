module precipitation__method_of_fragments

  use iso_c_binding
  implicit none
  
contains


  subroutine initialize_precipitation_method_of_fragments(iNumActiveCells )

    integer (kind=c_int), intent(in)  :: iNumActiveCells

    ! [ LOCALS ]
    integer (kind=c_int)        :: iNumActiveCells
    integer (kind=c_int)        :: iStat

    allocate( XXXX( iNumActiveCells ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

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

  end subroutine initialize_precipitation_method_of_fragments




end module precipitation__method_of_fragments