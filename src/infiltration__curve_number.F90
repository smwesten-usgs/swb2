module infiltation__curve_number

  use iso_c_binding, only : c_int, c_float, c_double, c_bool

  implicit none

  private

  real (kind=c_float), allocatable    :: CN(:,:)
  integer (kind=c_int), allocatable   :: iLanduseCodes(:)

contains

  subroutine initialize_infiltration__curve_number()

    ! [ LOCALS ]
    integer (kind=c_int)              :: iNumberOfLanduses
    integer (kind=c_int)              :: iNumberOfSoilGroups
    integer (kind=c_int), allocatable :: iCurveNumberSeqNums(:)
    type (STRING_LIST_T)              :: slList
    integer (kind=c_int)              :: iStat
    integer (kind=c_int)              :: iSoilsIndex
    character (len=:), allocatable    :: sText

    call slList%append("LU_Code")
    call slList%append("Landuse_Lookup_Code")

    !> Determine how many soil groups are present

    ! retrieve a string list of all keys associated with curve number (i.e. "CN_1", "CN_2", "CN_3", etc)
    slCurveNumber = PARAMS%grep_keys("CN")
    ! Convert the string list to an vector of integers; this call strips off the "CN_" part of label
    iCurveNumberSeqNums = slCurveNumber%asInt()
    ! count how many items are present in the vector; this should equal the number of soils groups
    iNumberOfSoilGroups = count( iCurveNumberSeqNums > 0 )

    !> Determine how many landuse codes are present
    call PARAMS%get_values( slList, this%iLanduseCodes )
    iNumberOfLanduses = count( this%iLanduseCodes > 0 )

    allocate( CN(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for curve number table", &
      __FILE__, __LINE__)

    ! we should have the curve number table fully filled out following this block
    do iSoilsIndex = 1, iNumberOfSoilGroups
      sText = "CN_"//asCharacter(iSoilsIndex)
      call PARAMS%get_values( sText, CN(:, iSoilsIndex) )
    enddo  
    
  end subroutine initialize_infiltration__curve_number


  function return_landuse_index_fn(this, iLanduseCode)  result(iLanduseIndex)

    class (RUNOFF_CURVE_NUMBER_T)     :: this

    ! [ LOCALS ]
    integer (kind=c_int)   :: iLanduseIndex
    logical (kind=c_bool)  :: lMatch

    iLanduseIndex = 0
    lMatch = lFALSE

    do while (iIndex < ubound(this%iLanduseCodes, 1) )

      iLanduseIndex = iLanduseIndex + 1

      if (iLanduseCode == this%iLanduseCodes(iLanduseIndex) ) then
        lMatch = lTRUE
        exit
      endif
      
    end do

    if ( .not. lMatch ) &
       call warn("Failed to find a matching landuse code in tables. LU code = " &
          //asCharacter(iLanduseCode), lFatal=lTRUE)

  end function return_landuse_index_fn(this, iLanduseCode)


  function get_base_curve_number_fn(iLanduseIndex, iSoilsGroup )  result( fCN )

    integer (kind=c_int), intent(in)   :: iLanduseIndex
    integer (kind=c_int), intent(in)   :: iSoilsGroup
    real (kind=c_float)                :: fCN

    if (iLanduseIndex > ubound(CN, 1) .or. iSoilsGroup > ubound(CN, 2) ) &
      call die( "Index out of bounds. iLanduseIndex = "//asCharacter(iLanduseIndex) &
         //"; iSoilsGroup = "//asCharacter(iSoilsGroup), __FILE__, __LINE__ )
    
    fCN = CN( iLanduseIndex, iSoilsGroup )

  end function get_base_curve_number_fn

!--------------------------------------------------------------------------

  function prob_runoff_enhancement( fCFGI )    result(rPf)

    real (kind=c_float), intent(in) :: fCFGI
    real (kind=c_float)             :: fPf

    if(rCFGI <= CFGI_LL ) then
      rPf = rZERO
    elseif(rCFGI >= CFGI_UL) then
      rPf = rONE
    else
      rPf = ( rCFGI - CFGI_LL ) / ( CFGI_UL - CFGI_LL )
    end if

  end function prob_runoff_enhancement

!--------------------------------------------------------------------------

  function update_curve_number_fn( iLanduseIndex, iSoilsGroup, fInflow, fCFGI, &
    lIsGrowingSeason )  result( fCN_adj )
    
    integer (kind=c_int), intent(in)  :: iLanduseIndex
    integer (kind=c_int), intent(in)  :: iSoilsIndex
    real (kind=c_float), intent(in)   :: fInflow
    real (kind=c_float), intent(in)   :: fCFGI
    logical (kind=c_bool), intent(in) :: lIsGrowingSeason
    
    ! [ LOCALS ]
    real (kind=c_float) :: fPf
    real (kind=c_float) :: fCN

    fCN = this%get_curvenum( iLanduseIndex, iSoilsGroup )

    ! Correct the curve number...
    if( fCFGI > CFGI_LL ) then

       fPf = prob_runoff_enhancement( fCFGI )

       ! use probability of runoff enhancement to calculate a weighted
       ! average of curve number under Type II vs Type III antecedent
       ! runoff conditions
       fCN_adj = fCN * (1-rPf) + &
                    ( fCN / ( 0.427 + 0.00573 * fCN ) * rPf )

    else if ( lIsGrowingSeason ) then

      if ( rTotalInflow < pConfig%rDRY_GROWING ) then              ! AMC I

  !      The following comes from page 192, eq. 3.145 of "SCS Curve Number
  !      Methodology"

        fCN_adj = fCN / (2.281 - 0.01281 * fCN )

      else if ( rTotalInflow >= pConfig%rDRY_GROWING &
          .and. rTotalInflow < pConfig%rWET_GROWING ) then         ! AMC II

         fCN_adj = fCN

      else                                                         ! AMC III
        fCN_adj = fCN / ( 0.427 + 0.00573 * fCN )
      end if

    else ! dormant (non-growing) season

      if ( rTotalInflow < pConfig%rDRY_DORMANT ) then              ! AMC I

        fCN_adj = cel%rBaseCN / (2.281 - 0.01281 * cel%rBaseCN)

      else if ( fInflow >= pConfig%rDRY_DORMANT &
          .and. fInflow < pConfig%rWET_DORMANT ) then              ! AMC II

        fCN_adj = fCN

      else                                                         ! AMC III

        fCN_adj = fCN / ( 0.427 + 0.00573 * fCN )

      end if

    end if

    ! ensure that whatever modification have been made to the curve number
    ! remain within reasonable bounds
    fCN_adj = MIN( fCN_adj, 100_c_float ) 
    fCN_adj = MAX( fCN_adj, 0_c_float )

  end function update_curve_number_fn

!--------------------------------------------------------------------------

  function calculate_runoff_fn(fCN, fInflow )   result(fOutflow)

    !! Calculates a single cell's runoff using curve numbers
    
    real (kind=c_float) :: fInclow
    real (kind=c_float) :: rOutFlow
    ! [ LOCALS ]
    real (kind=c_float) :: rP
    real (kind=c_float) :: rCN_05
    real (kind=c_float) :: rSMax


    call runoff_UpdateCurveNumber(pConfig,cel,iJulDay)

    rSMax = (1000_c_float / cel%rAdjCN) - 10_c_float

    if(pConfig%iConfigureInitialAbstraction == &
                                        CONFIG_SM_INIT_ABSTRACTION_TR55) then

      if ( rP > rPOINT2*rSMax ) then
        rOutFlow = ( rP - 0.2_c_float * rSMax )**2  / (rP + 0.8_c_float * rSMax)
      else
        rOutFlow = 0_c_float
      end if

    else if(pConfig%iConfigureInitialAbstraction == &
                                     CONFIG_SM_INIT_ABSTRACTION_HAWKINS) then

      ! Equation 9, Hawkins and others, 2002
      rCN_05 = 100_c_float / &
        ((1.879_c_float * ((100_c_float / cel%rAdjCN) - 1_c_float )**1.15_c_float) +1_c_float)


      ! Equation 8, Hawkins and others, 2002
      rSMax = 1.33_c_float * ( rSMax ) ** 1.15_c_float

      ! now consider runoff if Ia ~ 0.05S
      if ( rP > 0.05_c_float * rSMax ) then
        rOutFlow = ( rP - 0.05_c_float * rSMax )**2  / (rP + 0.95_c_float*rSMax)
      else
        rOutFlow = 0_c_float
      end if

    else
      call Assert(lFALSE, "Illegal initial abstraction method specified" )
    end if

    return

  end function calculate_runoff_fn











end module infiltration__curve_number
