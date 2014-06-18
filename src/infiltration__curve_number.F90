module infiltration__curve_number

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use string_list
  use parameters
  implicit none

  private

  real (kind=c_float), allocatable    :: CN(:,:)
  real (kind=c_float), allocatable    :: Smax(:,:)
  real (kind=c_float), allocatable    :: Scurr(:)
  integer (kind=c_int), allocatable   :: iLanduseCodes(:)

  public :: initialize_infiltration__curve_number

contains

  subroutine initialize_infiltration__curve_number( iNumberOfActiveCells )

    integer (kind=c_int), intent(in)  :: iNumberOfActiveCells

    ! [ LOCALS ]
    integer (kind=c_int)              :: iNumberOfLanduses
    integer (kind=c_int)              :: iNumberOfSoilGroups
    integer (kind=c_int), allocatable :: iCurveNumberSeqNums(:)
    type (STRING_LIST_T)              :: slList
    integer (kind=c_int)              :: iStat
    integer (kind=c_int)              :: iSoilsIndex
    character (len=:), allocatable    :: sText
    real (kind=c_float), allocatable  :: CN_AMCI(:,:)

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

    allocate( CN_AMCI(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for curve number AMCI table", &
      __FILE__, __LINE__)

    allocate( Smax(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for curve number SMax table", &
      __FILE__, __LINE__)

    allocate( Scurr( iNumberOfActiveCells), stat=iStat )

    ! we should have the curve number table fully filled out following this block
    do iSoilsIndex = 1, iNumberOfSoilGroups
      sText = "CN_"//asCharacter(iSoilsIndex)
      call PARAMS%get_values( sText, CN(:, iSoilsIndex) )
    enddo  

    ! calculate SMax based on CN for AMC I
    CN_AMCI = CN_II_to_CN_I( CN )
    Smax = ( 1000.0_c_float / CN_AMCI ) - 10.0_c_float
    
  end subroutine initialize_infiltration__curve_number

!--------------------------------------------------------------------------------------------------

  function return_landuse_index_fn(iLanduseCode)      result(iLanduseIndex)

    integer (kind=c_int), intent(in)         :: iLanduseCode
    integer (kind=c_int)                     :: iLanduseIndex

    ! [ LOCALS ]
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

  end function return_landuse_index_fn(this, iLanduseCode)

!--------------------------------------------------------------------------------------------------

  elemental function get_base_curve_number_fn(iLanduseIndex, iSoilsGroup )  result( fCN )

    integer (kind=c_int), intent(in)   :: iLanduseIndex
    integer (kind=c_int), intent(in)   :: iSoilsGroup
    real (kind=c_float)                :: fCN
   
    fCN = CN( iLanduseIndex, iSoilsGroup )

  end function get_base_curve_number_fn

!--------------------------------------------------------------------------------------------------

  elemental function prob_runoff_enhancement( fCFGI )    result(rPf)

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

  elemental function update_soil_moist_index_fn( fS_prev, fPET, fInflow, fRunoff )    result( fS_current )

    real (kind=c_float), intent(in)  :: fS_prev
    real (kind=c_float), intent(in)  :: fPET   ! Reference_ET0_adj
    real (kind=c_float), intent(in)  :: fInflow
    real (kind=c_float), intent(in)  :: fRunoff
    real (kind=c_float)              :: fS_current

    fS_current = fS_prev + 

  end function update_soil_moist_index_fn

!--------------------------------------------------------------------------------------------------

  function update_curve_number_fn( iLanduseIndex, iSoilsGroup, fInflow, fCFGI, &
    lIsGrowingSeason )  result( fCN_adj )
    
    integer (kind=c_int), intent(in)  :: iLanduseIndex
    integer (kind=c_int), intent(in)  :: iSoilsIndex
    real (kind=c_float), intent(in)   :: fInflow
    real (kind=c_float), intent(in)   :: fCFGI
    logical (kind=c_bool), intent(in) :: lIsGrowingSeason
    
    ! [ LOCALS ]
    real (kind=c_float) :: Pf
    real (kind=c_float) :: CN_base

    CN_base = this%get_curvenum( iLanduseIndex, iSoilsGroup )

    ! Correct the curve number...
    if( fCFGI > CFGI_LL ) then

       Pf = prob_runoff_enhancement( fCFGI )

       ! use probability of runoff enhancement to calculate a weighted
       ! average of curve number under Type II vs Type III antecedent
       ! runoff conditions
       CN_adj = CN_base * (1.0_c_float - Pf) + CN_II_to_III(CN_base) * Pf 

    else if ( lIsGrowingSeason ) then

      if ( rInflow < pConfig%rDRY_GROWING ) then              ! AMC I

  !      The following comes from page 192, eq. 3.145 of "SCS Curve Number
  !      Methodology"

        CN_adj = CN_II_to_CN_I( fCN )

      else if ( fInflow >= pConfig%rDRY_GROWING &
          .and. fInflow < pConfig%rWET_GROWING ) then         ! AMC II

         fCN_adj = fCN

      else                                                         ! AMC III
        fCN_adj = CN_II_to_CN_III( fCN )
      end if

    else ! dormant (non-growing) season

      if ( rInflow < pConfig%rDRY_DORMANT ) then              ! AMC I

        fCN_adj = CN_II_to_CN_I( fCN )

      else if ( fInflow >= pConfig%rDRY_DORMANT &
          .and. fInflow < pConfig%rWET_DORMANT ) then              ! AMC II

        fCN_adj = fCN

      else                                                         ! AMC III

        fCN_adj = CN_II_to_CN_III( fCN )

      end if

    end if

    ! ensure that whatever modification have been made to the curve number
    ! remain within reasonable bounds
    fCN_adj = MIN( fCN_adj, 100_c_float ) 
    fCN_adj = MAX( fCN_adj, 0_c_float )

  end function update_curve_number_fn

!--------------------------------------------------------------------------------------------------

  elemental function CN_II_to_CN_III(CN_II)   result(CN_III)

    real (kind=c_float), intent(in)  :: CN_II
    real (kind=c_float)              :: CN_III

    CN_III = CN_II / ( 0.427_c_float + 0.00573_c_float * CN_II )

    CN_III = max( CN_III,0.0_c_float )
    CN_III = min( CN_III, 100.0_c_float )

  end function CN_II_to_CN_III

!--------------------------------------------------------------------------------------------------

  elemental function CN_II_to_CN_I(CN_II)   result(CN_I)

    real (kind=c_float), intent(in)  :: CN_II
    real (kind=c_float)              :: CN_I

    CN_I = CN_II / (2.281_c_float - 0.01281_c_float * CN_II )

    CN_I = max( CN_I,0.0_c_float )
    CN_I = min( CN_I, 100.0_c_float )

  end function CN_II_to_CN_I

!--------------------------------------------------------------------------------------------------
  !> Calculate the runoff by means of the curve number method.
  !!
  !! Runoff is calculated using a modification of the curve number method. 
  !! Specifically, the initial abstraction is defined as 0.05 * SMax rather than the
  !! standard 0.20 * SMax of the original method. This redefinition of the initial abstraction
  !! term was found to be more appropriate for long-term continuous simulations than the 
  !! standard 0.2 * SMax of the original.
  !! @param[in]  iLanduseIndex  Pre-configured index number corresponding to a line in the landuse lookup table.
  !! @param[in]  iSoilsGroup   The numerical index associated with the soils group of interest.
  !! @param[in]  fInflow   The sum of net rainfall, snowmelt, and runon from upslope cells (and possibly irrigation).
  !! @param[in]  fCFGI   The current value of the continuous frozen-ground index.
  !! @param[in]  lIsGrowingSeason  Logical value indicating whether dormant season or growing season
  !!             criteria values are to be used when calculating runoff.
  !!
  !! @retval   fRunoff  Daily runoff calculated by means of the SCS Curve Number method.
  !!
  !! @note Reference: Woodward, D. E., R. H. Hawkins, R. Jiang, A. Hjelmfeldt Jr, J. Van Mullem,
  !!       and Q. D. Quan. “Runoff Curve Number Method: Examination of the Initial Abstraction Ratio.”
  !!       In Conference Proceeding Paper, World Water and Environmental Resources Congress, 2003.
  elemental function calculate_runoff_fn(iLanduseIndex, iSoilsgroup, fInflow, fCFGI, lIsGrowingSeason )   result(fRunoff)

    real (kind=c_float) :: fInclow
    real (kind=c_float) :: fRunoff
    
    ! [ LOCALS ]
    real (kind=c_float) :: P
    real (kind=c_float) :: CN_05
    real (kind=c_float) :: SMax
    real (kind=c_float) :: CN_adj

    CN_adj = update_curve_number_fn( iLanduseIndex, iSoilsGroup, fInflow, fCFGI, lIsGrowingSeason )

    SMax = (1000.0_c_float / cel%rAdjCN) - 10.0_c_float

    ! Equation 9, Hawkins and others, 2002
    CN_05 = 100_c_float / &
      ((1.879_c_float * ((100.0_c_float / CN_adj ) - 1.0_c_float )**1.15_c_float) + 1.0_c_float)

    ! Equation 8, Hawkins and others, 2002
    SMax = 1.33_c_float * ( SMax ) ** 1.15_c_float

    ! now consider runoff if Ia ~ 0.05S
    if ( P > 0.05_c_float * SMax ) then
      fOutFlow = ( P - 0.05_c_float * SMax )**2  / ( P + 0.95_c_float * SMax )
    else
      fOutFlow = 0.0_c_float
    end if

  end function calculate_runoff_fn


end module infiltration__curve_number
