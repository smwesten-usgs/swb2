module runoff__curve_number

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use constants_and_conversions, only     : FALSE, TRUE 
  use continuous_frozen_ground_index
  use exceptions
  use strings
  use string_list
  use parameters, only                    : PARAMS, PARAMS_DICT
  implicit none

  private

  real (kind=c_float), allocatable    :: CN_ARCIII(:,:)
  real (kind=c_float), allocatable    :: CN_ARCII(:,:)
  real (kind=c_float), allocatable    :: CN_ARCI(:,:)
  real (kind=c_float), allocatable    :: Smax(:,:)
  integer (kind=c_int), allocatable   :: iLanduseCodes(:)

  public :: runoff_curve_number_initialize
  public :: runoff_curve_number_calculate
  public :: update_curve_number_fn
  public :: CN_ARCI, CN_ARCII, CN_ARCIII, Smax

contains

  subroutine runoff_curve_number_initialize( )

    ! [ LOCALS ]
    integer (kind=c_int)              :: iNumberOfLanduses
    integer (kind=c_int)              :: iNumberOfSoilGroups
    integer (kind=c_int), allocatable :: iCurveNumberSeqNums(:)
    type (STRING_LIST_T)              :: slList
    type (STRING_LIST_T)              :: slCurveNumber
    integer (kind=c_int)              :: iStat
    integer (kind=c_int)              :: iSoilsIndex
    character (len=:), allocatable    :: sText
    real (kind=c_float), allocatable  :: CN(:)

    call slList%append("LU_Code")
    call slList%append("Landuse_Lookup_Code")

    !> Determine how many soil groups are present

    ! retrieve a string list of all keys associated with curve number (i.e. "CN_1", "CN_2", "CN_3", etc)
    slCurveNumber = PARAMS%grep_name( "CN", lFatal=TRUE )
    ! Convert the string list to an vector of integers; this call strips off the "CN_" part of label
    iCurveNumberSeqNums = slCurveNumber%asInt()
    ! count how many items are present in the vector; this should equal the number of soils groups
    iNumberOfSoilGroups = count( iCurveNumberSeqNums > 0 )

    !> Determine how many landuse codes are present
    call PARAMS%get_parameters( slKeys=slList, iValues=iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes >= 0 )

    allocate( CN_ARCIII(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for curve number table - AMC III", &
      __FILE__, __LINE__)

    allocate( CN_ARCII(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for curve number table - AMC II", &
      __FILE__, __LINE__)

    allocate( CN_ARCI(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for curve number table - AMC I", &
      __FILE__, __LINE__)

    allocate( Smax(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for curve number SMax table", &
      __FILE__, __LINE__)

    ! we should have the curve number table fully filled out following this block
    do iSoilsIndex = 1, iNumberOfSoilGroups
      sText = "CN_"//asCharacter(iSoilsIndex)
      call PARAMS%get_parameters( sKey=sText, fValues=CN )
      CN_ARCII(:, iSoilsIndex) = CN
    enddo  

    ! calculate SMax based on CN for AMC I
    CN_ARCI = CN_II_to_CN_I( CN_ARCII )
    CN_ARCIII = CN_II_to_CN_III( CN_ARCII )
    Smax = ( 1000.0_c_float / CN_ARCI ) - 10.0_c_float
    
  end subroutine runoff_curve_number_initialize

!--------------------------------------------------------------------------------------------------

  elemental function return_landuse_index_fn(iLanduseCode)      result(iLanduseIndex)

    integer (kind=c_int), intent(in)         :: iLanduseCode
    integer (kind=c_int)                     :: iLanduseIndex

    iLanduseIndex = 0

    do while (iLanduseIndex < ubound( iLanduseCodes, 1) )

      iLanduseIndex = iLanduseIndex + 1

      if (iLanduseCode == iLanduseCodes(iLanduseIndex) ) exit
      
    end do

  end function return_landuse_index_fn

!--------------------------------------------------------------------------------------------------

  elemental function prob_runoff_enhancement( fCFGI )    result(fPf)

    real (kind=c_float), intent(in) :: fCFGI
    real (kind=c_float)             :: fPf

    if(fCFGI <= CFGI_LL ) then
      fPf = 0_c_float
    elseif(fCFGI >= CFGI_UL) then
      fPf = 1.0_c_float
    else
      fPf = ( fCFGI - CFGI_LL ) / ( CFGI_UL - CFGI_LL )
    end if

  end function prob_runoff_enhancement

!--------------------------------------------------------------------------------------------------

  elemental function update_curve_number_fn( iLanduseIndex, iSoilsIndex, fSoilStorage, &
                          fSoilStorage_Max, fCFGI )  result( CN_adj )
    
    integer (kind=c_int), intent(in)  :: iLanduseIndex
    integer (kind=c_int), intent(in)  :: iSoilsIndex
    real (kind=c_float), intent(in)   :: fSoilStorage
    real (kind=c_float), intent(in)   :: fSoilStorage_Max
    real (kind=c_float), intent(in)   :: fCFGI
    real (kind=c_float)               :: CN_adj

    ! [ LOCALS ]
    real (kind=c_float) :: Pf
    real (kind=c_float) :: CN_base
    real (kind=c_float) :: fFraction_FC   ! fraction of field capacity
    real (kind=c_float) :: frac

    fFraction_FC = 0.0
    if (fSoilStorage_Max > 0.0) fFraction_FC = min( 1.0_c_float, fSoilStorage / fSoilStorage_Max )

    ! Correct the curve number...
    !
    ! current thinking on this: if the soil moisture is at average levels or *greater*
    ! *and* the CFGI indicates frozen ground conditions, assume that runoff enhancement
    ! is probable. If the soils froze under relatively dry conditions, assume that some pore
    ! space is open in the frozen ground.
    if( fCFGI > CFGI_LL .and. fFraction_FC > 0.5 ) then

      Pf = prob_runoff_enhancement( fCFGI )

      ! use probability of runoff enhancement to calculate a weighted
      ! average of curve number under Type II vs Type III antecedent
      ! runoff conditions
      CN_adj = CN_ARCII(iLanduseIndex, iSoilsIndex) * ( 1.0_c_float - Pf ) &
                + CN_ARCIII(iLanduseIndex, iSoilsIndex) * Pf 

    elseif ( fFraction_FC > 0.5_c_float ) then   ! adjust upward toward AMC III

      ! we want a number ranging from 0 to 1 indicating the relative split
      ! between CN AMCII and CN AMCIII
      frac = ( fFraction_FC - 0.5_c_float ) / 0.5_c_float

      CN_adj = CN_ARCII(iLanduseIndex, iSoilsIndex) * ( 1.0_c_float - frac ) &
                + CN_ARCIII(iLanduseIndex, iSoilsIndex) * frac 

    elseif ( fFraction_FC < 0.5_c_float ) then   ! adjust downward toward AMC I

      ! we want a number ranging from 0 to 1 indicating the relative split
      ! between CN AMCII and CN AMCI 
      !
      ! ex. fFraction_FC = 0.48; frac = 2.0 * 0.48 = 0.96
      !     CN_adj = 0.96 ( CN_ARCII ) + ( 1.0 - 0.96 ) * CN_ARCIII
      !
      frac = fFraction_FC / 0.5_c_float

      CN_adj = CN_ARCI(iLanduseIndex, iSoilsIndex) * ( 1.0_c_float - frac ) &
                + CN_ARCII(iLanduseIndex, iSoilsIndex) * frac 

    else

      CN_adj = CN_ARCII(iLanduseIndex, iSoilsIndex)

    endif  


    ! ensure that whatever modification have been made to the curve number
    ! remain within reasonable bounds
    CN_adj = MIN( CN_adj, 100.0_c_float ) 
    CN_adj = MAX( CN_adj, 0.0_c_float )

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
  elemental subroutine runoff_curve_number_calculate(runoff,                              &
                                                     landuse_index,                       &
                                                     soil_group,                         &
                                                     soil_storage,                        &
                                                     soil_storage_max,                    & 
                                                     inflow,                              &
                                                     continuous_frozen_ground_index ) 

    real (kind=c_float), intent(inout)  :: runoff
    integer (kind=c_int), intent(in)    :: landuse_index
    integer (kind=c_int), intent(in)    :: soil_group
    real (kind=c_float), intent(in)     :: soil_storage
    real (kind=c_float), intent(in)     :: soil_storage_max
    real (kind=c_float), intent(in)     :: inflow
    real (kind=c_float), intent(in)     :: continuous_frozen_ground_index
    
    ! [ LOCALS ]
    real (kind=c_float) :: CN_05
    real (kind=c_float) :: S
    real (kind=c_float) :: CN_adj

    CN_adj = update_curve_number_fn( landuse_index, soil_group, soil_storage, &
                          soil_storage_max, continuous_frozen_ground_index )

    S = ( 1000.0_c_float / CN_adj ) - 10.0_c_float

    ! Equation 9, Hawkins and others, 2002
    CN_05 = 100_c_float / &
            ((1.879_c_float * ((100.0_c_float / CN_adj ) - 1.0_c_float )**1.15_c_float) + 1.0_c_float)

    ! Equation 8, Hawkins and others, 2002
    S = 1.33_c_float * ( S ) ** 1.15_c_float

    ! now consider runoff if Ia ~ 0.05S
    if ( inflow > 0.05_c_float * S ) then
      runoff = ( inflow - 0.05_c_float * S )**2  / ( inflow + 0.95_c_float * S )
    else
      runoff = 0.0_c_float
    end if

  end subroutine runoff_curve_number_calculate


end module runoff__curve_number
