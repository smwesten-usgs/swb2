module runoff__curve_number

  use iso_c_binding, only                    : c_int, c_float, c_double, c_bool
  use constants_and_conversions, only        : FALSE, TRUE
  use continuous_frozen_ground_index, only   : CFGI_LL, CFGI_UL
  use datetime
  use exceptions
  use simulation_datetime
  use fstring
  use fstring_list
  use parameters, only                    : PARAMS, PARAMS_DICT
  implicit none

  private

  real (c_float), allocatable    :: CN_ARCIII(:,:)
  real (c_float), allocatable    :: CN_ARCII(:,:)
  real (c_float), allocatable    :: CN_ARCI(:,:)
  real (c_float), allocatable    :: PREV_5_DAYS_RAIN(:,:)
  integer (c_int), allocatable   :: iLanduseCodes(:)
  integer (c_int)                :: DAYCOUNT
  integer (c_int), parameter     :: FIVE_DAY_SUM = 6


  public :: runoff_curve_number_initialize
  public :: runoff_curve_number_calculate
  public :: update_previous_5_day_rainfall
  public :: update_curve_number_fn
  public :: CN_ARCI, CN_ARCII, CN_ARCIII
  public :: FIVE_DAY_SUM, PREV_5_DAYS_RAIN

  real (c_float), parameter :: AMC_DRY_GROWING = 1.40_c_float
  real (c_float), parameter :: AMC_DRY_DORMANT = 0.50_c_float
  real (c_float), parameter :: AMC_WET_GROWING = 2.10_c_float
  real (c_float), parameter :: AMC_WET_DORMANT = 1.10_c_float

  type (DATETIME_T) :: DATE_LAST_UPDATED

contains

  subroutine runoff_curve_number_initialize( cell_is_active )

   use ieee_arithmetic, only : ieee_is_nan, ieee_is_finite

    logical (c_bool), intent(in)   :: cell_is_active(:,:)


    ! [ LOCALS ]
    integer (c_int)              :: iNumberOfLanduses
    integer (c_int)              :: iNumberOfSoilGroups
    integer (c_int), allocatable :: iCurveNumberSeqNums(:)
    type (FSTRING_LIST_T)              :: slList
    type (FSTRING_LIST_T)              :: slCurveNumber
    integer (c_int)              :: iStat
    integer (c_int)              :: iSoilsIndex
    character (len=:), allocatable    :: sText
    real (c_float), allocatable  :: CN(:)

    call slList%append("LU_Code")
    call slList%append("Landuse_Lookup_Code")

    !> Determine how many soil groups are present

    ! retrieve a string list of all keys associated with curve number (i.e. "CN_1", "CN_2", "CN_3", etc)
    slCurveNumber = PARAMS%grep_name( "CN", lFatal=TRUE )
    ! Convert the string list to an vector of integers; this call strips off the "CN_" part of label
    iCurveNumberSeqNums = slCurveNumber%get_integer()
    ! count how many items are present in the vector; this should equal the number of soils groups
    iNumberOfSoilGroups = count( iCurveNumberSeqNums > 0 )

    !> Determine how many landuse codes are present
    call PARAMS%get_parameters( slKeys=slList, iValues=iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes >= 0 )

    allocate( CN_ARCIII(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for curve number table - AMC III", &
      __SRCNAME__, __LINE__)

    allocate( CN_ARCII(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for curve number table - AMC II", &
      __SRCNAME__, __LINE__)

    allocate( CN_ARCI(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for curve number table - AMC I", &
      __SRCNAME__, __LINE__)

    ! we should have the curve number table fully filled out following this block
    do iSoilsIndex = 1, iNumberOfSoilGroups
      sText = "CN_"//asCharacter(iSoilsIndex)
      call PARAMS%get_parameters( sKey=sText, fValues=CN )
      CN_ARCII(:, iSoilsIndex) = CN
    enddo

    allocate( PREV_5_DAYS_RAIN( count(cell_is_active), 6 ), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for curve number PREV_5_DAYS_RAIN table", &
      __SRCNAME__, __LINE__)

    PREV_5_DAYS_RAIN = 0.0_c_float

    ! DAYCOUNT will vary from 1 to 5 and serve as the second index value for
    ! PREV_5_DAYS_RAIN
    DAYCOUNT = 0

    ! calculate curve numbers for antecedent runof conditions I and III
    CN_ARCI = CN_II_to_CN_I( CN_ARCII )
    CN_ARCIII = CN_II_to_CN_III( CN_ARCII )

    call DATE_LAST_UPDATED%parsedate("01/01/1000")

  end subroutine runoff_curve_number_initialize

!--------------------------------------------------------------------------------------------------

  elemental function return_landuse_index_fn(iLanduseCode)      result(iLanduseIndex)

    integer (c_int), intent(in)         :: iLanduseCode
    integer (c_int)                     :: iLanduseIndex

    iLanduseIndex = 0

    do while (iLanduseIndex < ubound( iLanduseCodes, 1) )

      iLanduseIndex = iLanduseIndex + 1

      if (iLanduseCode == iLanduseCodes(iLanduseIndex) ) exit

    end do

  end function return_landuse_index_fn

!--------------------------------------------------------------------------------------------------

  elemental function prob_runoff_enhancement( fCFGI )    result(fPf)

    real (c_float), intent(in) :: fCFGI
    real (c_float)             :: fPf

    if(fCFGI <= CFGI_LL ) then
      fPf = 0_c_float
    elseif(fCFGI >= CFGI_UL) then
      fPf = 1.0_c_float
    else
      fPf = ( fCFGI - CFGI_LL ) / ( CFGI_UL - CFGI_LL )
    end if

  end function prob_runoff_enhancement

!--------------------------------------------------------------------------------------------------

  subroutine update_previous_5_day_rainfall( infil, indx )

    real (c_float), intent(in)            :: infil
    integer (c_int), intent(in)           :: indx

    ! we only want to increment DAYCOUNT 1x per day!
    if ( .not. DATE_LAST_UPDATED == SIM_DT%curr ) then

      if ( DAYCOUNT < 5 ) then
        DAYCOUNT = DAYCOUNT + 1
      else
        DAYCOUNT = 1
      endif

      DATE_LAST_UPDATED = SIM_DT%curr

    endif

    PREV_5_DAYS_RAIN( indx, DAYCOUNT ) = infil
    PREV_5_DAYS_RAIN( indx, FIVE_DAY_SUM ) = sum( PREV_5_DAYS_RAIN( indx, 1:5 ) )

  end subroutine
!--------------------------------------------------------------------------------------------------

!   elemental function update_curve_number_fn( iLanduseIndex, iSoilsIndex, fSoilStorage, &
!                           fSoilStorage_Max, fCFGI )  result( CN_adj )

!     integer (c_int), intent(in)  :: iLanduseIndex
!     integer (c_int), intent(in)  :: iSoilsIndex
!     real (c_float), intent(in)   :: fSoilStorage
!     real (c_float), intent(in)   :: fSoilStorage_Max
!     real (c_float), intent(in)   :: fCFGI
!     real (c_float)               :: CN_adj

!     ! [ LOCALS ]
!     real (c_float) :: Pf
!     real (c_float) :: CN_base
!     real (c_float) :: fFraction_FC   ! fraction of field capacity
!     real (c_float) :: frac

!     fFraction_FC = 0.0
!     if (fSoilStorage_Max > 0.0) fFraction_FC = min( 1.0_c_float, fSoilStorage / fSoilStorage_Max )

!     ! Correct the curve number...
!     !
!     ! current thinking on this: if the soil moisture is at average levels or *greater*
!     ! *and* the CFGI indicates frozen ground conditions, assume that runoff enhancement
!     ! is probable. If the soils froze under relatively dry conditions, assume that some pore
!     ! space is open in the frozen ground.
!     if( fCFGI > CFGI_LL .and. fFraction_FC > 0.5 ) then

!       Pf = prob_runoff_enhancement( fCFGI )

!       ! use probability of runoff enhancement to calculate a weighted
!       ! average of curve number under Type II vs Type III antecedent
!       ! runoff conditions
!       CN_adj = CN_ARCII(iLanduseIndex, iSoilsIndex) * ( 1.0_c_float - Pf ) &
!                 + CN_ARCIII(iLanduseIndex, iSoilsIndex) * Pf

!     elseif ( fFraction_FC > 0.5_c_float ) then   ! adjust upward toward AMC III

!       ! we want a number ranging from 0 to 1 indicating the relative split
!       ! between CN AMCII and CN AMCIII
!       frac = ( fFraction_FC - 0.5_c_float ) / 0.5_c_float

!       CN_adj = CN_ARCII(iLanduseIndex, iSoilsIndex) * ( 1.0_c_float - frac ) &
!                 + CN_ARCIII(iLanduseIndex, iSoilsIndex) * frac

!     elseif ( fFraction_FC < 0.5_c_float ) then   ! adjust downward toward AMC I

!       ! we want a number ranging from 0 to 1 indicating the relative split
!       ! between CN AMCII and CN AMCI
!       !
!       ! ex. fFraction_FC = 0.48; frac = 2.0 * 0.48 = 0.96
!       !     CN_adj = 0.96 ( CN_ARCII ) + ( 1.0 - 0.96 ) * CN_ARCIII
!       !
!       frac = fFraction_FC / 0.5_c_float

!       CN_adj = CN_ARCI(iLanduseIndex, iSoilsIndex) * ( 1.0_c_float - frac ) &
!                 + CN_ARCII(iLanduseIndex, iSoilsIndex) * frac

!     else

!       CN_adj = CN_ARCII(iLanduseIndex, iSoilsIndex)

!     endif


!     ! ensure that whatever modification have been made to the curve number
!     ! remain within reasonable bounds
!     CN_adj = MIN( CN_adj, 100.0_c_float )
!     CN_adj = MAX( CN_adj, 0.0_c_float )

!   end function update_curve_number_fn

!--------------------------------------------------------------------------------------------------

  elemental function update_curve_number_fn( iLanduseIndex, iSoilsIndex, cell_index,   &
                                             it_is_growing_season, fSoilStorage_Max,   &
                                             fCFGI )                                      result( CN_adj )

  integer (c_int), intent(in)  :: iLanduseIndex
  integer (c_int), intent(in)  :: iSoilsIndex
  integer (c_int), intent(in)  :: cell_index
  logical (c_bool), intent(in) :: it_is_growing_season
  real (c_float), intent(in)   :: fSoilStorage_Max
  real (c_float), intent(in)   :: fCFGI
  real (c_float)               :: CN_adj

  ! [ LOCALS ]
  real (c_float) :: Pf
  real (c_float) :: frac
  real (c_float) :: fInflow

  fInflow = PREV_5_DAYS_RAIN( cell_index, FIVE_DAY_SUM )

  associate ( CN_I   => CN_ARCI( iLanduseIndex, iSoilsIndex ),        &
              CN_II  => CN_ARCII( iLanduseIndex, iSoilsIndex ),       &
              CN_III => CN_ARCIII( iLanduseIndex, iSoilsIndex ) )

    ! Correct the curve number...

    if( ( fCFGI > CFGI_LL ) .and. ( fSoilStorage_Max > 0.0_c_float ) ) then

       Pf = prob_runoff_enhancement( fCFGI )

       ! use probability of runoff enhancement to calculate a weighted
       ! average of curve number under Type II vs Type III antecedent
       ! runoff conditions
       CN_adj = CN_II * (1-Pf) +  CN_III * Pf

    else if ( it_is_growing_season ) then

      if ( fInflow < AMC_DRY_GROWING ) then

        CN_adj = CN_I

      else if ( fInflow >= AMC_DRY_GROWING                                  &
          .and. fInflow < AMC_WET_GROWING ) then

         CN_adj = CN_II

      else

        CN_adj = CN_III

      end if

    else ! dormant (non-growing) season

      if ( fInflow < AMC_DRY_DORMANT ) then

        CN_adj = CN_I

      else if ( fInflow >= AMC_DRY_DORMANT                                  &
          .and. fInflow < AMC_WET_DORMANT ) then

        CN_adj = CN_II

      else

        CN_adj = CN_III

      end if

    end if

  end associate

  ! ensure that whatever modification have been made to the curve number
  ! remain within reasonable bounds
  CN_adj = MIN(CN_adj,100.0_c_float)
  CN_adj = MAX(CN_adj,30.0_c_float)


  end function update_curve_number_fn

!--------------------------------------------------------------------------------------------------

  elemental function CN_II_to_CN_III(CN_II)   result(CN_III)

    real (c_float), intent(in)  :: CN_II
    real (c_float)              :: CN_III

    CN_III = CN_II / ( 0.427_c_float + 0.00573_c_float * CN_II )

    CN_III = max( CN_III, 30.0_c_float )
    CN_III = min( CN_III, 100.0_c_float )

  end function CN_II_to_CN_III

!--------------------------------------------------------------------------------------------------

  elemental function CN_II_to_CN_I(CN_II)   result(CN_I)

    real (c_float), intent(in)  :: CN_II
    real (c_float)              :: CN_I

    ! The following comes from page 192, eq. 3.145 of "SCS Curve Number Methodology"
    CN_I = CN_II / (2.281_c_float - 0.01281_c_float * CN_II )

    CN_I = max( CN_I, 30.0_c_float )
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
                                                     curve_num_adj,                       &
                                                     cell_index,                          &
                                                     landuse_index,                       &
                                                     soil_group,                          &
                                                     it_is_growing_season,                &
                                                     inflow,                              &
                                                     soil_storage_max,                    &
                                                     continuous_frozen_ground_index )

    real (c_float), intent(inout)  :: runoff
    real (c_float), intent(inout)  :: curve_num_adj
    integer (c_int), intent(in)    :: cell_index
    integer (c_int), intent(in)    :: landuse_index
    integer (c_int), intent(in)    :: soil_group
    logical (c_bool), intent(in)   :: it_is_growing_season
    real (c_float), intent(in)     :: inflow
    real (c_float), intent(in)     :: soil_storage_max
    real (c_float), intent(in)     :: continuous_frozen_ground_index

    ! [ LOCALS ]
!    real (c_float) :: CN_05
    real (c_float) :: Smax
    real (c_float) :: CN_adj

    curve_num_adj = update_curve_number_fn( landuse_index, soil_group,                        &
                                            cell_index,                                       &
                                            it_is_growing_season,                             &
                                            soil_storage_max,                                 &
                                            continuous_frozen_ground_index )

    Smax = ( 1000.0_c_float / curve_num_adj ) - 10.0_c_float

!     ! Equation 9, Hawkins and others, 2002
!     CN_05 = 100_c_float / &
!             ((1.879_c_float * ((100.0_c_float / CN_adj ) - 1.0_c_float )**1.15_c_float) + 1.0_c_float)

    ! Equation 8, Hawkins and others, 2002
    ! adjust Smax for alternate initial abstraction amount
    Smax = 1.33_c_float * ( Smax**1.15_c_float )

    ! now consider runoff if Ia ~ 0.05S
    if ( inflow > 0.05_c_float * Smax ) then
      runoff = ( inflow - 0.05_c_float * Smax )**2  / ( inflow + 0.95_c_float * Smax )
    else
      runoff = 0.0_c_float
    end if

  end subroutine runoff_curve_number_calculate

end module runoff__curve_number
