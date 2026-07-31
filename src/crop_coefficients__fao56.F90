!> @file
!>  Contains a single module, \ref crop_coefficients__fao56, which
!>  provides support for modifying reference ET through the use of
!>  crop coefficients

!> Update crop coefficients for crop types in simulation.

module crop_coefficients__fao56

  use iso_c_binding, only             : c_int, c_float, c_double
  use constants_and_conversions, only : clip, TRUE, FALSE
  use logfiles, only                  : LOGS, LOG_ALL
  use exceptions, only                : assert, warn
  use fstring, only                   : asCharacter
  use fstring_list
  implicit none

  private

  public :: crop_coefficients_FAO56_initialize
  public :: crop_coefficients_FAO56_calculate_Kcb_Max
  public :: crop_coefficients_FAO56_interpolate_Kcb
  public :: crop_coefficients_FAO56_deallocate
  public :: KCB_MIN, KCB_INI, KCB_MID, KCB_END
  public :: KCB_l, JAN, DEC, KCB_METHOD, KCB_METHOD_GDD, KCB_METHOD_FAO56
  public :: KCB_METHOD_MONTHLY_VALUES, KCB_METHOD_NONE

  enum, bind(c)
    enumerator :: KCB_INI=13, KCB_MID, KCB_END, KCB_MIN
  end enum

  enum, bind(c)
    enumerator :: JAN = 1, FEB, MAR, APR, MAY, JUN, JUL, AUG, SEP, OCT, NOV, DEC
  end enum

   enum, bind(c)
     enumerator :: KCB_METHOD_NONE = 0, KCB_METHOD_GDD = 1, KCB_METHOD_MONTHLY_VALUES, KCB_METHOD_FAO56
   end enum

  ! Private, module level variables
  ! kept at a landuse code level (i.e. same value applies to all cells with same LU codes)
  integer (c_int), allocatable  :: LANDUSE_CODE(:)
!  real (c_float), allocatable   :: REW(:,:)
!  real (c_float), allocatable   :: TEW(:,:)
  real (c_float), allocatable   :: KCB_l(:,:)
  integer (c_int), allocatable  :: KCB_METHOD(:)

contains


  subroutine crop_coefficients_FAO56_initialize(params)

    use parameters, only : PARAMETERS_T
    use phenology, only : PHENOLOGY_METHOD_INDEX, PHENOLOGY_FAO56_GDD,    &
                          PHENOLOGY_FAO56_DATES, PHENOLOGY_NONE,           &
                          PHENOLOGY_DOY_BASED, PHENOLOGY_GDD_THRESHOLD,    &
                          GROWING_SEASON_START_GDD, GDD_INI, GDD_DEV,      &
                          GDD_MID, GDD_LATE

    type(PARAMETERS_T), intent(inout) :: params

    ! [ LOCALS ]
    type (FSTRING_LIST_T)             :: slList
    integer (c_int)                   :: iNumberOfLanduses
    integer (c_int)                   :: iIndex
    integer (c_int)                   :: iStat

    real (c_float), allocatable       :: Kcb_ini_l(:)
    real (c_float), allocatable       :: Kcb_mid_l(:)
    real (c_float), allocatable       :: Kcb_end_l(:)
    real (c_float), allocatable       :: Kcb_min_l(:)

    real (c_float), allocatable       :: Kcb_jan(:)
    real (c_float), allocatable       :: Kcb_feb(:)
    real (c_float), allocatable       :: Kcb_mar(:)
    real (c_float), allocatable       :: Kcb_apr(:)
    real (c_float), allocatable       :: Kcb_may(:)
    real (c_float), allocatable       :: Kcb_jun(:)
    real (c_float), allocatable       :: Kcb_jul(:)
    real (c_float), allocatable       :: Kcb_aug(:)
    real (c_float), allocatable       :: Kcb_sep(:)
    real (c_float), allocatable       :: Kcb_oct(:)
    real (c_float), allocatable       :: Kcb_nov(:)
    real (c_float), allocatable       :: Kcb_dec(:)



   !> create string list that allows for alternate heading identifiers for the landuse code
   slList = create_list("LU_Code, Landuse_Code, Landuse_Lookup_Code")

   !> Determine how many landuse codes are present
   call params%get_parameters( slKeys=slList, iValues=LANDUSE_CODE )
   iNumberOfLanduses = count( LANDUSE_CODE >= 0 )
   !> @todo Implement thorough input error checking:
   !! are all soils in grid included in table values?
   !> is soil suffix vector continuous?

   call params%get_parameters( sKey="Kcb_ini", fValues=KCB_ini_l)
   call params%get_parameters( sKey="Kcb_mid", fValues=KCB_mid_l)
   call params%get_parameters( sKey="Kcb_end", fValues=KCB_end_l)
   call params%get_parameters( sKey="Kcb_min", fValues=KCB_min_l)

   call params%get_parameters( sKey="Kcb_Jan", fValues=KCB_jan )
   call params%get_parameters( sKey="Kcb_Feb", fValues=KCB_feb )
   call params%get_parameters( sKey="Kcb_Mar", fValues=KCB_mar )
   call params%get_parameters( sKey="Kcb_Apr", fValues=KCB_apr )
   call params%get_parameters( sKey="Kcb_May", fValues=KCB_may )
   call params%get_parameters( sKey="Kcb_Jun", fValues=KCB_jun )
   call params%get_parameters( sKey="Kcb_Jul", fValues=KCB_jul )
   call params%get_parameters( sKey="Kcb_Aug", fValues=KCB_aug )
   call params%get_parameters( sKey="Kcb_Sep", fValues=KCB_sep )
   call params%get_parameters( sKey="Kcb_Oct", fValues=KCB_oct )
   call params%get_parameters( sKey="Kcb_Nov", fValues=KCB_nov )
   call params%get_parameters( sKey="Kcb_Dec", fValues=KCB_dec )


    allocate( KCB_l( 16, iNumberOfLanduses ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory for KCB_l array", &
      __FILE__, __LINE__ )

    allocate( KCB_METHOD( iNumberOfLanduses ), stat=iStat )
    call assert( iStat==0, "Failed to allocate memory for KCB_METHOD vector", &
      __FILE__, __LINE__ )

    KCB_METHOD = KCB_METHOD_NONE
    KCB_l = -9999.

    if (ubound(KCB_ini_l,1) == iNumberOfLanduses)  KCB_l( KCB_INI, :) = KCB_ini_l
    if (ubound(KCB_mid_l,1) == iNumberOfLanduses)  KCB_l( KCB_MID, :) = KCB_mid_l
    if (ubound(KCB_end_l,1) == iNumberOfLanduses)  KCB_l( KCB_END, :) = KCB_end_l
    if (ubound(KCB_min_l,1) == iNumberOfLanduses)  KCB_l( KCB_MIN, :) = KCB_min_l

    if (ubound(KCB_jan,1) == iNumberOfLanduses)   KCB_l( JAN, :) = KCB_jan
    if (ubound(KCB_feb,1) == iNumberOfLanduses)   KCB_l( FEB, :) = KCB_feb
    if (ubound(KCB_mar,1) == iNumberOfLanduses)   KCB_l( MAR, :) = KCB_mar
    if (ubound(KCB_apr,1) == iNumberOfLanduses)   KCB_l( APR, :) = KCB_apr
    if (ubound(KCB_may,1) == iNumberOfLanduses)   KCB_l( MAY, :) = KCB_may
    if (ubound(KCB_jun,1) == iNumberOfLanduses)   KCB_l( JUN, :) = KCB_jun
    if (ubound(KCB_jul,1) == iNumberOfLanduses)   KCB_l( JUL, :) = KCB_jul
    if (ubound(KCB_aug,1) == iNumberOfLanduses)   KCB_l( AUG, :) = KCB_aug
    if (ubound(KCB_sep,1) == iNumberOfLanduses)   KCB_l( SEP, :) = KCB_sep
    if (ubound(KCB_oct,1) == iNumberOfLanduses)   KCB_l( OCT, :) = KCB_oct
    if (ubound(KCB_nov,1) == iNumberOfLanduses)   KCB_l( NOV, :) = KCB_nov
    if (ubound(KCB_dec,1) == iNumberOfLanduses)   KCB_l( DEC, :) = KCB_dec

    ! Assign KCB_METHOD based on phenology module's per-landuse method determination.
    ! The phenology module has already validated and assigned PHENOLOGY_METHOD_INDEX;
    ! this module only needs to know what Kcb interpolation strategy to use.
    do iIndex = lbound(KCB_METHOD, 1), ubound(KCB_METHOD, 1)

      select case ( PHENOLOGY_METHOD_INDEX(iIndex) )

        case ( PHENOLOGY_FAO56_GDD )
          ! Phenology module determined this LU uses GDD-based stages
          if ( all( KCB_l( KCB_INI:KCB_MIN, iIndex ) > 0.0_c_float ) ) then
            KCB_METHOD( iIndex ) = KCB_METHOD_GDD
          else
            call warn("FAO56_GDD phenology active for landuse "              &
              //asCharacter(LANDUSE_CODE(iIndex))                            &
              //" but KCB values are missing or zero.", lFatal=TRUE)
          end if

        case ( PHENOLOGY_FAO56_DATES )
          ! Phenology module determined this LU uses date-based stages
          if ( all( KCB_l( KCB_INI:KCB_MIN, iIndex ) > 0.0_c_float ) ) then
            KCB_METHOD( iIndex ) = KCB_METHOD_FAO56
          else
            call warn("FAO56_DATES phenology active for landuse "            &
              //asCharacter(LANDUSE_CODE(iIndex))                            &
              //" but KCB values are missing or zero.", lFatal=TRUE)
          end if

        case ( PHENOLOGY_DOY_BASED, PHENOLOGY_GDD_THRESHOLD )
          ! Simple binary growing season — check for monthly Kcb first, then staged
          if ( all( KCB_l( JAN:DEC, iIndex ) > 0.0_c_float ) ) then
            KCB_METHOD( iIndex ) = KCB_METHOD_MONTHLY_VALUES
            KCB_l( KCB_MIN, iIndex ) = minval( KCB_l(JAN:DEC, iIndex) )
            KCB_l( KCB_MID, iIndex ) = maxval( KCB_l(JAN:DEC, iIndex) )
          else if ( all( KCB_l( KCB_INI:KCB_MIN, iIndex ) > 0.0_c_float ) ) then
            KCB_METHOD( iIndex ) = KCB_METHOD_FAO56
          else
            call warn("Crop coefficient module is active but landuse "       &
              //asCharacter(LANDUSE_CODE(iIndex))                            &
              //" has no valid Kcb values (monthly Kcb_Jan..Dec"             &
              //" or staged Kcb_ini/mid/end/min)."                           &
              //" All land uses must have explicit Kcb values when"           &
              //" the FAO-56 crop coefficient method is in use.", lFatal=TRUE)
          end if

        case ( PHENOLOGY_NONE )
          ! No phenology method — user must still supply Kcb values explicitly.
          ! Even barren/impervious land uses need Kcb values when the crop
          ! coefficient module is active (e.g., Kcb=0.0 for a parking lot,
          ! or Kcb=0.3 for weedy disturbed land).
          if ( all( KCB_l( JAN:DEC, iIndex ) > 0.0_c_float ) ) then
            KCB_METHOD( iIndex ) = KCB_METHOD_MONTHLY_VALUES
            KCB_l( KCB_MIN, iIndex ) = minval( KCB_l(JAN:DEC, iIndex) )
            KCB_l( KCB_MID, iIndex ) = maxval( KCB_l(JAN:DEC, iIndex) )
          else if ( all( KCB_l( KCB_INI:KCB_MIN, iIndex ) >= 0.0_c_float ) ) then
            KCB_METHOD( iIndex ) = KCB_METHOD_FAO56
          else
            call warn("Crop coefficient module is active but landuse "       &
              //asCharacter(LANDUSE_CODE(iIndex))                            &
              //" has no phenology method and no valid Kcb values."           &
              //" All land uses must have explicit Kcb values when"           &
              //" the FAO-56 crop coefficient method is in use,"             &
              //" even for non-vegetated surfaces (use Kcb=0.0 if"           &
              //" appropriate).", lFatal=TRUE)
          end if

        case default
          call warn("Unrecognized phenology method for landuse "             &
            //asCharacter(LANDUSE_CODE(iIndex)), lFatal=TRUE)

      end select

    end do

    ! Log summary of assigned KCB methods
    call LOGS%write(" ## Crop Coefficient Method Summary ##", iLinesAfter=1)
    call LOGS%write("Landuse Code | KCB Method")
    call LOGS%write("-------------|---------------------")
    do iIndex = 1, iNumberOfLanduses
      select case ( KCB_METHOD(iIndex) )
        case ( KCB_METHOD_GDD )
          call LOGS%write("  "//asCharacter(LANDUSE_CODE(iIndex))//"   | GDD")
        case ( KCB_METHOD_FAO56 )
          call LOGS%write("  "//asCharacter(LANDUSE_CODE(iIndex))//"   | FAO56_DATES")
        case ( KCB_METHOD_MONTHLY_VALUES )
          call LOGS%write("  "//asCharacter(LANDUSE_CODE(iIndex))//"   | MONTHLY")
        case ( KCB_METHOD_NONE )
          call LOGS%write("  "//asCharacter(LANDUSE_CODE(iIndex))//"   | NONE")
      end select
    end do

  end subroutine crop_coefficients_FAO56_initialize

!--------------------------------------------------------------------------------------------------

  !> @brief Deallocate module-level arrays.
  !!
  !! Intended for use in unit tests that need to re-initialize the module
  !! with different parameters. Not intended for production use.
  subroutine crop_coefficients_FAO56_deallocate()

    if (allocated(KCB_l))        deallocate(KCB_l)
    if (allocated(KCB_METHOD))   deallocate(KCB_METHOD)
    if (allocated(LANDUSE_CODE)) deallocate(LANDUSE_CODE)

  end subroutine crop_coefficients_FAO56_deallocate

!--------------------------------------------------------------------------------------------------

  !> @brief Calculate the maximum basal crop coefficient (Kcb_max).
  !!
  !! Equation 72, FAO-56, p 199. Adjusts for wind speed, humidity, and plant height.
  pure elemental function crop_coefficients_FAO56_calculate_Kcb_Max(wind_speed_meters_per_sec,   &
                                          relative_humidity_min_pct,   &
                                          Kcb,                         &
                                          plant_height_meters)                       result(kcb_max)

    real (c_float), intent(in) :: wind_speed_meters_per_sec
    real (c_float), intent(in) :: relative_humidity_min_pct
    real (c_float), intent(in) :: Kcb
    real (c_float), intent(in) :: plant_height_meters

    real (c_float)  :: kcb_max
    real (c_double) :: U2
    real (c_double) :: RHmin
    real (c_double) :: plant_height

    ! Limits are as suggested on page 123 of FAO-56 with respect to
    ! modifying mid-season KCB_mid values
    RHmin = clip( relative_humidity_min_pct, minval=20., maxval=80. )
    U2 = clip(wind_speed_meters_per_sec, minval=1., maxval=6.)
    plant_height = clip(plant_height_meters, minval=1., maxval=10.)

    ! equation 72, FAO-56, p 199
    kcb_max = real(max(  1.2_c_double + ( (0.04_c_double * (U2 - 2._c_double)               &
                                    - 0.004_c_double * (RHmin - 45._c_double) ) )      &
                                    * (plant_height_meters/3._c_double)**0.3_c_double, &
                    Kcb + 0.05_c_double ), c_float)

  end function crop_coefficients_FAO56_calculate_Kcb_Max

!--------------------------------------------------------------------------------------------------

  !> @brief Interpolate Kcb from growth stage and stage fraction.
  !!
  !! Pure Kcb interpolation: given the current growth_stage and the fractional
  !! position within that stage (from the phenology module), return the
  !! basal crop coefficient. For monthly Kcb land uses, uses the current month.
  !!
  !! @param[in]  landuse_index   Index into per-landuse Kcb arrays
  !! @param[in]  growth_stage    Current growth stage (DORMANT, INI, DEV, MID, LATE)
  !! @param[in]  stage_fraction  Position within current stage (0.0–1.0)
  !! @param[in]  current_month   Current month (1-12), used for monthly Kcb method
  !! @return     Kcb             Interpolated basal crop coefficient
  !---------------------------------------------------------------------------
  pure function crop_coefficients_FAO56_interpolate_Kcb( landuse_index,  &
                                                         growth_stage,   &
                                                         stage_fraction, &
                                                         current_month ) &
                                                         result( Kcb )

    use phenology, only : GROWTH_STAGE_INI, &
                          GROWTH_STAGE_DEV, GROWTH_STAGE_MID, GROWTH_STAGE_LATE

    integer (c_int), intent(in) :: landuse_index
    integer (c_int), intent(in) :: growth_stage
    real (c_float), intent(in)  :: stage_fraction
    integer (c_int), intent(in) :: current_month
    real (c_float)              :: Kcb

    if ( KCB_METHOD( landuse_index ) == KCB_METHOD_MONTHLY_VALUES ) then

      Kcb = KCB_l( current_month, landuse_index )

    else

      select case ( growth_stage )

        case ( GROWTH_STAGE_INI )
          Kcb = KCB_l( KCB_INI, landuse_index )

        case ( GROWTH_STAGE_DEV )
          ! Linear ramp from Kcb_ini to Kcb_mid over the development stage
          Kcb = KCB_l( KCB_INI, landuse_index )                            &
              + stage_fraction * ( KCB_l( KCB_MID, landuse_index )         &
                                 - KCB_l( KCB_INI, landuse_index ) )

        case ( GROWTH_STAGE_MID )
          Kcb = KCB_l( KCB_MID, landuse_index )

        case ( GROWTH_STAGE_LATE )
          ! Linear decline from Kcb_mid to Kcb_end over the late stage
          Kcb = KCB_l( KCB_MID, landuse_index )                            &
              + stage_fraction * ( KCB_l( KCB_END, landuse_index )         &
                                 - KCB_l( KCB_MID, landuse_index ) )

        case default
          ! DORMANT or unknown
          Kcb = KCB_l( KCB_MIN, landuse_index )

      end select

    end if

  end function crop_coefficients_FAO56_interpolate_Kcb

end module crop_coefficients__fao56
