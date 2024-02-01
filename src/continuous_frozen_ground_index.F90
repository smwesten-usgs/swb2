module continuous_frozen_ground_index

  use iso_c_binding, only          : c_short, c_int, c_float, c_double, c_long_long
  use constants_and_conversions
  use data_catalog, only           : DAT
  use data_catalog_entry, only     : DATA_CATALOG_ENTRY_T

  use exceptions
  implicit none

  private

  public update_continuous_frozen_ground_index, initialize_continuous_frozen_ground_index

contains

  subroutine initialize_continuous_frozen_ground_index( cfgi, cfgi_ll, cfgi_ul, active_cells )

    real (c_float), intent(inout)  :: cfgi(:)
    real (c_float), intent(inout)  :: cfgi_ll(:)
    real (c_float), intent(inout)  :: cfgi_ul(:)

    logical (c_bool), intent(in)   :: active_cells(:,:)

    ! [ LOCALS ]
    type (DATA_CATALOG_ENTRY_T), pointer :: pINITIAL_CFGI
    type (DATA_CATALOG_ENTRY_T), pointer :: pCFGI_LOWER_LIMIT
    type (DATA_CATALOG_ENTRY_T), pointer :: pCFGI_UPPER_LIMIT
    integer (c_int) :: i

    ! locate the data structure associated with the gridded initial_cfgi
    pINITIAL_CFGI => DAT%find("INITIAL_CONTINUOUS_FROZEN_GROUND_INDEX")

    if ( .not. associated( pINITIAL_CFGI ) ) then
      pINITIAL_CFGI => DAT%find("INITIAL_FROZEN_GROUND_INDEX")

      if ( .not. associated( pINITIAL_CFGI ) ) then

        call warn(sMessage="An INITIAL_CONTINUOUS_FROZEN_GROUND_INDEX grid (or constant) was not found.",    &
        sHints="Check your control file to see that a valid INITIAL_CONTINUOUS_FROZEN_GROUND_INDEX grid or"  &
          //" constant is specified.", lFatal=FALSE )

        cfgi = 0.0_c_float

      endif  

    else

      call pINITIAL_CFGI%getvalues()

      ! map the 2D array of INITIAL_CFGI values to the vector of active cells
      cfgi = pack( pINITIAL_CFGI%pGrdBase%rData, active_cells )

     if ( minval( cfgi ) < fZERO &
        .or. maxval( cfgi ) > 300.0_c_float )  &
       call warn(sMessage="One or more initial continuous frozen ground values outside of " &
         //"valid range (0 to 300)", lFatal=TRUE )

    endif



    ! locate the data structure associated with the gridded CFGI_LOWER_LIMIT
    pCFGI_LOWER_LIMIT => DAT%find("CFGI_LOWER_LIMIT")

    if ( .not. associated( pCFGI_LOWER_LIMIT ) ) then

      call warn(sMessage="No value supplied for CONTINUOUS_FROZEN_GROUND_INDEX_LOWER_LIMIT.",             &
      sHints="Value set to the default of 56, which was appropriate for the Pacific Northwestern U.S., "  &
      //"but may be inappropriate elsewhere.", lFatal=FALSE )

      cfgi_ll = 56.   ! units are degrees C-days

    else

      call pCFGI_LOWER_LIMIT%getvalues()

      ! map the 2D array of INITIAL_CFGI values to the vector of active cells
      cfgi_ll = pack( pCFGI_LOWER_LIMIT%pGrdBase%rData, active_cells )

     if ( minval( cfgi_ll ) < fZERO                                             &
        .or. maxval( cfgi_ll ) > 300.0_c_float )                                &
       call warn(sMessage="One or more CFGI lower limit values outside of "     &
         //"valid range (0 to 300)", lFatal=TRUE )

    endif


    ! locate the data structure associated with the gridded CFGI_UPPER_LIMIT
    pCFGI_UPPER_LIMIT => DAT%find("CFGI_UPPER_LIMIT")

    if ( .not. associated( pCFGI_UPPER_LIMIT ) ) then

      call warn(sMessage="No value supplied for CONTINUOUS_FROZEN_GROUND_INDEX_UPPER_LIMIT.",             &
      sHints="Value set to the default of 83, which was appropriate for the Pacific Northwestern U.S., "  &
      //"but may be inappropriate elsewhere.", lFatal=FALSE )

        cfgi_ul = 83. ! units are degree C-days

    else

      call pCFGI_UPPER_LIMIT%getvalues()

      ! map the 2D array of INITIAL_CFGI values to the vector of active cells
      cfgi_ul = pack( pCFGI_UPPER_LIMIT%pGrdBase%rData, active_cells )

     if ( minval( cfgi_ul ) < fZERO                                            &
        .or. maxval( cfgi_ul ) > 300.0_c_float )                               &
       call warn(sMessage="One or more CFGI upper limit values outside of "    &
         //"valid range (0 to 300)", lFatal=TRUE )

    endif

    if (any(cfgi_ul - cfgi_ll <= 0.0_c_float)) then
      call warn(sMessage="One or more CFGI upper limit values is less that its"     &
        //" corresponding CFGI lower limit values", lFatal=TRUE)
    endif


  end subroutine initialize_continuous_frozen_ground_index

!--------------------------------------------------------------------------------------------------

  !> Update the continuous frozen ground index (CFGI) for a cell.
  !!
  !! Computes the continuous frozen ground index
  !! @param[inout] rCFGI   Continuous frozen ground index to be updated.
  !! @param[in]    rTAvg_F Mean daily air temperature, in \degF
  !!
  !! @note Implemented as per Molnau and Bissel (1983).
  !!
  !! @note Molnau, M. and Bissell, V.C., 1983, A continuous frozen ground index for
  !! flood forecasting: In Proceedings 51st Annual Meeting Western Snow Conference,
  !! 109-119, Canadian Water Resources Assoc. Cambridge, Ont.
  elemental subroutine update_continuous_frozen_ground_index( fCFGI, fTMax_F, fTMin_F, fSnowCover )

    ! [ ARGUMENTS ]
    real (c_float), intent(inout)       :: fCFGI
    real (c_float), intent(in)          :: fTMax_F
    real (c_float), intent(in)          :: fTMin_F
    real (c_float), intent(in)          :: fSnowCover

    ! [ LOCALS ]
    real (c_float), parameter    :: fDecay_Coefficient_A                      = 0.97_c_float
    real (c_float), parameter    :: fSnow_Reduction_Coefficient_Freezing      = 0.08_c_float
    real (c_float), parameter    :: fSnow_Reduction_Coefficient_Thawing       = 0.5_c_float
    real (c_float), parameter    :: fCM_PER_INCH                              = 2.54_c_float
    real (c_float), parameter    :: FREEZING_POINT_DEG_C                      = 0.0_c_float

    real (c_float) :: fTAvg_C              ! temporary variable holding avg temp in C
    real (c_float) :: fSnowDepthCM         ! snow depth in centimeters


    fTAvg_C = F_to_C( (fTMax_F + fTMin_F) / 2.0_c_float )

    ! assuming snow depth is 10 times the water content of the snow in inches
    fSnowDepthCM = fSnowCover * 10.0_c_float * fCM_PER_INCH

    associate( Tavg => fTavg_C,                                              &
               A => fDecay_Coefficient_A,                                    &
               CFGI => fCFGI,                                                &
               K_freeze => fSnow_Reduction_Coefficient_Freezing,             &
               K_thaw => fSnow_Reduction_Coefficient_Thawing )

      if( Tavg > FREEZING_POINT_DEG_C ) then

        CFGI = max( A * CFGI - Tavg * exp ( -0.4_c_float * K_thaw * fSnowDepthCM ), 0.0_c_float )

      else ! temperature is below freezing

        CFGI = max( A * CFGI - Tavg * exp ( -0.4_c_float * K_freeze * fSnowDepthCM ), 0.0_c_float )

      end if

    end associate

  end subroutine update_continuous_frozen_ground_index

end module continuous_frozen_ground_index
