!> @file
!!  Contains a single module, \ref rooting_depth__FAO56, which
!!  provides support for dynamic rooting depth calculation

!! Calculate the effective root zone depth given the current stage
!! of plant growth, the soil type, and the crop type.
module rooting_depth__FAO56

  use iso_c_binding, only              : c_short, c_int, c_float, c_double
  use string_list, only                : STRING_LIST_T
  use parameters, only                 : PARAMS
  use crop_coefficients__FAO56, only   : KCB_l, KCB_MIN, KCB_INI, KCB_MID, KCB_END,               &
                                         KCB_METHOD, KCB_METHOD_GDD, KCB_METHOD_MONTHLY_VALUES,  &
                                         KCB_METHOD_FAO56, JAN, DEC
  implicit none

contains

!------------------------------------------------------------------------------

!> Calculate the effective root zone depth.
!!
!! Calculate the effective root zone depth given the current stage
!! of plant growth, the soil type, and the crop type.
!!
!! @param[inout] Zr_i Daily rooting depth estimate.
!! @param[in] Zr_max The maximum rooting depth for this crop.
!! @param[in] landuse_index Index corresponding to the line number of the table
!!             that holds data for a particular landuse.
!! @param[in] Kcb current crop coefficient value for a cell.
!! @note Implemented as equation 8-1 (Annex 8), FAO-56, Allen and others.
elemental subroutine update_rooting_depth( Zr_i, Zr_max, landuse_index, Kcb )

  real (kind=c_float), intent(inout)  :: Zr_i
  real (kind=c_float), intent(in)     :: Zr_max
  integer (kind=c_int), intent(in)    :: landuse_index
  real (kind=c_float), intent(in)     :: Kcb

  ! [ LOCALS ]
  ! 0.3048 feet equals 0.1 meters, which is seems to be the standard
  ! initial rooting depth in the FAO-56 methodology
  real (kind=c_float), parameter :: Zr_min = 0.3048
  real (kind=c_float)            :: MaxKCB
  real (kind=c_float)            :: MinKCB

  if ( KCB_METHOD( landuse_index ) == KCB_METHOD_MONTHLY_VALUES ) then
    MaxKCB = maxval( KCB_l( JAN:DEC, landuse_index ) )
    MinKCB = minval( KCB_l( JAN:DEC, landuse_index ) )
  else
    MaxKCB = maxval( KCB_l( KCB_INI:KCB_MIN, landuse_index ) )
    MinKCB = minval( KCB_l( KCB_INI:KCB_MIN, landuse_index ) )
  endif

  ! if there is not much difference between the MAX Kcb and MIN Kcb, assume that
  ! we are dealing with an area such as a forest, where we assume that the rooting
  ! depths are constant year-round
   if ( ( MaxKCB - MinKCB ) < 0.1_c_float ) then

     Zr_i = Zr_max

   elseif ( MaxKCB > 0.0_C_float ) then

     Zr_i = Zr_min + ( Kcb - MinKCB ) / ( MaxKCB - MinKCB ) * ( Zr_max - Zr_min )

   else

     Zr_i = Zr_min

   endif

end subroutine update_rooting_depth

end module rooting_depth__FAO56
