!> @file
!!  Contains a single module, \ref rooting_depth__FAO56, which
!!  provides support for dynamic rooting depth calculation

!! Calculate the effective root zone depth given the current stage
!! of plant growth, the soil type, and the crop type.
module rooting_depth__FAO56

  use iso_c_binding, only              : c_short, c_int, c_float, c_double, c_bool
  use constants_and_conversions, only  : TRUE, FALSE 
  use fstring_list
  use fstring, only                    : operator(.containssimilar.)
  use parameters, only                 : PARAMS
  use exceptions, only                 : assert, warn
  use crop_coefficients__FAO56, only   : KCB_l, KCB_MIN, KCB_INI, KCB_MID, KCB_END,               &
                                         KCB_METHOD, KCB_METHOD_GDD, KCB_METHOD_MONTHLY_VALUES,  &
                                         KCB_METHOD_FAO56, JAN, DEC
  implicit none

  private

  public :: initialize_rooting_depth, update_rooting_depth 

  logical(c_bool), allocatable :: VARIABLE_ROOTING_DEPTH(:)

contains

subroutine initialize_rooting_depth( )

  use parameters, only        : PARAMS, PARAMS_DICT

  integer (c_int)               :: number_of_landuses
  integer (c_int)               :: number_of_records
  integer (c_int)               :: indx
  logical (c_bool)              :: list_lengths_are_equal
  type (FSTRING_LIST_T)         :: slList
  type (FSTRING_LIST_T)         :: sl_variable_rooting_depth
  integer (c_int), allocatable  :: landuse_table_codes(:)
  logical (c_bool), allocatable :: tempbool(:)
  character (len=31)            :: temp_str
  integer (c_int)               :: status

  ! create list of possible table headings to look for...
  call slList%append( "LU_Code" )
  call slList%append( "Landuse_Lookup_Code" )

  !> Determine how many landuse codes are present
  call PARAMS%get_parameters( slKeys=slList, iValues=landuse_table_codes )
  number_of_landuses = count( landuse_table_codes >= 0 )
  call slList%clear()

 slList = create_list("allow_variable_rooting_depth, variable_rooting_depth")
 call PARAMS%get_parameters( slKeys=slList, slValues=sl_variable_rooting_depth, lFatal=FALSE )
 call slList%clear()

  number_of_records = sl_variable_rooting_depth%count
  list_lengths_are_equal = ( number_of_records == number_of_landuses )

  if ( .not. list_lengths_are_equal ) then

    call warn( sMessage="The number of landuses does not match the number of values supplied for the "    &
                        //"rooting depth method.",                              &
               sHints="By default, all rooting depths will be allowed to vary, using the FAO-56 methodology.",   &
               sModule=__FILE__, iLine=__LINE__, lFatal=FALSE )
    allocate(tempbool(number_of_landuses), stat=status)
    tempbool = TRUE
    call move_alloc(tempbool, VARIABLE_ROOTING_DEPTH)
  
  else

    allocate( VARIABLE_ROOTING_DEPTH( sl_variable_rooting_depth%count ), stat=status )
    call assert( status==0, "Problem allocating memory.", __FILE__, __LINE__ )

    do indx=1, sl_variable_rooting_depth%count
      temp_str = sl_variable_rooting_depth%get( indx )
      if ( (temp_str .containssimilar. "variable") .or. (temp_str .containssimilar. "varying")) then
        VARIABLE_ROOTING_DEPTH(indx) = TRUE
      else
        VARIABLE_ROOTING_DEPTH(indx) = FALSE
      endif
    enddo
  
  endif   

end subroutine initialize_rooting_depth

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

  real (c_float), intent(inout)  :: Zr_i
  real (c_float), intent(in)     :: Zr_max
  integer (c_int), intent(in)    :: landuse_index
  real (c_float), intent(in)     :: Kcb

  ! [ LOCALS ]
  ! 0.328 feet equals 0.1 meters, which is seems to be the standard
  ! initial rooting depth in the FAO-56 methodology
  real (c_float), parameter :: Zr_min = 0.328
  real (c_float)            :: MaxKCB
  real (c_float)            :: MinKCB

  if (VARIABLE_ROOTING_DEPTH(landuse_index)) then

    if ( KCB_METHOD( landuse_index ) == KCB_METHOD_MONTHLY_VALUES ) then
      MaxKCB = maxval( KCB_l( JAN:DEC, landuse_index ) )
      MinKCB = minval( KCB_l( JAN:DEC, landuse_index ) )
    else
      MaxKCB = maxval( KCB_l( KCB_INI:KCB_MIN, landuse_index ) )
      MinKCB = minval( KCB_l( KCB_INI:KCB_MIN, landuse_index ) )
    endif


!    if ( MinKCB > 0.49_c_float ) then

!      Zr_i = Zr_max

    if ( Kcb > MinKcb ) then

      ! scale the rooting depth in proportion to the progress within the Kcb curve...
      Zr_i = Zr_min + ( Kcb - MinKCB ) / ( MaxKCB - MinKCB ) * ( Zr_max - Zr_min )

    else

      Zr_i = Zr_min

    endif

  else
    
    ! user has specified a constant rooting depth. set value to Zr_max
    Zr_i = Zr_max

  endif

end subroutine update_rooting_depth

end module rooting_depth__FAO56
