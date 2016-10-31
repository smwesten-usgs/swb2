module storm_drain_capture

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_long_long,   &
                            c_size_t, c_ptrdiff_t

  use constants_and_conversions
  use logfiles, only                 : LOGS, LOG_ALL
  use exceptions, only               : warn, assert
  use parameters, only               : PARAMS
  use string_list, only              : STRING_LIST_T
  use strings, only                  : asCharacter
  implicit none

  private

  public :: STORM_DRAIN_CAPTURE_FRACTION
  public :: storm_drain_capture_initialize, storm_drain_capture_calculate

  real (kind=c_float), allocatable   :: STORM_DRAIN_CAPTURE_FRACTION(:)

contains

  subroutine storm_drain_capture_initialize( )

    ! [ LOCALS ]
    integer (kind=c_int)              :: number_of_landuse_codes
    integer (kind=c_int), allocatable :: lu_codes(:)
    integer (kind=c_int)              :: num_records
    logical                           :: are_lengths_unequal
    type (STRING_LIST_T)              :: string_list
    integer (kind=c_int)              :: status
    character (len=256)               :: sBuf

    call string_list%append("LU_Code")
    call string_list%append("Landuse_Code")    
    call string_list%append("Landuse_Lookup_Code")

    !> Determine how many landuse codes are present
    call PARAMS%get_parameters( slKeys=string_list, iValues=lu_codes )
    number_of_landuse_codes = count( lu_codes >= 0 )

    call string_list%clear()
    call string_list%append("Storm_drain_capture")
    call string_list%append("Storm_drain_capture_fraction")

    !> Determine how many storm_drain_capture codes are present
    call PARAMS%get_parameters( slKeys=string_list, fValues=STORM_DRAIN_CAPTURE_FRACTION )

    call string_list%clear()

   !> check: number of STORM_DRAIN_CAPTURE_FRACTION values == number of landuse codes?
    num_records = ubound(STORM_DRAIN_CAPTURE_FRACTION, 1)
    are_lengths_unequal = ( num_records /= number_of_landuse_codes )

    if ( are_lengths_unequal ) then

      sBuf = "The number of values specifying storm drain capture"                         &
        //" fraction ("                                                                    &
        //asCharacter( num_records )//") does not match the number of landuse values ("    &
<<<<<<< HEAD
        //asCharacter( number_of_landuse_codes )//"). Setting default storm drain"         &
        //" capture to 0.0 (ZERO)."


    ! character (len=*), intent(in)               :: sMessage
    ! character (len=*), intent(in), optional     :: sModule
    ! integer (kind=c_int), intent(in), optional  :: iLine 
    ! character (len=*), intent(in), optional     :: sHints
    ! logical (kind=c_bool), intent(in), optional :: lFatal
    ! integer (kind=c_int), intent(in), optional  :: iLogLevel
    ! logical (kind=c_bool), intent(in), optional :: lEcho

      call warn( sMessage=trim(sBuf), sModule=__FILE__, iLine=__LINE__, lFatal=.false._c_bool, iLogLevel=LOG_ALL )
=======
        //asCharacter( number_of_landuse_codes )//"). Setting default storm drain"        &
        //" capture to 0.0 (ZERO).", iLogLevel=LOG_ALL,                                    &
        sModule=__SRCNAME__, iLine=__LINE__, lFatal=.false._c_bool )
>>>>>>> e6ccd93f1fab827d30c5099a8e27f897364f2e69

      deallocate(STORM_DRAIN_CAPTURE_FRACTION, stat=status)
      call assert( status==0, "Problem deallocating STORM_DRAIN_CAPTURE_FRACTION", &
        __SRCNAME__, __LINE__ )

      allocate( STORM_DRAIN_CAPTURE_FRACTION( number_of_landuse_codes ), stat=status )
      call assert( status==0, "Problem allocating STORM_DRAIN_CAPTURE_FRACTION", &
        __SRCNAME__, __LINE__ )

      STORM_DRAIN_CAPTURE_FRACTION = 0.0_c_float
    endif

  end subroutine storm_drain_capture_initialize

!--------------------------------------------------------------------------------------------------

  elemental function storm_drain_capture_calculate( landuse_index )  result( capture_fraction )

    integer (kind=c_int), intent(in)   :: landuse_index
    real (kind=c_float)                :: capture_fraction

    capture_fraction = STORM_DRAIN_CAPTURE_FRACTION( landuse_index )

  end function storm_drain_capture_calculate

end module storm_drain_capture
