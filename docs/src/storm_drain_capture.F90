module storm_drain_capture

  use iso_c_binding

  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use datetime, only                 : DATETIME_T
  use grid, only                     : grid_WriteArcGrid
  use logfiles, only                 : LOGS, LOG_ALL
  use exceptions, only               : warn, assert
  use parameters, only               : PARAMS
  use simulation_datetime, only      : SIM_DT
  use fstring_list, only              : FSTRING_LIST_T
  use fstring, only                  : asCharacter
  implicit none

  private

  public :: STORM_DRAIN_CAPTURE_FRACTION
  public :: storm_drain_capture_initialize, storm_drain_capture_calculate

  type (DATA_CATALOG_ENTRY_T), pointer :: pSTORM_DRAIN_CAPTURE_FRACTION
  real (c_float), allocatable     :: STORM_DRAIN_CAPTURE_FRACTION(:)
  real (c_float), allocatable     :: STORM_DRAIN_CAPTURE_FRACTION_TABLE(:)
  type ( DATETIME_T )                  :: DATE_OF_LAST_RETRIEVAL

contains

  subroutine storm_drain_capture_initialize( is_cell_active, landuse_index )

    logical (c_bool), intent(in)     :: is_cell_active(:,:)
    integer (c_int), intent(in)      :: landuse_index(:)

    ! [ LOCALS ]
    integer (c_int)              :: number_of_landuse_codes
    integer (c_int), allocatable :: lu_codes(:)
    integer (c_int)              :: indx
    integer (c_int)              :: num_records
    logical                           :: are_lengths_unequal
    type (FSTRING_LIST_T)              :: string_list
    integer (c_int)              :: status
    character (len=256)               :: sBuf

    call string_list%append("LU_Code")
    call string_list%append("Landuse_Code")
    call string_list%append("Landuse_Lookup_Code")

    !> Determine how many UNIQUE landuse codes are present
    call PARAMS%get_parameters( slKeys=string_list, iValues=lu_codes )
    number_of_landuse_codes = count( lu_codes >= 0 )

    call string_list%clear()
    call string_list%append("Storm_drain_capture")
    call string_list%append("Storm_drain_capture_fraction")

    allocate( STORM_DRAIN_CAPTURE_FRACTION( count( is_cell_active ) ), stat=status )
    call assert( status==0, "Problem allocating STORM_DRAIN_CAPTURE_FRACTION", &
      __SRCNAME__, __LINE__ )

    ! set default value for STORM_DRAIN_CAPTURE_FRACTION
    STORM_DRAIN_CAPTURE_FRACTION = 0.0_c_float

    !> Determine how many storm_drain_capture codes are present
    call PARAMS%get_parameters( slKeys=string_list, fValues=STORM_DRAIN_CAPTURE_FRACTION_TABLE )

    call string_list%clear()

    ! attempt to locate storm drain capture information in gridded format
    pSTORM_DRAIN_CAPTURE_FRACTION => DAT%find( "STORM_DRAIN_CAPTURE_FRACTION" )

   if ( associated( pSTORM_DRAIN_CAPTURE_FRACTION ) ) then

     ! nothing to do...

   elseif ( STORM_DRAIN_CAPTURE_FRACTION_TABLE(1) > fTINYVAL ) then

     !> check: number of STORM_DRAIN_CAPTURE_FRACTION_TABLE values == number of landuse codes?
     num_records = ubound(STORM_DRAIN_CAPTURE_FRACTION_TABLE, 1)
     are_lengths_unequal = ( num_records /= number_of_landuse_codes )

     if ( are_lengths_unequal ) then

       sBuf = "The number of values specifying storm drain capture"                         &
         //" fraction ("                                                                    &
         //asCharacter( num_records )//") does not match the number of landuse values ("    &
         //asCharacter( number_of_landuse_codes )//"). Setting default storm drain"         &
         //" capture to 0.0 (ZERO)."

       call warn( sMessage=trim(sBuf),                                                      &
                 sModule=__SRCNAME__,                                                      &
                 iLine=__LINE__,                                                           &
                 lFatal=.false._c_bool,                                                    &
                 iLogLevel=LOG_ALL )

       STORM_DRAIN_CAPTURE_FRACTION = 0.0_c_float

     else

       do indx=lbound( landuse_index, 1 ), ubound( landuse_index, 1 )
         STORM_DRAIN_CAPTURE_FRACTION( indx ) = STORM_DRAIN_CAPTURE_FRACTION_TABLE( landuse_index( indx ) )
       enddo

      endif

      deallocate( STORM_DRAIN_CAPTURE_FRACTION_TABLE, stat=status )
      call assert( status==0, "Problem allocating STORM_DRAIN_CAPTURE_FRACTION_TABLE", &
        __SRCNAME__, __LINE__ )

    endif

  end subroutine storm_drain_capture_initialize

!--------------------------------------------------------------------------------------------------

  subroutine storm_drain_capture_calculate( capture_fraction, indx, is_cell_active )

    real (c_float), intent(inout)     :: capture_fraction
    integer (c_int), intent(in)       :: indx
    logical (c_bool), intent(in)      :: is_cell_active(:,:)

    ! attempt to update values from gridded source, if active
    if ( .not. DATE_OF_LAST_RETRIEVAL == SIM_DT%curr ) then

      associate ( dt => SIM_DT%curr )

        if ( associated( pSTORM_DRAIN_CAPTURE_FRACTION ) ) then
          call pSTORM_DRAIN_CAPTURE_FRACTION%getvalues( dt )
          if ( pSTORM_DRAIN_CAPTURE_FRACTION%lGridHasChanged ) then
            STORM_DRAIN_CAPTURE_FRACTION = pack( pSTORM_DRAIN_CAPTURE_FRACTION%pGrdBase%rData, is_cell_active )
            call grid_WriteArcGrid("Storm_drain_capture_fraction__as_read_into_SWB.asc", &
              pSTORM_DRAIN_CAPTURE_FRACTION%pGrdBase )
          endif
        endif

        DATE_OF_LAST_RETRIEVAL = SIM_DT%curr

      end associate

    endif

    if ( allocated( STORM_DRAIN_CAPTURE_FRACTION ) )                           &
      capture_fraction = STORM_DRAIN_CAPTURE_FRACTION( indx )

  end subroutine storm_drain_capture_calculate

end module storm_drain_capture
