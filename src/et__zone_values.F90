!> @file
!! Contains the module \ref et__zone_values.

!>
!!  Module \ref et__zone_values
!!  provides support for estimating fog drip given a zone map
!!  of ET_ZONE, and a table containing monthly
!!  fog factors.
module et__zone_values

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_bool
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use datetime
  use dictionary
  use exceptions
  use file_operations
  use simulation_datetime
  use strings
  use string_list

  implicit none

  private

  public :: et_zone_values_initialize, et_zone_values_calculate
  public :: pET_GRID, ET_RATIOS

  real (c_float), allocatable           :: ET_TABLE_VALUES(:,:)

  type (DATA_CATALOG_ENTRY_T), pointer       :: pET_ZONE
  type (DATA_CATALOG_ENTRY_T), pointer       :: pET_GRID

  integer (c_int), allocatable          :: ET_ZONE(:)

  real (c_float), allocatable           :: ET_RATIOS(:)

 contains

  !> Initialize the ET grid.
  !!
  !! Read in a et zone grid.
  !!
  subroutine et_zone_values_initialize( lActive )

    logical (c_bool), intent(in)   :: lActive(:,:)

    ! [ LOCALS ]
    integer (c_int)      :: iStat
    type (STRING_LIST_T)      :: slString

    ! locate the data structure associated with ANNUAL gridded ET
    pET_GRID => DAT%find("POTENTIAL_ET")
    if ( .not. associated(pET_GRID) ) then
      pET_GRID => DAT%find("REFERENCE_ET0")
      if (.not. associated(pET_GRID) ) &
        call die("A POTENTIAL_ET or REFERENCE_ET0 grid must be supplied in order to make"   &
                 //" use of this option.", __SRCNAME__, __LINE__)
    endif

! locate the data structure associated with the zone fog ratio entries
    pET_ZONE => DAT%find("ET_ZONE")
    if ( .not. associated(pET_ZONE) ) &
        call die("A ET_ZONE grid must be supplied in order to make use of this option.", __SRCNAME__, __LINE__)

    call pET_ZONE%getvalues( )

    allocate ( ET_ZONE( count( lActive ) ), stat=iStat )
    call assert(iStat==0, "Failed to allocate memory for the ET_ZONE variable", __SRCNAME__, __LINE__)

    ET_ZONE = pack( pET_ZONE%pGrdBase%iData, lActive )

    allocate ( ET_RATIOS( count( lActive ) ), stat=iStat)
    call assert(iStat==0, "Failed to allocate memory for the ET_RATIOS variable", __SRCNAME__, __LINE__)

    ! look up the name of the fragments file in the control file dictionary
    call CF_DICT%get_values( sKey="ET_RATIO_MONTHLY_FILE", slString=slString )

    ! use the first entry in the string list slString as the filename to open for
    ! use with the daily fragments routine
    call read_et_ratio_table( slString%get(1) )

  end subroutine et_zone_values_initialize

!--------------------------------------------------------------------------------------------------

  subroutine read_et_ratio_table( sFilename )

    character (len=*), intent(in)    :: sFilename

    ! [ LOCALS ]
    character (len=65536)   :: sRecord, sSubstring
    integer (c_int)    :: iStat
    integer (c_int)    :: iLineNum
    integer (c_int)    :: iFieldNum
    integer (c_int)    :: iIndex
    integer (c_int)    :: iNumLines
    integer (c_int)    :: iNumFields
    type (ASCII_FILE_T)     :: ET_RATIO_FILE

    integer (c_int), parameter :: ET_ZONE_FIELD = 1

    call ET_RATIO_FILE%open( sFilename = sFilename, &
                  sCommentChars = "#%!", &
                  sDelimiters = "WHITESPACE", &
                  lHasHeader = .false._c_bool )

    iNumLines = ET_RATIO_FILE%numLines()

    ! read in next line of file
    sRecord = ET_RATIO_FILE%readLine()

    iNumFields = fieldCount( sRecord )

    allocate(  ET_TABLE_VALUES( iNumLines, iNumFields ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory for et ratio table values", &
      __SRCNAME__, __LINE__ )

    iLineNum = 0
    iFieldNum = 0

    do

      ! read in next line of file
      sRecord = ET_RATIO_FILE%readLine()

      if ( ET_RATIO_FILE%isEOF() ) exit

      iLineNum = iLineNum + 1
      iFieldNum = 0

      ! read in ET_ZONE
      call chomp(sRecord, sSubstring, ET_RATIO_FILE%sDelimiters )

      if ( len_trim(sSubstring) == 0 ) &
      call die( "Missing ET ZONE in the monthly et ratio file",              &
               __SRCNAME__, __LINE__, "Problem occured on line number "       &
               //asCharacter(ET_RATIO_FILE%currentLineNum() )             &
               //" of file "//dquote(sFilename) )

       ET_TABLE_VALUES(ET_ZONE_FIELD, iLineNum ) = asInt( sSubstring )

       do iIndex = 2, iNumFields

!         ! read in ET for each month of yeat
         call chomp(sRecord, sSubstring, ET_RATIO_FILE%sDelimiters )

         if ( len_trim(sSubstring) == 0 ) &
           call die( "Missing or corrupt value in the et ratio file",        &
                     __SRCNAME__, __LINE__, "Problem occured on line number "    &
                     //asCharacter(ET_RATIO_FILE%currentLineNum() )          &
                     //" of file "//dquote(sFilename) )

         ET_TABLE_VALUES(iLineNum, iIndex ) = asFloat( sSubstring )

      enddo

    enddo

  end subroutine read_et_ratio_table

!--------------------------------------------------------------------------------------------------

  subroutine et_zone_values_calculate( )

    ! [ LOCALS ]
    integer (c_int)  :: iLineNum
    integer (c_int)  :: iFieldNum
    integer (c_int)  :: iET_zone_id
    real (c_float)   :: fFactor
    integer (c_int)  :: iCount

    ET_RATIOS = 0.0_c_float
    iCount = 0.0_c_float

    associate ( dt => SIM_DT%curr )

      call pET_GRID%getvalues( dt )

      ! this assumes the input file is structured such that fields 2-13
      ! correspond to the ET ratios for months 1-12 (Jan-Dec)
      iFieldNum = dt%iMonth + 1

      ! if it is the first day of the month, update the ratio values
      if ( dt%iDay == 1 ) then

        do iLineNum = lbound(ET_TABLE_VALUES, 1), ubound(ET_TABLE_VALUES, 1)

          iET_zone_id = ET_TABLE_VALUES(iLineNum, 1)
          iCount = iCount + count( ET_ZONE == iFieldNum )

          where ( ET_ZONE == iET_zone_id )

            ET_RATIOS = ET_TABLE_VALUES( iLineNum, iFieldNum )

          end where

        enddo

      endif

    end associate

  end subroutine et_zone_values_calculate

end module et__zone_values
