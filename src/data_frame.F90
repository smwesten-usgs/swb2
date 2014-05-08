module data_frame

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use iso_fortran_env, only : IOSTAT_END
  use constants_and_conversions
  use exceptions
  use strings
  use string_list
  use data_column
  use datetime
  use types_new
  implicit none

  private

  public :: DATA_FRAME_T


  type DATA_FRAME_T

    integer (kind=c_int), allocatable                        :: iDataTypes(:)
    type (STRING_LIST_T)                                     :: slColNames
    logical (kind=c_bool), dimension(:), allocatable         :: lMask
    type (T_DATA_COLUMN_PTR), allocatable                    :: columns(:)
    logical (kind=c_bool)                                    :: lHasDate = lFALSE

  contains

    ! procedure :: makeUnique => make_unique_identifier_sub
    !> take contents of serveral columns and concatenate them to
    !> create a unique ID (think METALICUS)

    procedure :: initialize_data_frame_sub
    generic   :: initialize => initialize_data_frame_sub

    procedure :: populate_data_frame_by_row_sub
    generic   :: putrow => populate_data_frame_by_row_sub

    procedure :: summarize_data_frame_sub
    generic   :: summarize => summarize_data_frame_sub

    procedure :: find_column_by_name_fn
    generic   :: findcol => find_column_by_name_fn

    procedure :: select_rows_from_column_int_sub
    procedure :: select_rows_from_column_float_sub
    procedure :: select_rows_from_column_double_sub
    procedure :: select_rows_from_column_datetime_sub
    procedure :: select_rows_from_column_string_sub

    generic   :: select => select_rows_from_column_int_sub, &
                                    select_rows_from_column_float_sub, &
                                    select_rows_from_column_double_sub, &
                                    select_rows_from_column_datetime_sub, &
                                    select_rows_from_column_string_sub

    procedure :: get_column_pointer_byindex_fn
    procedure :: get_column_pointer_byname_fn
    generic   :: getcol => get_column_pointer_byindex_fn, &
                                    get_column_pointer_byname_fn

  end type DATA_FRAME_T


contains


  subroutine select_rows_from_column_int_sub( this, sColname, iComparison, iValue1, iValue2)

    class (DATA_FRAME_T), intent(inout)            :: this
    character (len=*), intent(in)                  :: sColname
    integer (kind=c_int), intent(in)               :: iComparison
    integer (kind=c_int), intent(in)               :: iValue1
    integer (kind=c_int), intent(in), optional     :: iValue2

    ! [ LOCALS ]
    class (T_DATA_COLUMN), pointer :: pColumn

    pColumn => this%getcol( sColname )

    if ( associated(pColumn) ) then

      if (present(iValue2) ) then

        call pColumn%select( iValue1, iComparison, iValue2 )

      else

        call pColumn%select( iValue1, iComparison )

      endif

      this%lMask = pColumn%lMask

    endif

  end subroutine select_rows_from_column_int_sub




  subroutine select_rows_from_column_float_sub( this, sColname, iComparison, fValue1, fValue2)

    class (DATA_FRAME_T), intent(inout)            :: this
    character (len=*), intent(in)                  :: sColname
    integer (kind=c_int), intent(in)               :: iComparison
    real (kind=c_float), intent(in)                :: fValue1
    real (kind=c_float), intent(in), optional      :: fValue2

    ! [ LOCALS ]
    class (T_DATA_COLUMN), pointer :: pColumn

    pColumn => this%getcol( sColname )

    if ( associated(pColumn) ) then

      if (present(fValue2) ) then

        call pColumn%select( fValue1, iComparison, fValue2 )

      else

        call pColumn%select( fValue1, iComparison )

      endif

      this%lMask = pColumn%lMask

    endif

  end subroutine select_rows_from_column_float_sub



  subroutine select_rows_from_column_double_sub( this, sColname, iComparison, dValue1, dValue2)

    class (DATA_FRAME_T), intent(inout)            :: this
    character (len=*), intent(in)                  :: sColname
    integer (kind=c_int), intent(in)               :: iComparison
    real (kind=c_double), intent(in)               :: dValue1
    real (kind=c_double), intent(in), optional     :: dValue2

    ! [ LOCALS ]
    class (T_DATA_COLUMN), pointer :: pColumn
   
    pColumn => this%getcol( sColname )

    if ( associated(pColumn) ) then

      if (present(dValue2) ) then

        call pColumn%select( dValue1, iComparison, dValue2 )

      else

        call pColumn%select( dValue1, iComparison )

      endif

      this%lMask = pColumn%lMask

    endif

  end subroutine select_rows_from_column_double_sub


  subroutine select_rows_from_column_datetime_sub( this, sColname, iComparison, dtValue1, dtValue2)

    class (DATA_FRAME_T), intent(inout)            :: this
    character (len=*), intent(in)                  :: sColname
    integer (kind=c_int), intent(in)               :: iComparison
    type (T_DATETIME), intent(in)                  :: dtValue1
    type (T_DATETIME), intent(in), optional        :: dtValue2

    ! [ LOCALS ]
    class (T_DATA_COLUMN), pointer :: pColumn
  
    pColumn => this%getcol( sColname )

    if ( associated(pColumn) ) then

      if (present(dtValue2) ) then

        call pColumn%select( dtValue1, iComparison, dtValue2 )

      else

        call pColumn%select( dtValue1, iComparison )

      endif

      this%lMask = pColumn%lMask

    endif

  end subroutine select_rows_from_column_datetime_sub



  subroutine select_rows_from_column_string_sub( this, sColname, iComparison, sValue1, sValue2)

    class (DATA_FRAME_T), intent(inout)            :: this
    character (len=*), intent(in)                  :: sColname
    integer (kind=c_int), intent(in)               :: iComparison
    character (len=*), intent(in)                  :: sValue1
    character (len=*), intent(in), optional        :: sValue2

    ! [ LOCALS ]
    class (T_DATA_COLUMN), pointer :: pColumn

    pColumn => this%getcol( sColname )
    if ( associated(pColumn) ) then

      if (present(sValue2) ) then

        call pColumn%select( sValue1, iComparison, sValue2 )

      else

        call pColumn%select( sValue1, iComparison )

      endif

      this%lMask = pColumn%lMask

    endif

  end subroutine select_rows_from_column_string_sub


  function get_column_pointer_byindex_fn( this, iColNum )    result( pColumn )

    class (DATA_FRAME_T), intent(in)         :: this
    integer (kind=c_int)                     :: iColNum
    class (T_DATA_COLUMN), pointer           :: pColumn

    pColumn => null()

    if (allocated(this%columns)) then

      if (iColNum >= lbound(this%columns,1) &
         .and. iColNum <= ubound(this%columns,1) ) then

        pColumn => this%columns(iColNum)%pColumn

      endif

    else

      call warn("'Columns' member has not been allocated yet.", __FILE__, __LINE__)

    endif

  end function get_column_pointer_byindex_fn



  function get_column_pointer_byname_fn( this, sColName )    result( pColumn )

    class (DATA_FRAME_T), intent(in)         :: this
    character (len=*), intent(in)            :: sColName
    class (T_DATA_COLUMN), pointer           :: pColumn

    ! [ LOCALS ]
    integer (kind=c_int), allocatable :: iColNum(:)

    iColNum = this%findcol(sColName)

    if (ubound(iColNum,1) > 0) then

      pColumn => this%getcol(iColNum(1))

    else

      pColumn => null()
      call warn("Failed to find a column with name "//trim(sColName), __FILE__,__LINE__)

    endif

  end function get_column_pointer_byname_fn



  function find_column_by_name_fn( this, sColName )   result( iColNum )

    class (DATA_FRAME_T), intent(in)   :: this
    character (len=*), intent(in)      :: sColName
    integer (kind=c_int), allocatable  :: iColNum(:)

    ! [ LOCALS ]
    integer (kind=c_int) :: iCount

    iCount = this%slColNames%countmatching(sColName)

    if (iCount > 0) then

!      allocate(iColNum(iCount))
      iColNum = this%slColNames%which(sColName)

    else

      allocate(iColNum(1))
      iColNum = 0

    endif

  end function find_column_by_name_fn



  subroutine initialize_data_frame_sub( this, slColNames, iRecordCount, iDataTypes )

    class (DATA_FRAME_T), intent(inout)          :: this
    type (STRING_LIST_T), intent(in)             :: slColNames
    integer (kind=c_int), intent(in)             :: iRecordCount
    integer (kind=c_int), intent(in), optional   :: iDataTypes(:)

    ! [ LOCALS ]
    integer (kind=c_int)              :: iStat
    integer (kind=c_int)              :: iIndex
    integer (kind=c_int), allocatable :: iDataTypes_(:)
    character (len=64)                :: sText
    character (len=:), allocatable    :: sString
    integer (kind=c_int)              :: iNumberOfColumns

    this%slColNames = slColNames
    iNumberOfColumns = this%slColNames%count

    !> if iDataTypes is supplied, use it; otherwise assume all columns will be 
    !> of type STRING_DATA
    if ( present(iDataTypes) ) then
      iDataTypes_ = iDataTypes
      this%iDataTypes = iDataTypes
     else
       allocate(iDatatypes_( iNumberOfColumns ), stat=iStat)
       call assert(iStat==0, "Failed to allocate memory for data types array", __FILE__, __LINE__)
       iDataTypes_ = STRING_DATA
       this%iDataTypes = iDataTypes_
     endif   

    !> allocate space for the required number of class T_DATA_COLUMN_PTR
    allocate( this%columns( iNumberOfColumns ), stat=iStat )

    call assert(iStat==0, "Failed to allocate memory for data frame", __FILE__, __LINE__)

    do iIndex = 1, iNumberOfColumns

      allocate( T_DATA_COLUMN :: this%columns(iIndex)%pColumn, stat=iStat )
      
      associate ( col => this%columns(iIndex)%pColumn )
        call col%new( iDataType = iDataTypes_(iIndex), iCount = iRecordCount )
        sString = this%slColNames%get(iIndex)
        print *, " Creating new column for "//sString                          &
                 //" with room for "//asCharacter(iRecordCount)//" values."
      end associate

    enddo

    ! initialize dataframe row mask; all are selected initially
    allocate( this%lMask(iRecordCount), stat=iStat )
    this%lMask = lTRUE

  end subroutine initialize_data_frame_sub



  subroutine populate_data_frame_by_row_sub( this, sString, sDelimiters )

    class (DATA_FRAME_T), intent(inout) :: this
    character (len=*), intent(inout) :: sString
    character (len=*), intent(in) :: sDelimiters

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iColNum, iRowNum
    character (len=len(sString))  :: sSubString
    integer (kind=c_int) :: iRecnum

    integer (kind=c_int) :: iValue
    real (kind=c_float)  :: fValue
    real (kind=c_double) :: dValue

    iIndex = 0

    do while ( len_trim(sString) > 0)

      call chomp(sString, sSubString, sDelimiters)

      iIndex = iIndex + 1

      !iRowNum = this%Columns(iIndex)%pColumn%incrementRecnum()

      if (iIndex > ubound(this%columns,1))  call die ("Too many columns read in.")

      associate (col => this%columns(iIndex)%pColumn )

        select case ( col%datatype() )

          case (INTEGER_DATA)

            iValue = asInt(sSubString)
            iRecnum = col%putval( iValue )

          case (FLOAT_DATA)

            fValue = asFloat(sSubString)
            iRecnum = col%putval( fValue )

          case (DOUBLE_DATA)

            dValue = asDouble(sSubString)
            iRecnum = col%putval( dValue )

          case (STRING_DATA)

            iRecnum = col%putval( sSubString )

          case (DATETIME_DATA)

            iRecnum = col%putdatetime( sSubstring )

          case (DATE_DATA)

            iRecNum = col%putdate( sSubstring )

          case (TIME_DATA)

            iRecNum = col%puttime( sSubString )

          case default

            call die("Unhandled select case", __FILE__, __LINE__)

        end select

      end associate

    enddo


  end subroutine populate_data_frame_by_row_sub



  subroutine summarize_data_frame_sub( this )

    class (DATA_FRAME_T), intent(inout) :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int) :: iColNum, iRowNum

    ! [ LOCALS ]
    real (kind=c_double) :: dValue
    type (T_DATETIME) :: DT

    do iIndex = 1, ubound(this%Columns,1)

      write(*, "(/,a)") "Variable name: "//this%slColNames%get(iIndex)
      write (*, fmt="(5x, a, i8)") "Count: ", int( this%columns(iIndex)%pColumn%count( this%lMask ) )

      select case ( this%columns(iIndex)%pColumn%datatype() )

        case (INTEGER_DATA, FLOAT_DATA, DOUBLE_DATA)

          write (*, fmt="(7x, a, g15.5)") "Min: ", this%columns(iIndex)%pColumn%min( this%lMask )
          write (*, fmt="(7x, a, g15.5)") "Max: ", this%columns(iIndex)%pColumn%max( this%lMask )
          write (*, fmt="(7x, a, g15.5)") "Sum: ", this%columns(iIndex)%pColumn%sum( this%lMask )
          write (*, fmt="(7x, a, g15.5)") "Mean: ", this%columns(iIndex)%pColumn%mean( this%lMask )

        case (DATETIME_DATA, DATE_DATA)

          dValue = this%columns(iIndex)%pColumn%min( this%lMask )
          call DT%setJulianDate(dValue)
          write(*, fmt="(7x,a,a)") "Min: ", DT%prettydate()

          dValue = this%columns(iIndex)%pColumn%max( this%lMask )
          call DT%setJulianDate(dValue)
          write(*, fmt="(7x,a,a)") "Max: ", DT%prettydate()

      end select

    enddo


  end subroutine summarize_data_frame_sub


end module data_frame
