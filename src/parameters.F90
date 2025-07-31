module parameters

  use iso_c_binding, only    : c_int, c_float, c_double, c_bool
  use datetime, only         : DATETIME_T
  use exceptions
  use file_operations, only  : ASCII_FILE_T
  use logfiles
  use fstring
  use fstring_list, only     : FSTRING_LIST_T
  use dictionary
  use constants_and_conversions
  implicit none

  private

  !! module to provide a single POC for storage and retrieval of parameter scalars and vectors
  !! need to provide a method for storing, finding, and retrieving parameters.
  !! dictionary: keyword, string_list

  !! first create list of files. parse through each file, adding to the param dictionary.
  !! once complete, allow other modules to interrogate the dictionary. return matches in the
  !! data type required for the parameter. once all params are in place, the data structures can be
  !! deallocated.

  type, public :: PARAMETERS_T

    type (FSTRING_LIST_T)               :: filenames
    type (FSTRING_LIST_T)               :: delimiters
    type (FSTRING_LIST_T)               :: comment_chars
    integer (c_int)                     :: count           = 0

  contains

    procedure            :: add_filename_to_list_sub
    generic              :: add_file => add_filename_to_list_sub

    procedure            :: add_to_param_list_sub
    generic              :: add_parameters => add_to_param_list_sub

    procedure            :: munge_files_and_add_to_param_list_sub
    generic              :: munge_file => munge_files_and_add_to_param_list_sub

    procedure            :: get_parameter_values_int
    procedure            :: get_parameter_values_float
    procedure            :: get_parameter_values_logical
    procedure            :: get_parameter_table_float
    procedure            :: get_parameter_values_string_list

    generic              :: get_parameters => get_parameter_values_int,         &
                                              get_parameter_values_float,       &
                                              get_parameter_table_float,        &
                                              get_parameter_values_logical,     &
                                              get_parameter_values_string_list

    procedure            :: grep_name => grep_parameter_name


    ! other functionality:
    !  * retrieve parameter list:
    !      1) input = single string
    !      2) input = string list
    !  * basic error checking re: number of params in list



  end type PARAMETERS_T

  type (PARAMETERS_T), public :: PARAMS
  type (DICT_T), public       :: PARAMS_DICT

  integer (c_int), parameter  :: MAX_TABLE_RECORD_LEN = 2048

contains

  subroutine add_filename_to_list_sub( this, sFilename, sDelimiters, sCommentChars )

    class (PARAMETERS_T)                          :: this
    character (len=*), intent(in)                 :: sFilename
    character (len=*), intent(in), optional       :: sDelimiters
    character (len=*), intent(in), optional       :: sCommentChars

    ! [ LOCALS ]
    character (len=:), allocatable  :: sDelimiters_
    character (len=:), allocatable  :: sCommentChars_

    if ( present(sCommentChars) ) then
      sCommentChars_ = sCommentChars
    else
      sCommentChars_ = COMMENT_CHARACTERS
    endif

    if (present(sDelimiters) ) then
      sDelimiters_ = sDelimiters
    else
      sDelimiters_ = TAB
    endif

    this%count = this%count + 1

    call this%filenames%append( sFilename )
    call this%delimiters%append( sDelimiters_ )
    call this%comment_chars%append( sCommentChars_ )

  end subroutine add_filename_to_list_sub

!--------------------------------------------------------------------------------------------------

  subroutine munge_files_and_add_to_param_list_sub(this, comment_chars, delimiters)

    class (PARAMETERS_T)    :: this
    character (len=*), intent(in), optional  :: comment_chars
    character (len=*), intent(in), optional  :: delimiters

    ! [ LOCALS ]
    integer (c_int)                :: iFileIndex, iColIndex, iTempIndex
    integer (c_int)                :: iStat
    type (ASCII_FILE_T)            :: DF
    type (DICT_ENTRY_T), pointer   :: pDict
    type (DICT_ENTRY_T), pointer   :: pCurrentDict
    integer (c_int)                :: iNumberOfHeaderLines
    character (len=:), allocatable :: sNumberOfHeaderLines
    character (len=256)            :: column_name
    character (len=256)            :: tempstr2
    character (len=256)            :: qualified_column_name
    character (len=256)            :: filename1
    character (len=:), allocatable :: comment_chars_
    character (len=:), allocatable :: delimiters_
    logical (c_bool), allocatable  :: skip_this_column(:)
    type (FSTRING_LIST_T)          :: unique_file_list
    integer (kind=c_int)           :: row_indx
    integer (c_int)                :: number_of_columns
    character (len=MAX_TABLE_RECORD_LEN) :: sRecord, sItem

    if ( present(comment_chars) ) then
      comment_chars_ = trim(comment_chars)
    else
      comment_chars_ = COMMENT_CHARACTERS
    endif

    if ( present(delimiters) ) then
      delimiters_ = trim(delimiters)
    else
      delimiters_ = TAB
    endif

    !
    ! proper way to screen for duplicate filenames is further down in loop
    !
    !unique_file_list = this%filenames%unique()
    !if ( unique_file_list%get(1) .ne. '<NA>' ) then

    if ( this%filenames%get(1) .ne. '<NA>' ) then

      do iFileIndex = 1, this%filenames%count

        filename1 = this%filenames%get(iFileIndex)

        ! if this filename has already been seen and processed, ignore and move on to next filename
        if ( unique_file_list%count_matching(filename1) > 0 ) cycle

        call unique_file_list%append(filename1)

        ! open the file associated with current file index value
        call DF%open(sFilename = filename1,                                  &
                     sCommentChars = comment_chars_,                         &
                     sDelimiters = delimiters_ )

        ! obtain the headers from the file
        DF%slColNames = DF%readHeader()

        number_of_columns = DF%slColNames%count

        call LOGS%write( "Number of columns in file: "//asCharacter( number_of_columns ), iTab=35)

        !call DF%slColNames%print()

        if (allocated(skip_this_column))  deallocate(skip_this_column)
        allocate(skip_this_column(number_of_columns))
        skip_this_column = FALSE

        ! loop over each column header
        do iColIndex = 1, DF%slColNames%count

          column_name = DF%slColNames%get(iColIndex)

          if ( PARAMS_DICT%key_already_in_use( column_name ) ) then

            skip_this_column( iColIndex ) = TRUE

            call warn("Column name "//sQuote(column_name)//" already in use. Data in this column will be ignored." &
              //" [filename = "//sQuote(filename1)//"]")

            ! Dec 2019: let try eliminating duplicates from the dictionary altogether

            ! ! add dictionary entry to dictionary, tack "DUP" on end of name
            ! tempstr = trim(adjustl(tempstr))//"_DUP"
            ! ! update the string list to reflect duplicate entry

            ! call DF%slColNames%replace( iColIndex, tempstr )
            ! call pDict%add_key( asUppercase( tempstr ) )
            ! call PARAMS_DICT%add_entry( pDict )

          else

            ! create and allocate memory for dictionary entry
            pDict => null()
            allocate( pDict, stat=iStat )
            call assert(iStat == 0, "Failed to allocate memory for dictionary object", &
              __FILE__, __LINE__ )

            ! first add the key to the dictionary entry,
            ! then add dictionary entry to dictionary
            call pDict%add_key( column_name )
            call PARAMS_DICT%add_entry( pDict )

          endif

        enddo

        row_indx = 0

        ! now read in the remainder of the file
        do while ( .not. DF%isEOF() )

          ! read in next line of file
          sRecord = DF%readLine()

          ! skip blank lines
          if ( len_trim(sRecord) == 0 ) cycle

          row_indx = row_indx + 1

          ! loop over each column header
          do iColIndex = 1, DF%slColNames%count

            ! break off next column of data for the current record
            call chomp(sRecord, sItem, this%delimiters%get(iFileIndex) )

            if ( skip_this_column(iColIndex) )  cycle

            column_name = DF%slColNames%get(iColIndex)
          
            ! find pointer associated with header name
            ! (inefficient, but should be OK for small # of columns)
            pCurrentDict => PARAMS_DICT%get_entry( column_name )

            if ( associated( pCurrentDict )) then

              ! if not null, it means that we were able to return a pointer
              ! associated with the current column heading
              call pCurrentDict%add_value( sItem )

            else

              call warn("Internal programming error: null pointer detected" &
                //" -- was trying to find pointer associated with column "//dquote(DF%slColNames%get(iColIndex)), &
                __FILE__, __LINE__)

            endif

          enddo

        enddo

        deallocate(skip_this_column)

        call DF%close()

      enddo
      
    endif

  end subroutine munge_files_and_add_to_param_list_sub

!--------------------------------------------------------------------------------------------------

  subroutine add_to_param_list_sub(this, sKey, sValues, iValues, fValues, dValues, lValues)

    class (PARAMETERS_T)                         :: this
    character (len=*), intent(in)                :: sKey
    character (len=*), intent(in), optional      :: sValues(:)
    integer (c_int), intent(in), optional   :: iValues(:)
    real (c_float), intent(in), optional    :: fValues(:)
    real (c_double), intent(in), optional   :: dValues(:)
    logical (c_bool), intent(in), optional  :: lValues(:)

    ! [ LOCALS ]
    integer (c_int)         :: iStat
    type (DICT_ENTRY_T), pointer :: pDict
    integer (c_int)         :: iIndex
    type (DICT_ENTRY_T), pointer :: pCurrentDict
    character (len=MAX_TABLE_RECORD_LEN) :: sRecord, sItem


    pCurrentDict => null()
    pCurrentDict => PARAMS_DICT%get_entry( sKey )

    if ( .not. associated( pCurrentDict )) then

      ! if key does not currently exist, make a new entry with this key value
      allocate( pCurrentDict, stat=iStat )

      call assert(iStat == 0, "Failed to allocate memory for dictionary object", &
          __FILE__, __LINE__ )

      ! add dictionary entry to dictionary
      call pCurrentDict%add_key( sKey )
      call PARAMS_DICT%add_entry( pCurrentDict )

    endif

    if ( present( sValues ) ) then

      do iIndex = lbound(sValues,1), ubound(sValues,1)

        call pCurrentDict%add_value( sValues( iIndex ) )

      enddo

    else if ( present ( iValues ) ) then

      do iIndex = lbound(iValues,1), ubound(iValues,1)

        call pCurrentDict%add_value( iValues( iIndex ) )

      enddo

    else if ( present ( fValues ) ) then

      do iIndex = lbound(fValues,1), ubound(fValues,1)

        call pCurrentDict%add_value( fValues( iIndex ) )

      enddo

    else if ( present ( dValues ) ) then

      do iIndex = lbound(dValues,1), ubound(dValues,1)

        call pCurrentDict%add_value( dValues( iIndex ) )

      enddo

    else if ( present ( lValues ) ) then

      do iIndex = lbound(lValues,1), ubound(lValues,1)

        call pCurrentDict%add_value( lValues( iIndex ) )

      enddo

    endif

  end subroutine add_to_param_list_sub

!--------------------------------------------------------------------------------------------------

  function grep_parameter_name( this, sKey, lFatal )      result( slList )

    class (PARAMETERS_T)                                       :: this
    character (len=*), intent(in)                              :: sKey
    logical (c_bool), intent(in), optional                :: lFatal
    type ( FSTRING_LIST_T )                                     :: slList

    ! [ LOCALS ]
    logical (c_bool) :: lFatal_l

    if ( present (lFatal) ) then
      lFatal_l = lFatal
    else
      lFatal_l = FALSE
    endif

    slList = PARAMS_DICT%grep_keys( asUppercase( sKey ) )

    if ( lFatal_l ) then

       if ( slList%get(1) .strequal. "<NA>" )                              &
         call warn( "Failed to find a lookup table column whose name contains "        &
           //dQuote( sKey )//".", lFatal = lFatal_l )

    endif

  end function grep_parameter_name

!--------------------------------------------------------------------------------------------------

  subroutine get_parameter_values_logical( this, lValues, slKeys, sKey, lFatal )

    class (PARAMETERS_T)                                        :: this
    logical (c_bool), intent(in out), allocatable          :: lValues(:)
    type (FSTRING_LIST_T), intent(in out),              optional :: slKeys
    character (len=*),    intent(in ),                 optional :: sKey
    logical (c_bool), intent(in),                 optional :: lFatal

    ! [ LOCALS ]
    logical (c_bool) :: lFatal_l

    if ( present (lFatal) ) then
      lFatal_l = lFatal
    else
      lFatal_l = FALSE
    endif

    if ( present( slKeys) ) then

      call PARAMS_DICT%get_values( slKeys=slKeys, lValues=lValues,            &
        is_fatal=lFatal_l )

!       if ( any( iValues <= iTINYVAL ) ) &
!         call warn( "Failed to find a lookup table column named " &
!           //dQuote( slKeys%list_all() )//".", lFatal = lFatal_l )

    else if ( present( sKey) ) then

      call PARAMS_DICT%get_values( sKey=sKey, lValues=lValues )

!       if ( any( iValues <= iTINYVAL ) ) &
!         call warn( "Failed to find a lookup table column named " &
!           //dQuote( sKey )//".", lFatal = lFatal_l )

    endif

  end subroutine get_parameter_values_logical

!--------------------------------------------------------------------------------------------------

  subroutine get_parameter_values_datetime( this, dtValues, slKeys, sKey, lFatal )

    class (PARAMETERS_T)                                        :: this
    type (DATETIME_T), intent(in out), allocatable              :: dtValues(:)
    type (FSTRING_LIST_T), intent(in out),             optional :: slKeys
    character (len=*),    intent(in ),                 optional :: sKey
    logical (c_bool), intent(in),                      optional :: lFatal

    ! [ LOCALS ]
    logical (c_bool)                        :: lFatal_l
    type (FSTRING_LIST_T)                   :: slValues
    integer (c_int)                         :: n
    integer (c_int)                         :: istat

    if ( present (lFatal) ) then
      lFatal_l = lFatal
    else
      lFatal_l = FALSE
    endif

    if ( present( slKeys) ) then

      call PARAMS_DICT%get_values( slKeys=slKeys, slString=slValues, is_fatal=lFatal_l )

    else if ( present( sKey) ) then

      call PARAMS_DICT%get_values( sKey=sKey, slString=slValues )

    endif

    allocate(dtValues(slValues%count), stat=istat)

    do n=1, slValues%count
      call dtValues(n)%parseDate(slValues%get(n))
    enddo  

  end subroutine get_parameter_values_datetime

  !--------------------------------------------------------------------------------------------------

  subroutine get_parameter_values_string_list( this, slValues, slKeys, sKey, lFatal )

    class (PARAMETERS_T)                               :: this
    type (FSTRING_LIST_T), intent(out)                 :: slValues
    type (FSTRING_LIST_T), intent(inout), optional     :: slKeys
    character (len=*), intent(in ), optional           :: sKey
    logical (c_bool), intent(in), optional             :: lFatal

    ! [ LOCALS ]
    logical (c_bool) :: lFatal_l

    if ( present (lFatal) ) then
      lFatal_l = lFatal
    else
      lFatal_l = FALSE
    endif

    if ( present( slKeys) ) then

      call PARAMS_DICT%get_values( slKeys=slKeys, slString=slValues,          &
        is_fatal=lFatal_l )

       if ( slValues%get(1) .strequal. "<NA>" ) then
         call warn( "Failed to find a lookup table column named "             &
           //dQuote( slKeys%list_all() )//".", lFatal = lFatal_l )
       end if

    else if ( present( sKey) ) then

      call PARAMS_DICT%get_values( sKey=sKey, slString=slValues )

      if ( slValues%get(1) .strequal. "<NA>" ) then
        call warn( "Failed to find a lookup table column named "              &
          //dQuote( sKey )//".", lFatal = lFatal_l )
      end if

    endif

  end subroutine get_parameter_values_string_list

!--------------------------------------------------------------------------------------------------

  subroutine get_parameter_values_int( this, iValues, slKeys, sKey, lFatal )

    class (PARAMETERS_T)                                       :: this
    integer (c_int), intent(out), allocatable             :: iValues(:)
    type (FSTRING_LIST_T), intent(in out),             optional :: slKeys
    character (len=*),    intent(in ),                optional :: sKey
    logical (c_bool), intent(in),                optional :: lFatal

    ! [ LOCALS ]
    logical (c_bool) :: lFatal_l

    if ( present (lFatal) ) then
      lFatal_l = lFatal
    else
      lFatal_l = FALSE
    endif

    if ( present( slKeys) ) then

      call PARAMS_DICT%get_values( slKeys=slKeys, iValues=iValues,            &
        is_fatal=lFatal_l )

      if ( any( iValues <= iTINYVAL ) ) &
        call warn( "Failed to find a lookup table column named " &
          //dQuote( slKeys%list_all() )//".", lFatal = lFatal_l )

    else if ( present( sKey) ) then

      call PARAMS_DICT%get_values( sKey=sKey, iValues=iValues,                &
        is_fatal=lFatal_l )

      if ( any( iValues <= iTINYVAL ) ) &
        call warn( "Failed to find a lookup table column named " &
          //dQuote( sKey )//".", lFatal = lFatal_l )

    endif

  end subroutine get_parameter_values_int

!--------------------------------------------------------------------------------------------------

  subroutine get_parameter_values_float( this, fValues, slKeys, sKey, lFatal )

    class (PARAMETERS_T)                                       :: this
    real (c_float),  intent(in out), allocatable               :: fValues(:)
    type (FSTRING_LIST_T), intent(in out),            optional :: slKeys
    character (len=*),    intent(in ),                optional :: sKey
    logical (c_bool), intent(in),                     optional :: lFatal

    ! [ LOCALS ]
    logical (c_bool) :: lFatal_l

    if ( present (lFatal) ) then
      lFatal_l = lFatal
    else
      lFatal_l = FALSE
    endif

    if ( present( slKeys) ) then

      call PARAMS_DICT%get_values( slKeys=slKeys, fValues=fValues )

      if ( any( fValues <= fTINYVAL ) ) &
        call warn( "Failed to find a lookup table column named " &
          //dQuote( slKeys%list_all() )//".", lFatal = lFatal_l )

    else if ( present( sKey) ) then

      call PARAMS_DICT%get_values( sKey=sKey, fValues=fValues,                &
        is_fatal=lFatal_l )

      if ( any( fValues <= fTINYVAL ) ) &
        call warn( "Failed to find a lookup table column named " &
          //dQuote( sKey )//".", lFatal = lFatal_l )

    endif

  end subroutine get_parameter_values_float

!--------------------------------------------------------------------------------------------------

  subroutine get_parameter_table_float( this, fValues, sPrefix, iNumRows, lFatal )

    use fstring

    class (PARAMETERS_T)                                       :: this
    real (c_float),  intent(in out), allocatable               :: fValues(:,:)
    character (len=*),    intent(in)                           :: sPrefix
    integer (c_int), intent(in)                                :: iNumRows
    logical (c_bool), intent(in),                     optional :: lFatal

    ! [ LOCALS ]
    integer (c_int)             :: iIndex
    integer (c_int)             :: iStat
    character (len=256)         :: sText
    integer (c_int)             :: iNumCols
    type (FSTRING_LIST_T)        :: slList
    real (c_float), allocatable :: fTempVal(:)
    logical (c_bool)            :: lFatal_l

    if ( present (lFatal) ) then
      lFatal_l = lFatal
    else
      lFatal_l = FALSE
    endif

    slList = PARAMS%grep_name( sPrefix )

    iNumCols = slList%count

    if ( iNumCols == 0 ) then

      call warn( "Failed to find a lookup table column named " &
        //dQuote( sPrefix )//".", lFatal = lFatal_l )

    else

      allocate( fTempVal( iNumRows ), stat=iStat )
      call assert( iStat == 0, "Problem allocating memory.", __FILE__, __LINE__ )

      allocate( fValues( iNumRows, iNumCols ), stat=iStat )
      call assert( iStat == 0, "Problem allocating memory."                            &
        //"~iNumCols: "//asCharacter(iNumCols)//"; iNumRows: "//asCharacter(iNumRows)  &
        //" sPrefix: "//dQuote(sPrefix), __FILE__, __LINE__ )

      do iIndex = 1, iNumCols

        sText = trim( slList%get( iIndex ) )
        call PARAMS_DICT%get_values( sKey=sText, fValues=fTempVal,            &
          is_fatal=lFatal_l )

        call assert( size( fTempVal, 1) == size( fValues, 1),                 &
          "Mismatch in array size. Dictionary key: "//squote( sText )         &
          //"  expected size: "//asCharacter( size( fValues, 1) )             &
          //"  size of items in dictionary: "                                 &
          //asCharacter( size( fTempVal, 1) )   )

        fValues(:,iIndex) = fTempVal
      enddo

    endif

  end subroutine get_parameter_table_float

end module parameters
