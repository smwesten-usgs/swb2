module parameters

  use iso_c_binding, only    : c_int, c_float, c_double, c_bool
  use exceptions
  use file_operations, only  : ASCII_FILE_T
  use logfiles
  use fstring
  use fstring_list, only      : FSTRING_LIST_T
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
    integer (c_int)                    :: count           = 0

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

    generic              :: get_parameters => get_parameter_values_int,     &
                                              get_parameter_values_float,   &
                                              get_parameter_table_float,    &
                                              get_parameter_values_logical, &
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

  integer (c_int), parameter :: MAX_TABLE_RECORD_LEN = 512

contains

  subroutine add_filename_to_list_sub( this, sFilename, sDelimiters, sCommentChars )

    class (PARAMETERS_T)                          :: this
    character (len=*), intent(in)                 :: sFilename
    character (len=*), intent(in), optional       :: sDelimiters
    character (len=*), intent(in), optional       :: sCommentChars

    ! [ LOCALS ]
    character (len=:), allocatable  :: sDelimiters_l
    character (len=:), allocatable  :: sCommentChars_l

    if (present(sDelimiters) ) then
      sDelimiters_l = sDelimiters
    else
      sDelimiters_l = TAB
    endif

    if ( present(sCommentChars) ) then
      sCommentChars_l = sCommentChars
    else
      sCommentChars_l = "#!"
    endif

    this%count = this%count + 1

    call this%filenames%append( sFilename )
    call this%delimiters%append( sDelimiters_l )
    call this%comment_chars%append( sCommentChars_l )

  end subroutine add_filename_to_list_sub

!--------------------------------------------------------------------------------------------------

  subroutine munge_files_and_add_to_param_list_sub(this)

    class (PARAMETERS_T)    :: this

    ! [ LOCALS ]
    integer (c_int)                :: iFileIndex, iColIndex
    integer (c_int)                :: iStat
    type (ASCII_FILE_T)            :: DF
    type (DICT_ENTRY_T), pointer   :: pDict
    type (DICT_ENTRY_T), pointer   :: pCurrentDict
    integer (c_int)                :: iNumberOfHeaderLines
    character (len=:), allocatable :: sNumberOfHeaderLines
    character (len=256)            :: tempstr
    character (len=MAX_TABLE_RECORD_LEN) :: sRecord, sItem

    if ( this%count > 0 ) then

      ! iterate over the list of files
      do iFileIndex = 1, this%filenames%count

       ! open the file associated with current file index value
        call DF%open(sFilename = this%filenames%get(iFileIndex),             &
                     sCommentChars = this%comment_chars%get(iFileIndex),     &
                     sDelimiters = this%delimiters%get(iFileIndex) )

        ! obtain the headers from the file
        DF%slColNames = DF%readHeader()

        call LOGS%write( "Number of columns in file: "//asCharacter( DF%slColNames%count ), iTab=35)

        !call DF%slColNames%print()

        ! loop over each column header
        do iColIndex = 1, DF%slColNames%count

          ! create and allocate memory for dictionary entry

          pDict => null()
          allocate( pDict, stat=iStat )
          call assert(iStat == 0, "Failed to allocate memory for dictionary object", &
              __SRCNAME__, __LINE__ )

          ! this is first obvious failure point when compiling under Intel
          tempstr = DF%slColNames%get(iColIndex)

          if ( PARAMS_DICT%key_already_in_use( tempstr ) ) then

            ! add dictionary entry to dictionary, tack "DUP" on end of name
            tempstr = trim(adjustl(tempstr))//"_DUP"
            ! update the string list to reflect duplicate entry

            call DF%slColNames%replace( iColIndex, tempstr )
            call pDict%add_key( asUppercase( tempstr ) )
            call PARAMS_DICT%add_entry( pDict )

          else

            ! first add the key to the dictionary entry,
            ! then add dictionary entry to dictionary
            call pDict%add_key( asUppercase( DF%slColNames%get(iColIndex) ) )
            call PARAMS_DICT%add_entry( pDict )

          endif

        enddo

        ! now read in the remainder of the file
        do while ( .not. DF%isEOF() )

          ! read in next line of file
          sRecord = DF%readLine()

          ! skip blank lines
          if ( len_trim(sRecord) == 0 ) cycle

          ! loop over each column header
          do iColIndex = 1, DF%slColNames%count

            ! find pointer associated with header name
            ! (inefficient, but should be OK for small # of columns)
            pCurrentDict => PARAMS_DICT%get_entry( DF%slColNames%get(iColIndex) )

            ! break off next column of data for the current record
            call chomp(sRecord, sItem, this%delimiters%get(iFileIndex) )

            if ( associated( pCurrentDict )) then

              ! if not null, it means that we were able to return a pointer
              ! associated with the current column heading
              call pCurrentDict%add_value( sItem )

            else

              call warn("Internal programming error: null pointer detected" &
                //" -- was trying to find pointer associated with column "//dquote(DF%slColNames%get(iColIndex)), &
                __SRCNAME__, __LINE__)

            endif

          enddo

        enddo

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
          __SRCNAME__, __LINE__ )

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
      lFatal_l = lFALSE
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
      lFatal_l = lFALSE
    endif

    if ( present( slKeys) ) then

      call PARAMS_DICT%get_values( slKeys=slKeys, lValues=lValues,            &
        is_fatal=lFatal_l )

!       if ( any( iValues <= iTINYVAL ) ) &
!         call warn( "Failed to find a lookup table column named " &
!           //dQuote( slKeys%listall() )//".", lFatal = lFatal_l )

    else if ( present( sKey) ) then

      call PARAMS_DICT%get_values( sKey=sKey, lValues=lValues )

!       if ( any( iValues <= iTINYVAL ) ) &
!         call warn( "Failed to find a lookup table column named " &
!           //dQuote( sKey )//".", lFatal = lFatal_l )

    endif

  end subroutine get_parameter_values_logical

!--------------------------------------------------------------------------------------------------

  subroutine get_parameter_values_string_list( this, slValues, slKeys, sKey, lFatal )

    class (PARAMETERS_T)                                        :: this
    type (FSTRING_LIST_T), intent(out)                           :: slValues
    type (FSTRING_LIST_T), intent(in),                  optional :: slKeys
    character (len=*),    intent(in ),                 optional :: sKey
    logical (c_bool), intent(in),                 optional :: lFatal

    ! [ LOCALS ]
    logical (c_bool) :: lFatal_l

    if ( present (lFatal) ) then
      lFatal_l = lFatal
    else
      lFatal_l = lFALSE
    endif

    if ( present( slKeys) ) then

      call PARAMS_DICT%get_values( slKeys=slKeys, slString=slValues,          &
        is_fatal=lFatal_l )

       if ( slValues%get(1) .strequal. "<NA>" ) then
         call warn( "Failed to find a lookup table column named "        &
           //dQuote( slKeys%listall() )//".", lFatal = lFatal_l )
         slValues%is_populated = FALSE
       end if

    else if ( present( sKey) ) then

      call PARAMS_DICT%get_values( sKey=sKey, slString=slValues )

      if ( slValues%get(1) .strequal. "<NA>" ) then
        call warn( "Failed to find a lookup table column named "        &
          //dQuote( sKey )//".", lFatal = lFatal_l )
        slValues%is_populated = FALSE
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
      lFatal_l = lFALSE
    endif


    if ( present( slKeys) ) then

      call PARAMS_DICT%get_values( slKeys=slKeys, iValues=iValues,            &
        is_fatal=lFatal_l )

      if ( any( iValues <= iTINYVAL ) ) &
        call warn( "Failed to find a lookup table column named " &
          //dQuote( slKeys%listall() )//".", lFatal = lFatal_l )

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
    real (c_float),  intent(in out), allocatable          :: fValues(:)
    type (FSTRING_LIST_T), intent(in out),             optional :: slKeys
    character (len=*),    intent(in ),                optional :: sKey
    logical (c_bool), intent(in),                optional :: lFatal

    ! [ LOCALS ]
    logical (c_bool) :: lFatal_l

    if ( present (lFatal) ) then
      lFatal_l = lFatal
    else
      lFatal_l = lFALSE
    endif

    if ( present( slKeys) ) then

      call PARAMS_DICT%get_values( slKeys=slKeys, fValues=fValues )

      if ( any( fValues <= fTINYVAL ) ) &
        call warn( "Failed to find a lookup table column named " &
          //dQuote( slKeys%listall() )//".", lFatal = lFatal_l )

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
    real (c_float),  intent(in out), allocatable          :: fValues(:,:)
    character (len=*),    intent(in)                           :: sPrefix
    integer (c_int), intent(in)                           :: iNumRows
    logical (c_bool), intent(in),                optional :: lFatal

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
      lFatal_l = lFALSE
    endif

    slList = PARAMS%grep_name( sPrefix )

    iNumCols = slList%count

    if ( iNumCols == 0 ) then

      call warn( "Failed to find a lookup table column named " &
        //dQuote( sPrefix )//".", lFatal = lFatal_l )

    else

      allocate( fTempVal( iNumRows ), stat=iStat )
      call assert( iStat == 0, "Problem allocating memory.", __SRCNAME__, __LINE__ )

      allocate( fValues( iNumRows, iNumCols ), stat=iStat )
      call assert( iStat == 0, "Problem allocating memory."                            &
        //"~iNumCols: "//asCharacter(iNumCols)//"; iNumRows: "//asCharacter(iNumRows)  &
        //" sPrefix: "//dQuote(sPrefix), __SRCNAME__, __LINE__ )

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
