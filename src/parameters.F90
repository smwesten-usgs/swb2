module parameters

  use iso_c_binding, only    : c_int, c_float, c_double, c_bool
  use exceptions
  use file_operations, only  : ASCII_FILE_T
  use logfiles
  use strings
  use string_list, only      : STRING_LIST_T
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

    type (STRING_LIST_T)               :: filenames
    type (STRING_LIST_T)               :: delimiters
    type (STRING_LIST_T)               :: comment_chars
    integer (kind=c_int)               :: count           = 0

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

  integer (kind=c_int), parameter :: MAX_TABLE_RECORD_LEN = 512

contains

  subroutine add_filename_to_list_sub( this, sFilename, sDelimiters, sCommentChars )

    class (PARAMETERS_T)                          :: this
    character (len=*), intent(in)                 :: sFilename
    character (len=*), intent(in), optional       :: sDelimiters
    character (len=*), intent(in), optional       :: sCommentChars

    ! [ LOCALS ]
    character (len=:), allocatable  :: sDelimiters_
    character (len=:), allocatable  :: sCommentChars_

    if (present(sDelimiters) ) then
      sDelimiters_ = sDelimiters
    else
      sDelimiters_ = TAB
    endif

    if ( present(sCommentChars) ) then
      sCommentChars_ = sCommentChars
    else
      sCommentChars_ = "#!"
    endif

    this%count = this%count + 1

    call this%filenames%append( sFilename )
    call this%delimiters%append( sDelimiters_ )
    call this%comment_chars%append( sCommentChars_ )

  end subroutine add_filename_to_list_sub

!--------------------------------------------------------------------------------------------------

  subroutine munge_files_and_add_to_param_list_sub(this)

    class (PARAMETERS_T)    :: this

    ! [ LOCALS ]
    integer (kind=c_int)         :: iFileIndex, iColIndex
    integer (kind=c_int)         :: iStat
    type (ASCII_FILE_T)          :: DF
    type (DICT_ENTRY_T), pointer :: pDict
    type (DICT_ENTRY_T), pointer :: pCurrentDict
    integer (kind=c_int)         :: iNumberOfHeaderLines
    character (len=:), allocatable :: sNumberOfHeaderLines
    character (len=:), allocatable :: tempstr
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

        call LOGS%write( "Number of columns in file: "//asCharacter( DF%slColNames%count ), iTab=35 )

        ! loop over each column header
        do iColIndex = 1, DF%slColNames%count

          ! create and allocate memory for dictionary entry
          pDict => null()
          allocate( pDict, stat=iStat )
          call assert(iStat == 0, "Failed to allocate memory for dictionary object", &
              __SRCNAME__, __LINE__ )

          if ( PARAMS_DICT%key_already_in_use( DF%slColNames%get(iColIndex) ) ) then

            ! add dictionary entry to dictionary, tack "DUP" on end of name
            tempstr = trim( DF%slColNames%get(iColIndex) )//"_DUP"
            ! update the string list to reflect duplicate entry
            call DF%slColNames%replace( iColIndex, tempstr )
            call pDict%add_key( asUppercase( tempstr ) )
            call PARAMS_DICT%add_entry( pDict )

          else

            ! call warn( sMessage="Found a duplicate dictionary entry in file "        &
            !   //squote( this%filenames%get(iFileIndex) )//"; column name: "          &
            !   //squote( DF%slColNames%get(iColIndex) ),                              &
            !   sHints="Eliminate the duplicate column of information and try again.", &
            !   lFatal=TRUE )

            ! add dictionary entry to dictionary
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

            ! must avoid manipulating null pointers at all costs
            if ( associated( pCurrentDict )) then

              ! if not null, it means that we were able to return a pointer
              ! associated with the current column heading
              call pCurrentDict%add_entry( sItem )

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
    integer (kind=c_int), intent(in), optional   :: iValues(:)
    real (kind=c_float), intent(in), optional    :: fValues(:)
    real (kind=c_double), intent(in), optional   :: dValues(:)
    logical (kind=c_bool), intent(in), optional  :: lValues(:)

    ! [ LOCALS ]
    integer (kind=c_int)         :: iStat
    type (DICT_ENTRY_T), pointer :: pDict
    integer (kind=c_int)         :: iIndex
    type (DICT_ENTRY_T), pointer :: pCurrentDict
    character (len=MAX_TABLE_RECORD_LEN) :: sRecord, sItem


    pCurrentDict => null()
    pCurrentDict => PARAMS_DICT%get_entry( sKey )

    if ( .not. associated( pCurrentDict )) then

      ! if key doesn't currently exist, make a new entry with this key value
      allocate( pCurrentDict, stat=iStat )

      call assert(iStat == 0, "Failed to allocate memory for dictionary object", &
          __SRCNAME__, __LINE__ )

      ! add dictionary entry to dictionary
      call pCurrentDict%add_key( sKey )
      call PARAMS_DICT%add_entry( pCurrentDict )

    endif

    if ( present( sValues ) ) then

      do iIndex = lbound(sValues,1), ubound(sValues,1)

        call pCurrentDict%add_entry( sValues( iIndex ) )

      enddo

    else if ( present ( iValues ) ) then

      do iIndex = lbound(iValues,1), ubound(iValues,1)

        call pCurrentDict%add_entry( iValues( iIndex ) )

      enddo

    else if ( present ( fValues ) ) then

      do iIndex = lbound(fValues,1), ubound(fValues,1)

        call pCurrentDict%add_entry( fValues( iIndex ) )

      enddo

    else if ( present ( dValues ) ) then

      do iIndex = lbound(dValues,1), ubound(dValues,1)

        call pCurrentDict%add_entry( dValues( iIndex ) )

      enddo

    else if ( present ( lValues ) ) then

      do iIndex = lbound(lValues,1), ubound(lValues,1)

        call pCurrentDict%add_entry( lValues( iIndex ) )

      enddo

    endif

  end subroutine add_to_param_list_sub

!--------------------------------------------------------------------------------------------------

  function grep_parameter_name( this, sKey, lFatal )      result( slList )

    class (PARAMETERS_T)                                       :: this
    character (len=*), intent(in)                              :: sKey
    logical (kind=c_bool), intent(in), optional                :: lFatal
    type ( STRING_LIST_T )                                     :: slList

    ! [ LOCALS ]
    logical (kind=c_bool) :: lFatal_

    if ( present (lFatal) ) then
      lFatal_ = lFatal
    else
      lFatal_ = lFALSE
    endif

    slList = PARAMS_DICT%grep_keys( asUppercase( sKey ) )

    if ( lFatal_ ) then

       if ( slList%get(1) .strequal. "<NA>" )                              &
         call warn( "Failed to find a lookup table column whose name contains "        &
           //dQuote( sKey )//".", lFatal = lFatal_ )

    endif

  end function grep_parameter_name

!--------------------------------------------------------------------------------------------------

  subroutine get_parameter_values_logical( this, lValues, slKeys, sKey, lFatal )

    class (PARAMETERS_T)                                        :: this
    logical (kind=c_bool), intent(in out), allocatable          :: lValues(:)
    type (STRING_LIST_T), intent(in out),              optional :: slKeys
    character (len=*),    intent(in ),                 optional :: sKey
    logical (kind=c_bool), intent(in),                 optional :: lFatal

    ! [ LOCALS ]
    logical (kind=c_bool) :: lFatal_

    if ( present (lFatal) ) then
      lFatal_ = lFatal
    else
      lFatal_ = lFALSE
    endif

    if ( present( slKeys) ) then

      call PARAMS_DICT%get_values( slKeys=slKeys, lValues=lValues )

!       if ( any( iValues <= iTINYVAL ) ) &
!         call warn( "Failed to find a lookup table column named " &
!           //dQuote( slKeys%listall() )//".", lFatal = lFatal_ )

    else if ( present( sKey) ) then

      call PARAMS_DICT%get_values( sKey=sKey, lValues=lValues )

!       if ( any( iValues <= iTINYVAL ) ) &
!         call warn( "Failed to find a lookup table column named " &
!           //dQuote( sKey )//".", lFatal = lFatal_ )

    endif

  end subroutine get_parameter_values_logical

!--------------------------------------------------------------------------------------------------

  subroutine get_parameter_values_string_list( this, slValues, slKeys, sKey, lFatal )

    class (PARAMETERS_T)                                        :: this
    type (STRING_LIST_T), intent(out)                           :: slValues
    type (STRING_LIST_T), intent(in),                  optional :: slKeys
    character (len=*),    intent(in ),                 optional :: sKey
    logical (kind=c_bool), intent(in),                 optional :: lFatal

    ! [ LOCALS ]
    logical (kind=c_bool) :: lFatal_

    if ( present (lFatal) ) then
      lFatal_ = lFatal
    else
      lFatal_ = lFALSE
    endif

    if ( present( slKeys) ) then

      call PARAMS_DICT%get_values( slKeys=slKeys, slString=slValues )

       if ( slValues%get(1) .strequal. "<NA>" ) then
         call warn( "Failed to find a lookup table column named "        &
           //dQuote( slKeys%listall() )//".", lFatal = lFatal_ )
         slValues%is_populated = FALSE
       end if

    else if ( present( sKey) ) then

      call PARAMS_DICT%get_values( sKey=sKey, slString=slValues )

      if ( slValues%get(1) .strequal. "<NA>" ) then
        call warn( "Failed to find a lookup table column named "        &
          //dQuote( sKey )//".", lFatal = lFatal_ )
        slValues%is_populated = FALSE
      end if

    endif

  end subroutine get_parameter_values_string_list

!--------------------------------------------------------------------------------------------------

  subroutine get_parameter_values_int( this, iValues, slKeys, sKey, lFatal )

    class (PARAMETERS_T)                                       :: this
    integer (kind=c_int), intent(out), allocatable             :: iValues(:)
    type (STRING_LIST_T), intent(in out),             optional :: slKeys
    character (len=*),    intent(in ),                optional :: sKey
    logical (kind=c_bool), intent(in),                optional :: lFatal

    ! [ LOCALS ]
    logical (kind=c_bool) :: lFatal_

    if ( present (lFatal) ) then
      lFatal_ = lFatal
    else
      lFatal_ = lFALSE
    endif


    if ( present( slKeys) ) then

      call PARAMS_DICT%get_values( slKeys=slKeys, iValues=iValues )

      if ( any( iValues <= iTINYVAL ) ) &
        call warn( "Failed to find a lookup table column named " &
          //dQuote( slKeys%listall() )//".", lFatal = lFatal_ )

    else if ( present( sKey) ) then

      call PARAMS_DICT%get_values( sKey=sKey, iValues=iValues )

      if ( any( iValues <= iTINYVAL ) ) &
        call warn( "Failed to find a lookup table column named " &
          //dQuote( sKey )//".", lFatal = lFatal_ )

    endif

  end subroutine get_parameter_values_int

!--------------------------------------------------------------------------------------------------

  subroutine get_parameter_values_float( this, fValues, slKeys, sKey, lFatal )

    class (PARAMETERS_T)                                       :: this
    real (kind=c_float),  intent(in out), allocatable          :: fValues(:)
    type (STRING_LIST_T), intent(in out),             optional :: slKeys
    character (len=*),    intent(in ),                optional :: sKey
    logical (kind=c_bool), intent(in),                optional :: lFatal

    ! [ LOCALS ]
    logical (kind=c_bool) :: lFatal_

    if ( present (lFatal) ) then
      lFatal_ = lFatal
    else
      lFatal_ = lFALSE
    endif

    if ( present( slKeys) ) then

      call PARAMS_DICT%get_values( slKeys=slKeys, fValues=fValues )

      if ( any( fValues <= fTINYVAL ) ) &
        call warn( "Failed to find a lookup table column named " &
          //dQuote( slKeys%listall() )//".", lFatal = lFatal_ )

    else if ( present( sKey) ) then

      call PARAMS_DICT%get_values( sKey=sKey, fValues=fValues )

      if ( any( fValues <= fTINYVAL ) ) &
        call warn( "Failed to find a lookup table column named " &
          //dQuote( sKey )//".", lFatal = lFatal_ )

    endif

  end subroutine get_parameter_values_float

!--------------------------------------------------------------------------------------------------

  subroutine get_parameter_table_float( this, fValues, sPrefix, iNumRows, lFatal )

    use strings

    class (PARAMETERS_T)                                       :: this
    real (kind=c_float),  intent(in out), allocatable          :: fValues(:,:)
    character (len=*),    intent(in)                           :: sPrefix
    integer (kind=c_int), intent(in)                           :: iNumRows
    logical (kind=c_bool), intent(in),                optional :: lFatal

    ! [ LOCALS ]
    integer (kind=c_int)             :: iIndex
    integer (kind=c_int)             :: iStat
    character (len=256)              :: sText
    integer (kind=c_int)             :: iNumCols
    type (STRING_LIST_T)             :: slList
    real (kind=c_float), allocatable :: fTempVal(:)
    logical (kind=c_bool) :: lFatal_

    if ( present (lFatal) ) then
      lFatal_ = lFatal
    else
      lFatal_ = lFALSE
    endif

    slList = PARAMS%grep_name( sPrefix )

    iNumCols = slList%count

    if ( iNumCols == 0 ) then

      call warn( "Failed to find a lookup table column named " &
        //dQuote( sPrefix )//".", lFatal = lFatal_ )

    else

      allocate( fTempVal( iNumRows ), stat=iStat )
      call assert( iStat == 0, "Problem allocating memory.", __SRCNAME__, __LINE__ )

      allocate( fValues( iNumRows, iNumCols ), stat=iStat )
      call assert( iStat == 0, "Problem allocating memory."                            &
        //"~iNumCols: "//asCharacter(iNumCols)//"; iNumRows: "//asCharacter(iNumRows)  &
        //" sPrefix: "//dQuote(sPrefix), __SRCNAME__, __LINE__ )

      do iIndex = 1, iNumCols

        sText = trim( slList%get( iIndex ) )
        call PARAMS_DICT%get_values( sKey=sText, fValues=fTempVal )

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
