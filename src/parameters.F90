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

    procedure, private   :: add_filename_to_list_sub
    generic              :: add_file => add_filename_to_list_sub

    procedure, private   :: munge_files_and_add_to_param_list_sub
    generic              :: munge_file => munge_files_and_add_to_param_list_sub

    procedure, private   :: get_parameter_values_int
    procedure            :: get_parameter_values_float
    procedure, private   :: get_parameter_table_float

    generic              :: get_parameters => get_parameter_values_int,     &
                                              get_parameter_values_float,   &
                                              get_parameter_table_float

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

    class (PARAMETERS_T)                     :: this
    character (len=*), intent(in)                 :: sFilename
    character (len=*), intent(in), optional       :: sDelimiters
    character (len=*), intent(in), optional       :: sCommentChars

    ! [ LOCALS ]
    character (len=:), allocatable  :: sDelimiters_
    character (len=:), allocatable  :: sCommentChars_
    
    if (present(sDelimiters) ) then
      sDelimiters_ = sDelimiters
    else 
      sDelimiters_ = sTAB
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
              __FILE__, __LINE__ )

          ! add dictionary entry to dictionary
          call pDict%add_key( DF%slColNames%get(iColIndex) )
          call PARAMS_DICT%add_entry( pDict )

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
              call pCurrentDict%add_string( sItem ) 
            
            else
            
              call warn("Internal programming error: null pointer detected" &
                //" -- was trying to find pointer associated with column "//dquote(DF%slColNames%get(iColIndex)), &
                __FILE__, __LINE__)  
            
            endif 
          
          enddo  
        
        enddo

        call DF%close()

      enddo
    endif

  end subroutine munge_files_and_add_to_param_list_sub

!--------------------------------------------------------------------------------------------------

  function grep_parameter_name( this, sKey )      result( slList )

    class (PARAMETERS_T)                                       :: this
    character (len=*), intent(in)                              :: sKey
    type ( STRING_LIST_T )                                     :: slList

    slList = PARAMS_DICT%grep_keys( sKey )  

  end function grep_parameter_name  

!--------------------------------------------------------------------------------------------------

  subroutine get_parameter_values_int( this, iValues, slKeys, sKey, lFatal )

    class (PARAMETERS_T)                                       :: this
    integer (kind=c_int), intent(in out), allocatable          :: iValues(:)  
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
        call warn( "Failed to find any lookup table columns with headers containing the string(s) " &
          //dQuote( slKeys%listall() )//".", lFatal = lFatal_ )

    else if ( present( sKey) ) then

      call PARAMS_DICT%get_values( sKey=sKey, iValues=iValues )

      if ( any( iValues <= iTINYVAL ) ) &
        call warn( "Failed to find any lookup table columns with headers containing the string " &
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
        call warn( "Failed to find any lookup table columns with headers containing the string(s) " &
          //dQuote( slKeys%listall() )//".", lFatal = lFatal_ )

    else if ( present( sKey) ) then

      call PARAMS_DICT%get_values( sKey=sKey, fValues=fValues )

      if ( any( fValues <= fTINYVAL ) ) &
        call warn( "Failed to find any lookup table columns with headers containing the string " &
          //dQuote( sKey )//".", lFatal = lFatal_ )

    endif

  end subroutine get_parameter_values_float

!--------------------------------------------------------------------------------------------------

  subroutine get_parameter_table_float( this, fValues, sPrefix, iNumRows, lFatal )

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

      call warn( "Failed to find any lookup table columns with headers containing the string " &
        //dQuote( sPrefix )//".", lFatal = lFatal_ )

    else

      allocate( fTempVal( iNumRows ), stat=iStat )
      call assert( iStat == 0, "Problem allocating memory.", __FILE__, __LINE__ )

      allocate( fValues( iNumRows, iNumCols ), stat=iStat )
      call assert( iStat == 0, "Problem allocating memory."                            &
        //"~iNumCols: "//asCharacter(iNumCols)//"; iNumRows: "//asCharacter(iNumRows)  &
        //" sPrefix: "//dQuote(sPrefix), __FILE__, __LINE__ )

      do iIndex = 1, iNumCols
        sText = trim(sPrefix)//asCharacter(iIndex)
        call PARAMS_DICT%get_values( sKey=sText, fValues=fTempVal )

        fValues(:,iIndex) = fTempVal
      enddo  

    endif

  end subroutine get_parameter_table_float

end module parameters