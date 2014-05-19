module parameters

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  use strings
  use string_list
  implicit none

  private

  !! module to provide a single POC for storage and retrieval of parameter scalars and vectors
  !! need to provide a method for storing, finding, and retrieving parameters.
  !! dictionary: keyword, string_list
  
  !! first create list of files. parse through each file, adding to the param dictionary.
  !! once complete, allow other modules to interrogate the dictionary. return matches in the
  !! data type required for the parameter. once all params are in place, the data structures can be
  !! deallocated. 

  type, public :: PARAMETER_FILES_T

    type (STRING_LIST_T)  :: filenames
    type (STRING_LIST_T)  :: delimiters
    type (STRING_LIST_T)  :: comment_chars

  contains

    procedure, private   :: add_filename_to_list_sub
    generic              :: add => add_filename_to_list_sub

    procedure, private   :: munge_file_and_add_to_param_list_sub
    generic              :: munge => munge_file_and_add_to_param_list_sub

  end type PARAMETER_FILES_T    


  type, public :: PARAMETER_DICT_ENTRY_T

    character (len=:), allocatable            :: key
    type (STRING_LIST_T)                      :: params
    type (PARAMETER_DICT_ENTRY_T), pointer    :: next       => null()

  contains

    procedure, private   :: add_key_sub
    generic              :: addkey => add_key_sub

    procedure, private   :: add_parameter_value_sub
    generic              :: addparam => add_parameter_value_sub

  end type PARAMETER_DICT_ENTRY_T 


  type, public :: PARAMETER_DICT_T

    type (PARAMETER_DICT_ENTRY_T), pointer   :: first   => null()
    type (PARAMETER_DICT_ENTRY_T), pointer   :: last    => null()
    integer (kind=c_int)                     :: count   = 0

  contains
  
    procedure, private   :: find_matching_keyword_fn 


    procedure, private   :: count_matching_keywords_fn


  end type PARAMETER_DICT_T

contains

  



end module parameters