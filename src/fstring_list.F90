module fstring_list

  use iso_c_binding
  use fstring
  implicit none

  private

  public :: FSTRING_LIST_T

  ! public :: operator(+)
  ! interface operator(+)
  !   module procedure   :: concatenate_fstring_to_fstring_sub
  ! end interface operator(+)
  !
  public :: assignment(=)
  interface assignment(=)
    module procedure   :: assign_fstring_to_character_sub
  end interface assignment(=)

  type FSTRING_LIST_T

    character (len=:), allocatable    :: s
    integer (c_int)                   :: count = 0
    integer (c_int)                   :: missing_value_count = 0

  contains

    procedure   :: assign_character_to_fstring_sub
    generic     :: assignment(=) => assign_character_to_fstring_sub

    procedure   :: print_all_entries_sub
    generic     :: print_all => print_all_entries_sub

    procedure   :: print_as_markdown_sub
    generic     :: print => print_as_markdown_sub

    procedure   :: append_character_to_fstring_sub
    procedure   :: append_character_array_to_fstring_sub
    procedure   :: append_fstring_to_fstring_sub
    generic     :: append => append_character_to_fstring_sub,                  &
                             append_character_array_to_fstring_sub,            &
                             append_fstring_to_fstring_sub

    procedure   :: count_strings_in_list_fn
    generic     :: count_entries => count_strings_in_list_fn

    procedure   :: retrieve_value_from_list_at_index_fn
    procedure   :: retrieve_values_for_range_of_indices_fn
    generic     :: get => retrieve_value_from_list_at_index_fn,                &
                          retrieve_values_for_range_of_indices_fn

    procedure   :: retrieve_values_as_integer_fn
    generic     :: get_integer => retrieve_values_as_integer_fn

    procedure   :: retrieve_values_as_float_fn
    generic     :: get_float => retrieve_values_as_float_fn

    procedure   :: retrieve_values_as_double_fn
    generic     :: get_double => retrieve_values_as_double_fn

    procedure   :: retrieve_values_as_logical_fn
    generic     :: get_logical => retrieve_values_as_logical_fn

    procedure   :: replace_value_at_index_sub
    generic     :: replace => replace_value_at_index_sub

    procedure   :: list_all_fn
    generic     :: list_all => list_all_fn

    procedure   :: are_there_missing_list_values_fn
    generic     :: empty_entries_present => are_there_missing_list_values_fn

    procedure   :: quicksort_alpha_sub
    generic     :: sort => quicksort_alpha_sub

    procedure   :: quicksort_int_sub
    generic     :: sort_integer => quicksort_int_sub

    procedure   :: quicksort_float_sub
    generic     :: sort_float => quicksort_float_sub

    procedure   :: clear_list_sub
    generic     :: clear => clear_list_sub

    procedure   :: return_count_of_matching_strings_fn
    generic     :: count_matching => return_count_of_matching_strings_fn

!    procedure   :: is_substring_present_in_string_case_sensitive_fn
!    procedure   :: is_substring_present_in_string_case_insensitive_fn

    procedure   :: return_indices_of_matching_list_entries_fn
    generic     :: which => return_indices_of_matching_list_entries_fn

    procedure   :: return_subset_of_partial_matches_fn
    generic     :: grep => return_subset_of_partial_matches_fn

    procedure   :: return_list_of_unique_values_fn
    generic     :: unique => return_list_of_unique_values_fn

  end type FSTRING_LIST_T

  public :: NA_INT, NA_FLOAT, NA_DOUBLE
  integer (c_int), parameter  :: NA_INT    = - (huge(1_c_int)-1_c_int)
  real (c_float), parameter   :: NA_FLOAT  = - (huge(1._c_float)-1._c_float)
  real (c_double), parameter  :: NA_DOUBLE = - (huge(1._c_double)-1._c_double)

  public::split
  interface split
    module procedure :: split_character_into_fstring_list_fn
  end interface split

  public::create_list
  interface create_list
    module procedure :: split_character_into_fstring_list_fn
  end interface create_list

  type ALPHA_SORT_GROUP_T
    integer (c_int)                :: order
    character (len=:), allocatable :: alpha_value
  end type ALPHA_SORT_GROUP_T

  type INT_SORT_GROUP_T
    integer (c_int)                :: order
    integer (c_int)                :: int_value
  end type INT_SORT_GROUP_T

  type FLOAT_SORT_GROUP_T
    integer (c_int)                :: order
    real (c_float)                 :: float_value
  end type FLOAT_SORT_GROUP_T

contains

!--------------------------------------------------------------------------------------------------

  subroutine assign_character_to_fstring_sub(this, character_str)

    class (FSTRING_LIST_T), intent(inout)                :: this
    character (len=*), intent(in)                    :: character_str

    call this%clear()

    this%s = f_to_c_str(character_str)
    this%count = 1

  end subroutine assign_character_to_fstring_sub

!--------------------------------------------------------------------------------------------------

  subroutine assign_fstring_to_character_sub(this, character_str)

    type (FSTRING_LIST_T), intent(inout)            :: this
    character (len=:), allocatable                  :: character_str

    character (len=:), allocatable :: temp_str

    temp_str = this%get(1)
    character_str = trim(temp_str)

  end subroutine assign_fstring_to_character_sub

!--------------------------------------------------------------------------------------------------

  function split_character_into_fstring_list_fn(character_str, delimiter_chr)   result(new_fstring)

    character (len=*), intent(in)                    :: character_str
    character (len=1), intent(in), optional          :: delimiter_chr
    type (FSTRING_LIST_T)                                :: new_fstring

    character (len=len(character_str))  :: string
    character (len=len(character_str))  :: substring
    character (len=1)                   :: delimiter_chr_
    integer (c_int)                     :: num_delimiters
    integer (c_int)                     :: i

    if ( present(delimiter_chr) ) then
      delimiter_chr_ = delimiter_chr
    else
      delimiter_chr_ = ","
    endif

    string = character_str
    num_delimiters = 0

    do i=1, len_trim(string)
      if ( string(i:i) == delimiter_chr_ ) num_delimiters = num_delimiters + 1
    enddo

    if ( num_delimiters == 0 ) then

    else

! example: "one, two, three, four"
!           num_delimiters=3

      do i=1, num_delimiters
        call chomp(string, substring, delimiter_chr_)
        call new_fstring%append( substring )
      end do

      call chomp(string, substring, delimiter_chr_)
      call new_fstring%append( substring )

    endif

  end function split_character_into_fstring_list_fn

!--------------------------------------------------------------------------------------------------

  function count_strings_in_list_fn(this)        result(count)

    class (FSTRING_LIST_T), intent(inout), target        :: this
    integer (c_int)                                      :: count

    integer (c_int) :: i

    count = 0

    do i=1, len_trim(this%s)
      if( this%s(i:i) == c_null_char ) count = count + 1
    enddo

  end function count_strings_in_list_fn

!--------------------------------------------------------------------------------------------------

  subroutine append_character_to_fstring_sub(this, character_str)

    class (FSTRING_LIST_T), intent(inout), target        :: this
    character (len=*), intent(in)                        :: character_str

    if ( .not. allocated( this%s ) )  this%s = ""
    this%s = trim(this%s)//trim(adjustl(f_to_c_str(character_str)))
    if ( len_trim(character_str) == 0 )                                        &
       this%missing_value_count = this%missing_value_count + 1
    this%count = this%count + 1

  end subroutine append_character_to_fstring_sub

!--------------------------------------------------------------------------------------------------

subroutine append_character_array_to_fstring_sub(this, character_str)

  class (FSTRING_LIST_T), intent(inout), target        :: this
  character (len=*), intent(in)                    :: character_str(:)

  integer (c_int) :: i

  do i=1, size(character_str,1)

    this%s = trim(this%s)//trim(adjustl(f_to_c_str(character_str(i))))
    if ( len_trim(character_str(i)) == 0 )                                     &
       this%missing_value_count = this%missing_value_count + 1
    this%count = this%count + 1

  enddo

end subroutine append_character_array_to_fstring_sub

!--------------------------------------------------------------------------------------------------

subroutine append_fstring_to_fstring_sub(this, other_fstring)

  class (FSTRING_LIST_T), intent(inout), target        :: this
  type (FSTRING_LIST_T), intent(inout)                 :: other_fstring

  integer (c_int)                 :: i
  character (len=:), allocatable  :: temp_str

  do i=1, other_fstring%count

    temp_str = other_fstring%get(i)
    call this%append( other_fstring%get(i) )
    if ( len_trim(temp_str) == 0 )                                             &
       this%missing_value_count = this%missing_value_count + 1

  enddo

end subroutine append_fstring_to_fstring_sub

!--------------------------------------------------------------------------------------------------

  function are_there_missing_list_values_fn(this)    result(value)

    class (FSTRING_LIST_T), intent(inout), target   :: this
    logical (c_bool)                                :: value

    if ( this%missing_value_count > 0 )  then
      value = .true._c_bool
    else
      value = .false._c_bool
    endif

  end function are_there_missing_list_values_fn

!--------------------------------------------------------------------------------------------------

  subroutine print_all_entries_sub(this)

    class (FSTRING_LIST_T), intent(inout), target   :: this

    character (len=:), allocatable :: sbuf
    integer (c_int)                :: start_pos
    integer (c_int)                :: end_pos
    integer (c_int)                :: str_len
    integer (c_int)                :: i

    start_pos = 1
    end_pos = index( this%s, c_null_char ) - 1
    str_len = len_trim( this%s )

    do i=1, this%count_entries()

      write(*,fmt="(a)") this%s(start_pos:end_pos)

      start_pos = end_pos + 2
      end_pos = index( this%s(start_pos:str_len), c_null_char ) + start_pos - 2

    end do

  end subroutine print_all_entries_sub

!--------------------------------------------------------------------------------------------------

  subroutine list_finalize_sub(this)

    type (FSTRING_LIST_T), intent(inout)          :: this

    call this%clear()

  end subroutine list_finalize_sub

!--------------------------------------------------------------------------------------------------

  subroutine clear_list_sub(this)

    class (FSTRING_LIST_T), intent(inout)        :: this

    this%s = ""
    this%count = 0

  end subroutine clear_list_sub

!--------------------------------------------------------------------------------------------------

  function retrieve_values_as_integer_fn(this)   result(values)

    class (FSTRING_LIST_T), intent(inout), target  :: this
    integer (c_int), allocatable                   :: values(:)

    integer (c_int)    :: i
    integer (c_int)    :: value
    integer (c_int)    :: op_status
    character (len=64) :: sbuf

    if (this%count <= 0) then
      allocate(values(1),stat=op_status)
      values = NA_INT
    else

      allocate(values(this%count),stat=op_status)

      do i=1,this%count
        sbuf = this%get(i)
        value = as_integer(sbuf)
        if ( op_status==0 ) then
          values(i) = value
        else
          values(i) = NA_INT
        endif
      enddo

    endif

  end function retrieve_values_as_integer_fn

!--------------------------------------------------------------------------------------------------

  function retrieve_values_as_float_fn(this)   result(values)

    class (FSTRING_LIST_T), intent(inout), target    :: this
    real (c_float), allocatable                      :: values(:)

    integer (c_int)    :: i
    real (c_float)     :: value
    integer (c_int)    :: op_status
    character (len=64) :: sbuf

    if (this%count <= 0) then
      allocate(values(1),stat=op_status)
      values = NA_FLOAT
    else
      allocate(values(this%count),stat=op_status)

      do i=1,this%count
        sbuf = this%get(i)
        read(unit=sbuf, fmt=*, iostat=op_status) value
        if ( op_status==0 ) then
          values(i) = value
        else
          values(i) = NA_FLOAT
        endif
      enddo

    endif

  end function retrieve_values_as_float_fn

!--------------------------------------------------------------------------------------------------

function retrieve_values_as_double_fn(this)   result(values)

  class (FSTRING_LIST_T), intent(inout), target   :: this
  real (c_double), allocatable                    :: values(:)

  integer (c_int)    :: i
  real (c_double)    :: value
  integer (c_int)    :: op_status
  character (len=64) :: sbuf

  if (this%count <=0) then

    allocate(values(1),stat=op_status)
    values = NA_DOUBLE

  else

    allocate(values(this%count),stat=op_status)

    do i=1,this%count
      sbuf = this%get(i)
      read(unit=sbuf, fmt=*, iostat=op_status) value
      if ( op_status==0 ) then
        values(i) = value
      else
        values(i) = NA_DOUBLE
      endif
    enddo

  endif

end function retrieve_values_as_double_fn

!--------------------------------------------------------------------------------------------------

function retrieve_values_as_logical_fn(this)   result(values)

  class (FSTRING_LIST_T), intent(inout)         :: this
  logical (c_bool), allocatable             :: values(:)

  integer (c_int)    :: i
  logical (c_bool)   :: value
  integer (c_int)    :: op_status
  character (len=64) :: sbuf

  allocate(values(this%count),stat=op_status)

  do i=1,this%count
    sbuf = this%get(i)

    select case(sbuf)

      case("true","T","True","TRUE","1","Y","Yes","yes","YES")
        values(i) = .TRUE._c_bool
      case default
        values(i) = .FALSE._c_bool

    end select

  enddo

end function retrieve_values_as_logical_fn

!--------------------------------------------------------------------------------------------------

  function retrieve_value_from_list_at_index_fn(this, index_val)   result(text)

    class (FSTRING_LIST_T), intent(inout)     :: this
    integer (c_int), intent(in)               :: index_val
    character(len=:), allocatable             :: text

    integer (c_int)                :: start_pos
    integer (c_int)                :: end_pos
    integer (c_int)                :: str_len
    integer (c_int)                :: i

    start_pos = 1
    text = "<NA>"

    if ( allocated(this%s) ) then

      end_pos = index( this%s, c_null_char ) - 1
      str_len = len_trim( this%s )

      do i=1, this%count

        if ( index_val == i ) then
          text = this%s(start_pos:end_pos)
          exit
        endif

        ! skip over the 'c_null_char' to position of beginning of
        ! next substring
        start_pos = end_pos + 2
        end_pos = index( this%s(start_pos:str_len), c_null_char ) + start_pos - 2

      end do

    endif  

  end function retrieve_value_from_list_at_index_fn

!--------------------------------------------------------------------------------------------------

    subroutine replace_value_at_index_sub(this, index_val, character_str)

      class (FSTRING_LIST_T), intent(inout)     :: this
      integer (c_int), intent(in)               :: index_val
      character(len=*)                          :: character_str

      integer (c_int)                :: i
      type (FSTRING_LIST_T)          :: temp_list


      if (this%count > 0) then

        do i=1, this%count
          if ( index_val == i ) then
            call temp_list%append(character_str)
          else
            call temp_list%append(this%get(i))
          endif
        end do

        this%s = temp_list%s

      endif

    end subroutine replace_value_at_index_sub

!--------------------------------------------------------------------------------------------------

  function retrieve_values_for_range_of_indices_fn(this, start_indx, end_indx)   result(text)

    class (FSTRING_LIST_T), intent(inout)       :: this
    integer (c_int), intent(in)                 :: start_indx
    integer (c_int), intent(in)                 :: end_indx
    character (len=:), allocatable              :: text

    integer (c_int)                        :: i

    if (this%count == 0) then
      text = "<NA>"
    else
      do i=1, this%count
        if (i == start_indx) then
          text = trim(this%get(i))
        elseif (i > start_indx .and. i <= end_indx ) then
          text = trim(text)//" "//trim(this%get(i))
        endif
      enddo
    endif

  end function retrieve_values_for_range_of_indices_fn

!--------------------------------------------------------------------------------------------------

  function list_all_fn(this, delimiter_chr)  result( text )

    class (FSTRING_LIST_T), intent(inout)   :: this
    character (len=1), intent(in), optional :: delimiter_chr
    character (len=:), allocatable          :: text

    integer (c_int)  :: i

    if (this%count == 0) then
      text = "<NA>"
    elseif (present(delimiter_chr) ) then
      text = trim(this%get(1))
      do i=2, this%count
        text = trim(text)//delimiter_chr//trim(this%get(i))
      enddo
    else
      text = "(1) "//trim(this%get(1))
      do i=2, this%count
        text = trim(text)//" ("//as_character(i)//") "//trim(this%get(i))
      enddo
    endif

  end function list_all_fn

!--------------------------------------------------------------------------------------------------

  subroutine quicksort_alpha_sub(this, sort_order)

    class (FSTRING_LIST_T), intent(inout)         :: this
    character (len=*), intent(in), optional   :: sort_order

    type (ALPHA_SORT_GROUP_T), allocatable :: sort_group(:)
    integer (c_int)                        :: i
    integer (c_int)                        :: count
    logical (c_bool)                       :: decreasing_order

    decreasing_order = .false._c_bool

    if ( present(sort_order) ) then
      select case (sort_order)

      case ("Decreasing","decreasing","DECREASING")
        decreasing_order = .true._c_bool
      end select
    endif

    count = this%count

    allocate(sort_group(count))

    ! create the 'sort_group' data structure
    do i=1, count
      sort_group(i)%order = i
      sort_group(i)%alpha_value = this%get(i)
    enddo

    call qsort_alpha(sort_group, this%count)

    ! wipe out previous values
    call this%clear()

    if ( decreasing_order ) then
      ! copy sorted values back into list structure (DECREASING ORDER)
      do i=count, 1, -1
        call this%append(sort_group(i)%alpha_value)
      enddo
    else
      ! copy sorted values back into list structure (INCREASING ORDER)
      do i=1, count
        call this%append(sort_group(i)%alpha_value)
      enddo
    endif

  end subroutine quicksort_alpha_sub

!-------------------------------------------------------------------------------------------------

  subroutine quicksort_int_sub(this, sort_order)
    class (FSTRING_LIST_T), intent(inout)         :: this
    character (len=*), intent(in), optional   :: sort_order

    type (INT_SORT_GROUP_T), allocatable   :: sort_group(:)
    integer (c_int), allocatable           :: int_values(:)
    integer (c_int)                        :: i
    integer (c_int)                        :: count
    logical (c_bool)                       :: decreasing_order

    decreasing_order = .false._c_bool

    if ( present(sort_order) ) then
      select case (sort_order)
      case ("Decreasing","decreasing","DECREASING")
        decreasing_order = .true._c_bool
      end select
    endif

    count = this%count

    allocate(sort_group(count))
    allocate(int_values(count))

    int_values = this%get_integer()

    ! create the 'sort_group' data structure
    do i=1, count
      sort_group(i)%order = i
      sort_group(i)%int_value = int_values(i)
    enddo

    call qsort_int(sort_group, this%count)
    ! wipe out previous values
    call this%clear()
    if ( decreasing_order ) then
      ! copy sorted values back into list structure (DECREASING ORDER)
      do i=count, 1, -1
        call this%append( as_character(sort_group(i)%int_value) )
      enddo
    else
      ! copy sorted values back into list structure (INCREASING ORDER)
      do i=1, count
        call this%append( as_character(sort_group(i)%int_value) )
      enddo
    endif
  end subroutine quicksort_int_sub

!-------------------------------------------------------------------------------------------------

  subroutine quicksort_float_sub(this, sort_order)

    class (FSTRING_LIST_T), intent(inout)         :: this
    character (len=*), intent(in), optional   :: sort_order

    type (FLOAT_SORT_GROUP_T), allocatable :: sort_group(:)
    real (c_float), allocatable            :: float_values(:)
    integer (c_int)                        :: i
    integer (c_int)                        :: count
    logical (c_bool)                       :: decreasing_order

    decreasing_order = .false._c_bool

    if ( present(sort_order) ) then
      select case (sort_order)

        case ("Decreasing","decreasing","DECREASING")
          decreasing_order = .true._c_bool
      end select
    endif

    count = this%count

    allocate(sort_group(count))

    float_values = this%get_float()

    ! create the 'sort_group' data structure
    do i=1, count
      sort_group(i)%order = i
      sort_group(i)%float_value = float_values(i)
    enddo

    call qsort_float(sort_group, this%count)

    ! wipe out previous values
    call this%clear()

    if ( decreasing_order ) then
      ! copy sorted values back into list structure (DECREASING ORDER)
      do i=count, 1, -1
        call this%append( as_character(sort_group(i)%float_value) )
      enddo
    else
      ! copy sorted values back into list structure (INCREASING ORDER)
      do i=1, count
        call this%append( as_character(sort_group(i)%float_value) )
      enddo
    endif

  end subroutine quicksort_float_sub

!-------------------------------------------------------------------------------------------------

  recursive subroutine qsort_alpha(sort_group, nrec)

  ! NOTE: this code based on code found here:
  !       https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Fortran

  ! DUMMY ARGUMENTS
  type (ALPHA_SORT_GROUP_T), dimension(nrec), intent(in out) :: sort_group
  integer (c_int), intent(in)                                :: nrec

  ! LOCAL VARIABLES
  integer (c_int)                  :: left, right
  real (c_float)                   :: random
  character (len=:), allocatable   :: pivot
  type (ALPHA_SORT_GROUP_T)        :: temp
  integer (c_int)                  :: marker

      if (nrec > 1) then

          call random_number(random)
          pivot = sort_group(int(random*real(nrec-1))+1)%alpha_value   ! random pivor (not best performance, but avoids worst-case)
          left = 0
          right = nrec + 1

          do while (left < right)
              right = right - 1
              do while (sort_group(right)%alpha_value > pivot)
                  right = right - 1
              end do
              left = left + 1
              do while (sort_group(left)%alpha_value < pivot)
                  left = left + 1
              end do
              if (left < right) then
                  temp = sort_group(left)
                  sort_group(left) = sort_group(right)
                  sort_group(right) = temp
              end if
          end do

          if (left == right) then
              marker = left + 1
          else
              marker = left
          end if

          call qsort_alpha(sort_group(:marker-1),marker-1)
          call qsort_alpha(sort_group(marker:),nrec-marker+1)

      end if

  end subroutine qsort_alpha

!--------------------------------------------------------------------------------------------------

  recursive subroutine qsort_int(sort_group, nrec)

  ! NOTE: this code based on code found here:
  !       https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Fortran

  ! DUMMY ARGUMENTS
  type (INT_SORT_GROUP_T), dimension(nrec), intent(in out) :: sort_group
  integer (c_int), intent(in)                              :: nrec

  ! LOCAL VARIABLES
  integer (c_int)                  :: left, right
  real (c_float)                   :: random
  integer (c_int)                  :: pivot
  type (INT_SORT_GROUP_T)          :: temp
  integer (c_int)                  :: marker

      if (nrec > 1) then

          call random_number(random)
          pivot = sort_group(int(random*real(nrec-1))+1)%int_value   ! random pivor (not best performance, but avoids worst-case)
          left = 0
          right = nrec + 1

          do while (left < right)
              right = right - 1
              do while (sort_group(right)%int_value > pivot)
                  right = right - 1
              end do
              left = left + 1
              do while (sort_group(left)%int_value < pivot)
                  left = left + 1
              end do
              if (left < right) then
                  temp = sort_group(left)
                  sort_group(left) = sort_group(right)
                  sort_group(right) = temp
              end if
          end do

          if (left == right) then
              marker = left + 1
          else
              marker = left
          end if

          call qsort_int(sort_group(:marker-1),marker-1)
          call qsort_int(sort_group(marker:),nrec-marker+1)

      end if

  end subroutine qsort_int

!--------------------------------------------------------------------------------------------------

  recursive subroutine qsort_float(sort_group, nrec)

  ! NOTE: this code based on code found here:
  !       https://rosettacode.org/wiki/Sorting_algorithms/Quicksort#Fortran

  ! DUMMY ARGUMENTS
  type (FLOAT_SORT_GROUP_T), dimension(nrec), intent(in out) :: sort_group
  integer (c_int), intent(in)                                :: nrec

  ! LOCAL VARIABLES
  integer (c_int)                  :: left, right
  real (c_float)                   :: random
  real (c_float)                   :: pivot
  type (FLOAT_SORT_GROUP_T)        :: temp
  integer (c_int)                  :: marker

      if (nrec > 1) then

          call random_number(random)
          pivot = sort_group(int(random*real(nrec-1))+1)%float_value   ! random pivor (not best performance, but avoids worst-case)
          left = 0
          right = nrec + 1

          do while (left < right)
              right = right - 1
              do while (sort_group(right)%float_value > pivot)
                  right = right - 1
              end do
              left = left + 1
              do while (sort_group(left)%float_value < pivot)
                  left = left + 1
              end do
              if (left < right) then
                  temp = sort_group(left)
                  sort_group(left) = sort_group(right)
                  sort_group(right) = temp
              end if
          end do

          if (left == right) then
              marker = left + 1
          else
              marker = left
          end if

          call qsort_float(sort_group(:marker-1),marker-1)
          call qsort_float(sort_group(marker:),nrec-marker+1)

      end if

  end subroutine qsort_float

!--------------------------------------------------------------------------------------------------

  function return_count_of_matching_strings_fn(this, substr, match_case)    result(count)

    class (FSTRING_LIST_T), intent(inout)       :: this
    character (len=*), intent(in)               :: substr

    logical (c_bool), intent(in), optional  :: match_case
    integer (c_int)                         :: count

    ! [ LOCALS ]
    integer (c_int)  :: i
    integer (c_int)  :: status
    logical (c_bool) :: match_case_

    if ( present( match_case ) ) then
      match_case_ = match_case
    else
      match_case_ = .FALSE._c_bool
    endif

    count = 0

    if ( match_case_ ) then

      do i=1, this%count

        if ( this%get(i) .strequal. substr )  count = count + 1

      enddo

    else

      do i=1, this%count

        if ( this%get(i) .strapprox. substr )  count = count + 1

      enddo

    endif

  end function return_count_of_matching_strings_fn

!--------------------------------------------------------------------------------------------------

  function return_subset_of_partial_matches_fn( this, substr )     result(new_fstring)

    class (FSTRING_LIST_T), intent(inout)                  :: this
    character (len=*), intent(in)                          :: substr
    type (FSTRING_LIST_T)                                  :: new_fstring

    ! [ LOCALS ]
    integer (c_int)                 :: i
    character (len=:), allocatable  :: temp_str

    do i=1, this%count
      temp_str = this%get(i)
      if ( temp_str .containssimilar. substr )   call new_fstring%append(temp_str)
    enddo

    if ( new_fstring%count == 0 )  new_fstring = "<NA>"

  end function return_subset_of_partial_matches_fn

!--------------------------------------------------------------------------------------------------

  function return_indices_of_matching_list_entries_fn(this, character_str)   result(index_values)

    class (FSTRING_LIST_T), intent(inout)                  :: this
    character (len=*), intent(in)                          :: character_str
    integer (c_int), allocatable                           :: index_values(:)

    ! [ LOCALS ]
    integer (c_int)      :: i
    integer (c_int)      :: match_index
    logical (c_bool)     :: string_present( this%count )
    integer (c_int)      :: number_of_matches

    string_present = .false._c_bool
    match_index = 0

    do i=1, this%count
      if ( this%get(i) .strapprox. character_str )   string_present(i) = .true._c_bool
    enddo

    number_of_matches = count(string_present)
    if (number_of_matches > 0 ) then
      allocate( index_values(number_of_matches) )
      do i=1, this%count
        if (string_present(i)) then
          match_index = match_index + 1
          index_values(match_index) = i
        endif
      enddo
    else
      allocate( index_values(1) )
      index_values(1) = -9999
    endif

  end function return_indices_of_matching_list_entries_fn

!--------------------------------------------------------------------------------------------------

  function return_list_of_unique_values_fn(this)    result(new_fstring)

    class (FSTRING_LIST_T), intent(inout)   :: this
    type (FSTRING_LIST_T)                   :: new_fstring

    integer (c_int)                :: i
    character (len=:), allocatable :: temp_str

    do i=1, this%count

      temp_str = this%get(i)
      if ( new_fstring%count_matching( temp_str ) == 0 )  call new_fstring%append(temp_str)

    enddo

    if ( new_fstring%count == 0 )  new_fstring = "<NA>"

  end function return_list_of_unique_values_fn

!--------------------------------------------------------------------------------------------------

  subroutine print_as_markdown_sub(this, lu)

    use iso_fortran_env, only : OUTPUT_UNIT

    class (FSTRING_LIST_T), intent(inout)     :: this
    integer (c_int), optional             :: lu

    ! [ LOCALS ]
    integer (c_int)   :: lu_
    integer (c_int)   :: i

    if (present(lu) ) then
      lu_ = lu
    else
      lu_ = OUTPUT_UNIT
    endif

    write(lu_, fmt="('|',a,t21,'|',a,t72,'|')") "Index","Value"
    write(lu_, fmt="('|',a,t21,'|',a,t72,'|')") repeat("-",18)//":", repeat("-",49)//":"

    do i=1, this%count

      write(lu_, fmt="('|',i10,t21,'|',a,t72,'|')") i, this%get(i)

    enddo

  end subroutine print_as_markdown_sub

end module fstring_list
