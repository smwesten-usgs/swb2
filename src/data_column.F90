module data_column

use iso_c_binding, only : c_int, c_float, c_double, c_bool
use iso_fortran_env, only : IOSTAT_END
use constants_and_conversions
use strings
use string_list
use datetime
use exceptions
use types_new
implicit none

private

type, public :: T_DATA_COLUMN

  integer (kind=c_int), private :: iCurrentRecord = 0
  integer (kind=c_int), private :: iOrder
  integer (kind=c_int), private :: iDataType
  integer (kind=c_int) :: iCount
  logical (kind=c_bool), dimension(:), allocatable, public :: lMask
  integer (kind=c_int), dimension(:), allocatable, private :: iData
  real (kind=c_float), dimension(:), allocatable, private  :: fData
  real (kind=c_double), dimension(:), allocatable, private :: dData
  type (STRING_LIST_T), public                             :: stData
  type (T_DATETIME), dimension(:), pointer, public         :: pDatetime => null()

contains

  private

   procedure, private :: put_next_integer_value_fn
   procedure, private :: put_next_float_value_fn
   procedure, private :: put_next_double_value_fn
   procedure, private :: put_next_string_value_fn
   generic, public    :: putval => put_next_integer_value_fn, &
                                   put_next_float_value_fn, &
                                   put_next_double_value_fn, &
                                   put_next_string_value_fn

  procedure, private :: put_next_datetime_value_fn
  generic, public    :: putdatetime => put_next_datetime_value_fn                                 

  procedure, private :: put_next_date_value_fn
  generic, public    :: putdate => put_next_date_value_fn                                 

  procedure, private :: put_next_time_value_fn
  generic, public    :: puttime => put_next_time_value_fn                                 

  !> DOXYGEN_IMPL data_dolumn::set_mask_int_sub
  procedure, private :: set_mask_int_sub
  !> DOXYGEN_IMPL data_column::set_mask_float_sub
  procedure, private :: set_mask_float_sub
  !> DOXYGEN_IMPL data_column::set_mask_double_sub
  procedure, private :: set_mask_double_sub
  !> DOXYGEN_IMPL data_column::set_mask_datetime_sub
  procedure, private :: set_mask_datetime_sub
  !> DOXYGEN_IMPL data_column::set_mask_string_sub
  procedure, private :: set_mask_string_sub

  generic, public    :: select => set_mask_int_sub, &
                                  set_mask_float_sub, &
                                  set_mask_double_sub, &
                                  set_mask_datetime_sub, &
                                  set_mask_string_sub

  !> DOXYGEN_IMPL data_column::create_new_column_sub
  procedure, private :: create_new_column_sub
  generic, public    :: new => create_new_column_sub

  !> DOXYGEN_IMPL data_column::sum_of_column_elements_fn
  procedure, private :: sum_of_column_elements_fn
  generic, public    :: sum => sum_of_column_elements_fn

  !> DOXYGEN_IMPL data_column::return_current_record_number_fn
  procedure, private :: return_current_record_number_fn
  generic, public    :: currentRecnum => return_current_record_number_fn

  !> DOXYGEN_IMPL data_column::increment_current_record_number_fn
  procedure, private :: increment_current_record_number_fn
  generic, public    :: incrementRecnum => increment_current_record_number_fn

  !> DOXYGEN_IMPL data_column::return_data_type_fn
  procedure, private :: return_data_type_fn
  generic, public    :: datatype => return_data_type_fn

  !> DOXYGEN_IMPL data_column::set_data_type_sub
  procedure, private :: set_data_type_sub
  generic, public    :: setdatatype => set_data_type_sub

  !> DOXYGEN_IMPL data_column::min_of_column_elements_fn
  procedure, private :: min_of_column_elements_fn
  generic, public    :: min => min_of_column_elements_fn
  
  !> DOXYGEN_IMPL data_column::max_of_column_elements_fn
  procedure, private :: max_of_column_elements_fn
  generic, public    :: max => max_of_column_elements_fn

  !> DOXYGEN_IMPL data_column::count_of_column_elements_fn
  procedure, private :: count_of_column_elements_fn
  generic, public    :: count => count_of_column_elements_fn
  
  !> DOXYGEN_IMPL data_column::mean_of_column_elements_fn
  procedure, private :: mean_of_column_elements_fn
  generic, public    :: mean => mean_of_column_elements_fn

  procedure, public :: getval => get_date_value_at_index_fn

end type T_DATA_COLUMN

!------

type, public :: T_DATA_COLUMN_PTR
  
  class (T_DATA_COLUMN), pointer :: pColumn

end type T_DATA_COLUMN_PTR


  integer (kind=c_int), parameter, public :: GT = 1
  integer (kind=c_int), parameter, public :: GE = 2
  integer (kind=c_int), parameter, public :: LT = 3
  integer (kind=c_int), parameter, public :: LE = 4
  integer (kind=c_int), parameter, public :: EQ = 5
  integer (kind=c_int), parameter, public :: OUTSIDE_RANGE = 6
  integer (kind=c_int), parameter, public :: INSIDE_RANGE = 7
      
contains

  subroutine set_mask_int_sub(this, iValue1, iComparison, iValue2)

    class (T_DATA_COLUMN), intent(inout) :: this
    integer (kind=c_int), intent(in)             :: iValue1
    integer (kind=c_int), intent(in)             :: iComparison
    integer (kind=c_int), intent(in), optional   :: iValue2

    this%lMask = lFALSE

    if (iComparison == OUTSIDE_RANGE .or. iComparison == INSIDE_RANGE) &
      call assert(present(iValue2), "The second value is not optional for this comparison type", &
        __FILE__, __LINE__)

    select case (iComparison)

      case (GT)

        where (this%iData > iValue1)  this%lMask = lTRUE 
      
      case (GE)

        where (this%iData >= iValue1)  this%lMask = lTRUE

      case (LT)

        where (this%iData < iValue1)  this%lMask = lTRUE
      
      case (LE)

        where (this%iData <= iValue1)  this%lMask = lTRUE

      case (EQ)

        where (this%iData == iValue1)  this%lMask = lTRUE

      case (OUTSIDE_RANGE)

        where (this%iData > iValue2 .or. this%iData < iValue1)  this%lMask = lTRUE

      case (INSIDE_RANGE)

        where (this%iData <= iValue2 .and. this%iData >= iValue1)  this%lMask = lTRUE

      case default

        call die("Unhandled select case option", __FILE__, __LINE__)

    end select  

  end subroutine set_mask_int_sub



  subroutine set_mask_float_sub(this, fValue1, iComparison, fValue2)

    class (T_DATA_COLUMN), intent(inout) :: this
    real (kind=c_float), intent(in)              :: fValue1
    integer (kind=c_int), intent(in)             :: iComparison
    real (kind=c_float), intent(in), optional    :: fValue2

    this%lMask = lFALSE

    if (iComparison == OUTSIDE_RANGE .or. iComparison == INSIDE_RANGE) &
      call assert(present(fValue2), "The second value is not optional for this comparison type", &
        __FILE__, __LINE__)

    select case (iComparison)

      case (GT)

        where (this%fData > fValue1)  this%lMask = lTRUE 
      
      case (GE)

        where (this%fData >= fValue1)  this%lMask = lTRUE

      case (LT)

        where (this%fData < fValue1)  this%lMask = lTRUE
      
      case (LE)

        where (this%fData <= fValue1)  this%lMask = lTRUE

      case (EQ)

        where (this%fData == fValue1)  this%lMask = lTRUE

      case (OUTSIDE_RANGE)

        where (this%fData > fValue2 .or. this%fData < fValue1)  this%lMask = lTRUE

      case (INSIDE_RANGE)

        where (this%fData <= fValue2 .and. this%fData >= fValue1)  this%lMask = lTRUE

      case default

        call die("Unhandled select case option", __FILE__, __LINE__)

    end select  

  end subroutine set_mask_float_sub





  subroutine set_mask_double_sub(this, dValue1, iComparison, dValue2)

    class (T_DATA_COLUMN), intent(inout)          :: this
    real (kind=c_double), intent(in)              :: dValue1
    integer (kind=c_int), intent(in)              :: iComparison
    real (kind=c_double), intent(in), optional    :: dValue2

    this%lMask = lFALSE

    if (iComparison == OUTSIDE_RANGE .or. iComparison == INSIDE_RANGE) &
      call assert(present(dValue2), "The second value is not optional for this comparison type", &
        __FILE__, __LINE__)

    select case (iComparison)

      case (GT)

        where (this%dData > dValue1)  this%lMask = lTRUE 
      
      case (GE)

        where (this%dData >= dValue1)  this%lMask = lTRUE

      case (LT)

        where (this%dData < dValue1)  this%lMask = lTRUE
      
      case (LE)

        where (this%dData <= dValue1)  this%lMask = lTRUE

      case (EQ)

        where (this%dData == dValue1)  this%lMask = lTRUE

      case (OUTSIDE_RANGE)

        where (this%dData > dValue2 .or. this%dData < dValue1)  this%lMask = lTRUE

      case (INSIDE_RANGE)

        where (this%dData <= dValue2 .and. this%dData >= dValue1)  this%lMask = lTRUE

      case default

        call die("Unhandled select case option", __FILE__, __LINE__)

    end select  

  end subroutine set_mask_double_sub




  subroutine set_mask_datetime_sub(this, dtValue1, iComparison, dtValue2)

    class (T_DATA_COLUMN), intent(inout)       :: this
    type (T_DATETIME), intent(in)              :: dtValue1
    integer (kind=c_int), intent(in)           :: iComparison
    type (T_DATETIME), intent(in), optional    :: dtValue2

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    this%lMask = lFALSE

    if (iComparison == OUTSIDE_RANGE .or. iComparison == INSIDE_RANGE) &
      call assert(present(dtValue2), "The second value is not optional for this comparison type", &
        __FILE__, __LINE__)

    select case (iComparison)

      case (GT)

        do iIndex=1, ubound(this%pDatetime,1)
          if ( this%pDatetime(iIndex) > dtValue1 )   this%lMask = lTRUE 
        enddo

      case (GE)

        do iIndex=1, ubound(this%pDatetime,1)
          if ( this%pDatetime(iIndex) >= dtValue1 )   this%lMask = lTRUE 
        enddo

      case (LT)

        do iIndex=1, ubound(this%pDatetime,1)
          if ( this%pDatetime(iIndex) < dtValue1 )   this%lMask = lTRUE 
        enddo
      
      case (LE)

        do iIndex=1, ubound(this%pDatetime,1)
          if ( this%pDatetime(iIndex) <= dtValue1 )   this%lMask = lTRUE 
        enddo

      case (EQ)

        do iIndex=1, ubound(this%pDatetime,1)
          if ( this%pDatetime(iIndex) == dtValue1 )   this%lMask = lTRUE 
        enddo

      case (OUTSIDE_RANGE)

        do iIndex=1, ubound(this%pDatetime,1)
          if ( this%pDatetime(iIndex) > dtValue2 &
              .or. this%pDatetime(iIndex) < dtValue1 )   this%lMask = lTRUE 
        enddo

      case (INSIDE_RANGE)

        do iIndex=1, ubound(this%pDatetime,1)
          if ( this%pDatetime(iIndex) > dtValue1 &
              .and. this%pDatetime(iIndex) < dtValue2 )   this%lMask = lTRUE 
        enddo

      case default

        call die("Unhandled select case option", __FILE__, __LINE__)

    end select  

  end subroutine set_mask_datetime_sub


  subroutine set_mask_string_sub(this, sText1, iComparison, sText2)

    class (T_DATA_COLUMN), intent(inout)       :: this
    character (len=*), intent(in)              :: sText1
    integer (kind=c_int), intent(in)           :: iComparison
    character (len=*), intent(in), optional    :: sText2

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex
    integer (kind=c_int), allocatable :: iMatches(:)

    this%lMask = lFALSE

    if (iComparison == OUTSIDE_RANGE .or. iComparison == INSIDE_RANGE) &
      call assert(present(sText2), "The second value is not optional for this comparison type", &
        __FILE__, __LINE__)

    select case (iComparison)

      case (GT)


      case (GE)


      case (LT)

      
      case (LE)


      case (EQ)

        iMatches = this%stData%which(sText1)

        if (size(iMatches,1) > 0) then
          
          do iIndex=1, ubound(iMatches,1)
            if (iMatches(iIndex) <= ubound(this%lMask,1))   this%lMask( iMatches(iIndex) ) = lTRUE
          enddo  
        endif

      case (OUTSIDE_RANGE)


      case (INSIDE_RANGE)


      case default

        call die("Unhandled select case option", __FILE__, __LINE__)

    end select  

  end subroutine set_mask_string_sub





  function get_date_value_at_index_fn(this, iIndex)   result(pDatetime)

    class (T_DATA_COLUMN), intent(in) :: this
    integer (kind=c_int), intent(in)       :: iIndex
    class (T_DATETIME), pointer            :: pDatetime

    select case ( this%iDataType )

      case (DATETIME_DATA, DATE_DATA)

        if (associated(this%pDatetime)) then

          if (iIndex >= lbound(this%pDatetime,1) &
              .and. iIndex <= ubound(this%pDatetime,1)) then

            pDatetime => this%pDatetime(iIndex)

          endif
      
        endif    

      case default  

        pDatetime => null()

      end select  

  end function get_date_value_at_index_fn
  
!--------------------------------------------------------------------

  function put_next_integer_value_fn(this, iValue)   result(iRecNum)

    class (T_DATA_COLUMN), intent(inout)   :: this
    integer (kind=c_int), intent(in)       :: iValue
    integer (kind=c_int)                   :: iRecNum

    !iRecNum = this%currentRecnum()
    iRecNum = this%incrementRecnum()
    this%iData(iRecNum) = iValue

  end function put_next_integer_value_fn

!--------------------------------------------------------------------

  function put_next_float_value_fn(this, fValue)   result(iRecNum)

    class (T_DATA_COLUMN), intent(inout)   :: this
    real (kind=c_float), intent(in)        :: fValue
    integer (kind=c_int)                   :: iRecNum

    !iRecNum = this%currentRecnum()
    iRecNum = this%incrementRecnum()
    this%fData(iRecNum) = fValue
    !iRecNum = this%incrementRecnum()

  end function put_next_float_value_fn

!--------------------------------------------------------------------

  function put_next_double_value_fn(this, dValue)   result(iRecNum)

    class (T_DATA_COLUMN), intent(inout)   :: this
    real (kind=c_double), intent(in)       :: dValue
    integer (kind=c_int)                   :: iRecNum

    !iRecNum = this%currentRecnum()
    iRecNum = this%incrementRecnum()
    this%dData(iRecNum) = dValue

  end function put_next_double_value_fn

!--------------------------------------------------------------------

  function put_next_datetime_value_fn(this, sDatetime)   result(iRecNum)

    class (T_DATA_COLUMN), intent(inout)            :: this
    character (len=*), intent(in)                   :: sDatetime
    integer (kind=c_int)                            :: iRecNum

    ! [ LOCALS ]
    type (T_DATETIME), pointer :: pDT

!    iRecNum = this%currentRecnum()
    iRecNum = this%incrementRecnum()
    pDT => this%pDatetime(iRecNum)
    call pDT%parseDate( sDatetime ) 
!    iRecNum = this%incrementRecnum()

  end function put_next_datetime_value_fn

!--------------------------------------------------------------------

  function put_next_date_value_fn(this, sDate)   result(iRecNum)

    class (T_DATA_COLUMN), intent(inout)        :: this
    character (len=*), intent(in)               :: sDate
    integer (kind=c_int)                        :: iRecNum

    ! [ LOCALS ]
    type (T_DATETIME), pointer :: pDate

 !   iRecNum = this%currentRecnum()
    iRecNum = this%incrementRecnum()
    pDate => this%pDatetime(iRecNum)
    call pDate%parseDate( sDate ) 

  end function put_next_date_value_fn

!--------------------------------------------------------------------

  function put_next_time_value_fn(this, sTime)   result(iRecNum)

    class (T_DATA_COLUMN), intent(inout)        :: this
    character (len=*), intent(in)               :: sTime
    integer (kind=c_int)                        :: iRecNum

    ! [ LOCALS ]
    type (T_DATETIME), pointer :: pTime

    iRecNum = this%currentRecnum()
    pTime => this%pDatetime(iRecNum)
    call pTime%parseTime( sTime ) 

  end function put_next_time_value_fn

!--------------------------------------------------------------------

  function put_next_string_value_fn(this, sString)   result(iRecNum)

    class (T_DATA_COLUMN), intent(inout)   :: this
    character (len=*)                      :: sString
    integer (kind=c_int)                   :: iRecNum

    iRecNum = this%currentRecnum()
    call this%stData%append(sString)
    iRecNum = this%incrementRecnum()

  end function put_next_string_value_fn

!--------------------------------------------------------------------

  function return_current_record_number_fn(this)     result(iRecNum)

    class (T_DATA_COLUMN), intent(in)   :: this
    integer (kind=c_int)                :: iRecNum

    iRecNum = this%iCurrentRecord

  end function return_current_record_number_fn
  
!--------------------------------------------------------------------

  function increment_current_record_number_fn(this, iIncrementAmt)  result(iRecNum)

    class (T_DATA_COLUMN), intent(inout)   :: this
    integer (kind=c_int), optional         :: iIncrementAmt
    integer (kind=c_int)                   :: iRecNum

    if (present(iIncrementAmt) ) then

      this%iCurrentRecord = this%iCurrentRecord + iIncrementAmt

    else

      this%iCurrentRecord = this%iCurrentRecord + 1

    endif

    iRecNum = this%iCurrentRecord

  end function increment_current_record_number_fn

!--------------------------------------------------------------------

  subroutine set_data_type_sub(this, iDataType)

    class (T_DATA_COLUMN), intent(inout) :: this
    integer (kind=c_int), intent(in)     :: iDataType 

    this%iDataType = iDataType

  end subroutine set_data_type_sub

!--------------------------------------------------------------------

  function return_data_type_fn(this)    result(iDataType)

    class (T_DATA_COLUMN), intent(in) :: this
    integer (kind=c_int)              :: iDataType

    iDataType = this%iDataType

  end function return_data_type_fn  

!--------------------------------------------------------------------

  function sum_of_column_elements_fn(this, lMask) result(dSum)

    class(T_DATA_COLUMN), intent(in) :: this
    logical (kind=c_bool), intent(in), optional :: lMask(:)    
    real (kind=c_double) :: dSum

    select case ( this%datatype() )

      case (INTEGER_DATA)

        if (present (lMask) ) then
          dSum = sum(this%iData, lMask)
        else
          dSum = sum(this%iData, this%lMask)        
        endif  

      case (FLOAT_DATA)
      
        if (present (lMask) ) then
          dSum = sum(this%fData, lMask)
        else
          dSum = sum(this%fData, this%lMask)        
        endif  

      case (DOUBLE_DATA)
      
        if (present (lMask) ) then
          dSum = sum(this%dData, lMask)
        else
          dSum = sum(this%dData, this%lMask)        
        endif  

      case default
      
        dSum = -9999.

    end select

  end function sum_of_column_elements_fn

!--------------------------------------------------------------------

  function mean_of_column_elements_fn(this, lMask) result(dMean)

    class(T_DATA_COLUMN), intent(in) :: this
    logical (kind=c_bool), intent(in), optional :: lMask(:)    
    real (kind=c_double) :: dMean

    select case ( this%datatype() )

      case (INTEGER_DATA)

        if (present (lMask) ) then
          dMean = sum(this%iData, lMask) / real(count(lMask), kind=c_double)
        else
          dMean = sum(this%iData, this%lMask) / real(count(this%lMask), kind=c_double)
        endif  

      case (FLOAT_DATA)
      
        if (present (lMask) ) then
          dMean = sum(this%fData, lMask) / real(count(lMask), kind=c_double)
        else
          dMean = sum(this%fData, this%lMask) / real(count(this%lMask), kind=c_double)
        endif  

      case (DOUBLE_DATA)
      
        if (present (lMask) ) then
          dMean = sum(this%dData, lMask) / real(count(lMask), kind=c_double)
        else
          dMean = sum(this%dData, this%lMask) / real(count(this%lMask), kind=c_double)
        endif  

      case default
      
        dMean = -9999.

    end select

  end function mean_of_column_elements_fn

!--------------------------------------------------------------------

  function min_of_column_elements_fn(this, lMask) result(dMin)

    class(T_DATA_COLUMN), intent(in) :: this
    logical (kind=c_bool), intent(in), optional :: lMask(:)    
    real (kind=c_double) :: dMin

    select case (this%iDataType)

      case (INTEGER_DATA)

        if (present (lMask) ) then
          dMin = minval(this%iData, lMask)
        else
          dMin = minval(this%iData, this%lMask)        
        endif  

      case (FLOAT_DATA)
      
        if (present (lMask) ) then
          dMin = minval(this%fData, lMask)
        else
          dMin = minval(this%fData, this%lMask)        
        endif  

      case (DOUBLE_DATA)
      
        if (present (lMask) ) then
          dMin = minval(this%dData, lMask)
        else
          dMin = minval(this%dData, this%lMask)        
        endif  

      case (DATETIME_DATA, DATE_DATA)

        if (present (lMask) ) then
          dMin = minval(this%pDatetime%dJulianDate, lMask)
        else
          dMin = minval(this%pDatetime%dJulianDate, this%lMask)
        endif  

      case default
      
        dMin = -9999.

    end select

  end function min_of_column_elements_fn

!--------------------------------------------------------------------

  function max_of_column_elements_fn(this, lMask) result(dMax)

    class(T_DATA_COLUMN), intent(in) :: this
    logical (kind=c_bool), intent(in), optional :: lMask(:)
    real (kind=c_double) :: dMax

    select case (this%iDataType)

      case (INTEGER_DATA)
      
        if (present (lMask) ) then
          dMax = maxval(this%iData, lMask)
        else
          dMax = maxval(this%iData, this%lMask)        
        endif  

      case (FLOAT_DATA)
      
        if (present (lMask) ) then
          dMax = maxval(this%fData, lMask)
        else
          dMax = maxval(this%fData, this%lMask)        
        endif  

      case (DOUBLE_DATA)
      
        if (present (lMask) ) then
          dMax = maxval(this%dData, lMask)
        else
          dMax = maxval(this%dData, this%lMask)        
        endif  

      case (DATETIME_DATA, DATE_DATA)

        if (present (lMask) ) then
          dMax = maxval(this%pDatetime%dJulianDate, lMask)
        else
          dMax = maxval(this%pDatetime%dJulianDate, this%lMask)
        endif  

      case default
      
        dMax = -9999.

    end select

  end function max_of_column_elements_fn

!--------------------------------------------------------------------

  function count_of_column_elements_fn(this, lMask) result(dCount)

    class(T_DATA_COLUMN), intent(in) :: this
    logical (kind=c_bool), intent(in), optional :: lMask(:)
    real (kind=c_double) :: dCount  

    if ( present(lMask) ) then

      dCount = count(lMask)

    else 
    
      dCount = count(this%lMask)

    endif  

    
  end function count_of_column_elements_fn

!--------------------------------------------------------------------

  subroutine create_new_column_sub(this, iDataType, iCount)

    class (T_DATA_COLUMN) :: this
    integer (kind=c_int), intent(in) :: iDataType
    integer (kind=c_int),intent(in)  :: iCount

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat

    !> lMask entries will be used later on to assist in 
    !> subsetting of particular chunks of data
    
    select case ( iDataType )

      case (INTEGER_DATA)
        
        allocate( this%iData(iCount), stat = iStat )

      case (FLOAT_DATA)

        allocate( this%fData(iCount), stat = iStat)

      case (DOUBLE_DATA)

        allocate( this%dData(iCount), stat = iStat)

      case (STRING_DATA)

        iStat = 0

      case (DATETIME_DATA, DATE_DATA, TIME_DATA)

        if (.not. associated(this%pDatetime))  &
           allocate( this%pDatetime(iCount), stat=iStat)

      case default

        call die("Unhandled case select", __FILE__, __LINE__)

    end select

    allocate( this%lMask(iCount), stat = iStat)
    this%lMask = lTRUE

    call this%setdatatype(iDataType)

  call assert( iStat == 0, "Failed to allocate memory while creating a new column", &
        __FILE__, __LINE__)

  end subroutine create_new_column_sub


end module data_column