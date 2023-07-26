!> @file
!! Contains a single module, @ref constants_and_conversions, which contains useful
!! physical constants and basic conversion utilities

!> This module contains physical constants and convenience functions aimed at
!! performing unit conversions. The functions included in this module perform
!! conversions between various temperature and distance units.
module constants_and_conversions

  use iso_c_binding, only : c_short, c_int, c_long, c_float, c_double, c_bool, &
                            c_long_long
  implicit none

  !
  integer (c_int), parameter :: DATATYPE_INT     = 0
  integer (c_int), parameter :: DATATYPE_FLOAT   = 1
  integer (c_int), parameter :: DATATYPE_REAL    = 1
  integer (c_int), parameter :: DATATYPE_DOUBLE  = 2
  integer (c_int), parameter :: DATATYPE_SHORT   = 3
  integer (c_int), parameter :: DATATYPE_NA      = -9999

  ! [ common mathematical constants ]
  public :: PI, TWOPI, HALFPI
  real (c_double), parameter    :: PI = 3.1415926535897932384626433_c_double
  real (c_double), parameter    :: TWOPI = 3.1415926535897932384626433_c_double * 2.0_c_double
  real (c_double), parameter    :: HALFPI = 3.1415926535897932384626433_c_double / 2.0_c_double

  ! [ trig conversion factors ]
  public :: DEGREES_TO_RADIANS, RADIANS_TO_DEGREES
  real (c_double), parameter    :: DEGREES_TO_RADIANS = TWOPI / 360.0_c_double
  real (c_double), parameter    :: RADIANS_TO_DEGREES = 360.0_c_double / TWOPI

  ! [ common 'magic' numbers and logicals ]
  public :: TRUE, FALSE
  public :: rBIGVAL, iBIGVAL, dBIGVAL, iTINYVAL, rTINYVAL, dTINYVAL
  public :: rFREEZING, dFREEZING
  public :: rZERO
  logical (c_bool), parameter   :: TRUE = .true._c_bool
  logical (c_bool), parameter   :: FALSE = .false._c_bool

  real (c_float), parameter     :: rBIGVAL = HUGE(0.0_c_float)
  real (c_double), parameter    :: dBIGVAL = HUGE(0.0_c_double)
  integer(c_int), parameter     :: iBIGVAL = HUGE(0_c_int)
  real (c_float), parameter     :: rTINYVAL = -(HUGE(0.0_c_float) - 1.0)
  real (c_double), parameter    :: dTINYVAL = -(HUGE(0.0_c_double) - 1.0)
  real (c_float), parameter     :: fTINYVAL = -(HUGE(0.0_c_float) - 1.0)
  integer(c_int), parameter     :: iTINYVAL = -(HUGE(0_c_int) - 1)
  real (c_float), parameter     :: rFREEZING = 32.0_c_float
  real (c_float), parameter     :: fFREEZING = 32.0_c_float
  real (c_double), parameter    :: dFREEZING = 32.0_c_double
  integer (c_int), parameter    :: iZERO = 0_c_int
  real (c_float), parameter     :: fZERO = 0.0_c_float
  real (c_float), parameter     :: rZERO = 0.0_c_float
  real (c_double), parameter    :: dZERO = 0.0_c_double

    ! [ evil global variables ]
  character (len=1)            :: OS_NATIVE_PATH_DELIMITER
  integer (c_long_long)        :: RANDOM_START = 0

  ! [ select conversion factors ]
  real (c_double), parameter, public :: C_PER_F    = 5.0_c_double / 9.0_c_double
  real (c_double), parameter, public :: F_PER_C    = 9.0_c_double / 5.0_c_double
  real (c_double), parameter, public :: M_PER_FOOT = 0.3048_c_double
  real (c_double), parameter, public :: MM_PER_IN  = 25.4_c_double
  real (c_double), parameter, public :: FREEZING_POINT_OF_WATER_KELVIN = 273.15_c_double
  real (c_double), parameter, public :: FREEZING_POINT_OF_WATER_FAHRENHEIT = 32.0_c_double

  public :: OUTPUT_PREFIX_NAME, OUTPUT_DIRECTORY_NAME, DATA_DIRECTORY_NAME,     &
            LOOKUP_TABLE_DIRECTORY_NAME
  character (len=:), allocatable    :: OUTPUT_DIRECTORY_NAME
  character (len=:), allocatable    :: OUTPUT_PREFIX_NAME
  character (len=:), allocatable    :: DATA_DIRECTORY_NAME
  character (len=:), allocatable    :: LOOKUP_TABLE_DIRECTORY_NAME

  type BOUNDS_T
    character (len=:), allocatable  :: sPROJ4_string
    integer (c_int)            :: iNumCols
    integer (c_int)            :: iNumRows
    real (c_double)            :: fX_ll, fY_ll
    real (c_double)            :: fX_ur, fY_ur
    real (c_double)            :: fGridcellSize
  end type BOUNDS_T

  type (BOUNDS_T), public :: BNDS

  public :: operator(.approxequal.)
  interface operator(.approxequal.)
    module procedure approx_equal_float_float
    module procedure approx_equal_double_double
    module procedure approx_equal_float_double
  end interface operator(.approxequal.)



  !> establish generic interfaces to single and double precision functions
  public :: C_to_F
  interface C_to_F
    module procedure CtoF_sgl_fn
    module procedure CtoF_dbl_fn
  end interface C_to_F

  public F_to_C
  interface F_to_C
    module procedure FtoC_sgl_fn
    module procedure FtoC_dbl_fn
  end interface F_to_C

  public :: F_to_K
  interface F_to_K
    module procedure FtoK_sgl_fn
    module procedure FtoK_dbl_fn
  end interface F_to_K

  public :: C_to_K
  interface C_to_K
    module procedure CtoK_sgl_fn
    module procedure CtoK_dbl_fn
  end interface C_to_K

  public :: deg_to_rad
  interface deg_to_rad
    module procedure deg_to_rad_sgl_fn
    module procedure deg_to_rad_dbl_fn
  end interface deg_to_rad

  public :: rad_to_deg
  interface rad_to_deg
    module procedure rad_to_deg_sgl_fn
    module procedure rad_to_deg_dbl_fn
  end interface rad_to_deg

  public :: asFloat
  interface asFloat
    module procedure char2real
    module procedure short2real
    module procedure int2real
    module procedure dbl2real
    module procedure bool2real
  end interface asFloat

  public :: asDouble
  interface asDouble
    module procedure char2dbl
    module procedure short2dbl
    module procedure int2dbl
    module procedure real2dbl
    module procedure bool2dbl
  end interface asDouble

  public asInt
  interface asInt
    module procedure short2int
    module procedure char2int
    module procedure real2int
    module procedure dbl2int
  end interface asInt

  public asLogical
  interface asLogical
    module procedure short2logical
    module procedure int2logical
    module procedure real2logical
    module procedure dbl2logical
    module procedure char2logical
  end interface asLogical

  public :: mm_to_in
  interface mm_to_in
    module procedure mm_to_inches_sgl_fn
    module procedure mm_to_inches_dbl_fn
  end interface mm_to_in

  public :: in_to_mm
  interface in_to_mm
    module procedure inches_to_mm_sgl_fn
    module procedure inches_to_mm_dbl_fn
  end interface in_to_mm

  public :: clip
  interface clip
    module procedure enforce_bounds_int_fn
    module procedure enforce_bounds_sgl_fn
    module procedure enforce_bounds_dbl_fn
  end interface clip

  public :: char_ptr_to_fortran_string
  public :: c_to_fortran_string
  public :: fortran_to_c_string
  public :: is_numeric

  real (c_float), parameter  :: TOLERANCE_FLOAT = 1.0e-6_c_float
  real (c_double), parameter :: TOLERANCE_DOUBLE = 1.0e-9_c_double

contains

  function fix_pathname( input_pathname )    result( output_pathname )

    character (len=*), intent(in)            :: input_pathname
    character (len=len_trim(input_pathname)) :: output_pathname

    ! [ LOCALS ]
    integer (c_int) :: indx, jndx

    do indx=1,len_trim(input_pathname)

      jndx = min( indx+1, len_trim(input_pathname) )
      ! allow for escaping of spaces in pathnames
      if (     ( (input_pathname(indx:indx) == '/')                            &
                .or. (input_pathname(indx:indx) == '\') )                      &
          .and. (input_pathname(jndx:jndx) /= ' ' ) ) then

        output_pathname(indx:indx) = OS_NATIVE_PATH_DELIMITER

      else

        output_pathname(indx:indx) = input_pathname(indx:indx)

      endif

    enddo

  end function fix_pathname


  elemental function approx_equal_float_float(fValue1, fValue2)  result(lBool)

    real (c_float), intent(in)    :: fValue1
    real (c_float), intent(in)    :: fValue2
    logical (c_bool)              :: lBool

    if ( abs( fValue1 - fValue2 ) < TOLERANCE_FLOAT ) then
      lBool = TRUE
    else
      lBool = FALSE
    endif

  end function approx_equal_float_float




  elemental function approx_equal_float_double(fValue1, fValue2)  result(lBool)

    real (c_float), intent(in)    :: fValue1
    real (c_double), intent(in)    :: fValue2
    logical (c_bool)              :: lBool

    if ( abs( fValue1 - real(fValue2, c_float) ) < TOLERANCE_FLOAT ) then
      lBool = TRUE
    else
      lBool = FALSE
    endif

  end function approx_equal_float_double





  elemental function approx_equal_double_double(fValue1, fValue2)  result(lBool)

    real (c_double), intent(in)    :: fValue1
    real (c_double), intent(in)    :: fValue2
    logical (c_bool)               :: lBool

    if ( abs( fValue1 - fValue2 ) < TOLERANCE_DOUBLE ) then
      lBool = TRUE
    else
      lBool = FALSE
    endif

  end function approx_equal_double_double

!--------------------------------------------------------------------------

  !> Determine if string contains numeric values.
  !! @param[in]  degrees     String value.
  !! @retval     is_numeric  True if any numeric values are present in the string.

  impure elemental function is_numeric( value )

    character (len=*), intent(in)    :: value
    logical (c_bool)            :: is_numeric

    ! [ LOCALS ]
    character (len=256) :: sbuf

    sbuf = keepnumeric( value )

    if ( len_trim( sbuf ) == 0 ) then
      is_numeric = FALSE
    else
      is_numeric = TRUE
    endif

  end function is_numeric

!--------------------------------------------------------------------------

  !> Convert degrees to radians.
  !! @param[in]  degrees   Angle in degrees.
  !! @retval     radians  Angle in radians.

  elemental function deg_to_rad_sgl_fn( degrees )    result( radians )

    real (c_float), intent(in)    :: degrees
    real (c_float)                :: radians

    radians = degrees * DEGREES_TO_RADIANS

  end function deg_to_rad_sgl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees to radians.
  !! @param[in]  degrees   Angle in degrees.
  !! @retval     radians   Angle in radians.

  elemental function deg_to_rad_dbl_fn(degrees)    result(radians)

    real (c_double), intent(in)    :: degrees
    real (c_double)                :: radians

    radians = degrees * DEGREES_TO_RADIANS

  end function deg_to_rad_dbl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert radians to degrees.
  !! @param[in]  radians   Angle in radians.
  !! @retval     degrees   Angle in degrees.

  elemental function rad_to_deg_sgl_fn(radians)    result(degrees)

    real (c_float), intent(in)    :: radians
    real (c_float)                :: degrees

    degrees = radians * RADIANS_TO_DEGREES

  end function rad_to_deg_sgl_fn

!--------------------------------------------------------------------------------------------------

  !> Convert radians to degrees.
  !! @param[in]  radians   Angle in radians.
  !! @retval     degrees   Angle in degrees.

  elemental function rad_to_deg_dbl_fn(radians)    result(degrees)

    real (c_double), intent(in)    :: radians
    real (c_double)                :: degrees

    degrees = radians * RADIANS_TO_DEGREES

  end function rad_to_deg_dbl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Fahrenheit to degrees Celsius.
  !! @param[in]  degrees_F   Temperature in degrees Fahrenheit.
  !! @retval     degrees_C   Temperature in degrees Celcius.

  elemental function FtoC_sgl_fn(degrees_F)   result(degrees_C)

    real (c_float),intent(in) :: degrees_F
    real (c_float) :: degrees_C

    degrees_C = (degrees_F - rFREEZING) * C_PER_F

  end function FtoC_sgl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Fahrenheit to degrees Celsius.
  !! @param[in] degrees_F Temperature in degrees Fahrenheit.
  !! @retval degrees_C Temperature in degrees Celcius.

  elemental function FtoC_dbl_fn( degrees_F )   result( degrees_C )

    real (c_double),intent(in) :: degrees_F
    real (c_double) :: degrees_C

    degrees_C = ( degrees_F - dFREEZING ) * C_PER_F

  end function FtoC_dbl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Celsius to degrees Fahrenheit.
  !! @param[in] degrees_C Temperature in degrees Celsius.
  !! @retval degrees_F Temperature in degrees Fahrenheit.

  elemental function CtoF_sgl_fn( degrees_C )   result( degrees_F )

    real (c_float),intent(in) :: degrees_C
    real (c_float) :: degrees_F

    degrees_F = degrees_C * F_PER_C + dFREEZING

  end function CtoF_sgl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Celsius to degrees Fahrenheit.
  !! @param[in] degrees_C Temperature in degrees Celsius.
  !! @retval degrees_F Temperature in degree

  elemental function CtoF_dbl_fn( degrees_C )   result( degrees_F )

    real (c_double),intent(in) :: degrees_C
    real (c_double) :: degrees_F

    degrees_F = degrees_C * F_PER_C + dFREEZING

  end function CtoF_dbl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Fahrenheit to Kelvins.
  !! @param[in] degrees_F Temperature in degrees Fahrenheit.
  !! @retval degrees_K Temperature in Kelvins.

  elemental function FtoK_sgl_fn( degrees_F )    result( degrees_K )

    real (c_float),intent(in) :: degrees_F
    real (c_float) :: degrees_K

    degrees_K = (degrees_F - dFREEZING) * C_PER_F + 273.15_c_double

  end function FtoK_sgl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Fahrenheit to Kelvins.
  !! @param[in] degrees_F Temperature in degrees Fahrenheit.
  !! @retval degrees_K Temperature in Kelvins.

  elemental function FtoK_dbl_fn( degrees_F )    result( degrees_K )

    real (c_double),intent(in) :: degrees_F
    real (c_double) :: degrees_K

    degrees_K = (degrees_F - dFREEZING) * C_PER_F + 273.15_c_double

  end function FtoK_dbl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Celsius to Kelvins.
  !! @param[in] degrees_C Temperature in degrees Celcius.
  !! @retval degrees_K Temperature in Kelvins.

  elemental function CtoK_sgl_fn( degrees_C )    result( degrees_K )

    real (c_float), intent(in) :: degrees_C
    real (c_float) :: degrees_K

    degrees_K = degrees_C + 273.15_c_double

  end function CtoK_sgl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Fahrenheit to Kelvins.
  !! @param[in] degrees_C Temperature in degrees Celsius.
  !! @retval degrees_K Temperature in Kelvins.

  elemental function CtoK_dbl_fn( degrees_C )    result( degrees_K )

    real (c_double), intent(in) :: degrees_C
    real (c_double) :: degrees_K

    degrees_K = degrees_C + 273.15_c_double

  end function CtoK_dbl_fn

!--------------------------------------------------------------------------------------------------

  !>  Convert inches to mm.
  !! @param[in] inches Value in inches.
  !! @retval mm Value in millimeters.

  elemental function inches_to_mm_sgl_fn( inches )   result( mm )

    real (c_float),intent(in) :: inches
    real (c_float)            :: mm

    mm = inches * 25.4_c_double

  end function inches_to_mm_sgl_fn

!--------------------------------------------------------------------------------------------------

  !>  Convert inches to mm.
  !! @param[in] inches Value in inches.
  !! @retval mm Value in millimeters.

  elemental function inches_to_mm_dbl_fn( inches )   result( mm )

    real (c_double),intent(in) :: inches
    real (c_double)            :: mm

    mm = inches * 25.4_c_double

  end function inches_to_mm_dbl_fn

!--------------------------------------------------------------------------------------------------

  !>  Convert millimeters to inches.
  !! @param[in] mm Value in millimeters.
  !! @retval inches Value in inches.

  elemental function mm_to_inches_sgl_fn(mm) result(inches)

    real (c_float),intent(in) :: mm
    real (c_float) :: inches

    inches = mm / 25.4_c_double

  end function mm_to_inches_sgl_fn

!--------------------------------------------------------------------------------------------------

  !>  Convert millimeters to inches.
  !! @param[in] mm Value in millimeters.
  !! @retval inches Value in inches.

  elemental function mm_to_inches_dbl_fn( mm ) result( inches )

    real (c_double),intent(in) :: mm
    real (c_double) :: inches

    inches = mm / 25.4_c_double

  end function mm_to_inches_dbl_fn

!--------------------------------------------------------------------------------------------------

  !> Convert a short integer to a logical value

  elemental function short2logical(iShortVal)    result(lValue)

    integer (c_short), intent(in)    :: iShortVal
    logical (c_bool)                 :: lValue

    if ( iShortVal == 0 ) then
      lValue = FALSE
    else
      lValue = TRUE
    endif

  end function short2logical

!--------------------------------------------------------------------------------------------------

  !> Convert an integer to a logical value

  elemental function int2logical(iValue)    result(lValue)

    integer (c_int), intent(in)      :: iValue
    logical (c_bool)                 :: lValue

    if ( iValue == 0 ) then
      lValue = FALSE
    else
      lValue = TRUE
    endif

  end function int2logical

!--------------------------------------------------------------------------------------------------

  !> Convert a real to a logical value

  elemental function real2logical(rValue)    result(lValue)

    real (c_float), intent(in)      :: rValue
    logical (c_bool)                :: lValue

    ! [ LOCALS ]
    real (c_float), parameter :: fMinResolution = 2.0 * spacing(1.0_c_float)

    if ( rValue > -fMinResolution .and. rValue < fMinResolution ) then
      lValue = FALSE
    else
      lValue = TRUE
    endif

  end function real2logical

!--------------------------------------------------------------------------------------------------

  !> Convert a double to a logical value

  elemental function dbl2logical(rValue)    result(lValue)

    real (c_double), intent(in)      :: rValue
    logical (c_bool)                 :: lValue

    ! [ LOCALS ]
    real (c_double), parameter :: dMinResolution = 2.0 * spacing(1.0_c_float)

    if ( rValue > -dMinResolution .and. rValue < dMinResolution ) then
      lValue = FALSE
    else
      lValue = TRUE
    endif

  end function dbl2logical

!--------------------------------------------------------------------------------------------------

  !> Convert a character string to a logical value

  elemental function char2logical(sValue)    result(lValue)

    character (len=*), intent(in)      :: sValue
    logical (c_bool)              :: lValue

    select case ( sValue )

      case ( "TRUE", "True", "true", "T", "YES", "Yes", "yes", "1" )

        lValue = TRUE

      case default

        lValue = FALSE

    end select

  end function char2logical

!--------------------------------------------------------------------------------------------------

  !> Convert a short integer to an integer

  elemental function short2int(iShortVal)    result(iValue)

    integer (c_short), intent(in)    :: iShortVal
    integer (c_int)                  :: iValue

    iValue = int( iShortVal, c_int )

  end function short2int

!--------------------------------------------------------------------------------------------------

  !> Convert a character value into a integer

  impure elemental function char2int(sValue)  result(iValue)

    character (len=*), intent(in) :: sValue
    integer (c_int) :: iValue

    ! [ LOCALS ]
    integer (c_int) :: iStat
    character (len=:), allocatable :: sTempVal
    real (c_float)  :: rValue

    sTempVal = keepnumeric(sValue)

    ! if the cleaned up string appears to be a real value,
    ! attempt to read as real and convert to int
    if ( scan(sTempVal, ".") /= 0 ) then

      read(unit=sTempVal, fmt=*, iostat=iStat) rValue

      if (iStat == 0)  iValue = int(rValue, c_int)

    else

      read(unit=sTempVal, fmt=*, iostat=iStat) iValue

    endif

    if (iStat /= 0)  iValue = iTINYVAL

  end function char2int

!--------------------------------------------------------------------------------------------------

!> Convert a real value into a integer

elemental function real2int(rValue)  result(iValue)

  real (c_float), intent(in) :: rValue
  integer (c_int) :: iValue

  iValue = int(rValue, c_int)

end function real2int

!--------------------------------------------------------------------------------------------------

!> Convert a double-precision value to an integer

elemental function dbl2int(rValue)  result(iValue)

  real (c_double), intent(in) :: rValue
  integer (c_int) :: iValue

  iValue = int(rValue, c_int)

end function dbl2int

!--------------------------------------------------------------------------------------------------

!> Convert a character value into a real

elemental function char2real(sValue)  result(rValue)

  character (len=*), intent(in) :: sValue
  real (c_float) :: rValue

  ! [ LOCALS ]
  integer (c_int) :: iStat

  read(unit=sValue, fmt=*, iostat=iStat) rValue

  if (iStat /= 0) rValue = rTINYVAL

end function char2real

!--------------------------------------------------------------------------------------------------

!> Convert a short int value into a real

elemental function short2real(iValue)  result(rValue)

  integer (c_short), intent(in) :: iValue
  real (c_float)                :: rValue

  rValue = real(iValue, c_float)

end function short2real

!--------------------------------------------------------------------------------------------------

!> Convert an int value into a real

elemental function int2real(iValue)  result(rValue)

  integer (c_int), intent(in) :: iValue
  real (c_float) :: rValue

  rValue = real(iValue, c_float)

end function int2real

!--------------------------------------------------------------------------------------------------

!> Convert a dbl value into a real

elemental function dbl2real(dpValue)  result(rValue)

  real (c_double), intent(in) :: dpValue
  real (c_float) :: rValue

  rValue = real(dpValue, c_float)

end function dbl2real

!--------------------------------------------------------------------------------------------------

!> Convert a boolean value into a real
elemental pure function bool2real(lValue)   result(rValue)
  logical (c_bool), intent(in) :: lValue
  real (c_float)               :: rValue

  if (lValue) then
    rValue = 1.0_c_float
  else
    rValue = 0.0_c_float
  end if

end function bool2real

!--------------------------------------------------------------------------------------------------

!> Convert a character value into a double

elemental function char2dbl(sValue)  result(dValue)

  character (len=*), intent(in)    :: sValue
  real (c_double) :: dValue

  ! [ LOCALS ]
  integer (c_int) :: iStat

  read(unit=sValue, fmt=*, iostat=iStat) dValue

  if (iStat /= 0)  dValue = dTINYVAL

end function char2dbl

!--------------------------------------------------------------------------------------------------

!> Convert a short int value into a double

elemental function short2dbl(iValue)  result(dValue)

  integer (c_short), intent(in) :: iValue
  real (c_double)               :: dValue

  dValue = real(iValue, c_double)

end function short2dbl

!--------------------------------------------------------------------------------------------------

!> Convert an int value into a double

elemental function int2dbl(iValue)  result(dValue)

  integer (c_int), intent(in) :: iValue
  real (c_double) :: dValue

  dValue = real(iValue, c_double)

end function int2dbl

!--------------------------------------------------------------------------------------------------

!> Convert a real value into a double

elemental function real2dbl(fValue)  result(dValue)

  real (c_float), intent(in) :: fValue
  real (c_double) :: dValue

  dValue = real(fValue, c_double)

end function real2dbl

!--------------------------------------------------------------------------------------------------

!> Convert a boolean value into a double
elemental pure function bool2dbl(lValue)   result(dValue)
  logical (c_bool), intent(in) :: lValue
  real (c_double)              :: dValue

  if (lValue) then
    dValue = 1.0_c_double
  else
    dValue = 0.0_c_double
  end if

end function bool2dbl

!--------------------------------------------------------------------------------------------------

function char_ptr_to_fortran_string( cpCharacterPtr )  result(sText)

  use iso_c_binding
  implicit none

  type(c_ptr) :: cpCharacterPtr
  character(len=256) :: sText
  character (c_char), pointer, dimension(:) :: fptr
  integer (c_int) :: iCount

    sText = repeat(" ", 256)

    call c_f_pointer(cpCharacterPtr, fptr, [256])
    iCount = 0
    do
      iCount = iCount + 1
      if( index(string=fptr(iCount), substring=c_null_char ) /= 0) exit
      sText(iCount:iCount) = fptr(iCount)

    enddo

end function char_ptr_to_fortran_string

!--------------------------------------------------------------------------------------------------

elemental function c_to_fortran_string( cCharacterString )  result(sText)

  use iso_c_binding
  implicit none

  character (len=*), intent(in) :: cCharacterString
  character(len=len(cCharacterString) - 1) :: sText
  integer (c_int) :: iIndex

  sText = ""

  iIndex = index( string=cCharacterString, substring=c_null_char )

  if(iIndex == 0) then

    sText = trim(adjustl(cCharacterString))

  else

    sText = cCharacterString(1:iIndex-1)

  endif

end function c_to_fortran_string

!--------------------------------------------------------------------------------------------------

elemental function fortran_to_c_string( sText )  result(cCharacterString)

  use iso_c_binding
  implicit none

  character (len=*), intent(in) :: sText
  character(len=256) :: cCharacterString
  integer (c_int) :: iIndex

  iIndex = index(string=sText, substring=c_null_char)

  if (iIndex == 0) then
    cCharacterString = trim(sText)//c_null_char
  else
    cCharacterString  = sText(1:iIndex)
  endif

end function fortran_to_c_string

  !> Strip everything except numeric characters from a text string.
  !!
  !! Keep only the numeric characters in a text string.
  !! @param[in] sTextIn
  impure elemental function keepnumeric(sText1)            result(sText)

    ! ARGUMENTS
    character (len=*), intent(in)           :: sText1
    character (len=len(sText1))             :: sText

    ! LOCALS
    character (len=512)            :: sTemp
    character (len=512)            :: sBuf
    integer (c_int)           :: iR                 ! Index in sRecord
    integer (c_int)           :: iIndex1, iIndex2
    character (len=:), allocatable :: sTargetCharacters_l

    ! TargetCharacter omits the period ("."): do not want a real value returned
    ! as a funky integer (e.g. string "3.141" returned as integer 3141 )
    sTargetCharacters_l = "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM" &
      //"!@#$%^&*()_+-={}[]|\:;'<,>?/~`'"//'"'

    ! eliminate any leading spaces
    sTemp = adjustl(sText1)
    sBuf = ""
    iIndex2 = 0

    do iIndex1 = 1,len_trim(sText1)

      iR = SCAN(sTemp(iIndex1:iIndex1), sTargetCharacters_l)

      if(iR==0) then
        iIndex2 = iIndex2 + 1
        sBuf(iIndex2:iIndex2) = sTemp(iIndex1:iIndex1)
      end if

    enddo

    sText = trim(sBuf)

  end function keepnumeric

  elemental function enforce_bounds_int_fn(value, minval, maxval)  result(retval)

    integer (c_int), intent(in)   :: value
    integer (c_int), intent(in)   :: minval
    integer (c_int), intent(in)   :: maxval
    integer (c_int)               :: retval

    retval = min( max( value, minval ), maxval)

  end function enforce_bounds_int_fn 

  elemental function enforce_bounds_sgl_fn(value, minval, maxval)  result(retval)

    real (c_float), intent(in)   :: value
    real (c_float), intent(in)   :: minval
    real (c_float), intent(in)   :: maxval
    real (c_float)               :: retval

    retval = min( max( value, minval ), maxval)

  end function enforce_bounds_sgl_fn 

  elemental function enforce_bounds_dbl_fn(value, minval, maxval)  result(retval)
  
    real (c_double), intent(in)   :: value
    real (c_double), intent(in)   :: minval
    real (c_double), intent(in)   :: maxval
    real (c_double)               :: retval
  
    retval = min( max( value, minval ), maxval)
  
  end function enforce_bounds_dbl_fn 

end module constants_and_conversions
