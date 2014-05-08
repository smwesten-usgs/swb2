!> @file
!! Contains a single module, @ref constants_and_conversions, which contains useful
!! physical constants and basic conversion utilities

!> This module contains physical constants and convenience functions aimed at 
!! performing unit conversions. The functions included in this module perform
!! conversions between various temperature and distance units.
module constants_and_conversions

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  implicit none

  ! [ common mathematical constants ]
  public :: PI, TWOPI, HALFPI
  real (kind=c_double), parameter    :: PI = 3.1415926535897932384626433_c_double
  real (kind=c_double), parameter    :: TWOPI = 3.1415926535897932384626433_c_double * 2_c_double
  real (kind=c_double), parameter    :: HALFPI = 3.1415926535897932384626433_c_double / 2_c_double

  ! [ trig conversion factors ]
  public :: DEGREES_TO_RADIANS, RADIANS_TO_DEGREES
  real (kind=c_double), parameter    :: DEGREES_TO_RADIANS = TWOPI / 360_c_double
  real (kind=c_double), parameter    :: RADIANS_TO_DEGREES = 360_c_double / TWOPI
 
  ! [ common 'magic' numbers and logicals ]
  public :: lTRUE, lFALSE
  public :: rBIGVAL, iBIGVAL, dBIGVAL, iTINYVAL, rTINYVAL, dTINYVAL
  public :: rFREEZING, dFREEZING
  public :: rZERO
  logical (kind=c_bool), parameter   :: lTRUE = .true._c_bool
  logical (kind=c_bool), parameter   :: lFALSE = .false._c_bool
  real (kind=c_float), parameter     :: rBIGVAL = HUGE(0_c_float)
  real (kind=c_double), parameter    :: dBIGVAL = HUGE(0_c_double)
  integer(kind=c_int), parameter     :: iBIGVAL = HUGE(0_c_int)
  real (kind=c_float), parameter     :: rTINYVAL = -(HUGE(0_c_float) - 1)
  real (kind=c_double), parameter    :: dTINYVAL = -(HUGE(0_c_double) - 1)  
  integer(kind=c_int), parameter     :: iTINYVAL = -(HUGE(0_c_int) - 1)
  real (kind=c_float), parameter     :: rFREEZING = 32_c_float
  real (kind=c_double), parameter    :: dFREEZING = 32_c_double
  real (kind=c_float), parameter  :: fZERO = 0.0_c_float
  real (kind=c_float), parameter  :: rZERO = 0.0_c_float
  real (kind=c_double), parameter :: dZERO = 0.0_c_double

  ! [ special ASCII characters ]
  public :: sTAB, sWHITESPACE, sBACKSLASH, sFORWARDSLASH, sRETURN, sCOMMENT_CHARS
  character (len=1), parameter :: sTAB = achar(9)
  character (len=2), parameter :: sWHITESPACE = achar(9)//" "
  character (len=1), parameter :: sBACKSLASH = achar(92)
  character (len=1), parameter :: sFORWARDSLASH = achar(47)
  character (len=1), parameter :: sRETURN = achar(13)
  character (len=3), parameter :: sCOMMENT_CHARS = "#!%"
  character (len=1), parameter :: sDOUBLE_QUOTE = achar(34)

  ! [ select conversion factors ]
  real (kind=c_double), parameter :: C_PER_F = 5_c_double / 9_c_double
  real (kind=c_double), parameter :: F_PER_C = 9_c_double / 5_c_double  

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
    module procedure int2real
    module procedure dbl2real
  end interface asFloat

  public :: asDouble
  interface asDouble
    module procedure char2dbl
    module procedure int2dbl
    module procedure real2dbl
  end interface asDouble

  public asInt
  interface asInt
    module procedure char2int
    module procedure real2int
    module procedure dbl2int
  end interface asInt

  public :: mm_to_in
  interface mm_to_in
    module procedure mm_to_inches_sgl_fn
    module procedure mm_to_inches_dbl_fn
  end interface mm_to_in    

  public :: char_ptr_to_fortran_string
  public :: c_to_fortran_string
  public :: fortran_to_c_string

contains

!--------------------------------------------------------------------------

  !> Convert degrees to radians.
  !! @param[in]  degrees   Angle in degrees.
  !! @retval     radians  Angle in radians.
  function deg_to_rad_sgl_fn( degrees )    result( radians )

    real (kind=c_float), intent(in)    :: degrees
    real (kind=c_float)                :: radians

    radians = degrees * DEGREES_TO_RADIANS

  end function deg_to_rad_sgl_fn    


!--------------------------------------------------------------------------------------------------

  !> Convert degrees to radians.
  !! @param[in]  degrees   Angle in degrees.
  !! @retval     radians   Angle in radians.
  function deg_to_rad_dbl_fn(degrees)    result(radians)

    real (kind=c_double), intent(in)    :: degrees
    real (kind=c_double)                :: radians

    radians = degrees * DEGREES_TO_RADIANS

  end function deg_to_rad_dbl_fn    


!--------------------------------------------------------------------------------------------------

  !> Convert radians to degrees.
  !! @param[in]  radians   Angle in radians.
  !! @retval     degrees   Angle in degrees.
  function rad_to_deg_sgl_fn(radians)    result(degrees)

    real (kind=c_float), intent(in)    :: radians
    real (kind=c_float)                :: degrees

    degrees = radians * RADIANS_TO_DEGREES

  end function rad_to_deg_sgl_fn    

!--------------------------------------------------------------------------------------------------

  !> Convert radians to degrees.
  !! @param[in]  radians   Angle in radians.
  !! @retval     degrees   Angle in degrees.
  function rad_to_deg_dbl_fn(radians)    result(degrees)

    real (kind=c_double), intent(in)    :: radians
    real (kind=c_double)                :: degrees

    degrees = radians * RADIANS_TO_DEGREES

  end function rad_to_deg_dbl_fn    


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Fahrenheit to degrees Celsius.
  !! @param[in]  degrees_F   Temperature in degrees Fahrenheit.
  !! @retval     degrees_C   Temperature in degrees Celcius.
  function FtoC_sgl_fn(degrees_F)   result(degrees_C)
 
    real (kind=c_float),intent(in) :: degrees_F
    real (kind=c_float) :: degrees_C

    degrees_C = (degrees_F - rFREEZING) * C_PER_F

  end function FtoC_sgl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Fahrenheit to degrees Celsius.
  !! @param[in] degrees_F Temperature in degrees Fahrenheit.
  !! @retval degrees_C Temperature in degrees Celcius.
  function FtoC_dbl_fn( degrees_F )   result( degrees_C )
   
    real (kind=c_double),intent(in) :: degrees_F
    real (kind=c_double) :: degrees_C

    degrees_C = ( degrees_F - dFREEZING ) * C_PER_F

  end function FtoC_dbl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Celsius to degrees Fahrenheit.
  !! @param[in] degrees_C Temperature in degrees Celsius.
  !! @retval degrees_F Temperature in degrees Fahrenheit.
  function CtoF_sgl_fn( degrees_C )   result( degrees_F )

    real (kind=c_float),intent(in) :: degrees_C
    real (kind=c_float) :: degrees_F

    degrees_F = degrees_C * F_PER_C + dFREEZING

  end function CtoF_sgl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Celsius to degrees Fahrenheit.
  !! @param[in] degrees_C Temperature in degrees Celsius.
  !! @retval degrees_F Temperature in degree
  function CtoF_dbl_fn( degrees_C )   result( degrees_F )

    real (kind=c_double),intent(in) :: degrees_C
    real (kind=c_double) :: degrees_F

    degrees_F = degrees_C * F_PER_C + dFREEZING

  end function CtoF_dbl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Fahrenheit to Kelvins.
  !! @param[in] degrees_F Temperature in degrees Fahrenheit.
  !! @retval degrees_K Temperature in Kelvins.
  function FtoK_sgl_fn( degrees_F )    result( degrees_K )

    real (kind=c_float),intent(in) :: degrees_F
    real (kind=c_float) :: degrees_K

    degrees_K = (degrees_F - dFREEZING) * C_PER_F + 273.15_c_double

  end function FtoK_sgl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Fahrenheit to Kelvins.
  !! @param[in] degrees_F Temperature in degrees Fahrenheit.
  !! @retval degrees_K Temperature in Kelvins.
  function FtoK_dbl_fn( degrees_F )    result( degrees_K )

    real (kind=c_double),intent(in) :: degrees_F
    real (kind=c_double) :: degrees_K

    degrees_K = (degrees_F - dFREEZING) * C_PER_F + 273.15_c_double

  end function FtoK_dbl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Celsius to Kelvins.
  !! @param[in] degrees_C Temperature in degrees Celcius.
  !! @retval degrees_K Temperature in Kelvins.
  function CtoK_sgl_fn( degrees_C )    result( degrees_K )

    real (kind=c_float), intent(in) :: degrees_C
    real (kind=c_float) :: degrees_K

    degrees_K = degrees_C + 273.15_c_double

  end function CtoK_sgl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Fahrenheit to Kelvins.
  !! @param[in] degrees_C Temperature in degrees Celsius.
  !! @retval degrees_K Temperature in Kelvins.
  function CtoK_dbl_fn( degrees_C )    result( degrees_K )

    real (kind=c_double), intent(in) :: degrees_C
    real (kind=c_double) :: degrees_K

    degrees_K = degrees_C + 273.15_c_double

  end function CtoK_dbl_fn


!--------------------------------------------------------------------------------------------------

  !>  Convert millimeters to inches.
  !! @param[in] mm Value in millimeters.
  !! @retval inches Value in inches.
  function mm_to_inches_sgl_fn(r_mm) result(r_in)

    real (kind=c_float),intent(in) :: r_mm
    real (kind=c_float) :: r_in

    r_in = r_mm / 25.4_c_double

  end function mm_to_inches_sgl_fn

!--------------------------------------------------------------------------------------------------

  !>  Convert millimeters to inches.
  !! @param[in] mm Value in millimeters.
  !! @retval inches Value in inches.
  function mm_to_inches_dbl_fn( mm ) result( inches )

    real (kind=c_double),intent(in) :: mm
    real (kind=c_double) :: inches

    inches = mm / 25.4_c_double

  end function mm_to_inches_dbl_fn

!--------------------------------------------------------------------------------------------------

!> Convert a character value into a integer
function char2int(sValue)  result(iValue)

  character (len=*) :: sValue
  integer (kind=c_int) :: iValue

  ! [ LOCALS ]
  integer (kind=c_int) :: iStat

  read(UNIT=sValue,FMT=*, iostat=iStat) iValue

  if (iStat /= 0)  iValue = iTINYVAL

end function char2int

!--------------------------------------------------------------------------------------------------

!> Convert a real value into a integer
function real2int(rValue)  result(iValue)

  real (kind=c_float) :: rValue
  integer (kind=c_int) :: iValue

  iValue = int(rValue, kind=c_int)

end function real2int

!--------------------------------------------------------------------------------------------------

!> Convert a double-precision value to an integer
function dbl2int(rValue)  result(iValue)

  real (kind=c_double) :: rValue
  integer (kind=c_int) :: iValue

  iValue = int(rValue, kind=c_int)

end function dbl2int

!--------------------------------------------------------------------------------------------------

!> Convert a character value into a real
function char2real(sValue)  result(rValue)

  character (len=*) :: sValue
  real (kind=c_float) :: rValue

  read(UNIT=sValue,FMT=*) rValue

end function char2real

!--------------------------------------------------------------------------------------------------

!> Convert an int value into a real
function int2real(iValue)  result(rValue)

  integer (kind=c_int) :: iValue
  real (kind=c_float) :: rValue

  rValue = real(iValue, kind=c_float)

end function int2real

!--------------------------------------------------------------------------------------------------

!> Convert a dbl value into a real
function dbl2real(dpValue)  result(rValue)

  real (kind=c_double) :: dpValue
  real (kind=c_float) :: rValue

  rValue = real(dpValue, kind=c_float)

end function dbl2real

!--------------------------------------------------------------------------------------------------

!> Convert a character value into a double
function char2dbl(sValue)  result(dValue)

  character (len=*)    :: sValue
  real (kind=c_double) :: dValue

  read(UNIT=sValue,FMT=*) dValue

end function char2dbl

!--------------------------------------------------------------------------------------------------

!> Convert an int value into a double
function int2dbl(iValue)  result(dValue)

  integer (kind=c_int) :: iValue
  real (kind=c_double) :: dValue

  dValue = real(iValue, kind=c_double)

end function int2dbl

!--------------------------------------------------------------------------------------------------

!> Convert a real value into a double
function real2dbl(fValue)  result(dValue)

  real (kind=c_float) :: fValue
  real (kind=c_double) :: dValue

  dValue = real(fValue, kind=c_double)

end function real2dbl

!--------------------------------------------------------------------------------------------------

function char_ptr_to_fortran_string( cpCharacterPtr )  result(sText)

  use iso_c_binding
  implicit none

  type(c_ptr) :: cpCharacterPtr
  character(len=256) :: sText
  character (kind=c_char), pointer, dimension(:) :: fptr
  integer (kind=c_int) :: iCount

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
  character(len=256) :: sText
  integer (kind=c_int) :: iIndex

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
  integer (kind=c_int) :: iIndex

  iIndex = index(string=sText, substring=c_null_char)

  if (iIndex == 0) then
    cCharacterString = trim(sText)//c_null_char
  else
    cCharacterString  = sText(1:iIndex)
  endif

end function fortran_to_c_string


end module constants_and_conversions
