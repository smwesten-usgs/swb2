!> @file
!! Contains a single module, @ref constants_and_conversions, which contains useful
!! physical constants and basic conversion utilities

!> This module contains physical constants and convenience functions aimed at 
!! performing unit conversions. The functions included in this module perform
!! conversions between various temperature and distance units.
module constants_and_conversions

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_bool
  implicit none

  ! 
  integer (kind=c_int), parameter :: DATATYPE_INT     = 0
  integer (kind=c_int), parameter :: DATATYPE_FLOAT   = 1
  integer (kind=c_int), parameter :: DATATYPE_REAL    = 1
  integer (kind=c_int), parameter :: DATATYPE_SHORT   = 2
  integer (kind=c_int), parameter :: DATATYPE_DOUBLE  = 3
  integer (kind=c_int), parameter :: DATATYPE_NA      = -9999

  ! [ common mathematical constants ]
  public :: PI, TWOPI, HALFPI
  real (kind=c_double), parameter    :: PI = 3.1415926535897932384626433_c_double
  real (kind=c_double), parameter    :: TWOPI = 3.1415926535897932384626433_c_double * 2.0_c_double
  real (kind=c_double), parameter    :: HALFPI = 3.1415926535897932384626433_c_double / 2.0_c_double

  ! [ trig conversion factors ]
  public :: DEGREES_TO_RADIANS, RADIANS_TO_DEGREES
  real (kind=c_double), parameter    :: DEGREES_TO_RADIANS = TWOPI / 360.0_c_double
  real (kind=c_double), parameter    :: RADIANS_TO_DEGREES = 360.0_c_double / TWOPI
 
  ! [ common 'magic' numbers and logicals ]
  public :: lTRUE, lFALSE
  public :: rBIGVAL, iBIGVAL, dBIGVAL, iTINYVAL, rTINYVAL, dTINYVAL
  public :: rFREEZING, dFREEZING
  public :: rZERO
  logical (kind=c_bool), parameter   :: lTRUE = .true._c_bool
  logical (kind=c_bool), parameter   :: lFALSE = .false._c_bool
  real (kind=c_float), parameter     :: rBIGVAL = HUGE(0.0_c_float)
  real (kind=c_double), parameter    :: dBIGVAL = HUGE(0.0_c_double)
  integer(kind=c_int), parameter     :: iBIGVAL = HUGE(0_c_int)
  real (kind=c_float), parameter     :: rTINYVAL = -(HUGE(0.0_c_float) - 1.0)
  real (kind=c_double), parameter    :: dTINYVAL = -(HUGE(0.0_c_double) - 1.0)  
  real (kind=c_float), parameter     :: fTINYVAL = -(HUGE(0.0_c_float) - 1.0)
  integer(kind=c_int), parameter     :: iTINYVAL = -(HUGE(0_c_int) - 1)
  real (kind=c_float), parameter     :: rFREEZING = 32.0_c_float
  real (kind=c_float), parameter     :: fFREEZING = 32.0_c_float
  real (kind=c_double), parameter    :: dFREEZING = 32.0_c_double
  integer (kind=c_int), parameter    :: iZERO = 0_c_int
  real (kind=c_float), parameter     :: fZERO = 0.0_c_float
  real (kind=c_float), parameter     :: rZERO = 0.0_c_float
  real (kind=c_double), parameter    :: dZERO = 0.0_c_double


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
  real (kind=c_double), parameter :: C_PER_F    = 5.0_c_double / 9.0_c_double
  real (kind=c_double), parameter :: F_PER_C    = 9.0_c_double / 5.0_c_double  
  real (kind=c_double), parameter :: M_PER_FOOT = 0.3048_c_double 


! this type is still needed for the grid module
type T_CELL
      integer (kind=c_int) :: iFlowDir = iZERO    ! Flow direction from flow-dir grid
      integer (kind=c_int) :: iSoilGroup = iZERO  ! Soil type from soil-type grid
      integer (kind=c_int) :: iLandUseIndex       ! Index (row num) of land use table
      integer (kind=c_int) :: iLandUse = iZERO    ! Land use from land-use grid
      integer (kind=c_int) :: iIrrigationTableIndex = iZERO  ! Index (row num) of irrigation table
      real (kind=c_float) :: rElevation =rZERO            ! Ground elevation
      real (kind=c_float) :: rSoilWaterCapInput = rZERO   ! Soil water capacity from grid file
      real (kind=c_float) :: rSoilWaterCap = rZERO        ! Soil water capacity adjusted for LU/LC
      real (kind=c_float) :: rSoilMoisture = rZERO        ! Soil moisture in inches of water
      real (kind=c_float) :: rCurrentRootingDepth = 0.2   ! Current rooting depth for use w FAO56 calculations
      real (kind=c_float) :: rKcb = rZERO                 ! crop coefficient for this cell
      real (kind=c_float) :: rTotalAvailableWater = rZERO
      real (kind=c_float) :: rReadilyAvailableWater = rZERO

  end type T_CELL

  type BOUNDS_T
    character (len=:), allocatable  :: sPROJ4_string
    integer (kind=c_int)            :: iNumCols
    integer (kind=c_int)            :: iNumRows
    real (kind=c_double)            :: fX_ll, fY_ll
    real (kind=c_double)            :: fX_ur, fY_ur
    real (kind=c_double)            :: fGridcellSize
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
  end interface asFloat

  public :: asDouble
  interface asDouble
    module procedure char2dbl
    module procedure short2dbl
    module procedure int2dbl
    module procedure real2dbl
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

  public :: char_ptr_to_fortran_string
  public :: c_to_fortran_string
  public :: fortran_to_c_string

  real (kind=c_float), parameter  :: TOLERANCE_FLOAT = 1.0e-6_c_float
  real (kind=c_double), parameter :: TOLERANCE_DOUBLE = 1.0e-9_c_double

contains


  elemental function approx_equal_float_float(fValue1, fValue2)  result(lBool)

    real (kind=c_float), intent(in)    :: fValue1
    real (kind=c_float), intent(in)    :: fValue2
    logical (kind=c_bool)              :: lBool

    if ( abs( fValue1 - fValue2 ) < TOLERANCE_FLOAT ) then
      lBool = lTRUE
    else
      lBool = lFALSE
    endif

  end function approx_equal_float_float




  elemental function approx_equal_float_double(fValue1, fValue2)  result(lBool)

    real (kind=c_float), intent(in)    :: fValue1
    real (kind=c_double), intent(in)    :: fValue2
    logical (kind=c_bool)              :: lBool

    if ( abs( fValue1 - real(fValue2, kind=c_float) ) < TOLERANCE_FLOAT ) then
      lBool = lTRUE
    else
      lBool = lFALSE
    endif

  end function approx_equal_float_double





  elemental function approx_equal_double_double(fValue1, fValue2)  result(lBool)

    real (kind=c_double), intent(in)    :: fValue1
    real (kind=c_double), intent(in)    :: fValue2
    logical (kind=c_bool)               :: lBool

    if ( abs( fValue1 - fValue2 ) < TOLERANCE_DOUBLE ) then
      lBool = lTRUE
    else
      lBool = lFALSE
    endif

  end function approx_equal_double_double


!--------------------------------------------------------------------------

  !> Convert degrees to radians.
  !! @param[in]  degrees   Angle in degrees.
  !! @retval     radians  Angle in radians.

  elemental function deg_to_rad_sgl_fn( degrees )    result( radians )

    real (kind=c_float), intent(in)    :: degrees
    real (kind=c_float)                :: radians

    radians = degrees * DEGREES_TO_RADIANS

  end function deg_to_rad_sgl_fn    


!--------------------------------------------------------------------------------------------------

  !> Convert degrees to radians.
  !! @param[in]  degrees   Angle in degrees.
  !! @retval     radians   Angle in radians.

  elemental function deg_to_rad_dbl_fn(degrees)    result(radians)

    real (kind=c_double), intent(in)    :: degrees
    real (kind=c_double)                :: radians

    radians = degrees * DEGREES_TO_RADIANS

  end function deg_to_rad_dbl_fn    


!--------------------------------------------------------------------------------------------------

  !> Convert radians to degrees.
  !! @param[in]  radians   Angle in radians.
  !! @retval     degrees   Angle in degrees.

  elemental function rad_to_deg_sgl_fn(radians)    result(degrees)

    real (kind=c_float), intent(in)    :: radians
    real (kind=c_float)                :: degrees

    degrees = radians * RADIANS_TO_DEGREES

  end function rad_to_deg_sgl_fn    

!--------------------------------------------------------------------------------------------------

  !> Convert radians to degrees.
  !! @param[in]  radians   Angle in radians.
  !! @retval     degrees   Angle in degrees.

  elemental function rad_to_deg_dbl_fn(radians)    result(degrees)

    real (kind=c_double), intent(in)    :: radians
    real (kind=c_double)                :: degrees

    degrees = radians * RADIANS_TO_DEGREES

  end function rad_to_deg_dbl_fn    


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Fahrenheit to degrees Celsius.
  !! @param[in]  degrees_F   Temperature in degrees Fahrenheit.
  !! @retval     degrees_C   Temperature in degrees Celcius.

  elemental function FtoC_sgl_fn(degrees_F)   result(degrees_C)
 
    real (kind=c_float),intent(in) :: degrees_F
    real (kind=c_float) :: degrees_C

    degrees_C = (degrees_F - rFREEZING) * C_PER_F

  end function FtoC_sgl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Fahrenheit to degrees Celsius.
  !! @param[in] degrees_F Temperature in degrees Fahrenheit.
  !! @retval degrees_C Temperature in degrees Celcius.

  elemental function FtoC_dbl_fn( degrees_F )   result( degrees_C )
   
    real (kind=c_double),intent(in) :: degrees_F
    real (kind=c_double) :: degrees_C

    degrees_C = ( degrees_F - dFREEZING ) * C_PER_F

  end function FtoC_dbl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Celsius to degrees Fahrenheit.
  !! @param[in] degrees_C Temperature in degrees Celsius.
  !! @retval degrees_F Temperature in degrees Fahrenheit.

  elemental function CtoF_sgl_fn( degrees_C )   result( degrees_F )

    real (kind=c_float),intent(in) :: degrees_C
    real (kind=c_float) :: degrees_F

    degrees_F = degrees_C * F_PER_C + dFREEZING

  end function CtoF_sgl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Celsius to degrees Fahrenheit.
  !! @param[in] degrees_C Temperature in degrees Celsius.
  !! @retval degrees_F Temperature in degree

  elemental function CtoF_dbl_fn( degrees_C )   result( degrees_F )

    real (kind=c_double),intent(in) :: degrees_C
    real (kind=c_double) :: degrees_F

    degrees_F = degrees_C * F_PER_C + dFREEZING

  end function CtoF_dbl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Fahrenheit to Kelvins.
  !! @param[in] degrees_F Temperature in degrees Fahrenheit.
  !! @retval degrees_K Temperature in Kelvins.

  elemental function FtoK_sgl_fn( degrees_F )    result( degrees_K )

    real (kind=c_float),intent(in) :: degrees_F
    real (kind=c_float) :: degrees_K

    degrees_K = (degrees_F - dFREEZING) * C_PER_F + 273.15_c_double

  end function FtoK_sgl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Fahrenheit to Kelvins.
  !! @param[in] degrees_F Temperature in degrees Fahrenheit.
  !! @retval degrees_K Temperature in Kelvins.

  elemental function FtoK_dbl_fn( degrees_F )    result( degrees_K )

    real (kind=c_double),intent(in) :: degrees_F
    real (kind=c_double) :: degrees_K

    degrees_K = (degrees_F - dFREEZING) * C_PER_F + 273.15_c_double

  end function FtoK_dbl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Celsius to Kelvins.
  !! @param[in] degrees_C Temperature in degrees Celcius.
  !! @retval degrees_K Temperature in Kelvins.

  elemental function CtoK_sgl_fn( degrees_C )    result( degrees_K )

    real (kind=c_float), intent(in) :: degrees_C
    real (kind=c_float) :: degrees_K

    degrees_K = degrees_C + 273.15_c_double

  end function CtoK_sgl_fn


!--------------------------------------------------------------------------------------------------

  !> Convert degrees Fahrenheit to Kelvins.
  !! @param[in] degrees_C Temperature in degrees Celsius.
  !! @retval degrees_K Temperature in Kelvins.

  elemental function CtoK_dbl_fn( degrees_C )    result( degrees_K )

    real (kind=c_double), intent(in) :: degrees_C
    real (kind=c_double) :: degrees_K

    degrees_K = degrees_C + 273.15_c_double

  end function CtoK_dbl_fn


!--------------------------------------------------------------------------------------------------

  !>  Convert millimeters to inches.
  !! @param[in] mm Value in millimeters.
  !! @retval inches Value in inches.

  elemental function mm_to_inches_sgl_fn(mm) result(inches)

    real (kind=c_float),intent(in) :: mm
    real (kind=c_float) :: inches

    inches = mm / 25.4_c_double

  end function mm_to_inches_sgl_fn

!--------------------------------------------------------------------------------------------------

  !>  Convert millimeters to inches.
  !! @param[in] mm Value in millimeters.
  !! @retval inches Value in inches.

  elemental function mm_to_inches_dbl_fn( mm ) result( inches )

    real (kind=c_double),intent(in) :: mm
    real (kind=c_double) :: inches

    inches = mm / 25.4_c_double

  end function mm_to_inches_dbl_fn

!--------------------------------------------------------------------------------------------------

  !> Convert a short integer to a logical value

  elemental function short2logical(iShortVal)    result(lValue)

    integer (kind=c_short), intent(in)    :: iShortVal
    logical (kind=c_bool)                 :: lValue

    if ( iShortVal == 0 ) then
      lValue = lFALSE
    else
      lValue = lTRUE
    endif

  end function short2logical  

!--------------------------------------------------------------------------------------------------

  !> Convert an integer to a logical value

  elemental function int2logical(iValue)    result(lValue)

    integer (kind=c_int), intent(in)      :: iValue
    logical (kind=c_bool)                 :: lValue

    if ( iValue == 0 ) then
      lValue = lFALSE
    else
      lValue = lTRUE
    endif

  end function int2logical  

!--------------------------------------------------------------------------------------------------

  !> Convert a real to a logical value

  elemental function real2logical(rValue)    result(lValue)

    real (kind=c_float), intent(in)      :: rValue
    logical (kind=c_bool)                :: lValue

    ! [ LOCALS ]
    real (kind=c_float), parameter :: fMinResolution = 2.0 * spacing(1.0_c_float)

    if ( rValue > -fMinResolution .and. rValue < fMinResolution ) then
      lValue = lFALSE
    else
      lValue = lTRUE
    endif

  end function real2logical  

!--------------------------------------------------------------------------------------------------

  !> Convert a double to a logical value

  elemental function dbl2logical(rValue)    result(lValue)

    real (kind=c_double), intent(in)      :: rValue
    logical (kind=c_bool)                 :: lValue

    ! [ LOCALS ]
    real (kind=c_double), parameter :: dMinResolution = 2.0 * spacing(1.0_c_float)

    if ( rValue > -dMinResolution .and. rValue < dMinResolution ) then
      lValue = lFALSE
    else
      lValue = lTRUE
    endif

  end function dbl2logical  

!--------------------------------------------------------------------------------------------------

  !> Convert a character string to a logical value

  elemental function char2logical(sValue)    result(lValue)

    character (len=*), intent(in)      :: sValue
    logical (kind=c_bool)              :: lValue

    select case ( sValue )

      case ( "TRUE", "True", "true", "T", "YES", "Yes", "yes", "1" )

        lValue = lTRUE

      case default

        lValue = lFALSE

    end select    

  end function char2logical  

!--------------------------------------------------------------------------------------------------

  !> Convert a short integer to an integer

  elemental function short2int(iShortVal)    result(iValue)

    integer (kind=c_short), intent(in)    :: iShortVal
    integer (kind=c_int)                  :: iValue

    iValue = int( iShortVal, kind=c_int )

  end function short2int  

!--------------------------------------------------------------------------------------------------

  !> Convert a character value into a integer

  elemental function char2int(sValue)  result(iValue)

    character (len=*), intent(in) :: sValue
    integer (kind=c_int) :: iValue

    ! [ LOCALS ]
    integer (kind=c_int) :: iStat
    character (len=:), allocatable :: sTempVal 
    real (kind=c_float)  :: rValue

    sTempVal = keepnumeric(sValue)  

    ! if the cleaned up string appears to be a real value, 
    ! attempt to read as real and convert to int
    if ( scan(sTempVal, ".") /= 0 ) then

      read(unit=sTempVal, fmt=*, iostat=iStat) rValue

      if (iStat == 0)  iValue = int(rValue, kind=c_int)

    else  

      read(unit=sTempVal, fmt=*, iostat=iStat) iValue

    endif

    if (iStat /= 0)  iValue = iTINYVAL

  end function char2int

!--------------------------------------------------------------------------------------------------

!> Convert a real value into a integer

elemental function real2int(rValue)  result(iValue)

  real (kind=c_float), intent(in) :: rValue
  integer (kind=c_int) :: iValue

  iValue = int(rValue, kind=c_int)

end function real2int

!--------------------------------------------------------------------------------------------------

!> Convert a double-precision value to an integer

elemental function dbl2int(rValue)  result(iValue)

  real (kind=c_double), intent(in) :: rValue
  integer (kind=c_int) :: iValue

  iValue = int(rValue, kind=c_int)

end function dbl2int

!--------------------------------------------------------------------------------------------------

!> Convert a character value into a real

elemental function char2real(sValue)  result(rValue)

  character (len=*), intent(in) :: sValue
  real (kind=c_float) :: rValue

  ! [ LOCALS ]
  integer (kind=c_int) :: iStat

  read(unit=sValue, fmt=*, iostat=iStat) rValue

  if (iStat /= 0) rValue = rTINYVAL

end function char2real

!--------------------------------------------------------------------------------------------------

!> Convert a short int value into a real

elemental function short2real(iValue)  result(rValue)

  integer (kind=c_short), intent(in) :: iValue
  real (kind=c_float)                :: rValue

  rValue = real(iValue, kind=c_float)

end function short2real

!--------------------------------------------------------------------------------------------------

!> Convert an int value into a real

elemental function int2real(iValue)  result(rValue)

  integer (kind=c_int), intent(in) :: iValue
  real (kind=c_float) :: rValue

  rValue = real(iValue, kind=c_float)

end function int2real

!--------------------------------------------------------------------------------------------------

!> Convert a dbl value into a real

elemental function dbl2real(dpValue)  result(rValue)

  real (kind=c_double), intent(in) :: dpValue
  real (kind=c_float) :: rValue

  rValue = real(dpValue, kind=c_float)

end function dbl2real

!--------------------------------------------------------------------------------------------------

!> Convert a character value into a double

elemental function char2dbl(sValue)  result(dValue)

  character (len=*), intent(in)    :: sValue
  real (kind=c_double) :: dValue

  ! [ LOCALS ]
  integer (kind=c_int) :: iStat

  read(unit=sValue, fmt=*, iostat=iStat) dValue

  if (iStat /= 0)  dValue = dTINYVAL

end function char2dbl

!--------------------------------------------------------------------------------------------------

!> Convert a short int value into a double

elemental function short2dbl(iValue)  result(dValue)

  integer (kind=c_short), intent(in) :: iValue
  real (kind=c_double)               :: dValue

  dValue = real(iValue, kind=c_double)

end function short2dbl

!--------------------------------------------------------------------------------------------------

!> Convert an int value into a double

elemental function int2dbl(iValue)  result(dValue)

  integer (kind=c_int), intent(in) :: iValue
  real (kind=c_double) :: dValue

  dValue = real(iValue, kind=c_double)

end function int2dbl

!--------------------------------------------------------------------------------------------------

!> Convert a real value into a double

elemental function real2dbl(fValue)  result(dValue)

  real (kind=c_float), intent(in) :: fValue
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

  !> Strip everything except numeric characters from a text string.
  !!
  !! Keep only the numeric characters in a text string.
  !! @param[in] sTextIn
  elemental function keepnumeric(sText1)            result(sText)

    ! ARGUMENTS
    character (len=*), intent(in)           :: sText1
    character (len=:), allocatable          :: sText

    ! LOCALS
    character (len=:), allocatable :: sTemp
    character (len=512)            :: sBuf
    integer (kind=c_int)           :: iR                 ! Index in sRecord
    integer (kind=c_int)           :: iIndex1, iIndex2
    character (len=:), allocatable :: sTargetCharacters_

    ! TargetCharacter omits the period ("."): don't want a real value returned
    ! as a funky integer (e.g. string "3.141" returned as integer 3141 )
    sTargetCharacters_ = "qwertyuiopasdfghjklzxcvbnmQWERTYUIOPASDFGHJKLZXCVBNM" &
      //"!@#$%^&*()_+-={}[]|\:;'<,>?/~`'"//sDOUBLE_QUOTE

    ! eliminate any leading spaces
    sTemp = adjustl(sText1)
    sBuf = ""
    iIndex2 = 0

    do iIndex1 = 1,len_trim(sText1)

      iR = SCAN(sTemp(iIndex1:iIndex1), sTargetCharacters_)
  
      if(iR==0) then
        iIndex2 = iIndex2 + 1
        sBuf(iIndex2:iIndex2) = sTemp(iIndex1:iIndex1)
      end if

    enddo

    sText = trim(sBuf)

  end function keepnumeric


end module constants_and_conversions
