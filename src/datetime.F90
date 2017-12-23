!> @file
!!  Contains Fortran module @ref datetime, which
!! @ref DATETIME_T class and associated time and date-related routines.

!> This module contains the @ref DATETIME_T class and associated
!! time and date-related routines, along with the @ref MONTH_T class which
!! defines month names and three-letter abbreviations.

module datetime

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_bool
  use strings
  use exceptions
  use constants_and_conversions

  implicit none
  private

  public :: gregorian_date, julian_day, isLeap, day_of_year, mmdd2doy

  type, public :: DATETIME_T

    integer (kind=c_short)  :: iMonth = 1
    integer (kind=c_short)  :: iDay = 1
    integer (kind=c_int)    :: iYear = 1
    integer (kind=c_short)  :: iHour = 0
    integer (kind=c_short)  :: iMinute = 0
    integer (kind=c_short)  :: iSecond = 0
    integer (kind=c_int)    :: iWaterYearHigh = 0
    integer (kind=c_int)    :: iWaterYearLow = 0
    real (kind=c_double)    :: dJulianDate = 0.0_c_double

  contains

    procedure  :: calcJulianDay => calc_julian_day_sub
    procedure  :: dayspermonth => get_days_in_month_fn
    procedure  :: daysperyear => get_days_in_year_fn
    procedure  :: calcGregorianDate => calc_gregorian_date_sub
    procedure  :: calcWaterYear => calc_water_year_sub
    procedure  :: parseDate => parse_text_to_date_sub
    procedure  :: parseTime => parse_text_to_time_sub
    procedure  :: isLeapYear => is_leap_year_fn

    procedure  :: setTimeFormat => set_time_format_indices
    procedure  :: setDateFormat => set_Date_format_indices

    procedure :: is_date_greater_than
    !> ">" operator for comparing two date objects; see @ref is_date_greater_than for implementation details.
    generic   :: operator( > ) => is_date_greater_than

    procedure :: is_date_less_than
    !> "<" operator for comparing two date objects
    generic   :: operator( < ) => is_date_less_than

    procedure :: is_date_GT_or_equal_to
    !> ">=" operator for comparing two date objects
    generic   :: operator( >= ) => is_date_GT_or_equal_to

    procedure :: is_date_LT_or_equal_to
    !> "<=" operator for comparing two date objects
    generic   :: operator( <= ) => is_date_LT_or_equal_to

    procedure :: is_date_equal_to
    !> "==" operator for comparing two date objects
    generic   :: operator( == ) => is_date_equal_to

    procedure :: date_minus_date_fn
    procedure :: date_minus_float_fn
    procedure :: date_minus_int_fn
    !> "-" operator for subtracting two date objects
    generic   :: operator( - ) => date_minus_date_fn,   &
                                  date_minus_float_fn,  &
                                  date_minus_int_fn

    procedure :: date_plus_float_fn
    generic   :: operator( + ) => date_plus_float_fn

    procedure  :: setDay => date_set_day_sub
    procedure  :: setMonth => date_set_month_sub
    procedure  :: setYear => date_set_year_sub
    procedure  :: addDay => date_plus_day_sub
    procedure  :: subtractDay => date_minus_day_sub
    procedure  :: addYear => date_plus_year_sub
    procedure  :: subtractYear => date_minus_year_sub
    procedure  :: prettydate => write_pretty_date_fn
    procedure  :: prettydatetime => write_pretty_datetime_fn
    procedure  :: listdatetime => write_list_datetime_fn
    procedure  :: listdate => write_list_date_fn
    procedure  :: listtime => write_list_time_fn
    procedure  :: systime => system_time_to_date_sub
    procedure  :: getDayOfYear => get_day_of_year_fn
    procedure  :: getJulianDay => get_julian_day_fn
    procedure  :: setJulianDate => set_julian_date_sub
    procedure  :: getFractionOfDay => get_fraction_of_day_fn

  end type DATETIME_T


  ! the following values are determined by the date format string; defaults to MM/DD/YYYY
  character (len=14), private :: sDATE_FORMAT = "MM/DD/YYYY"
  character (len=14), public  :: sDEFAULT_DATE_FORMAT = "MM/DD/YYYY"
  integer (kind=c_int), private :: iScanMM1 = 1
  integer (kind=c_int), private :: iScanMM2 = 2
  integer (kind=c_int), private :: iScanDelim1 = 3
  integer (kind=c_int), private :: iScanDD1 = 4
  integer (kind=c_int), private :: iScanDD2 = 5
  integer (kind=c_int), private :: iScanDelim2 = 6
  integer (kind=c_int), private :: iScanYYYY1 = 7
  integer (kind=c_int), private :: iScanYYYY2 = 10

  character (len=14), private :: sTIME_FORMAT = "HH:MM:SS"
  character (len=14), public  :: sDEFAULT_TIME_FORMAT = "HH:MM:SS"
  integer (kind=c_int), private :: iScanHour1 = 1
  integer (kind=c_int), private :: iScanHour2 = 2
  integer (kind=c_int), private :: iScanMin1 = 4
  integer (kind=c_int), private :: iScanMin2 = 5
  integer (kind=c_int), private :: iScanSec1 = 7
  integer (kind=c_int), private :: iScanSec2 = 8

  !> Container for month name and length information

  type MONTH_T
    character (len=3) :: sName          ! Abbreviated name
    character (len=9) :: sFullName      ! Full month name
    integer (kind=c_int) :: iStart      ! Starting (Julian) date
    integer (kind=c_int) :: iEnd        ! Ending (Julian) date
    integer (kind=c_int) :: iMonth      ! Month number (1-12)
    integer (kind=c_int) :: iNumDays    ! Max number of days in month
  end type MONTH_T

  !> Month information

  type ( MONTH_T ), public, target :: MONTHS(12) =     &
   [  MONTH_T( 'Jan','January  ',   1,  31, 1, 31),    &
      MONTH_T( 'Feb','February ',  32,  59, 2, 29),    &
      MONTH_T( 'Mar','March    ',  60,  90, 3, 31),    &
      MONTH_T( 'Apr','April    ',  91, 120, 4, 30),    &
      MONTH_T( 'May','May      ', 121, 151, 5, 31),    &
      MONTH_T( 'Jun','June     ', 152, 181, 6, 30),    &
      MONTH_T( 'Jul','July     ', 182, 212, 7, 31),    &
      MONTH_T( 'Aug','August   ', 213, 243, 8, 31),    &
      MONTH_T( 'Sep','September', 244, 273, 9, 30),    &
      MONTH_T( 'Oct','October  ', 274, 304, 10, 31),   &
      MONTH_T( 'Nov','November ', 305, 334, 11, 30),   &
      MONTH_T( 'Dec','December ', 335, 365, 12, 31)  ]

contains

!------------------------------------------------------------------------------

subroutine set_default_date_format(sDateFormat)

  character (len=*), intent(in) :: sDateFormat

  sDEFAULT_DATE_FORMAT = sDateFormat

end subroutine set_default_date_format

!------------------------------------------------------------------------------

subroutine set_default_time_format(sTimeFormat)

  character (len=*), intent(in) :: sTimeFormat

  sDEFAULT_TIME_FORMAT = sTimeFormat

end subroutine set_default_time_format

!------------------------------------------------------------------------------

subroutine set_date_format_indices(this, sDateFormat)

  class (DATETIME_T), intent(inout) :: this
  character (len=*), intent(in), optional :: sDateFormat

  ! [ LOCALS ]
  character (len=14) :: sDateFmt
  character (len=6), parameter :: DELIMITERS = "/-_\. "

  if(present(sDateFormat) ) then
    sDateFmt = sDateFormat
    sDATE_FORMAT = sDateFormat
  else
    sDateFmt = sDEFAULT_DATE_FORMAT
    sDATE_FORMAT = sDEFAULT_DATE_FORMAT
  endif

  iScanMM1 = scan(string=sDateFmt,set="M")
  iScanMM2 = scan(string=sDateFmt,set="M", back=lTRUE )
  iScanDD1 = scan(string=sDateFmt,set="D")
  iScanDD2 = scan(string=sDateFmt,set="D", back=lTRUE )
  iScanYYYY1 = scan(string=sDateFmt,set="Y")
  iScanYYYY2 = scan(string=sDateFmt,set="Y", back=lTRUE )
  iScanDelim1 = scan(string=trim(sDateFmt), set=DELIMITERS)
  iScanDelim2 = scan(string=trim(sDateFmt), set=DELIMITERS, back=lTRUE)

  call Assert(iScanMM1 > 0 .and. iScanMM2 > 0 &
    .and. iScanDD1 > 0 .and. iScanDD2 > 0 &
    .and. iScanYYYY1 > 0 .and. iScanYYYY2 > 0, &
    "Failed to properly parse the date format string "//dquote(sDateFormat), &
    __SRCNAME__, __LINE__)
    ! perhaps there are no delimiters? if not, these values CAN be zero
!  call assert(iScanDelim1 > 0 .and. iScanDelim2 > 0, &
!    "Failed to properly parse the delimiters in the date format string "//dquote(sDateFormat), &
!    __SRCNAME__, __LINE__)

end subroutine set_date_format_indices

!------------------------------------------------------------------------------

subroutine set_time_format_indices(this, sTimeFormat)

  class (DATETIME_T), intent(inout) :: this
  character (len=*), intent(in), optional :: sTimeFormat

  ! [ LOCALS ]
  character (len=14) :: sTimeFmt

  if(present(sTimeFormat) ) then
    sTimeFmt = sTimeFormat
    sTIME_FORMAT = sTimeFormat
  else
    sTimeFmt = sTIME_FORMAT     ! if no arg supplied, default to module variable
  endif

  iScanHour1 = scan(string=sTimeFmt,set="H")
  iScanHour2 = scan(string=sTimeFmt,set="H", back=lTRUE )
  iScanMin1 = scan(string=sTimeFmt,set="M")
  iScanMin2 = scan(string=sTimeFmt,set="M", back=lTRUE )
  iScanSec1 = scan(string=sTimeFmt,set="S")
  iScanSec2 = scan(string=sTimeFmt,set="S", back=lTRUE )

  call Assert(iScanHour1 > 0 .and. iScanHour2 > 0 &
        .and. iScanMin1 > 0 .and. iScanMin2 > 0 &
        .and. iScanSec1 > 0 .and. iScanSec2 > 0, &
        "Failed to properly parse the time format string "//dquote(sTimeFormat), &
        __SRCNAME__, __LINE__)

end subroutine set_time_format_indices

!------------------------------------------------------------------------------

subroutine parse_text_to_date_sub(this, sString, sFilename, iLinenumber )

  class (DATETIME_T), intent(inout)          :: this
  character (len=*), intent(in)              :: sString
  character (len=*), intent(in), optional    :: sFilename
  integer (kind=c_int), intent(in), optional :: iLinenumber

  ! [ LOCALS ]
  integer (kind=c_int) :: iStat
  integer (kind=c_int) :: iMonth
  integer (kind=c_int) :: iDay
  integer (kind=c_int) :: iYear
  integer (kind=c_int) :: iMonthOffset, iDayOffset
  character (len=256)  :: sStr
  character (len=256)  :: sMonth, sDay, sYear, sBuf
  character (len=256)  :: sFilename_
  integer (kind=c_int) :: iLinenumber_

  if ( present( sFilename) ) then
    sFilename_ = sFilename
  else
    sFilename_ = "<unknown>"
  endif

  if ( present( iLinenumber ) ) then
    iLinenumber_ = iLinenumber
  else
    iLinenumber_ = -9999
  endif

  ! these offset amounts have value of 1 if the program detects a single-digit date value
  iMonthOffset = 0; iDayOffset = 0

  sStr = trim(adjustl(sString))

  sMonth = sStr(iScanMM1 : iScanMM2 )
  sBuf = clean(sMonth)
  if(len_trim(sBuf) /= len_trim(sMonth)) then   ! we have a case where there is no leading zero
    iMonthOffset = 1
    sMonth = trim(sBuf)
  endif
  read(sMonth,fmt=*, iostat = iStat) iMonth

  if ( .not. (iStat==0 .and. (iMonth > 0 .and. iMonth <= 12) ) ) then

    call Assert(lFALSE, &
      "Error parsing month value - got "//trim(sMonth)//";"// &
      " date text: "//trim(sStr), __SRCNAME__, __LINE__, sFilename_, iLinenumber_ )

  endif

  sDay = sStr( iScanDD1 - iMonthOffset : iScanDD2 -iMonthOffset )
  sBuf = clean(sDay)
  if(len_trim(sBuf) /= len_trim(sDay)) then   ! we have a case where there is no leading zero
    iDayOffset = 1
    sDay = trim(sBuf)
  endif
  read(sDay, fmt=*, iostat = iStat) iDay

  if ( .not. (iStat==0 .and. (iDay > 0 .and. iDay <= 31) ) ) then

    call Assert(lFALSE, &
      "Error parsing day value - got "//trim(sDay)//";"// &
      " date text: "//trim(sStr),sFilename_, iLinenumber_, __SRCNAME__, __LINE__ )

  endif

  sYear = sStr( iScanYYYY1 - iMonthOffset - iDayOffset: iScanYYYY2 - iMonthOffset - iDayOffset)
  read(sYear,fmt=*, iostat = iStat) iYear

  if ( iStat/=0 ) then

    call Assert(lFALSE, &
      "Error parsing year value - got "//trim(sYear)//";"// &
      " date text: "//trim(sStr),sFilename_, iLinenumber_, __SRCNAME__, __LINE__ )

  endif

!  if(iYear <= 99 ) iYear = iYear + 1900    ! this might be a lethal assumption

  this%iMonth = iMonth
  this%iYear = iYear
  this%iDay = iDay

  this%dJulianDate = this%dJulianDate + julian_day( iMonth=iMonth, iDay=iDay, iYear=iYear )

end subroutine parse_text_to_date_sub

!------------------------------------------------------------------------------

subroutine parse_text_to_time_sub(this, sString)

  class (DATETIME_T), intent(inout) :: this
  character (len=*), intent(in) :: sString


  ! [ LOCALS ]
  integer (kind=c_int) :: iStat
  integer (kind=c_int) :: iHour
  integer (kind=c_int) :: iMinute
  integer (kind=c_int) :: iSecond
  integer (kind=c_int) :: iOffset

  character (len=256) :: sHour, sMinute, sSecond


  character (len=256) :: sTimeFmt
  character (len=256) :: sStr
  character (len=256) :: sBuf

  iOffset = 0

  sStr = trim(adjustl(sString))

  sHour =   sStr( iScanHour1 : iScanHour2 )

  sBuf = clean(sHour)
  if(len_trim(sBuf) /= len_trim(sHour)) then   ! we have a case where there is no leading zero
    iOffset = 1
    sHour = trim(sBuf)
  endif
  read(sHour,fmt=*, iostat = iStat) iHour
  call Assert(iStat==0, "Error parsing hour value - got "//trim(sHour)//";"// &
    " time text: "//trim(sStr), __SRCNAME__,__LINE__)

  sMinute = sStr(iScanMin1 - iOffset : iScanMin2 - iOffset )
  read(sMinute,fmt=*, iostat = iStat) iMinute
  call Assert(iStat==0, "Error parsing minutes value - got "//trim(sMinute)//";"// &
    " time text: "//trim(sStr), __SRCNAME__,__LINE__)

  if(iScanSec1 /= 0) then
    sSecond = sStr(iScanSec1 - iOffset : iScanSec2 - iOffset )
    read(sSecond,fmt=*, iostat = iStat) iSecond
    call Assert(iStat==0, "Error parsing hour value - got "//trim(sSecond)//";"// &
      " time text: "//trim(sStr), __SRCNAME__,__LINE__)
  else
    iSecond = 0
  endif

  this%iHour = iHour
  this%iMinute = iMinute
  this%iSecond = iSecond

  this%dJulianDate = this%dJulianDate + real( iHour, kind=c_double) / 24.0_c_double      &
                                      + real( iMinute, kind=c_double) / 1440.0_c_double  &
                                      + real( iSecond, kind=c_double) / 86400.0_c_double

end subroutine parse_text_to_time_sub

!--------------------------------------------------------------------------

subroutine calc_water_year_sub(this)

  class (DATETIME_T) :: this

    if(this%iMonth > 9) then
       this%iWaterYearHigh = this%iYear + 1
    else
       this%iWaterYearHigh = &
       this%iYear
    end if

    if(this%iMonth < 4) then
      this%iWaterYearLow = this%iYear - 1
    else
      this%iWaterYearLow = this%iYear
    endif

end subroutine calc_water_year_sub

!--------------------------------------------------------------------------

! subroutine populate_julian_day_sub(this, iMonth, iDay, iYear, &
!                                 iHour, iMinute, iSecond)
!
!   class (DATETIME_T) :: this
!   integer (kind=c_int), intent(in) :: iMonth
!   integer (kind=c_int), intent(in) :: iDay
!   integer (kind=c_int), intent(in) :: iYear
!   integer (kind=c_int), intent(in) :: iHour
!   integer (kind=c_int), intent(in) :: iMinute
!   integer (kind=c_int), intent(in) :: iSecond
!
!   ! [LOCALS]
! !  integer (kind=c_int) :: iJulianDay
! !  real (kind=c_double) :: rFractionOfDay
!
!   this%iMonth = iMonth
!   this%iDay = iDay
!   this%iYear = iYear
!   this%iHour = iHour
!   this%iMinute = iMinute
!   this%iSecond = iSecond
!
! !  this%iJulianDay = julian_day( this%iYear, this%iMonth, this%iDay)
!   this%iJulianDay = julian_day( int(this%iYear, kind=c_int), &
!                                 int(this%iMonth, kind=c_int), &
!                                 int(this%iDay, kind=c_int))
!
!   this%rFractionOfDay = real(this%iHour, kind=c_double) / 24_c_double + &
!                    real(this%iMinute, kind=c_double) / 1440_c_double + &
!                    real(this%iSecond, kind=c_double) / 86400_c_double
!
! !  this%rJulianDay = real(iJulianDay, kind=c_double) + rFractionOfDay !&
! !                                     - 2400000.5_c_double
!
!   ! 2400000.5 is subtracted to yield one definition of a "MODIFIED JUILAN DAY"
!
! end subroutine populate_julian_day_sub

!--------------------------------------------------------------------------

subroutine calc_julian_day_sub(this, iMonth, iDay, iYear, &
                                iHour, iMinute, iSecond)

  class (DATETIME_T) :: this
  integer (kind=c_int), intent(in), optional :: iMonth
  integer (kind=c_int), intent(in), optional :: iDay
  integer (kind=c_int), intent(in), optional :: iYear
  integer (kind=c_int), intent(in), optional :: iHour
  integer (kind=c_int), intent(in), optional :: iMinute
  integer (kind=c_int), intent(in), optional :: iSecond

  if(present(iMonth) ) this%iMonth = iMonth
  if(present(iDay) ) this%iDay = iDay
  if(present(iYear) ) this%iYear = iYear
  if(present(iHour) ) this%iHour = iHour
  if(present(iMinute) ) this%iMinute = iMinute
  if(present(iSecond) ) this%iSecond = iSecond

  this%dJulianDate = real( julian_day( int(this%iYear, kind=c_int), &
                          int(this%iMonth, kind=c_int), &
                          int(this%iDay, kind=c_int) ), kind=c_double) + &
                          real(this%iHour, kind=c_double) / 24_c_double + &
                          real(this%iMinute, kind=c_double) / 1440_c_double + &
                          real(this%iSecond, kind=c_double) / 86400_c_double

!  this%rJulianDay = real(iJulianDay, kind=c_double) + rFractionOfDay ! - 2400000.5_c_double

end subroutine calc_julian_day_sub

!--------------------------------------------------------------------------

elemental subroutine calc_gregorian_date_sub(this)

  class (DATETIME_T), intent(inout) :: this

  ! [ LOCALS ]
  integer (kind=c_int) :: iMonth
  integer (kind=c_int) :: iDay
  integer (kind=c_int) :: iYear
  integer (kind=c_int) :: iHour
  integer (kind=c_int) :: iMinute
  integer (kind=c_int) :: iSecond
  integer (kind=c_int) :: iJulianDay

  real(kind=c_float) :: rHour, rMinute, rSecond

  iJulianDay = this%getJulianDay()

  call gregorian_date( iJulianDay, iYear, iMonth, iDay )

  this%iYear = iYear
  this%iMonth = iMonth
  this%iDay = iDay

  rHour = this%getFractionOfDay() * 24_c_double
  iHour = iHour

  rMinute = (rHour - real(iHour, kind=c_float) ) * 1440_c_double
  iMinute = int(rMinute, kind=c_int)

  rSecond = ( rMinute - real(iMinute, kind=c_float) ) * 86400_c_double
  iSecond = int(rSecond, kind=c_int)

  this%iHour = iHour
  this%iMinute = iMinute
  this%iHour = iSecond

end subroutine calc_gregorian_date_sub

!!***

!--------------------------------------------------------------------------
!!****f* types/gregorian_date
! NAME
!   gregorian_date - Convert from a Julian day number to a Gregorian date.
!
! SYNOPSIS
!   Conversion to a Gregorian calendar date from a Julian date.
!   Valid for any Gregorian calendar date producing a Julian day number
!   greater than zero.
!
! INPUTS
!   iJD     integer number of days that have elapsed since noon
!           Greenwich Mean Time (UT or TT) Monday, January 1, 4713 BC
! OUTPUTS
!   iYear   4-digit year
!   iMonth  2-digit month (1-12)
!   iDay    2-digit day (1-31)
!
! NOTES
!   Reference: Fliegel, H. F. and van Flandern, T. C. (1968).
!   Communications of the ACM, Vol. 11, No. 10 (October, 1968).
!   Modified from code found at:
!       http://aa.usno.navy.mil/faq/docs/JD_Formula.html
!
! SOURCE

elemental subroutine gregorian_date(iJD, iYear, iMonth, iDay, iOrigin)

!! COMPUTES THE GREGORIAN CALENDAR DATE (YEAR,MONTH,DAY)
!! GIVEN THE JULIAN DATE (JD).

  ! [ ARGUMENTS ]
  integer (kind=c_int), value        :: iJD
  integer (kind=c_int), intent(inout)        :: iYear, iMonth, iDay
  integer (kind=c_int), intent(in), optional :: iOrigin
  ! [ LOCALS ]
  integer (kind=c_int) iI,iJ,iK,iL,iN
  integer (kind=c_int) :: iOffset

  if(present(iOrigin)) then
    iOffset = iOrigin
  else
    iOffset = 0
  endif

  ! allow for an alternate "origin" to be specified... technically,
  ! this is no longer a "Julian" day, but alas... This modification
  ! was required in order to process the "time" variables from global
  ! climate models, which seem to be defined as something like this:
  ! time:units = "days since 1960-01-01 00:00:00"
  !
  ! for the above example, JD = 2436935 on the first day; the NetCDF "time"
  ! variable will be equal to 0.  Thus, in order to get the conversion
  ! right, we must add 0 + 2436935 to yield a true Julian Day.

  iJD = iJD + iOffset

  iL= iJD + 68569_c_int
  iN= 4*iL / 146097_c_int
  iL= iL - (146097_c_int * iN + 3_c_int)/4_c_int
  iI= 4000_c_int * (iL + 1_c_int) / 1461001_c_int
  iL= iL - 1461_c_int * iI / 4_c_int + 31_c_int
  iJ= 80_c_int * iL / 2447_c_int
  iK= iL - 2447_c_int * iJ / 80_c_int
  iL= iJ / 11_c_int
  iJ= iJ + 2_c_int - 12_c_int * iL
  iI= 100_c_int * (iN - 49_c_int) + iI + iL

  iYear = iI
  iMonth = iJ
  iDay = iK

end subroutine gregorian_date


!--------------------------------------------------------------------------
!!****f* types/julian_day
! NAME
!   julian_day - Convert from a Gregorian calendar date to a Julian day number.
!
! SYNOPSIS
!   Conversion from a Gregorian calendar date to a Julian day number.
!   Valid for any Gregorian calendar date producing a Julian day
!   greater than zero.
!
! INPUTS
!   iYear   4-digit year
!   iMonth  2-digit month (1-12)
!   iDay    2-digit day (1-31)
!
! OUTPUTS
!   iJD     integer number of days that have elapsed since noon
!           Greenwich Mean Time (UT or TT) Monday, January 1, 4713 BC
!
! SOURCE

function julian_day ( iYear, iMonth, iDay, iOrigin ) result(iJD)

  ! [ ARGUMENTS ]
  integer (kind=c_int), intent(in) :: iYear, iMonth, iDay
  integer (kind=c_int), optional :: iOrigin

  ! [ LOCALS ]
  integer (kind=c_int) i,j,k
  integer (kind=c_int) :: iOffset
  character (len=256) :: sBuf

  ! [ RETURN VALUE ]
  integer (kind=c_int) :: iJD

  i= iYear
  j= iMonth
  k= iDay

  if(.not. (iMonth >= 1 .and. iMonth <= 12)) then
    write(sBuf,fmt="('Illegal month value given: ',i4)") iMonth
    call Assert( lFALSE, trim(sBuf), __SRCNAME__, __LINE__)
  elseif(.not. (iDay >= 1 .and. iDay <= 31)) then
    write(sBuf,fmt="('Illegal day value given: ',i4)") iDay
    call Assert( lFALSE, trim(sBuf), __SRCNAME__, __LINE__)
  endif

  if(present(iOrigin)) then
    iOffset = iOrigin
  else
    iOffset = 0
  endif

  iJD= ( k-32075_c_int + 1461_c_int * (i + 4800_c_int + (j - 14_c_int) / 12_c_int) &
        /4_c_int + 367_c_int * (j - 2_c_int - (j - 14_c_int)/ 12_c_int * 12_c_int) &
        /12_c_int - 3_c_int *((i + 4900_c_int + (j - 14_c_int) &
        /12_c_int)/100_c_int)/4_c_int ) - iOffset

end function julian_day

!------------------------------------------------------------------------------

elemental function is_date_greater_than(date1, date2)   result(lResult)

  class(DATETIME_T), intent(in) :: date1
  class(DATETIME_T), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=c_bool ) :: lResult

  lResult = lFALSE

!  if(date2%iJulianDay == date1%iJulianDay &
!     .and. date1%rFractionOfDay > date2%rFractionOfDay) then
!     lResult = lTRUE
!  elseif(date1%iJulianDay > date2%iJulianDay) then
!    lResult = lTRUE
!  endif

  if( date1%dJulianDate > date2%dJulianDate )  lResult = lTRUE

end function is_date_greater_than

!------------------------------------------------------------------------------

elemental function is_date_less_than(date1, date2)   result(lResult)

  class(DATETIME_T), intent(in) :: date1
  class(DATETIME_T), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=c_bool ) :: lResult

  lResult = lFALSE

!  if(date1%iJulianDay == date2%iJulianDay &
!     .and. date1%rFractionOfDay < date2%rFractionOfDay) then
!     lResult = lTRUE
!  elseif(date1%iJulianDay < date2%iJulianDay) then
!    lResult = lTRUE
!  endif

  if( date1%dJulianDate < date2%dJulianDate )  lResult = lTRUE

end function is_date_less_than

!------------------------------------------------------------------------------

elemental function is_date_LT_or_equal_to(date1, date2)   result(lResult)

  class ( DATETIME_T ), intent(in) :: date1
  type ( DATETIME_T ), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=c_bool ) :: lResult

  lResult = lFALSE

  if( date1%dJulianDate <= date2%dJulianDate )  lResult = lTRUE

end function is_date_LT_or_equal_to

!------------------------------------------------------------------------------

elemental function is_date_GT_or_equal_to(date1, date2)   result(lResult)

  class ( DATETIME_T), intent(in) :: date1
  type ( DATETIME_T), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=c_bool ) :: lResult

  lResult = lFALSE

  if( date1%dJulianDate >= date2%dJulianDate )  lResult = lTRUE

end function is_date_GT_or_equal_to

!------------------------------------------------------------------------------

elemental function is_date_equal_to(date1, date2)   result(lResult)

  class(DATETIME_T), intent(in) :: date1
  class(DATETIME_T), intent(in) :: date2

  ! [ LOCALS ]
  logical(kind=c_bool ) :: lResult

  lResult = lFALSE

  if( date1%getJulianDay() == date2%getJulianDay() .and. &
             date1%iHour == date2%iHour .and. &
             date1%iMinute == date2%iMinute .and. &
             date1%iSecond == date2%iSecond) then

     lResult = lTRUE

  endif

end function is_date_equal_to

!------------------------------------------------------------------------------

elemental function date_minus_date_fn(date1, date2)  result(rDelta)

  class(DATETIME_T), intent(in) :: date1
  class(DATETIME_T), intent(in) :: date2
  real (kind=c_double) :: rDelta

  rDelta = date1%dJulianDate - date2%dJulianDate

end function date_minus_date_fn

!------------------------------------------------------------------------------

elemental function date_plus_float_fn(date1, fValue)  result(newdate)

  class(DATETIME_T), intent(in)   :: date1
  real (kind=c_float), intent(in) :: fValue
  type(DATETIME_T), allocatable  :: newdate

  allocate( newdate )
  newdate%dJulianDate = date1%dJulianDate + real( fValue, kind=c_double)
  call newdate%calcGregorianDate()

end function date_plus_float_fn

!------------------------------------------------------------------------------

elemental function date_minus_float_fn(date1, fValue)  result(newdate)

  class(DATETIME_T), intent(in)   :: date1
  real (kind=c_float), intent(in) :: fValue
  type(DATETIME_T), allocatable  :: newdate

  allocate( newdate )
  newdate%dJulianDate = date1%dJulianDate - real( fValue, kind=c_double)
  call newdate%calcGregorianDate()

end function date_minus_float_fn

!-------------------------------------------------------------------------------

elemental function date_minus_int_fn(date1, iValue)  result(newdate)

  class(DATETIME_T), intent(in)    :: date1
  integer (kind=c_int), intent(in) :: iValue
  type(DATETIME_T), allocatable    :: newdate

  allocate( newdate )
  newdate%dJulianDate = date1%dJulianDate - real( iValue, kind=c_double)
  call newdate%calcGregorianDate()

end function date_minus_int_fn

!------------------------------------------------------------------------------

function write_pretty_datetime_fn(this)     result(sDateTimeText)

  class(DATETIME_T) :: this
  character (len=20) :: sDateTimeText

  write(sDateTimeText, fmt="(a3,' ',i2.2,' ',i4.4, 1x, i2.2,':',i2.2,':',i2.2)") &
    MONTHS(this%iMonth)%sName, this%iDay, this%iYear, this%iHour, this%iMinute, this%iSecond

end function write_pretty_datetime_fn

!------------------------------------------------------------------------------

function write_pretty_date_fn(this)     result(sDateText)

  class(DATETIME_T) :: this
  character (len=10) :: sDateText

  write(sDateText, fmt="(i4.4,'-',i2.2,'-',i2.2)") &
    this%iYear, this%iMonth, this%iDay

end function write_pretty_date_fn

!------------------------------------------------------------------------------

function write_list_date_fn(this)                     result(sDateText)

  class(DATETIME_T) :: this
  character (len=10) :: sDateText

  ! [ LOCALS ]
  integer (kind=c_int), dimension(5) :: iStat
!  sDateText = this%listdatetime()

  write(sDateText(iScanMM1:iScanMM2),fmt="(i2.2)", iostat=iStat(1)) this%iMonth
  write(sDateText(iScanDD1:iScanDD2),fmt="(i2.2)", iostat=iStat(2)) this%iDay
  write(sDateText(iScanYYYY1:iScanYYYY2),fmt="(i4.4)",iostat=iStat(3)) this%iYear
  if(iScanDelim1 > 0) write(sDateText(iScanDelim1:iScanDelim1), &
     fmt="(a1)",iostat=iStat(4)) &
     sDATE_FORMAT(iScanDelim1:iScanDelim1)
  if(iScanDelim2 > 0) write(sDateText(iScanDelim2:iScanDelim2), &
     fmt="(a1)",iostat=iStat(5)) &
     sDATE_FORMAT(iScanDelim2:iScanDelim2)

  call Assert(all(iStat==0),"Problem parsing the date format '"// &
     trim(sDATE_FORMAT)//"' for output", &
    __SRCNAME__, __LINE__)

end function write_list_date_fn

!------------------------------------------------------------------------------

function write_list_time_fn(this)                     result(sTimeText)

  class(DATETIME_T) :: this
  character (len=8) :: sTimeText

  write(sTimeText,fmt="(i2.2,':',i2.2':',i2.2)") this%iHour, this%iMinute, this%iSecond

end function write_list_time_fn

!------------------------------------------------------------------------------

function write_list_datetime_fn(this)    result(sDatetimeText)

  class(DATETIME_T) :: this
!  character(len=*), optional :: sDateFormat
!  logical (kind=c_bool), optional :: lDateOnly
  character (len=19) :: sDatetimeText

  ! [ LOCALS ]
!  character(len=25) sDateFmt
!  integer (kind=c_int) :: iScanMM1, iScanMM2
!  integer (kind=c_int) :: iScanDD1, iScanDD2
!  integer (kind=c_int) :: iScanYYYY1, iScanYYYY2
!  integer (kind=c_int) :: iScanDelim1, iScanDelim2
  character (len=32) :: sBuf
!  character (len=6), parameter :: DELIMITERS = "/-_\. "
  integer (kind=c_int), dimension(5) :: iStat
!  logical (kind=c_bool) lListTime

  sDateTimeText = ""

!  if(present(sDateFormat)) then
!    sDateFmt = uppercase(trim(adjustl(sDateFormat)))
!  else
!    sDateFmt = "MM/DD/YYYY"
!  endif

!  if(present(lDateOnly)) then
!    lListTime = .not. lDateOnly
!  else
!    lListTime = lTRUE
!  endif

!  iScanMM1 = scan(string=sDateFmt,set="M")
!  iScanMM2 = scan(string=sDateFmt,set="M", back=lTRUE )

!  iScanDD1 = scan(string=sDateFmt,set="D")
!  iScanDD2 = scan(string=sDateFmt,set="D", back=lTRUE )

!  iScanYYYY1 = scan(string=sDateFmt,set="Y")
!  iScanYYYY2 = scan(string=sDateFmt,set="Y", back=lTRUE )

!  iScanDelim1 = scan(string=trim(sDateFmt), set=DELIMITERS)
!  iScanDelim2 = scan(string=trim(sDateFmt), set=DELIMITERS, back=lTRUE)

  write(sDateTimeText(iScanMM1:iScanMM2),fmt="(i2.2)", iostat=iStat(1)) this%iMonth
  write(sDateTimeText(iScanDD1:iScanDD2),fmt="(i2.2)", iostat=iStat(2)) this%iDay
  write(sDateTimeText(iScanYYYY1:iScanYYYY2),fmt="(i4.4)",iostat=iStat(3)) this%iYear
  if(iScanDelim1 > 0) write(sDateTimeText(iScanDelim1:iScanDelim1), &
     fmt="(a1)",iostat=iStat(4)) &
     sDATE_FORMAT(iScanDelim1:iScanDelim1)
  if(iScanDelim2 > 0) write(sDateTimeText(iScanDelim2:iScanDelim2), &
     fmt="(a1)",iostat=iStat(5)) &
     sDATE_FORMAT(iScanDelim2:iScanDelim2)

  call Assert(all(iStat==0),"Problem parsing the date format '"// &
     trim(sDATE_FORMAT)//"' for output", &
    __SRCNAME__, __LINE__)

  write(sBuf,fmt="(1x,i2.2,':',i2.2':',i2.2)") this%iHour, this%iMinute, this%iSecond

!  if(lListTime) then
    sDateTimeText = trim(sDateTimeText) // trim(sBuf)
!  else
!    sDateTimeText = trim(sDateTimeText)
!  endif

end function write_list_datetime_fn

!------------------------------------------------------------------------------

subroutine system_time_to_date_sub(this)

  class (DATETIME_T) :: this

  ! [ LOCALS ]
  character (len=16) :: sDateText
  character (len=16) :: sTimeText
  integer (kind=c_int), dimension(8) :: iValues

  call DATE_AND_TIME(sDateText, sTimeText)
  call DATE_AND_TIME(VALUES = iValues)

  call this%setDateFormat("YYYYMMDD")
  call this%setTimeFormat("HHMMSS")

  call this%parseDate(sDateText)
  call this%parseTime(sTimeText)
  call this%calcJulianDay()
  this%dJulianDate = this%dJulianDate + &
      (real(iValues(8), kind=c_double) / 86400_c_double / 1000_c_double) ! milliseconds

  call this%setDateFormat()
  call this%setTimeFormat()

end subroutine system_time_to_date_sub


!--------------------------------------------------------------------------

!> \brief Return the number of days in the given year.
!!
!! This function simply returns the number of days given the current year.
!function day_of_year(iJulianDay) result(iDOY)

!  integer (kind=c_int), intent(in) :: iJulianDay

  ! [ LOCALS ]
!  integer (kind=c_int) :: iFirstDay, iCurrDay, iDOY
!  integer (kind=c_int) :: iYear, iMonth, iDay

  ! first get the value for the current year
!  call gregorian_date(iJulianDay, iYear, iMonth, iDay)

  ! now calculate the Julian day for the first of the year
!  iFirstDay = julian_day ( iYear, 1, 1 )

  ! return the current day of the year
!  iDOY = iJulianDay - iFirstDay + 1

!  return

!end function day_of_year

!--------------------------------------------------------------------------
!!****f* types/solstice
! NAME
!   solstice - Returns 0 normally, or a value >0 during solstice or equinox.
!
! SYNOPSIS
!    Returns the following:
!      0: non-solstice and non-equinox day
!      1: Vernal equinox
!      2: Summer Solstice
!      3: Autumnal equinox
!      4: Winter solstice
!
! INPUTS
!   iJD     Julian day value
!
! OUTPUTS
!   iSol    Code as described above
!
! SOURCE

!function solstice (iJD)  result (iSol)

  ! [ ARGUMENTS ]
!  integer (kind=c_int), intent(in) :: iJD

  ! [ LOCALS ]
!  integer (kind=c_int) iMonth, iDay, iYear


!  ! [ RETURN VALUE ]
!  integer (kind=c_int) :: iSol

!  call gregorian_date(iJD, iYear, iMonth, iDay)

!  if(iMonth==3 .and. iDay == 20) then
!    iSol = 1
!  elseif(iMonth==6 .and. iDay == 21) then
!    iSol = 2
!  elseif(iMonth==9 .and. iDay == 22) then
!    iSol = 3
!  elseif(iMonth==12 .and. iDay == 21) then
!   iSol = 4
!  else
!    iSol = 0
!  endif
!
!  return
!
!end function solstice

!------------------------------------------------------------------------------

elemental subroutine set_julian_date_sub(this, dValue)

  class (DATETIME_T), intent(inout)      :: this
  real (kind=c_double), intent(out)   :: dValue

  this%dJulianDate = dValue

  call this%calcGregorianDate()

end subroutine set_julian_date_sub

!------------------------------------------------------------------------------

elemental function get_julian_day_fn(this)                   result(iJulianDay)

  class(DATETIME_T), intent(in)    :: this
  integer (kind=c_int)             :: iJulianDay

  iJulianDay = int(this%dJulianDate, kind=c_int)

end function get_julian_day_fn

!------------------------------------------------------------------------------

elemental function get_fraction_of_day_fn(this)           result(dFractionOfDay)

  class(DATETIME_T), intent(in)      :: this
  real (kind=c_double)               :: dFractionOfDay

  dFractionOfDay = this%dJulianDate - real( int(this%dJulianDate, kind=c_int ), kind=c_double)

end function get_fraction_of_day_fn

!------------------------------------------------------------------------------

function get_days_in_month_fn(this)          result(iDaysInMonth)

  class(DATETIME_T) :: this
  integer (kind=c_int) :: iDaysInMonth

  ! [ LOCALS ]
  integer (kind=c_int), dimension(12), parameter :: iNumberOfDaysInMonth = &
    [31,28,31,30,31,30,31,31,30,31,30,31]

  iDaysInMonth = iNumberOfDaysInMonth(this%iMonth)

  if (this%isLeapYear() ) then
    iDaysInMonth = max(iDaysInMonth, 29)
  endif

end function get_days_in_month_fn

!------------------------------------------------------------------------------

function get_days_in_year_fn(this)  result(iDaysInYear)

  class(DATETIME_T) :: this
  integer (kind=c_int) :: iDaysInYear

  if (this%isLeapYear() ) then
    iDaysInYear = 366
  else
    iDaysInYear = 365
  endif

end function get_days_in_year_fn

!------------------------------------------------------------------------------

subroutine date_plus_year_sub(this)

  class(DATETIME_T) :: this

  this%iYear = this%iYear + 1_c_int
  call this%calcJulianDay()

end subroutine date_plus_year_sub

!------------------------------------------------------------------------------

subroutine date_set_day_sub(this, newday)

  class(DATETIME_T)                  :: this
  integer (kind=c_int), intent(in)   :: newday

  this%iDay = newday
  call this%calcJulianDay()

end subroutine date_set_day_sub

!------------------------------------------------------------------------------

subroutine date_set_month_sub(this, newmonth)

  class(DATETIME_T)                  :: this
  integer (kind=c_int), intent(in)   :: newmonth

  this%iMonth = newmonth
  call this%calcJulianDay()

end subroutine date_set_month_sub

!------------------------------------------------------------------------------

subroutine date_set_year_sub(this, newyear)

  class(DATETIME_T)                  :: this
  integer (kind=c_int), intent(in)   :: newyear

  this%iYear = newyear
  call this%calcJulianDay()

end subroutine date_set_year_sub

!------------------------------------------------------------------------------

subroutine date_minus_year_sub(this)

  class(DATETIME_T) :: this

  this%iYear = this%iYear - 1_c_int
  call this%calcJulianDay()

end subroutine date_minus_year_sub

!------------------------------------------------------------------------------

subroutine date_plus_day_sub(this)

  class(DATETIME_T) :: this

  this%dJulianDate = this%dJulianDate + 1_c_double
  call this%calcGregorianDate()

end subroutine date_plus_day_sub

!------------------------------------------------------------------------------

subroutine date_minus_day_sub(this)

  class(DATETIME_T) :: this

  this%dJulianDate = this%dJulianDate - 1_c_double
  call this%calcGregorianDate()

end subroutine date_minus_day_sub

!------------------------------------------------------------------------------

function is_leap_year_fn(this)   result(lIsLeapYear)

  class(DATETIME_T)       :: this
  logical (kind=c_bool)   :: lIsLeapYear

  lIsLeapYear = ( mod(this%iYear, 4) == 0 .and. mod(this%iYear, 100) /= 0 ) .or. &
                 ( mod(this%iYear, 400) == 0 .and. this%iYear /= 0 )

end function is_leap_year_fn

!------------------------------------------------------------------------------

function mmddyyyy2julian(sMMDDYYYY)  result(iJD)

  character (len=*) :: sMMDDYYYY
  integer (kind=c_int) :: iJD

  ! [ LOCALS ]
  integer (kind=c_int) :: iMonth
  integer (kind=c_int) :: iDay
  integer (kind=c_int) :: iYear
  character (len=256) :: sItem, sBuf
  integer (kind=c_int) :: iStat

  sItem = sMMDDYYYY

  ! parse month value
  call chomp(sItem, sBuf, "/-")
  read(sBuf,*,iostat = iStat) iMonth
  call Assert(iStat==0, "Problem reading month value from string "//TRIM(sMMDDYYYY), &
    __SRCNAME__,__LINE__)

  ! parse day value
  call chomp(sItem, sBuf, "/-")
  read(sBuf,*,iostat=iStat) iDay
  call Assert(iStat==0, "Problem reading day value from string "//TRIM(sMMDDYYYY), &
    __SRCNAME__,__LINE__)

  ! parse year value
  call chomp(sItem, sBuf, "/-")
  read(sBuf,*,iostat=iStat) iYear
  call Assert(iStat==0, "Problem reading year value from string "//TRIM(sMMDDYYYY), &
    __SRCNAME__,__LINE__)

  iJD = julian_day ( iYear, iMonth, iDay)

end function mmddyyyy2julian

!------------------------------------------------------------------------------

function mmdd2doy(sMMDD, sInputItemName )  result(iDOY)

  character (len=*)               :: sMMDD
  character (len=*), optional     :: sInputItemName

  integer (kind=c_int) :: iDOY

  ! [ LOCALS ]
  integer (kind=c_int) :: iMonth
  integer (kind=c_int) :: iDay
  integer (kind=c_int) :: iYear
  character (len=256) :: sItem, sBuf
  integer (kind=c_int) :: iStat
  integer (kind=c_int) :: iJD
  integer (kind=c_int) :: iStartingJD
  character (len=256)  :: sInputItemName_

  sItem = sMMDD

  if ( present( sInputItemName ) ) then
    sInputItemName_ = trim( sInputItemName )
  else
    sInputItemName_ = "unknown"
  endif

  ! parse month value
  call chomp(sItem, sBuf, "/-")
  read(sBuf,*,iostat = iStat) iMonth
  call Assert(iStat==0, "Problem reading month value from string "//TRIM(sMMDD),   &
    __SRCNAME__,__LINE__,                                                          &
    sHints="The offending string was "//sQuote(sMMDD)//", which was encountered "   &
           //"while attempting to read in "//sQuote( sInputItemName_ ) )

  ! parse day value
  call chomp(sItem, sBuf, "/-")
  read(sBuf,*,iostat=iStat) iDay
  call Assert(iStat==0, "Problem reading day value from string "//TRIM(sMMDD),   &
    __SRCNAME__,__LINE__,                                                          &
    sHints="The offending string was "//sQuote(sMMDD)//", which was encountered "   &
           //"while attempting to read in "//sQuote( sInputItemName_ ) )

  ! we don't really care about the year value here; any year value could have been used
  iStartingJD = julian_day ( 1999, 1, 1)
  iJD = julian_day ( 1999, iMonth, iDay)

  iDOY = iJD - iStartingJD + 1

end function mmdd2doy

!------------------------------------------------------------------------------

function mmddyyyy2doy(sMMDDYYYY)  result(iDOY)

  character (len=*) :: sMMDDYYYY
  integer (kind=c_int) :: iDOY

  ! [ LOCALS ]
  integer (kind=c_int) :: iMonth
  integer (kind=c_int) :: iDay
  integer (kind=c_int) :: iYear
  character (len=256) :: sItem, sBuf
  integer (kind=c_int) :: iStat
  integer (kind=c_int) :: iJD
  integer (kind=c_int) :: iStartingJD

  sItem = sMMDDYYYY

  ! parse month value
  call chomp(sItem, sBuf, "/-")
  read(sBuf,*,iostat = iStat) iMonth
  call assert(iStat==0, "Problem reading month value from string "//TRIM(sMMDDYYYY), &
    __SRCNAME__,__LINE__)

  ! parse day value
  call chomp(sItem, sBuf, "/-")
  read(sBuf,*,iostat=iStat) iDay
  call assert(iStat==0, "Problem reading day value from string "//TRIM(sMMDDYYYY), &
    __SRCNAME__,__LINE__)

  ! parse year value
  call chomp(sItem, sBuf, "/-")
  read(sBuf,*,iostat=iStat) iYear
  call assert(iStat==0, "Problem reading year value from string "//TRIM(sMMDDYYYY), &
    __SRCNAME__,__LINE__)

  iStartingJD = julian_day ( iYear, 1, 1)
  iJD = julian_day ( iYear, iMonth, iDay)

  iDOY = iJD - iStartingJD + 1

end function mmddyyyy2doy

!--------------------------------------------------------------------------

function get_day_of_year_fn(this)  result(iDOY)

  class(DATETIME_T)    :: this
  integer (kind=c_int) :: iDOY

  iDOY = day_of_year( int( this%dJulianDate, kind=c_int) )

end function get_day_of_year_fn

!--------------------------------------------------------------------------

function day_of_year(iJD) result(iDOY)

  integer (kind=c_int), value :: iJD

  ! [ LOCALS ]
  integer (kind=c_int) :: iFirstDay, iLastDay, iDOY
  integer (kind=c_int) :: iMonth, iDay, iYear


  call gregorian_date(iJD, iYear, iMonth, iDay)
  iFirstDay = julian_day ( iYear, 1, 1 )

  iDOY = iJD - iFirstDay + 1

end function day_of_year

!--------------------------------------------------------------------------

function isLeap(iYear)   result(lResult)

  integer (kind=c_int), intent(in)     :: iYear
  logical (kind=c_bool) :: lResult

  lResult = ( mod(iYear, 4) == 0 .and. mod(iYear, 100) /= 0 ) .or. &
                 ( mod(iYear, 400) == 0 .and. iYear /= 0 )

end function isLeap

!--------------------------------------------------------------------------

end module datetime
