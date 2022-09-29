module test_datetime

  use fruit
  use datetime
  use exceptions
  use logfiles, only                    : LOGS
  use constants_and_conversions, only   : FALSE, TRUE, iTINYVAL
  implicit none

contains

subroutine start_supressing_fatal_errors()

  ! override exceptions module default behavior, which is to stop program execution entirely
  HALT_UPON_FATAL_ERROR = FALSE

  call LOGS%set_echo( TRUE )
  call LOGS%write(repeat("-",80), iLinesBefore=2)
  call LOGS%write(">>>>> FATAL ERROR WAS SUPPRESSED FOR UNIT TESTING. ERROR MESSAGE: >>>>>", iLinesAfter=2)

end subroutine start_supressing_fatal_errors

!-------------------------------------------------------------------------------

subroutine stop_supressing_fatal_errors()

  ! reset global variable to normal mode of operation (normal == CRASH, HARD, when error is detected)
  HALT_UPON_FATAL_ERROR = TRUE

  call LOGS%write("<<<<< END OF FATAL ERROR SUPPRESSED FOR UNIT TESTING <<<<<", iLinesBefore=2)
  call LOGS%write(repeat("-",80), iLinesAfter=2)
  call LOGS%set_echo( FALSE )

end subroutine stop_supressing_fatal_errors

!-------------------------------------------------------------------------------

  subroutine test_datetime_basic_dateparse
   ! datetime: parse with default mm/dd/yyyy date format
     type (DATETIME_T) :: dt

     call dt%parseDate("03/15/2011", sFilename=trim(__FILE__), iLineNumber=__LINE__)
     call assert_equals (3, int(dt%iMonth))
     call assert_equals (15, int(dt%iDay))
     call assert_equals (2011, int(dt%iYear))

   end subroutine test_datetime_basic_dateparse

!-------------------------------------------------------------------------------

   subroutine test_datetime_illegal_values
   ! datetime: parse with non-existant day value
     type (DATETIME_T) :: dt

     call start_supressing_fatal_errors()

     call dt%parseDate("2/29/2001", sFilename=trim(__FILE__), iLineNumber=__LINE__)
     call assert_true( dt%dJulianDate < 0.)

     call stop_supressing_fatal_errors()

   end subroutine test_datetime_illegal_values

 !-------------------------------------------------------------------------------

   subroutine test_datetime_basic_mangled_dateparse
    ! datetime: parse with default mm/dd/yyyy date format, missing '0' values in month and day
      type (DATETIME_T) :: dt

      call dt%parseDate("3/2/2011", sFilename=trim(__FILE__), iLineNumber=__LINE__)
      call assert_equals (3, int(dt%iMonth))
      call assert_equals (2, int(dt%iDay))
      call assert_equals (2011, int(dt%iYear))
 
    end subroutine test_datetime_basic_mangled_dateparse
 
  !-------------------------------------------------------------------------------
 
   subroutine test_datetime_custom_dateparse
   ! datetime: parse with custom yyyy-mm-dd date format

     type (DATETIME_T) :: dt

     call dt%setDateFormat("YYYY-MM-DD")
     call dt%parseDate("1776-07-4", sFilename=trim(__FILE__), iLineNumber=__LINE__)
     call assert_equals (7, int(dt%iMonth))
     call assert_equals (4, int(dt%iDay))
     call assert_equals (1776, int(dt%iYear))

   end subroutine test_datetime_custom_dateparse

 !-------------------------------------------------------------------------------

   subroutine test_datetime_addition
   ! datetime: add 5 to Julian day and return the correct Gregorian date
     type (DATETIME_T)    :: dt
     integer              :: indx

     call dt%calcJulianDay(iMonth=2, iDay=29, iYear=2000)

      do indx=1,5
       call dt%addDay()
     enddo

     call assert_equals (3, int(dt%iMonth))
     call assert_equals (5, int(dt%iDay))
     call assert_equals (2000, int(dt%iYear))

   end subroutine test_datetime_addition

 !-------------------------------------------------------------------------------

   subroutine test_datetime_julian_date_illegal_month
    ! datetime: supply illegal month value to Julian Date routine
      type (DATETIME_T)    :: dt
      integer              :: indx

      call start_supressing_fatal_errors()

      call dt%calcJulianDay(iMonth=0, iDay=28, iYear=2000) 
      call assert_equals(iTINYVAL, int(dt%iJulianDay))

      call stop_supressing_fatal_errors()
      
    end subroutine test_datetime_julian_date_illegal_month
 
 !-------------------------------------------------------------------------------

    subroutine test_datetime_julian_date_illegal_day
      ! datetime: supply illegal day value to Julian Date routine
        type (DATETIME_T)    :: dt
        integer              :: indx
  
        call start_supressing_fatal_errors()

        call dt%calcJulianDay(iMonth=2, iDay=0, iYear=2000)
        call assert_equals(iTINYVAL, int(dt%iJulianDay))

        call stop_supressing_fatal_errors()

      end subroutine test_datetime_julian_date_illegal_day
  
 !-------------------------------------------------------------------------------

      subroutine test_datetime_julian_date_illegal_month_day
        ! datetime: supply illegal month and day value to Julian Date routine
          type (DATETIME_T)    :: dt
          integer              :: indx
    
          call start_supressing_fatal_errors()

          call dt%calcJulianDay(iMonth=13, iDay=0, iYear=2000)     
          call assert_equals(iTINYVAL, int(dt%iJulianDay))

          call stop_supressing_fatal_errors()
          
        end subroutine test_datetime_julian_date_illegal_month_day
  
  end module test_datetime
