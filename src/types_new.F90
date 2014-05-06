!> @file Common global general-purpose definitions

module types_new

  use iso_c_binding, only : c_int, c_float, c_double, c_bool
  implicit none

  public

  enum, bind(c)
    enumerator :: INTEGER_DATA = 1, FLOAT_DATA = 2, DOUBLE_DATA = 3, STRING_DATA = 4, &
                  DATETIME_DATA = 5, DATE_DATA = 6, TIME_DATA = 7
  end enum

contains

function pf(lBool)   result(sPassFail)

  logical (kind=c_bool), intent(in) :: lBool
  character (len=4) :: sPassFail  

  if (lBool) then
    sPassFail = "PASS"
  else
    sPassFail = "FAIL"  
  endif

end function pf  


function ok(lBool)   result(sOK)

  logical (kind=c_bool), intent(in) :: lBool
  character (len=2) :: sOK  

  if (lBool) then
    sOK = "OK"
  else
    sOK = "  "  
  endif

end function ok  

end module types_new