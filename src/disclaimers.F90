module disclaimers

  use constants_and_conversions, only  : TRUE
  use logfiles, only                   : LOGS, LOG_ALL
  implicit none

contains

  subroutine log_provisional_disclaimer()

    call LOGS%write( sMessage='Disclaimer', lEcho=TRUE)
    call LOGS%write( sMessage='==========', lEcho=TRUE,iLinesAfter=1)

    call LOGS%write( sMessage='This software is preliminary or provisional and is subject to revision. It is', lEcho=TRUE)
    call LOGS%write( sMessage='being provided to meet the need for timely best science. The software has not', lEcho=TRUE)
    call LOGS%write( sMessage='received final approval by the U.S. Geological Survey (USGS). No warranty,', lEcho=TRUE)
    call LOGS%write( sMessage='expressed or implied, is made by the USGS or the U.S. Government as to the', lEcho=TRUE)
    call LOGS%write( sMessage='functionality of the software and related material nor shall the fact of release', lEcho=TRUE)
    call LOGS%write( sMessage='constitute any such warranty. The software is provided on the condition that', lEcho=TRUE)
    call LOGS%write( sMessage='neither the USGS nor the U.S. Government shall be held liable for any damages', lEcho=TRUE)
    call LOGS%write( sMessage='resulting from the authorized or unauthorized use of the software.', lEcho=TRUE, ilinesAfter=1)

  end subroutine log_provisional_disclaimer

end module disclaimers
