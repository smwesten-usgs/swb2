module weather_data_tabular

  use iso_c_binding, only              : c_short, c_int, c_float, c_double, c_bool
  use constants_and_conversions, only  : TRUE, FALSE, M_PER_FOOT, in_to_mm
  use fstring_list, only               : FSTRING_LIST_T, create_list, NA_FLOAT
  use fstring, only                    : asCharacter, sQuote, operator(.contains.)
  use parameters, only                 : PARAMS
  use simulation_datetime, only        : SIM_DT
  use datetime, only                   : DATETIME_T, operator(<), operator(>)
  use logfiles, only                   : LOGS, LOG_ALL
  use exceptions, only                 : assert, warn, die

  implicit none

  real (c_float), allocatable    :: TMIN(:)
  real (c_float), allocatable    :: TMAX(:)
  real (c_float), allocatable    :: PRECIP(:)
  type (DATETIME_T), allocatable :: WEATHER_DATE(:)

  integer (c_int), save  :: DATE_INDX = 1

  private DATE_INDX

contains

  subroutine weather_data_tabular_initialize()

    type (FSTRING_LIST_T)              :: slList
    integer (kind=c_int)               :: indx
    integer (kind=c_int)               :: iStat
    character (len=:), allocatable     :: date_str
    type (DATETIME_T)                  :: tempdate
    type (DATETIME_T)                  :: firstdate
    type (DATETIME_T)                  :: lastdate
    integer (c_int)                    :: error_count
    integer (c_int)                    :: first_indx


    call PARAMS%get_parameters(sKey="PRCP", fValues=PRECIP)
    call PARAMS%get_parameters(sKey="TMIN", fValues=TMIN)
    call PARAMS%get_parameters(sKey="TMAX", fValues=TMAX)

    !> read in dates as a string list
    call PARAMS%get_parameters( sKey="Date", slValues=slList)

    allocate(WEATHER_DATE(slList%count), stat=iStat)
    call assert( iStat==0, "Failed to allocate memory for WEATHER_DATE array", &
      __FILE__, __LINE__ )

    call LOGS%write("  Date       | PRCP          | TMIN         | TMAX")  
    call LOGS%write("-------------|---------------|--------------|--------------")

    call WEATHER_DATE(1)%setDateFormat(sDateFormat="YYYY-MM-DD")

    print *, "Scanning weather data file for anomalies... There are "//asCharacter(slList%count)//" entries."


    error_count = 0

    do indx=1, slList%count
      
      date_str = slList%get(indx)
      call WEATHER_DATE(indx)%parsedate( date_str, __FILE__, __LINE__ )
      
      if (indx == 1)  tempdate = WEATHER_DATE(1)

      do
        if ( ( tempdate < SIM_DT%start ) .or. ( tempdate > SIM_DT%end ) ) then

          error_count = 0
          ! NOP
          exit
   
        elseif ( (WEATHER_DATE(indx) == tempdate) .and. (error_count == 0)) then

          if (PRECIP(indx) <= NA_FLOAT)  call warn("Missing PRCP value for "//WEATHER_DATE(indx)%prettydate(), lFatal=TRUE) 
          if (TMAX(indx) <= NA_FLOAT)    call warn("Missing TMAX value for "//WEATHER_DATE(indx)%prettydate(), lFatal=TRUE) 
          if (TMIN(indx) <= NA_FLOAT)    call warn("Missing TMIN value for "//WEATHER_DATE(indx)%prettydate(), lFatal=TRUE) 
    
          call LOGS%write(WEATHER_DATE(indx)%prettydate()//"  |  "//asCharacter(PRECIP(indx))//"  |  "  &
                                                  //asCharacter(TMIN(indx))//"  |  "                    &
                                                  //asCharacter(TMAX(indx)))                                        
    
          exit
 
        elseif ((WEATHER_DATE(indx) == tempdate) .and. (error_count > 0)) then

          call warn("Missing or out of order date sequence "//asCharacter(error_count)//" value(s) in"  &
           //" single-station weather data file. Problem date(s) span "//firstdate%prettydate()//" to "  &
           //lastdate%prettydate()//".", lFatal=TRUE)
          error_count = 0
          exit
        elseif (error_count == 0) then

          firstdate = tempdate
          error_count = error_count + 1
          lastdate = WEATHER_DATE(indx) - 1
          call tempdate%addDay()
          cycle
        else

          error_count = error_count + 1
          call tempdate%addDay()
          cycle
          
        endif    

      enddo

      call tempdate%addDay()

    enddo

    ! can we really have set up the code to use a GLOBAL value for the default
    ! date format?? That needs to change.
    call WEATHER_DATE(1)%setDateFormat(sDateFormat="MM-DD-YYYY")

  end subroutine weather_data_tabular_initialize

!-----------------------------------------------------------------------------------

  subroutine weather_data_find_date_indx(dt)

    type (DATETIME_T), intent(in) :: dt

    integer (c_int)   :: indx
    logical (c_bool)  :: date_found

    date_found = FALSE

    do indx=DATE_INDX, size(WEATHER_DATE%iJulianDay,1)

      if (WEATHER_DATE(indx) == dt) then
        date_found = TRUE
        DATE_INDX = indx
        exit
      endif  

    enddo

    call assert(date_found, "Failed to find a matching date value in the daily weather data table.", &
      __FILE__, __LINE__)

  end subroutine weather_data_find_date_indx

!-----------------------------------------------------------------------------------

  subroutine weather_data_tabular_get_precip( dt, precip_value )

    type (DATETIME_T), intent(in)    :: dt
    real (kind=c_float), intent(out) :: precip_value

    call weather_data_find_date_indx(dt)

    precip_value = PRECIP(DATE_INDX)

  end subroutine weather_data_tabular_get_precip

!-----------------------------------------------------------------------------------

  subroutine weather_data_tabular_get_tmax( dt, tmax_value )

    type (DATETIME_T), intent(in)    :: dt
    real (kind=c_float), intent(out) :: tmax_value

    call weather_data_find_date_indx(dt)
    tmax_value = TMAX(DATE_INDX)

  end subroutine weather_data_tabular_get_tmax

!-----------------------------------------------------------------------------------

  subroutine weather_data_tabular_get_tmin( dt, tmin_value )

    type (DATETIME_T), intent(in)    :: dt
    real (kind=c_float), intent(out) :: tmin_value

    call weather_data_find_date_indx(dt)
    tmin_value = TMIN(DATE_INDX)

  end subroutine weather_data_tabular_get_tmin


end module weather_data_tabular