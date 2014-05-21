!> @file
!> Contains a single module, @ref model, which keeps track of the model date and executes
!>  necessary process modules.

!> Allocates memory to store intermediate and final calculation results,
!> keeps track of the model date, reads tabular climate data, and calls the
!> necessary process modules in turn.
module model

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_long_long
  use types
  use data_catalog
  use data_catalog_entry
  use datetime
  use swb_grid
  use stats
  use runoff_curve_number
  use et_crop_coefficients
  use et_thornthwaite_mather
    use et_hargreaves
  use et_jensen_haise
    use irrigation
  use netcdf4_support
  use sm_thornthwaite_mather
  use snow
  use water_balance

   implicit none

  !! Counter for moving average water inputs
  integer (kind=c_int) :: iDayCtr

  !! Generic grids used to shuffle data between subroutines
  type ( T_GENERAL_GRID ),pointer :: pGenericGrd_int
  type ( T_GENERAL_GRID ),pointer :: pGenericGrd_sgl

  
  real(kind=c_float) :: rStartTime,rEndTime

contains


!--------------------------------------------------------------------------
!!****s* model/model_Solve
! NAME
!   model_Solve - Reads and initializes model grids and executes process
!                 subroutines.
!
! SYNOPSIS
!   Reads and initializes model grids, reads climate data file, and handles
!   calls each process subroutine on a daily basis.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!   pConfig - Pointer to the model configuration data structure.
!
! OUTPUTS
!   NONE
!
!!***

subroutine model_Solve( pGrd, pConfig, pGraph)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd               ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains
    ! model options, flags, and other settings

  type (T_GRAPH_CONFIGURATION), dimension(:), pointer :: pGraph
    ! pointer to data structure that holds parameters for creating
    ! DISLIN plots

  ! [ LOCALS ]
  integer (kind=c_int) :: i, j, k, iStat, iDayOfYear, iMonth
  integer (kind=c_int) :: tj, ti
  integer (kind=c_int) :: iTempDay, iTempMonth, iTempYear
  integer (kind=c_long_long) :: iPos
  integer (kind=c_int) :: iIndex
  integer (kind=c_int) :: jj, ii, iNChange, iUpstreamCount, iPasses, iTempval
  integer (kind=c_int) :: iCol, iRow
  character(len=3) :: sMonthName
  logical (kind=c_bool) :: lMonthEnd

  real (kind=c_float) :: rmin,ravg,rmax

  type (T_CELL),pointer :: cel
  character (len=256) :: sBuf

  type (T_TIME_SERIES_FILE), pointer :: pTS

  ! allocate memory for the time-series data pointer
  ALLOCATE (pTS, STAT=iStat)
  call Assert( iStat == 0, &
    "Could not allocate memory for time-series data structure", &
    TRIM(__FILE__),__LINE__)

  pConfig%iNumDaysInYear = num_days_in_year(pConfig%iYear)

  FIRST_YEAR_pt_1: if(pConfig%lFirstYearOfSimulation) then

    pGenericGrd_int => grid_Create ( pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
       pGrd%rX1, pGrd%rY1, DATATYPE_INT )

    pGenericGrd_sgl => grid_Create ( pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
       pGrd%rX1, pGrd%rY1, DATATYPE_REAL )

    ! perform some basic sanity checking on the specified model options
    call model_CheckConfigurationSettings( pGrd, pConfig )

  endif FIRST_YEAR_pt_1

  ! close any existing open time-series files...
  close(LU_TS)

  if(.not. pConfig%lGriddedData) then
  ! Connect to the single-site time-series file
    open ( LU_TS, file=pConfig%sTimeSeriesFilename, &
      status="OLD", iostat=iStat )
    write(UNIT=LU_LOG,FMT=*)  "Opening time series file: ", &
      TRIM(pConfig%sTimeSeriesFilename)
    flush(LU_LOG)
    call Assert ( iStat == 0, &
      "Can't open time-series data file" )
!    pConfig%iCurrentJulianDay = pConfig%iCurrentJulianDay + 1
    call gregorian_date(pConfig%iCurrentJulianDay, &
      iTempYear, iTempMonth, iTempDay)
    pConfig%iYear = iTempYear
    pConfig%iMonth = iTempMonth
    pConfig%iDay = iTempDay
  end if

  ! Zero out monthly and annual accumulators
  call stats_InitializeMonthlyAccumulators()
  call stats_InitializeAnnualAccumulators()

  ! ***************************
  ! ***** BEGIN MAIN LOOP *****
  ! ***************************

  MAIN_LOOP: do

  ! new day: initialize stats accumulators
  call stats_InitializeDailyAccumulators()

  ! blow away any remnant climate values
  pTS%rPrecip = iNO_DATA_NCDC
  pTS%rRH = iNO_DATA_NCDC
  pTS%rMaxT = iNO_DATA_NCDC
  pTS%rMinT = iNO_DATA_NCDC
  pTS%rWindSpd = iNO_DATA_NCDC
  pTS%rMinRH = iNO_DATA_NCDC
  pTS%rSunPct = iNO_DATA_NCDC
  pTS%lEOF = lFALSE

  ! if we are not using gridded climate data, here is where we read in
  ! the current days' values from the single-site time series file.
  if(.not. pConfig%lGriddedData) then

    call model_ReadTimeSeriesFile(pConfig, pTS)
    if(pTS%lEOF) then
      close(unit=LU_TS)
      exit MAIN_LOOP
    end if

  end if

  call LookupMonth(pConfig%iMonth,pConfig%iDay,pConfig%iYear, &
    pConfig%iDayOfYear,sMonthName,lMonthEnd)

  ! initialize (or re-initialize) landuse-associated variables;
  ! must be done whenever a new landuse grid is provided

  call LULC%getvalues( pGrdBase=pGrd, iMonth=pConfig%iMonth, &
    iDay=pConfig%iDay, iYear=pConfig%iYear )

  ! if a new landuse grid is found, then we need to perform a basic reinitialization
  ! of active/inactive cells, total soil water capacity, etc.
  if ( LULC%lGridHasChanged ) then
    pGrd%Cells%iLandUse = pGrd%iData
    LULC%lGridHasChanged = lFALSE

    pGenericGrd_int%iData = pGrd%Cells%iLandUse
    pGenericGrd_int%iMask = pGrd%iMask

    call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Landuse_Grid_" // &
      trim(asCharacter(pConfig%iYear))//"_"//trim(asCharacter(pConfig%iMonth)) &
      //"_"//trim(asCharacter(pConfig%iYear))// &
      "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_int, iOutputFormat=pConfig%iOutputFormat )

    call make_shaded_contour(pGrd=pGenericGrd_int, &
      sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Landuse_Grid_" // &
      trim(asCharacter(pConfig%iYear))//"_"//trim(asCharacter(pConfig%iMonth)) &
      //"_"//trim(asCharacter(pConfig%iYear))//".png", &
      sTitleTxt="Landuse Grid", &
      sAxisTxt="Landuse Code" )

    if (pConfig%lFirstYearOfSimulation) then
      ! read in flow direction, soil group, and AWC grids
      call model_InitializeDataStructures( pGrd, pConfig )
    endif

    call model_setInactiveCells( pGrd, pConfig )

    ! (Re)-initialize the model
    call model_InitializeLanduseRelatedParams( pGrd, pConfig )
    call sm_thornthwaite_mather_UpdatePctSM( pGrd )

    !> @todo Check the logic here. It would seem that a new irrigation table
    !! index *should* be created if we have dynamic data rather than static data
    if (pConfig%iConfigureFAO56 /= CONFIG_FAO56_NONE .and. &
      ( pConfig%iConfigureLanduse /= CONFIG_LANDUSE_STATIC_GRID &
        .and. pConfig%iConfigureLanduse /= CONFIG_LANDUSE_CONSTANT) ) then
      call model_CreateIrrigationTableIndex(pGrd, pConfig )
    endif

    write(UNIT=LU_LOG,FMT=*)  "model.F90: model_InitializeET"
    flush(unit=LU_LOG)
    call model_InitializeET( pGrd, pConfig )

  endif

  FIRST_YEAR_pt_2: if(pConfig%lFirstYearOfSimulation) then

    if (pConfig%iConfigureFAO56 /= CONFIG_FAO56_NONE .and. &
       ( pConfig%iConfigureLanduse == CONFIG_LANDUSE_STATIC_GRID &
        .or. pConfig%iConfigureLanduse == CONFIG_LANDUSE_CONSTANT) ) then
      call model_CreateIrrigationTableIndex(pGrd, pConfig )
    endif

    ! initialize binary and stats output files
    call model_InitializeInputAndOutput( pGrd, pConfig )

   ! Are we solving using the downhill algorithm?
    if ( pConfig%iConfigureRunoffMode == CONFIG_RUNOFF_DOWNHILL ) then
      ! if a routing table exists, read it in; else initialize and
      ! save the routing table for future use
      write(UNIT=LU_LOG,FMT=*) "Configuring runoff for the downhill flow routing option..."
      call model_ConfigureRunoffDownhill( pGrd, pConfig)
    end if

    ! Unless we are *not* routing water, we *must* call InitializeFlowDirection
    if( pConfig%iConfigureRunoffMode /= CONFIG_RUNOFF_NO_ROUTING) then
      write(UNIT=LU_LOG,FMT=*) "Initializing flow direction..."
      call model_InitializeFlowDirection( pGrd , pConfig)
    end if

    pConfig%lFirstYearOfSimulation = lFALSE

  end if FIRST_YEAR_pt_2

  if(pConfig%lFirstDayOfSimulation) then
    ! scan through list of potential output variables; if any
    ! output is desired for a variable, note the current position
    ! within the file, move to the position reserved for the first day's
    ! date, write the date, and return to the position where the data
    ! for the first day will be written
    do k=1,iNUM_VARIABLES
      if(STAT_INFO(k)%iDailyOutput > iNONE &
        .or. STAT_INFO(k)%iMonthlyOutput > iNONE &
        .or. STAT_INFO(k)%iAnnualOutput > iNONE)  then
        inquire(UNIT=STAT_INFO(k)%iLU,POS=iPos)  ! establish location to return to
        write(UNIT=STAT_INFO(k)%iLU,POS=iSTARTDATE_POS) &
          pConfig%iMonth,pConfig%iDay, pConfig%iYear
        write(UNIT=STAT_INFO(k)%iLU,POS=iPos ) ! return to prior location in bin file
      end if
      pConfig%lFirstDayOfSimulation = lFALSE
    end do

  end if

  if(pConfig%lWriteToScreen) then
    write(UNIT=LU_STD_OUT,FMT=*)
    if(pConfig%lANSI_Colors) then
      write(UNIT=LU_STD_OUT,FMT="(1x,a7,a80,a7)") sBOLDWHITE,REPEAT('=',80),sWHITE
    else
      write(UNIT=LU_STD_OUT,FMT="(1x,a80)") REPEAT('=',80)
    end if
    write(UNIT=LU_STD_OUT,FMT="(1x,'DAY: ',i3,4x,A3,4x,i2,'/',i2,'/',i4)") &
      pConfig%iDayOfYear,sMonthName,pConfig%iMonth,pConfig%iDay,pConfig%iYear
!      write(UNIT=LU_STD_OUT,FMT="(1x,a80)") REPEAT('-',80)
    write(UNIT=LU_STD_OUT,FMT=*)
  end if

  ! write timestamp to the unformatted fortran file(s)
  do k=1,iNUM_VARIABLES
    if(STAT_INFO(k)%iDailyOutput > iNONE &
      .or. STAT_INFO(k)%iMonthlyOutput > iNONE &
      .or. STAT_INFO(k)%iAnnualOutput > iNONE)  then
    write(UNIT=STAT_INFO(k)%iLU) pConfig%iDay,pConfig%iMonth, &
      pConfig%iYear, pConfig%iDayOfYear
!    inquire(UNIT=STAT_INFO(k)%iLU, POS=STAT_INFO(k)%iPos)
!    write(UNIT=STAT_INFO(k)%iLU) iNO_DATA_NCDC  ! dummy value for now
    end if
  end do

!***********************************************************************

!  ! Initialize precipitation value for current day
!  call model_GetDailyPrecipValue(pGrd, pConfig, pTS%rPrecip, &
!    pConfig%iMonth, pConfig%iDay, pConfig%iYear, pConfig%iCurrentJulianDay)

  ! Initialize temperature values for current day
!  call model_GetDailyTemperatureValue(pGrd, pConfig, &
!    pTS%rAvgT, pTS%rMinT, pTS%rMaxT, pTS%rRH, &
!    pConfig%iMonth, pConfig%iDay, pConfig%iYear, pConfig%iCurrentJulianDay)

   call model_GetDailyPrecipAndTemperatureValue(pGrd, pConfig, pTS%rPrecip, &
    pTS%rAvgT, pTS%rMinT, pTS%rMaxT, pConfig%iMonth, pConfig%iDay, &
    pConfig%iYear, pConfig%iCurrentJulianDay)

  write(UNIT=LU_LOG,FMT="(1x,'Beginning calculations for day: '," &
    //"i3,4x,A3,4x,i2,'/',i2,'/',i4)") &
    pConfig%iDayOfYear,sMonthName,pConfig%iMonth,pConfig%iDay,pConfig%iYear

  if(pConfig%lWriteToScreen) then
    write(UNIT=LU_STD_OUT,FMT="(t39,a,t53,a,t69,a)") "min","mean","max"
    call stats_WriteMinMeanMax(LU_STD_OUT,"Gross Precipitation (in)" , pGrd%Cells(:,:)%rGrossPrecip )
    call stats_WriteMinMeanMax(LU_STD_OUT,"Minimum Temp (F)" , pGrd%Cells(:,:)%rTMin )
    call stats_WriteMinMeanMax(LU_STD_OUT,"Mean Temp (F)" , pGrd%Cells(:,:)%rTAvg )
    call stats_WriteMinMeanMax(LU_STD_OUT,"Maximum Temp (F)" , pGrd%Cells(:,:)%rTMax )
!      write(UNIT=LU_STD_OUT,FMT="(1x,a80)") REPEAT('-',80)
  write(UNIT=LU_STD_OUT,FMT=*)
end if

  call model_UpdateContinuousFrozenGroundIndex( pGrd , pConfig)

  call model_UpdateGrowingSeason( pGrd, pConfig )

  if(pConfig%iConfigureSnow == CONFIG_SNOW_ORIGINAL_SWB) then
    call model_ProcessRain(pGrd, pConfig, pConfig%iDayOfYear, pConfig%iMonth)
  else if(pConfig%iConfigureSnow == CONFIG_SNOW_NEW_SWB) then
    call model_ProcessRainPRMS(pGrd, pConfig, pConfig%iDayOfYear, &
      pConfig%iMonth, pConfig%iNumDaysInYear)
  else
    call Assert(lFALSE,"Unhandled snow module option specified", &
      TRIM(__FILE__),__LINE__)
  end if

  call model_ProcessET( pGrd, pConfig, pConfig%iDayOfYear, &
    pConfig%iNumDaysInYear, pTS%rRH, pTS%rMinRH, &
    pTS%rWindSpd, pTS%rSunPct )

  if(pConfig%iConfigureFAO56 /= CONFIG_FAO56_NONE ) then
    call model_UpdateGrowingDegreeDay( pGrd , pConfig)
    call et_kc_ApplyCropCoefficients(pGrd, pConfig)
  endif

  if( pConfig%lEnableIrrigation )  call irrigation_UpdateAmounts(pGrd, pConfig)

  call model_ProcessRunoff(pGrd, pConfig, pConfig%iDayOfYear, pConfig%iMonth)

  call calculate_water_balance( pGrd, pConfig, pConfig%iDayOfYear, &
    pConfig%iDay ,pConfig%iMonth, pConfig%iYear)

  ! if desired, output daily mass balance file and daily model grids
  if(pConfig%lWriteToScreen) then
    call stats_DumpDailyAccumulatorValues(LU_STD_OUT, pConfig)
  else
    write(UNIT=LU_STD_OUT,FMT="(a,i2.2,a,i2.2,a,i4.4)") &
      "Simulation day: ",pConfig%iMonth,"/", pConfig%iDay, "/", pConfig%iYear
  end if

  ! if desired, output daily mass balance file and daily model grids
  if ( pConfig%lReportDaily ) then

    call stats_WriteDailyAccumulatorValuesCSV(LU_CSV_MIN,pConfig%iMonth, &
      pConfig%iDay,pConfig%iYear,iMIN)
    call stats_WriteDailyAccumulatorValuesCSV(LU_CSV_MEAN,pConfig%iMonth, &
      pConfig%iDay,pConfig%iYear,iMEAN)
    call stats_WriteDailyAccumulatorValuesCSV(LU_CSV_MAX,pConfig%iMonth, &
      pConfig%iDay,pConfig%iYear,iMAX)
    call stats_WriteMSBReport(pGrd,pConfig%iMonth,pConfig%iDay, &
      pConfig%iYear,pConfig%iDayOfYear)

  end if

  call model_WriteGrids(pGrd=pGrd, pConfig=pConfig, iOutputType=WRITE_ASCII_GRID_DAILY)

  ! Write the results at each month-end
  if ( lMonthEnd ) then

    call model_WriteGrids(pGrd=pGrd, pConfig=pConfig, iOutputType=WRITE_ASCII_GRID_MONTHLY)

  if ( pConfig%lWriteToScreen) call stats_DumpMonthlyAccumulatorValues(LU_STD_OUT, &
    pConfig%iMonth, sMonthName, pConfig)

    write(UNIT=LU_LOG,FMT="(A,i2,A,i4)") &
      "finished monthly calculations for: ", &
        pConfig%iMonth, "/", pConfig%iYear
    flush(UNIT=LU_LOG)

  end if

  !-------------------------------------------------------------------------
  ! time control block follows; if next day is part of a new year, exit loop
  !-------------------------------------------------------------------------
  call gregorian_date(pConfig%iCurrentJulianDay + 1, &
    iTempYear, iTempMonth, iTempDay)

  call MODEL_SIM%addDay()

  if(pConfig%iYear /= iTempYear) then
    close(unit=LU_TS)
    exit MAIN_LOOP
  else
    pConfig%iMonth = iTempMonth
    pConfig%iDay = iTempDay
    pConfig%iCurrentJulianDay = pConfig%iCurrentJulianDay + 1
  end if

  end do MAIN_LOOP


  call model_WriteGrids(pGrd=pGrd, pConfig=pConfig, iOutputType=WRITE_ASCII_GRID_ANNUAL)

  ! model_Solve has been called once... any further calls will not require
  !    re-initialization of data structures and data arrays
  pConfig%lFirstYearOfSimulation = lFALSE

  if(pConfig%lWriteToScreen) &
    call stats_DumpAnnualAccumulatorValues(LU_STD_OUT, pConfig, pConfig%iYear)

  call stats_WriteAnnualAccumulatorValuesCSV(LU_CSV_ANNUAL,pConfig%iYear)

  ! update value of last year
  if( .not. pConfig%lGriddedData) pConfig%iEndYear = pConfig%iYear

  do iIndex=1,iNUM_VARIABLES

    ! write the end date of the simulation (up to this point) into the header of
    ! the binary file (*.bin)
    if(STAT_INFO(iIndex)%iDailyOutput > iNONE &
      .or. STAT_INFO(iIndex)%iMonthlyOutput > iNONE &
      .or. STAT_INFO(iIndex)%iAnnualOutput > iNONE)  then

      inquire(UNIT=STAT_INFO(iIndex)%iLU,POS=iPos)  ! establish location to return to

      write(UNIT=STAT_INFO(iIndex)%iLU,POS=iENDDATE_POS) &
        pConfig%iMonth,pConfig%iDay, pConfig%iYear

      write(UNIT=STAT_INFO(iIndex)%iLU,POS=iPos ) ! return to prior location in bin file

    end if

  end do

  ! update current date so all is well when next years file is opened
  pConfig%iMonth = iTempMonth
  pConfig%iDay = iTempDay
  pConfig%iCurrentJulianDay = pConfig%iCurrentJulianDay + 1


  DEALLOCATE(pTS, STAT=iStat)
  call Assert( iStat == 0, &
    "Could not deallocate memory for time-series data structure")

end subroutine model_Solve



subroutine model_UpdateGrowingSeason( pGrd, pConfig )

  implicit none

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings

  ! [ LOCALS ]
  type (T_CELL),pointer :: cel              ! pointer to a particular cell
  integer (kind=c_int) :: iCol, iRow

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      cel => pGrd%Cells(iCol, iRow)

      if ( cel%lGrowingSeason ) then   ! check for killing frost

        if ( cel%rTMin <= 28. ) cel%lGrowingSeason = lFALSE

      else  ! it is NOT currently growing season; should it be?

        if ( cel%rGDD_28F > 90. ) cel%lGrowingSeason = lTRUE

      endif

    enddo
  enddo

end subroutine model_UpdateGrowingSeason


!--------------------------------------------------------------------------
!!****s* model/model_UpdateGrowingDegreeDay( pGrd )
! NAME
!   model_UpdateGrowingDegreeDay - Updates the growing degree-day
!                                  on a cell-by-cell basis.
! SYNOPSIS
!   Updates the growing degree-day
!   on a cell-by-cell basis.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!
! OUTPUTS
!   NONE
!
! NOTES
!   ** Code refers to parameters that are set within types.f95.
!   ** Code directly modifies model grid values through the manipulation
!       of pointers.
!
! SOURCE
subroutine model_UpdateGrowingDegreeDay( pGrd , pConfig)

  implicit none

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  ! [ LOCALS ]
  real (kind=c_float) :: rDD                        ! daily departure from TBase
  type (T_CELL),pointer :: cel                      ! pointer to a particular cell
  real (kind=c_float) :: rA, rAt
  real (kind=c_float) :: rTMax
  real (kind=c_float) :: rW
  integer (kind=c_int) :: iCol,iRow
  real (kind=c_float) :: rGDD_BaseTemp, rGDD_MaxTemp
  logical (kind=c_bool) :: lAssertTest

  ! zero out growing degree day at start of calendar year
!  if(pConfig%iDayOfYear == 1) pGrd%Cells%rGDD = 0.

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX  ! last subscript in a Fortran array should be the slowest-changing
      cel => pGrd%Cells(iCol,iRow)

      if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle

      ! cap the maximum value used in GDD calculations on the basis of the value
      ! provided by user...

      lAssertTest = cel%iLandUseIndex >= 1 .and. cel%iLandUseIndex <= pConfig%iNumberOfLanduses

      if(.not. lAssertTest) &
        call assert(lAssertTest, &
        "Array index out of bounds. Variable is iLandUseIndex with a value of " &
        //trim(int2char(cel%iLandUseIndex)), trim(__FILE__),__LINE__)

      rGDD_BaseTemp = pConfig%IRRIGATION(cel%iLandUseIndex)%rGDD_BaseTemp
      rGDD_MaxTemp = pConfig%IRRIGATION(cel%iLandUseIndex)%rGDD_MaxTemp

      rTMax = min(rGDD_MaxTemp, cel%rTMax)

      if(rTMax <= rGDD_BaseTemp) then

        rDD = 0.

      elseif(cel%rTMin >= rGDD_BaseTemp) then

        rDD = cel%rTAvg - rGDD_BaseTemp

      else

        rW = (rTMax - cel%rTMin) / 2.

        rAt = ( rGDD_BaseTemp - cel%rTAvg) / rW

        if(rAt > 1) rAt = 1.
        if(rAt < -1) rAt = -1.

        rA = asin(rAt)

        rDD = (( rW * cos(rA)) - ((rGDD_BaseTemp - cel%rTAvg) &
               * ((dpPI / 2.) - rA))) / dpPI

      end if

      cel%rGDD = cel%rGDD + rDD

    end do

  end do

end subroutine model_UpdateGrowingDegreeDay

!!***
!--------------------------------------------------------------------------
!!****s* model/model_ProcessRain
! NAME
!   model_ProcessRain - Processes the daily rainfall. Upon return, the
!                       daily precipitation is corrected to account for
!                       snow accumulation / snowmelt.
!
! SYNOPSIS
!   Processes daily precipitation values. Daily precipitation values
!   are altered to account for interception and with regard to the
!   form that the precipitation takes (rain or snow). If the form of the
!   precipitation is snow, the net precipitation value is zeroed out, and
!   the precipitation value is moved into the snowfall value.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!   pConfig - Pointer to the model configuration data structure.
!   iDayOfYear - Day of the current year (January 1 = 1).
!   iMonth - Month corresponding to the current model day (January = 1).
!
! OUTPUTS
!   NONE
!
! NOTES
!   ** Code refers to parameters that are set within types.f95.
!   ** Code directly modifies model grid values through the manipulation
!       of pointers.
!
! SOURCE

subroutine model_ProcessRain( pGrd, pConfig, iDayOfYear, iMonth)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  integer (kind=c_int),intent(in) :: iDayOfYear  ! Day of the year
  integer (kind=c_int), intent(in) :: iMonth     ! Integer month value (1-12)
  ! [ LOCALS ]
  real (kind=c_double) :: dpPotentialMelt,dpPotentialInterception,dpInterception
  real (kind=c_double) :: dpPreviousSnowCover,dpChgInSnowCover, dpSnowCover
  real (kind=c_double) :: dpNetPrecip    ! all forms of precip, after interception
  real (kind=c_double) :: dpNetRainfall  ! precip as RAINFALL, after interception
  integer (kind=c_int) :: iRow, iCol
  type (T_CELL),pointer :: cel
  integer (kind=c_int) :: iNumGridCells
  real (kind=c_double) :: rMin, rMean, rMax, rSum, rSum2
  integer (kind=c_int) :: iRowCount
  real (kind=c_float) ::  rMonthlySnowRunoff
  logical (kind=c_bool) :: lFREEZING

  ! [ LOCAL PARAMETERS ]
  real (kind=c_float), parameter :: rMELT_INDEX = 1.5_c_float

  ! set snowmelt to zero uniformly across model grid
  pGrd%Cells(:,:)%rSnowMelt = rZERO

  ! set snowfall to zero uniformly across model grid
  pGrd%Cells(:,:)%rSnowFall_SWE = rZERO

  ! calculate number of cells in model grid
  iNumGridCells = pGrd%iNumGridCells

  ! Use "potential interception" for each cell to compute net precip

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      cel => pGrd%Cells(iCol,iRow)

      if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) then

        dpChgInSnowCover = dpZERO
        dpSnowCover = dpZERO
        dpPreviousSnowCover = dpZERO
        dpNetRainfall = dpZERO

      else

        ! allow for correction factor to be applied to precip gage input data
        if ( cel%rTAvg - (cel%rTMax-cel%rTMin) / 3.0_c_float <= rFREEZING ) then
          lFREEZING = lTRUE
          cel%rGrossPrecip = cel%rGrossPrecip * pConfig%rSnowFall_SWE_Corr_Factor
        else
          lFREEZING = lFALSE
          cel%rGrossPrecip = cel%rGrossPrecip * pConfig%rRainfall_Corr_Factor
        end if

        ! this simply retrieves the table value for the given landuse
        dpPotentialInterception = rf_model_GetInterception(pConfig,cel)

        ! save the current snowcover value, create local copy as well
        dpPreviousSnowCover = real(cel%rSnowCover, kind=c_double)
        dpSnowCover = real(cel%rSnowCover, kind=c_double)

        ! calculate NET PRECIPITATION; assign value of zero if all of the
        ! GROSS PRECIP is captured by the INTERCEPTION process
        dpNetPrecip = real(cel%rGrossPrecip, kind=c_double) - dpPotentialInterception

        if ( dpNetPrecip < dpZERO ) dpNetPrecip = dpZERO

        dpInterception = real(cel%rGrossPrecip, kind=c_double) - dpNetPrecip

        ! negative interception can only be generated if the user has supplied
        ! *negative* values for GROSS PRECIPITATION; this has happened,
        ! mostly due to interpolation schemes that generate pockets
        ! of negative precip values
        if(dpInterception < dpZERO) &
          call Assert(lFALSE, &
            "Negative value for interception was calculated on day " &
            //int2char(iDayOfYear)//" iRow: "//trim(int2char(iRow)) &
            //"  iCol: "//trim(int2char(iCol)), &
            trim(__FILE__), __LINE__)

        cel%rInterception = real(dpInterception, kind=c_double)
!      cel%rInterceptionStorage = cel%rInterceptionStorage + cel%rInterception

        ! NOW we're through with INTERCEPTION calculations
        ! Next, we partition between snow and rain

        ! Assume that all precipitation is rain, for now
        dpNetRainfall = dpNetPrecip

        ! Is it snowing?
        if (lFREEZING ) then
          dpSnowCover = dpSnowCover + dpNetPrecip
          cel%rSnowFall_SWE = dpNetPrecip
          dpNetRainfall = dpZERO      ! For now -- if there is snowmelt, we do it next
        end if

        ! Is there any melting?
        if ( cel%rTAvg > rFREEZING ) then
          dpPotentialMelt = rMELT_INDEX * ( cel%rTMax - rFREEZING ) &
                            * dpC_PER_F / rMM_PER_INCH

          if(dpSnowCover > dpPotentialMelt) then
            cel%rSnowMelt = dpPotentialMelt
            dpSnowCover = dpSnowCover - dpPotentialMelt
          else   ! not enough snowcover to satisfy the amount that *could* melt
            cel%rSnowMelt = dpSnowCover
            dpSnowCover = dpZERO
          end if

        end if

        dpChgInSnowCover = dpSnowCover - dpPreviousSnowCover

        ! copy temporary double-precision values back to single-precision
        cel%rSnowCover = real(dpSnowCover, kind=c_float)
        cel%rNetRainfall = real(dpNetRainfall, kind=c_float)

      endif

      call stats_UpdateAllAccumulatorsByCell( &
        REAL(dpChgInSnowCover,kind=c_double), iCHG_IN_SNOW_COV,iMonth,iZERO)

      call stats_UpdateAllAccumulatorsByCell( &
        dpNetRainfall,iNET_RAINFALL,iMonth,iZERO)

      call stats_UpdateAllAccumulatorsByCell( &
        REAL(cel%rSnowMelt,kind=c_double),iSNOWMELT,iMonth,iZERO)

      call stats_UpdateAllAccumulatorsByCell( &
        REAL(cel%rSnowFall_SWE,kind=c_double),iSNOWFALL_SWE,iMonth,iZERO)

      call stats_UpdateAllAccumulatorsByCell( &
        dpSnowCover,iSNOWCOVER,iMonth,iZERO)

    end do

  end do

  ! a call to the UpdateAllAccumulatorsByCell subroutine with a value of "iNumGridCalls"
  ! as the final argument triggers the routine to update monthly and annual stats
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iCHG_IN_SNOW_COV,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iNET_RAINFALL,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iSNOWMELT,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iSNOWFALL_SWE,iMonth,iNumGridCells)
  call stats_UpdateAllAccumulatorsByCell(dpZERO,iSNOWCOVER,iMonth,iNumGridCells)

end subroutine model_ProcessRain


!!***

!--------------------------------------------------------------------------
!!****s* model/model_ProcessRunoff
! NAME
!   model_ProcessRunoff - Calls method-specific subroutines to handle
!                         surface runoff calculation.
!
! SYNOPSIS
!   This subroutine calls the appropriate subroutine for calculating
!   surface runoff based on whether the user has selected the iterative
!   or downhill solution.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!   pConfig - Pointer to the model configuration data structure.
!   iDayOfYear - Day of the current year (January 1 = 1).
!   iMonth - Month corresponding to the current model day (January = 1).
!
! OUTPUTS
!   NONE
!
! NOTES
!   ** Code refers to parameters that are set within types.f95.
!   ** Code directly modifies model grid values through the manipulation
!       of pointers.
!
! SOURCE

subroutine model_ProcessRunoff(pGrd, pConfig, iDayOfYear, iMonth)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  integer (kind=c_int),intent(in) :: iDayOfYear    ! day of current year (January 1 = 1)
  ! [ LOCALS ]
  integer (kind=c_int) :: iCount
  integer (kind=c_int) :: j, i
  real (kind=c_double) :: xmin, xmax, ymin, ymax
  integer (kind=c_int), intent(in) :: iMonth     ! Integer month value (1-12)
  integer (kind=c_int) :: iNumGridCells
  integer (kind=c_int), parameter :: iMAX_ITERATIONS = 200000
  integer (kind=c_int) :: iIterationNum

  ! calculate number of cells in model grid
  iNumGridCells = pGrd%iNX * pGrd%iNY

  ! Iteratively processes the runoff event; first initialize the upstream flows
  pGrd%Cells(:,:)%rInFlow = rZERO
  pGrd%Cells(:,:)%rOutFlow = rZERO
  pGrd%Cells(:,:)%rFlowOutOfGrid = rZERO
  iIterationNum = 0

  if ( pConfig%iConfigureRunoffMode == CONFIG_RUNOFF_ITERATIVE ) then

    do
      iIterationNum = iIterationNum + 1
      iCount = if_model_RunoffIteration( pGrd, pConfig, iDayOfYear, iMonth )
      if ( iCount == 0 ) then
        exit
      endif
      if (iIterationNum > iMAX_ITERATIONS) then
        call assert(lFALSE, "Maximum number of iterations exceeded.", &
        trim(__FILE__), __LINE__)
      endif
    end do

  else if (pConfig%iConfigureRunoffMode == CONFIG_RUNOFF_DOWNHILL ) then

    call model_RunoffDownhill( pGrd, pConfig, iDayOfYear, iMonth )

  else if (pConfig%iConfigureRunoffMode == CONFIG_RUNOFF_NO_ROUTING ) then
    call model_Runoff_NoRouting( pGrd, pConfig, iDayOfYear, iMonth )

  else
    call Assert(lFALSE,"Internal error selecting a runoff routing module" &
      //" runoff mode = "//TRIM(int2char(pConfig%iConfigureRunoffMode)), &
      TRIM(__FILE__),__LINE__)
  end if

  ! Update the moving average counter
  iDayCtr = iDayCtr + 1
  if ( iDayCtr > iMOVING_AVG_TERMS ) iDayCtr = 1

  ! Update the inflow buffer (used to determine antecedent runoff conditions)
  pGrd%Cells(:,:)%rNetInflowBuf(iDayCtr) = pGrd%Cells(:,:)%rNetPrecip &
    + pGrd%Cells(:,:)%rSnowMelt + pGrd%Cells(:,:)%rInflow

  return
end subroutine model_ProcessRunoff


!--------------------------------------------------------------------------

function rf_model_GetInterception( pConfig, cel ) result(rIntRate)
  !! Looks up the interception value for land-use type iType.

  ! [ ARGUMENTS ]
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  type (T_CELL),pointer :: cel

  ! [ RETURN VALUE ]
  real (kind=c_float) :: rIntRate

  ! [ LOCALS ]
  integer ( kind=c_int ) :: i
  logical ( kind=c_bool ) :: lAssertTest
  type ( T_LANDUSE_LOOKUP ),pointer :: pLU

  lAssertTest = cel%iLandUseIndex >= 1 .and. cel%iLandUseIndex <= pConfig%iNumberOfLanduses

  if(.not. lAssertTest) &
    call assert(lAssertTest, &
      "Array index out of bounds. Variable is iLandUseIndex with a value of " &
      //trim(int2char(cel%iLandUseIndex)), trim(__FILE__),__LINE__)

  pLU => pConfig%LU(cel%iLandUseIndex)

  ! Default is zero
  rIntRate = rZERO
  if ( cel%lGrowingSeason ) then
    rIntRate = pLU%rIntercept_GrowingSeason
  else
    rIntRate = pLU%rIntercept_NonGrowingSeason
  end if

  if (rIntRate < rZero) then

    call echolog("Negative interception value encountered. Check your lookup tables." &
      //"~landuse code: "//trim(asCharacter(pLU%iLanduseType)) &
      //"~landuse description: "//trim(pLU%sLanduseDescription) )

    call Assert(lFALSE, "")
  endif

end function rf_model_GetInterception

!--------------------------------------------------------------------------




end module model
