module model_initialize

  use iso_c_binding, only : c_short, c_int, c_float, c_double, c_long_long
  use types

  implicit none

  private

  !! For the "downhill" solution
  integer (kind=c_int) :: iOrderCount
  integer (kind=c_int), dimension(:), allocatable :: iOrderCol
  integer (kind=c_int), dimension(:), allocatable :: iOrderRow

contains



subroutine model_CheckConfigurationSettings( pGrd, pConfig )

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd               ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains


  call assert(FLOWDIR%iSourceDataType /= DATATYPE_NA, &
    "No flow direction information has been specified. If you are not" &
    //"~routing flow, add a directive such as 'FLOW_DIRECTION CONSTANT 1'" &
    //" to your ~control file." )

  call assert(LULC%iSourceDataType /= DATATYPE_NA, &
    "No landuse data grid has been specified. If you have only" &
    //"~a single landuse type, add a directive such as 'LANDUSE CONSTANT 21'" &
    //"~to your control file." )

  call assert(AWC%iSourceDataType /= DATATYPE_NA, &
    "No available water capacity grid has been specified. If you do not presently" &
    //"~have an AWC grid, you may add a directive such as 'WATER_CAPACITY CONSTANT 2.6'" &
    //"~to your control file in order to run SWB." )

  call assert(HSG%iSourceDataType /= DATATYPE_NA, &
    "No hydrologic soils group grid has been specified. If you do not presently" &
    //"~have an HSG grid, you may add a directive such as 'HYDROLOGIC_SOIL_GROUP CONSTANT 1'" &
    //"~to your control file in order to run SWB." )

  if (pConfig%lEnableIrrigation .and. pConfig%iConfigureFAO56 == CONFIG_FAO56_NONE ) then
    call assert( lFALSE, "The irrigation module must be used with one of the FAO-56 crop~" &
      //"coefficient submodels enabled. These can be enabled by adding one of the following~" &
      //"to your control file:~"//sTAB//"~" &
      //sTAB//"FAO56 CROP_COEFFICIENTS_ONE_FACTOR_STANDARD~" &
      //sTAB//"FAO56 CROP_COEFFICIENTS_TWO_FACTOR_STANDARD~" &
      //sTAB//"FAO56 CROP_COEFFICIENTS_ONE_FACTOR_NONSTANDARD~" &
      //sTAB//"FAO56 CROP_COEFFICIENTS_TWO_FACTOR_NONSTANDARD~")
  endif

  if ( pConfig%lGriddedData ) then

    call assert( pConfig%iConfigureTemperature /= CONFIG_NONE, &
      "No temperature data have been specified. A data source for both the minumum~" &
      //"and maximum air temperature must be specified in order to run SWB." )

    call assert( pConfig%iConfigurePrecip /= CONFIG_NONE, &
      "No precipitation data have been specified. A data source for ~" &
      //"daily precipitation must be specified in order to run SWB." )

  endif

end subroutine model_CheckConfigurationSettings


subroutine model_InitializeFlowDirection( pGrd , pConfig)
  !! Scans the flow direction grid for closed depressions and marks them
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd         ! pointer to model grid

  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings

  ! [ LOCALS ]
  integer (kind=c_int) :: iRow,iCol
  integer (kind=c_int) :: iTgt_Row,iTgt_Col
  type (T_CELL),pointer :: cel
  character (len=256) :: sBuf

  ! [ PARAMETERS ]
  integer (kind=c_short),parameter :: DIR_DEPRESSION=0
  integer (kind=c_short),parameter :: DIR_RIGHT=1
  integer (kind=c_short),parameter :: DIR_DOWN_RIGHT=2
  integer (kind=c_short),parameter :: DIR_DOWN=4
  integer (kind=c_short),parameter :: DIR_DOWN_LEFT=8
  integer (kind=c_short),parameter :: DIR_LEFT=16
  integer (kind=c_short),parameter :: DIR_UP_LEFT=32
  integer (kind=c_short),parameter :: DIR_UP=64
  integer (kind=c_short),parameter :: DIR_UP_RIGHT=128

  ! no point in doing these calculations unless we're really going to
  ! route water
  if(pConfig%iConfigureRunoffMode==CONFIG_RUNOFF_NO_ROUTING) return

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX

      cel => pGrd%Cells(iCol,iRow)

      if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) then

       cel%iTgt_Row = iROUTE_LEFT_GRID
       cel%iTgt_Col = iROUTE_LEFT_GRID
       cycle

     endif

  select case (pGrd%Cells(iCol,iRow)%iFlowDir)
    case ( DIR_DEPRESSION )
      iTgt_Col = iROUTE_DEPRESSION               ! added Jan 2009 SMW
      iTgt_Row = iROUTE_DEPRESSION               ! added Jan 2009 SMW
      continue
    case ( DIR_RIGHT )
      iTgt_Row = iRow
      iTgt_Col = iCol+1
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_LEFT ) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case ( DIR_DOWN_RIGHT )
      iTgt_Row = iRow+1
      iTgt_Col = iCol+1
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_UP_LEFT ) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case ( DIR_DOWN )
      iTgt_Row = iRow+1
      iTgt_Col = iCol
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_UP ) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case ( DIR_DOWN_LEFT )
      iTgt_Row = iRow+1
      iTgt_Col = iCol-1
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_UP_RIGHT ) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case ( DIR_LEFT )
      iTgt_Row = iRow
      iTgt_Col = iCol-1
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_RIGHT) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case ( DIR_UP_LEFT )
      iTgt_Row = iRow-1
      iTgt_Col = iCol-1
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_DOWN_RIGHT) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case ( DIR_UP )
      iTgt_Row = iRow-1
      iTgt_Col = iCol
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_DOWN) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case ( DIR_UP_RIGHT )
      iTgt_Row = iRow-1
      iTgt_Col = iCol+1
      if ( iTgt_Row >= 1 .and. iTgt_Row <= pGrd%iNY .and. &
        iTgt_Col >= 1 .and. iTgt_Col <= pGrd%iNX ) then
      if ( pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir == DIR_DOWN_LEFT) then
        pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
        pGrd%Cells(iTgt_Col,iTgt_Row)%iFlowDir = DIR_DEPRESSION
        write(UNIT=LU_LOG,FMT=*) 'depression found in cell (row, col): ',iRow,iCol
      end if
      end if
    case default  !! flow direction indeterminate
    !!
    !!  NOTE: This may not be the correct way to deal with indeterminate
    !!        flow directions!!
    !!
      write ( unit=sBuf, fmt='("Flow direction grid element (",i5,",",i5,' &
        // '") contains undefined flow direction with integer value: ",i4)' ) &
          iCol,iRow,pGrd%Cells(iCol,iRow)%iFlowDir
      write(UNIT=LU_LOG,FMT=*)  sBuf
      pGrd%Cells(iCol,iRow)%iFlowDir = DIR_DEPRESSION
      iTgt_Col = iROUTE_DEPRESSION
      iTgt_Row = iROUTE_DEPRESSION
  end select

  ! does the current value of either target point outside of the grid?
  if ( iTgt_Row == 0 .or. iTgt_Row > pGrd%iNY .or. &
    iTgt_Col == 0 .or. iTgt_Col > pGrd%iNX ) then
      iTgt_Row = iROUTE_LEFT_GRID
      iTgt_Col = iROUTE_LEFT_GRID
  end if

  ! now assign the value of the targets to the iTgt element of the
  ! grid data structure
  pGrd%Cells(iCol,iRow)%iTgt_Row = iTgt_Row
  pGrd%Cells(iCol,iRow)%iTgt_Col = iTgt_Col
end do
end do

#ifdef DEBUG_PRINT

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      if(pGrd%Cells(iCol,iRow)%iTgt_Col==iCol .and. pGrd%Cells(iCol,iRow)%iTgt_Row==iRow) then
        write(unit=LU_LOG,FMT=*) 'ALERT** target is the same as the originating cell'
        write(unit=LU_LOG,FMT=*) '  ORIG   (iRow, iCol) : ',iRow, iCol
        write(unit=LU_LOG,FMT=*) '  ==> FLOWDIR: ',pGrd%Cells(iCol,iRow)%iFlowDir
        write(unit=LU_LOG,FMT=*) '  TARGET (iRow, iCol) : ',pGrd%Cells(iCol,iRow)%iTgt_Row, &
          pGrd%Cells(iCol,iRow)%iTgt_Col
        write(unit=LU_LOG,FMT=*) '  ==> FLOWDIR: ' , &
          pGrd%Cells(pGrd%Cells(iCol,iRow)%iTgt_Row,pGrd%Cells(iCol,iRow)%iTgt_Col)%iFlowDir
      end if
    end do
end do

#endif

  return
end subroutine model_InitializeFlowDirection


subroutine model_DownstreamCell(pGrd,iRow,iCol,iTgt_Row,iTgt_Col)
  !! Determines the "downstream" cell for cell (iRow,iCol) and returns the index in
  !! (iTgt_Row,iTgt_Col)
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd        ! pointer to model grid
  integer (kind=c_int),intent(in) :: iRow,iCol
  integer (kind=c_int),intent(out) :: iTgt_Row,iTgt_Col
  ! [ PARAMETERS ]
  integer (kind=c_short),parameter :: DIR_DEPRESSION=0
  integer (kind=c_short),parameter :: DIR_RIGHT=1
  integer (kind=c_short),parameter :: DIR_DOWN_RIGHT=2
  integer (kind=c_short),parameter :: DIR_DOWN=4
  integer (kind=c_short),parameter :: DIR_DOWN_LEFT=8
  integer (kind=c_short),parameter :: DIR_LEFT=16
  integer (kind=c_short),parameter :: DIR_UP_LEFT=32
  integer (kind=c_short),parameter :: DIR_UP=64
  integer (kind=c_short),parameter :: DIR_UP_RIGHT=128

  select case (pGrd%Cells(iCol,iRow)%iFlowDir)
    case ( DIR_DEPRESSION )
      iTgt_Row = iROUTE_DEPRESSION  ! value is -999
      iTgt_Col = iROUTE_DEPRESSION  ! value is -999
    case ( DIR_RIGHT )
      iTgt_Row = iRow
      iTgt_Col = iCol+1
    case ( DIR_DOWN_RIGHT )
      iTgt_Row = iRow+1
      iTgt_Col = iCol+1
    case ( DIR_DOWN )
      iTgt_Row = iRow+1
      iTgt_Col = iCol
    case ( DIR_DOWN_LEFT )
      iTgt_Row = iRow+1
      iTgt_Col = iCol-1
    case ( DIR_LEFT )
      iTgt_Row = iRow
      iTgt_Col = iCol-1
    case ( DIR_UP_LEFT )
      iTgt_Row = iRow-1
      iTgt_Col = iCol-1
    case ( DIR_UP )
      iTgt_Row = iRow-1
      iTgt_Col = iCol
    case ( DIR_UP_RIGHT )
      iTgt_Row = iRow-1
      iTgt_Col = iCol+1
  end select

  ! the following code was trapping all 'iROUTE_DEPRESSION' values and
  ! converting them to iROUTE_LEFT_GRID values
  !
  ! changed test from "iTgt_Row < 1" to "iTgt_Row == 0"

  if ( iTgt_Row == 0 .or. iTgt_Row > pGrd%iNY .or. &
    iTgt_Col == 0 .or. iTgt_Col > pGrd%iNX ) then    ! Left the grid?
  iTgt_Row = iROUTE_LEFT_GRID
  iTgt_Col = iROUTE_LEFT_GRID
  end if

  return
end subroutine model_DownstreamCell



subroutine model_InitializeDataStructures( pGrd, pConfig )

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd               ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains

  call FLOWDIR%getvalues( pGrdBase=pGrd)
  pGrd%Cells%iFlowDir = pGrd%iData

  pGenericGrd_int%iData = pGrd%Cells%iFlowDir

  where(pGenericGrd_int%iData< 0)
    pGenericGrd_int%iMask = iINACTIVE_CELL
  elsewhere
    pGenericGrd_int%iMask = iACTIVE_CELL
  endwhere

  call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Flow_Direction_Grid" // &
    "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_int, iOutputFormat=pConfig%iOutputFormat )

  call make_shaded_contour(pGrd=pGenericGrd_int, &
     sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Flow_Direction_Grid.png", &
     sTitleTxt="D8 Flow Direction Grid", &
     sAxisTxt="Flow Direction" )

  call HSG%getvalues( pGrdBase=pGrd)
  pGrd%Cells%iSoilGroup = pGrd%iData

  where(pGenericGrd_int%iData< 0)
    pGenericGrd_int%iMask = iINACTIVE_CELL
  elsewhere
    pGenericGrd_int%iMask = iACTIVE_CELL
  endwhere

  pGenericGrd_int%iData = pGrd%Cells%iSoilGroup
  call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Hydrologic_Soils_Group" // &
    "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_int, iOutputFormat=pConfig%iOutputFormat )

  call make_shaded_contour(pGrd=pGenericGrd_int, &
      sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Hydrologic_Soils_Group.png", &
      sTitleTxt="Hydrologic Soils Group", &
      sAxisTxt="HSG" )

  call AWC%getvalues( pGrdBase=pGrd)
  pGrd%Cells%rSoilWaterCapInput = pGrd%rData

  write(LU_LOG, fmt="(a, f14.3)") "  Minimum AWC: ", minval(pGrd%Cells%rSoilWaterCapInput)
  write(LU_LOG, fmt="(a, f14.3)") "  Maximum AWC: ", maxval(pGrd%Cells%rSoilWaterCapInput)

  pGenericGrd_sgl%rData = pGrd%Cells%rSoilWaterCapInput

  where(pGenericGrd_sgl%rData< 0)
    pGenericGrd_sgl%iMask = iINACTIVE_CELL
  elsewhere
    pGenericGrd_sgl%iMask = iACTIVE_CELL
  endwhere

  call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Available_Water_Capacity" // &
    "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_sgl, iOutputFormat=pConfig%iOutputFormat )

  call make_shaded_contour(pGrd=pGenericGrd_sgl, &
     sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Available_Water_Capacity.png", &
     sTitleTxt="Available Water Capacity", &
     sAxisTxt="AWC (inches per foot)" )

  if (.not. associated(ROUTING_FRAC) ) then

  elseif (ROUTING_FRAC%iSourceDataType /= DATATYPE_NA) then

    call ROUTING_FRAC%getvalues( pGrdBase=pGrd)
    pGrd%Cells%rRouteFraction = pGrd%rData

    write(LU_LOG, fmt="(a, f14.3)") "  Minimum routing fraction: ", minval(pGrd%Cells%rRouteFraction)
    write(LU_LOG, fmt="(a, f14.3)") "  Maximum routing fraction: ", maxval(pGrd%Cells%rRouteFraction)

    pGenericGrd_sgl%rData = pGrd%Cells%rRouteFraction

    where(pGenericGrd_sgl%rData< 0)
      pGenericGrd_sgl%iMask = iINACTIVE_CELL
    elsewhere
      pGenericGrd_sgl%iMask = iACTIVE_CELL
    endwhere

    call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Routing_Fraction" // &
      "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_sgl, iOutputFormat=pConfig%iOutputFormat )

    call make_shaded_contour(pGrd=pGenericGrd_sgl, &
       sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Routing_Fraction.png", &
       sTitleTxt="Routing Fraction", &
       sAxisTxt="Routing Fraction (unitless)" )

  endif

  if (.not. associated(MASK)) then

  elseif (MASK%iSourceDataType /= DATATYPE_NA) then

    call MASK%getvalues( pGrdBase=pGrd)

    where ( pGrd%iData > 0 )
      pGrd%iMask = iACTIVE_CELL
    elsewhere
      pGrd%iMask = iINACTIVE_CELL
    endwhere

    pGenericGrd_int%iData = pGrd%iData
    call grid_WriteGrid(sFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Basin_Mask" // &
      "."//trim(pConfig%sOutputFileSuffix), pGrd=pGenericGrd_int, iOutputFormat=pConfig%iOutputFormat )

    call make_shaded_contour(pGrd=pGenericGrd_int, &
       sOutputFilename=trim(pConfig%sOutputFilePrefix) // "INPUT_Basin_mask.png", &
       sTitleTxt="Input Basin Mask", &
       sAxisTxt="Basin Mask (unitless)" )

  endif

end subroutine model_InitializeDataStructures

!--------------------------------------------------------------------------

subroutine model_InitializeInputAndOutput( pGrd, pConfig )

    ! [ ARGUMENTS ]
    type ( T_GENERAL_GRID ),pointer :: pGrd               ! pointer to model grid
    type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains

   ! [ LOCALS ]
   integer (kind=c_int) :: iStat

  call stats_OpenBinaryFiles(pConfig, pGrd)

  call stats_InitializeVolumeConversion(pGrd)

#ifdef DEBUG_PRINT
  call grid_WriteArcGrid("SSF_Grid_Cells."//trim(pConfig%sOutputFileSuffix), &
    pGrd%rX0, pGrd%rX1,pGrd%rY0,pGrd%rY1,REAL(pGrd%Cells(:,:)%iNumFilesSSF) )
#endif

  ! open file into which daily summaries of variables will be written
  if ( pConfig%lReportDaily ) call stats_OpenMSBReport()

  ! open CSV file for daily stats summary
  if ( pConfig%lReportDaily ) then
    open(LU_CSV_MIN, file='SWB_daily_MINIMUM_values.csv',iostat=iStat,&
      status='REPLACE')
    open(LU_CSV_MEAN, file='SWB_daily_MEAN_values.csv',iostat=iStat,&
      status='REPLACE')
    open(LU_CSV_MAX, file='SWB_daily_MAXIMUM_values.csv',iostat=iStat,&
      status='REPLACE')

    call Assert(iStat == 0, &
      "Problem opening CSV files for summary statistics output.")

    call stats_WriteDailyAccumulatorHeaderCSV(LU_CSV_MIN,iMIN)
    call stats_WriteDailyAccumulatorHeaderCSV(LU_CSV_MEAN,iMEAN)
    call stats_WriteDailyAccumulatorHeaderCSV(LU_CSV_MAX,iMAX)
  end if

  ! open CSV file for annual stats summary
  open(LU_CSV_ANNUAL, file='SWB_annual_statistics.csv',iostat=iStat,&
    status='REPLACE')
  call Assert(iStat == 0, &
    "Problem opening CSV file for summary annual statistics output.")
  call stats_WriteAnnualAccumulatorHeaderCSV(LU_CSV_ANNUAL)

end subroutine model_InitializeInputAndOutput

!----------------------------------------------------------------------

subroutine model_InitializeRunoff( pGrd, pConfig )

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd               ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains


  ! If we are routing water, we *must* call InitializeFlowDirection
  if( pConfig%iConfigureRunoffMode /= CONFIG_RUNOFF_NO_ROUTING) then
    write(UNIT=LU_LOG,FMT=*)  "model.F90: model_InitializeFlowDirection"
    call model_InitializeFlowDirection( pGrd , pConfig)
  end if

  ! Are we solving using the downhill algorithm?
  if ( pConfig%iConfigureRunoffMode == CONFIG_RUNOFF_DOWNHILL ) then
    ! if a routing table exists, read it in; else initialize and
    ! save the routing table for future use
    write(UNIT=LU_LOG,FMT=*)  "model.F90: model_ConfigureRunoffDownhill"
    call model_ConfigureRunoffDownhill( pGrd, pConfig)
  end if

end subroutine model_InitializeRunoff

!----------------------------------------------------------------------

subroutine model_InitializeLanduseRelatedParams( pGrd, pConfig )

  type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings

  write(UNIT=LU_LOG,FMT=*)  "model.F90: model_CreateLanduseIndex"
  flush(unit=LU_LOG)
  call model_CreateLanduseIndex(pGrd, pConfig )

  write(UNIT=LU_LOG,FMT=*) "model.F90: calling model_InitializeSM"
  flush(unit=LU_LOG)
  call model_InitializeSM(pGrd, pConfig)

  write(UNIT=LU_LOG,FMT=*)  "model.F90: runoff_InitializeCurveNumber"
  flush(unit=LU_LOG)
  call runoff_InitializeCurveNumber( pGrd ,pConfig)

end subroutine model_InitializeLanduseRelatedParams

!--------------------------------------------------------------------------

subroutine model_InitializeET( pGrd, pConfig )
  !! Depending on the ET model in use, initializes the values for ET
  !! calculations.
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings


  write(UNIT=LU_LOG,FMT=*)  "model_InitializeET : "
  write(UNIT=LU_LOG,FMT=*)  "  filename = ", TRIM(pConfig%sTimeSeriesFilename)

  select case ( pConfig%iConfigureET )
    case ( CONFIG_ET_NONE )
      call Assert( .false._c_bool, "No ET configuration was specified" )
    case ( CONFIG_ET_THORNTHWAITE_MATHER )
      call et_tm_initialize ( pGrd, pConfig, pConfig%sTimeSeriesFilename)
    case ( CONFIG_ET_TURC )
      call et_turc_initialize ( pGrd, pConfig%sTimeSeriesFilename)
    case ( CONFIG_ET_JENSEN_HAISE )
      call et_jh_initialize ( pGrd, pConfig%sTimeSeriesFilename)
    case ( CONFIG_ET_BLANEY_CRIDDLE )
      call et_bc_initialize ( pGrd, pConfig%sTimeSeriesFilename)
    case ( CONFIG_ET_HARGREAVES )

  end select

end subroutine model_InitializeET


!--------------------------------------------------------------------------

subroutine model_setInactiveCells( pGrd, pConfig )

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd               ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig      ! pointer to data structure that contains

  where ( pGrd%Cells%iSoilGroup <= 0 .or. &
          pGrd%Cells%iFlowDir < 0 .or. &
          pGrd%Cells%iLandUse <= 0)

    pGrd%iMask = iINACTIVE_CELL

  elsewhere

    pGrd%iMask = iACTIVE_CELL

  endwhere

  call echolog("Finished converting cells with missing data (negative values) to inactive cells." &
    //"~ A total of "//trim(asCharacter(count(pGrd%iMask==iINACTIVE_CELL))) &
    //" cells were inactivated out of "//trim(asCharacter(pConfig%iNumGridCells))//" cells.")

end subroutine model_setInactiveCells



subroutine model_InitializeSM(pGrd, pConfig )
  !! Depending on the SM model in use, computes the change in soil moisture
  !! and also the recharge (if any) for each cell in the grid, given the
  !! precipitation rPrecip and the snow melt rSnowMelt
  !!
  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd         ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  ! [ LOCALS ]
  integer (kind=c_int) :: iCol,iRow,k
  type ( T_CELL ),pointer :: cel            ! pointer to cell data structure
  type ( T_LANDUSE_LOOKUP ),pointer :: pLU  ! pointer to landuse data structure
  logical ( kind=c_bool ) :: lMatch

  ! [ LOCAL PARAMETERS ]

  if(pConfig%iConfigureSMCapacity==CONFIG_SM_CAPACITY_CALCULATE) then
    ! Update the soil-water capacity based on land-cover and soil type
    do iRow=1,pGrd%iNY
      do iCol=1,pGrd%iNX

        lMatch = lFALSE
        cel => pGrd%Cells(iCol,iRow)

        if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle

        ! loop over all LAND use types...
        do k=1,size(pConfig%LU,1)
          pLU => pConfig%LU(k)
          if ( pLU%iLandUseType == cel%iLandUse ) then

            !> must guard against segfaulting due to illegal values of
            !> soil type
            if (cel%iSoilGroup > uBound(pConfig%ROOTING_DEPTH,2) &
                 .or. cel%iSoilGroup < lBound(pConfig%ROOTING_DEPTH,2) ) then

              call assert(lFALSE, &
                 "Soil group value is out of bounds: " &
                 //"col: "//trim(asCharacter(iCol)) &
                 //"  row: "//trim(asCharacter(iRow)) &
                 //"  value: "//trim(asCharacter(cel%iSoilGroup)), &
                 trim(__FILE__), __LINE__)
            endif

            cel%rSoilWaterCap = cel%rSoilWaterCapInput * &
               pConfig%ROOTING_DEPTH(k,cel%iSoilGroup)
            lMatch=lTRUE
            exit
          end if
        end do

        if(.not. lMATCH) then
          call Assert(lFALSE,&
            "Failed to match landuse grid with landuse table during soil moisture initialization~" &
            //" Row: "//trim(int2char(iRow))//"  Col: "//trim(int2char(iCol)) &
            //"  cell LU: "//trim(int2char(int(cel%iLandUse, kind=c_int) ) ) )
        endif
      end do
    end do
  end if

  select case ( pConfig%iConfigureSM )

    case ( CONFIG_SM_NONE )
      call Assert( lFALSE, "No soil moisture calculation method was specified" )
    case ( CONFIG_SM_TM_LOOKUP_TABLE )
      call sm_thornthwaite_mather_Initialize ( pGrd, pConfig )
    case ( CONFIG_SM_TM_EQUATIONS )
      call sm_thornthwaite_mather_Initialize ( pGrd, pConfig )
    case default

  end select

end subroutine model_InitializeSM

!--------------------------------------------------------------------------

subroutine model_CreateLanduseIndex(pGrd, pConfig )

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd         ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  ! [ LOCALS ]
  integer (kind=c_int) :: iCol,iRow,k
  type ( T_CELL ),pointer :: cel            ! pointer to cell data structure

  type ( T_LANDUSE_LOOKUP ),pointer :: pLU  ! pointer to landuse data structure
  logical ( kind=c_bool ) :: lMatch

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX

      if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle

      lMatch = lFALSE
      cel => pGrd%Cells(iCol,iRow)

      do k=1,size(pConfig%LU,1)
        pLU => pConfig%LU(k)
        if ( pLU%iLandUseType == cel%iLandUse ) then
          ! save index of matching landuse for ease of processing land use properties later
          cel%iLandUseIndex = k
 !         ! need to ensure that the soil type doesn't exceed
 !         ! the max number of soil types or we get a core dump
          call Assert(cel%iSoilGroup <= size(pConfig%MAX_RECHARGE,2), &
             "Value in soil type grid exceeds the maximum " &
             // "number of soil types in the land use lookup table.", &
             trim(__FILE__),__LINE__)
 !         cel%rMaxRecharge = pConfig%MAX_RECHARGE(k,INT(cel%iSoilGroup,kind=c_int))
          lMatch=lTRUE
          exit
        end if
      end do
      if(.not. lMATCH) then
        call echolog ("iRow: "//trim(asCharacter(iRow))//"  iCol: "//trim(asCharacter(iCol)) &
          //"  cell LU: "//trim(asCharacter( cel%iLandUse )) )
        call Assert(lFALSE,&
          "Failed to match landuse grid with landuse table during creation of landuse indices", &
          trim(__FILE__),__LINE__)
      endif
    end do
  end do

end subroutine model_CreateLanduseIndex

!--------------------------------------------------------------------------------------------

subroutine model_CreateIrrigationTableIndex(pGrd, pConfig )

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ), pointer      :: pGrd         ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  ! [ LOCALS ]
  integer (kind=c_int)     :: iCol,iRow,j
  type ( T_CELL ), pointer :: cel            ! pointer to cell data structure
  logical ( kind=c_bool )  :: lMatch

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX

      cel => pGrd%Cells(iCol,iRow)

      lMatch = lFALSE
      do j=1,size(pConfig%IRRIGATION,1)
        if(cel%iLanduse == pConfig%IRRIGATION(j)%iLandUseType) then
          cel%iIrrigationTableIndex = j
          lMatch = lTRUE
          exit
        endif
      enddo

      call assert(lMatch, "Unknown landuse code found while reading from the " &
        //"crop coefficient and irrigation parameters table.~Landuse specified "&
        //"in the landuse grid but not found in the irrigation table.~ " &
        //"  Landuse grid value: "//trim(int2char(cel%iLanduse)),trim(__FILE__), __LINE__)

		enddo
	enddo

end subroutine model_CreateIrrigationTableIndex


!!***

!--------------------------------------------------------------------------
!!****s* model/model_ConfigureRunoffDownhill
! NAME
!   model_ConfigureRunoffDownhill - Establishes sorted list of grid cells
!                                   (upstream-to-downstream) for use in the
!                                   downhill solution method.
!
! SYNOPSIS
!   This subroutine is only called if the user selects the downhill surface
!   runoff solution method. This routine systematically combs the model domain
!   looking for cells which either receive no runoff, or receive runoff from
!   cells that are already in the downhill runoff routing table.
!
!   The routing table is saved in a file named 'swb_routing.bin', which
!   is an unformatted Fortran data file.  Once this table exists, the
!   code will take routing information from this table first before
!   running through the process of determining a routing table from scratch.
!
! INPUTS
!   pGrd - Pointer to the model grid data structure.
!   pConfig - Pointer to the model configuration data structure.
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

subroutine model_ConfigureRunoffDownhill( pGrd, pConfig)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  ! [ LOCALS ]
  integer (kind=c_int) :: iCol, iRow, iStat, tj, ti, iTgt_Row, iTgt_Col,k,iCumlCount,iCount
  integer (kind=c_int) :: iRowSub, iColSub, iNChange, iUpstreamCount, iPasses
  integer (kind=c_int) :: ic
  integer (kind=c_int) :: iNumGridCells, iNumActiveGridCells
  integer (kind=c_int) :: iNumIterationsNochange
  integer (kind=c_int) :: LU_TEMP
  logical (kind=c_bool) :: lExist
  logical (kind=c_bool) :: lCircular = lFALSE
  type( T_GENERAL_GRID ), pointer :: pTempGrid
  type (T_CELL),pointer :: cel

  ! calculate number of gridcells in model domain
  iNumGridCells = pGrd%iNY * pGrd%iNX

  iNumActiveGridCells = count(pGrd%iMask == iACTIVE_CELL)

  ! set iteration counter
  iNumIterationsNochange = 0

  pTempGrid=>grid_Create( pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, &
    pGrd%rX1, pGrd%rY1, DATATYPE_INT )

  allocate(iOrderCol(iNumActiveGridCells), iOrderRow(iNumActiveGridCells), stat=iStat)
  call Assert( iStat == 0, &
    "Could not allocate order of solution vectors for downhill procedure")

  INQUIRE( FILE='swb_routing_table.bin', EXIST=lExist)

  EXISTS: if (.not. lExist) then

    iPasses = 0
    write(UNIT=LU_LOG,FMT=*) "Configuring the downhill routing table..."
    flush(UNIT=LU_LOG)
    iOrderCount = 0
    pGrd%Cells%lDownhillMarked = lFALSE

    do
      iNChange = 0
      do iRow=1,pGrd%iNY
        do iCol=1,pGrd%iNX  ! last index in a Fortan array should be the slowest changing
          cel => pGrd%Cells(iCol,iRow)
          if (pGrd%iMask(iCol, iRow) == iINACTIVE_CELL) cycle
          if ( cel%lDownhillMarked ) cycle
          ! Count upstream cells
          iUpstreamCount = 0

          cel%iSumUpslopeCells = 0
          cel%iNumUpslopeConnections = 0

          ! now search all adjacent cells which have current cell
          ! as their target

          lCircular = lFALSE

          do iRowSub=iRow-1,iRow+1
            if (iRowSub>=1 .and. iRowSub<=pGrd%iNY) then     ! we're within array bounds
              do iColSub=iCol-1,iCol+1
                if (iColSub>=1 .and. iColSub<=pGrd%iNX) then              ! we're within array bounds
                  if (iRow==iRowSub .and. iCol==iColSub) cycle            ! Skip current inquiry cell
                  if (pGrd%iMask(iColSub, iRowSub) == iINACTIVE_CELL ) cycle     ! Don't count inactive neighbors
                  call model_DownstreamCell(pGrd,iRowSub,iColSub,iTgt_Row,iTgt_Col)

                  if (iTgt_Row==iRow .and. iTgt_Col==iCol ) then          ! target cell points at current inquiry cell
                    if (pGrd%Cells(iColSub,iRowSub)%lDownhillMarked) then

                      cel%iSumUpslopeCells = cel%iSumUpslopeCells &
                         + pGrd%Cells(iColSub,iRowSub)%iSumUpslopeCells + 1
                       cel%iNumUpslopeConnections = cel%iNumUpslopeConnections + 1

                    else

                      iUpstreamCount = iUpstreamCount+1
                      ! we've found a cell that points to the current model
                      ! cell; does our current model cell point back at it?
                      ! if so, we have circular flow
                      call model_DownstreamCell(pGrd,iRow,iCol,iTgt_Row,iTgt_Col)
                      if (iTgt_Row==iRowSub .and. iTgt_Col==iColSub )  lCircular = lTRUE

                    endif

                  end if
                end if
              end do
            end if
          end do

          ! If there are none, we can mark this cell
          ! If we have circular flow (a points to b, b points to a),
          ! we can mark the current cell; both a and b will be set to
          ! closed depressions in subsequent processing
          if ( iUpstreamCount == 0  &
            .or. (iUpstreamCount == 1 .and. lCircular)) then
            iNChange = iNChange+1
            cel%lDownhillMarked = lTRUE
            iOrderCount = iOrderCount+1
            iOrderCol(iOrderCount) = iCol
            iOrderRow(iOrderCount) = iRow
            !write(UNIT=LU_LOG,FMT=*) 'found ',iOrderCount, iRow, iCol
          elseif ( iNumIterationsNochange > 10 ) then
            ! convert offending cell into a depression
            ! we've gotten to this point because flow paths are circular;
            ! this is likely in a flat area of the DEM, and is in reality
            ! likely to be a depression
            iNChange = iNChange+1
            cel%lDownhillMarked = lTRUE
            cel%iFlowDir = 0
            iOrderCount = iOrderCount+1
            iOrderCol(iOrderCount) = iCol
            iOrderRow(iOrderCount) = iRow
            !write(UNIT=LU_LOG,FMT=*) 'found ',iOrderCount, iRow, iCol
          end if

        end do  ! loop over rows
      end do  ! loop over columns

      if ( iNChange==0 ) then

        iNumIterationsNochange = iNumIterationsNochange + 1

        iCumlCount = 0
        write(LU_LOG,"(/,1x,'Summary of remaining unmarked cells')")

        ! loop over possible (legal) values of the flow direction grid
        do k=0,128
          iCount=COUNT(.not. pGrd%Cells%lDownHillMarked &
            .and.pGrd%Cells%iFlowDir==k .and. pGrd%iMask == iACTIVE_CELL)
          if(iCount>0) then
            iCumlCount = iCumlCount + iCount
            write(LU_LOG,FMT="(3x,i8,' unmarked grid cells have flowdir value: ',i8)") &
              iCount, k
          end if
        end do

        write(LU_LOG,FMT="(3x,a)") repeat("-",60)
        write(LU_LOG,FMT="(3x,i8,' Total cells with nonzero flow " &
          //"direction values')") iCumlCount

#ifdef DEBUG_PRINT

        where( pGrd%Cells%lDownHillMarked .or.  pGrd%iMask /=  iACTIVE_CELL )
          pTempGrid%iData = iROUTE_CELL_MARKED
        elsewhere
          pTempGrid%iData = pGrd%Cells%iFlowDir
        end where

!        call genericgraph(pTempGrid)
#endif

        call grid_WriteArcGrid("iteration"//TRIM(int2char(iPasses))// &
          "problem_gridcells.asc", pTempGrid)

      else
        ! reset iteration counter
        iNumIterationsNochange = 0
      end if

      if(iOrderCount == iNumActiveGridCells) exit
      iPasses = iPasses+1
      write(UNIT=LU_LOG,FMT=*) 'Iteration ',iPasses,'  ',iOrderCount,&
        ' of ',iNumGridCells,' cells have been configured'

    end do

    write(UNIT=LU_LOG,FMT=*) "  Number of passes required: ",iPasses
    write(UNIT=LU_LOG,FMT=*) ""
    flush(UNIT=LU_LOG)

    open ( LU_ROUTING, FILE='swb_routing.bin',FORM='UNFORMATTED', &
      status='REPLACE',ACCESS='STREAM')

    open (unit=newunit(LU_TEMP), FILE='swb_routing_log.csv', FORM='FORMATTED', &
      status='REPLACE')
    write (LU_TEMP,fmt="(a)") "Row number, Col number, Num contributing cells, Num upslope connections"

    write(LU_ROUTING) iOrderCount
    write(LU_TEMP, fmt="(i12)") iOrderCount

    do ic=1,iOrderCount
      write(LU_ROUTING) iOrderCol(ic),iOrderRow(ic)
      write(LU_TEMP, fmt="(i12,',',i12,',',i12,',',i12)") iOrderRow(ic),iOrderCol(ic), &
         pGrd%Cells(iOrderCol(ic),iOrderRow(ic))%iSumUpslopeCells, &
         pGrd%Cells(iOrderCol(ic),iOrderRow(ic))%iNumUpslopeConnections
    end do
    flush(UNIT=LU_ROUTING)
    close(UNIT=LU_ROUTING)
    close(UNIT=LU_TEMP)

    pTempGrid%iData = pGrd%Cells%iSumUpslopeCells

    call make_shaded_contour(pGrd=pTempGrid, &
      sOutputFilename=trim(pConfig%sOutputFilePrefix) // "CALC_Upslope_Contributing_Area.png", &
      sTitleTxt="Upslope Contributing Area", &
      sAxisTxt="Number of Cells" )

    pTempGrid%iData = pGrd%Cells%iNumUpslopeConnections

    call make_shaded_contour(pGrd=pTempGrid, &
      sOutputFilename=trim(pConfig%sOutputFilePrefix) // "CALC_Num_Upslope_Connections.png", &
      sTitleTxt="Number of Upslope Connecting Cells", &
      sAxisTxt="Number of Cells" )

  else ! routing table already exists

    pGrd%Cells%lDownhillMarked = lTRUE
    open(LU_ROUTING, FILE='swb_routing.bin',FORM='UNFORMATTED', ACCESS='STREAM')
    read(LU_ROUTING) iOrderCount

    ! crude error checking to see whether the routing table has the right
    ! number of elements
    call Assert(LOGICAL(iOrderCount==iNumActiveGridCells,kind=c_bool), &
      'Problem with existing routing file.  Delete swb_routing.bin and rerun')

    do ic=1,iOrderCount
      read(LU_ROUTING) iOrderCol(ic),iOrderRow(ic)
    end do

    close(UNIT=LU_ROUTING)

    write(UNIT=LU_LOG,FMT=*) ""
    write(UNIT=LU_LOG,FMT=*)  "*****************************************************************************"
    write(UNIT=LU_LOG,FMT=*)  "NOTE: Read in downhill routing information from existing swb_routing.bin file"
    write(UNIT=LU_LOG,FMT=*)  "*****************************************************************************"
    write(UNIT=LU_LOG,FMT=*) ""
    flush(UNIT=LU_LOG)

    write(UNIT=LU_STD_OUT,FMT=*)  "*****************************************************************************"
    write(UNIT=LU_STD_OUT,FMT=*)  "NOTE: Read in downhill routing information from existing swb_routing.bin file"
    write(UNIT=LU_STD_OUT,FMT=*)  "*****************************************************************************"

  end if EXISTS

end subroutine model_ConfigureRunoffDownhill


end module model_initialize