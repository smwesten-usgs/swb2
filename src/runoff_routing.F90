subroutine model_RunoffDownhill(pGrd, pConfig, iDayOfYear, iMonth)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  integer (kind=c_int),intent(in) :: iDayOfYear
  integer (kind=c_int), intent(in) :: iMonth       ! Integer month value (1-12)
  ! [ LOCALS ]
  integer (kind=c_int) :: ic,iTgt_Col,iTgt_Row,iFrac
  real (kind=c_float) :: rP,rR,rDelta
  type (T_CELL),pointer :: cel
  type (T_CELL),pointer :: target_cel

  ! Reset the upstream flows (note that iOrderCount, iOrderCol, and iOrderRow are globals)
  do ic=1,iOrderCount

    cel => pGrd%Cells(iOrderCol(ic),iOrderRow(ic))

    if (pGrd%iMask(iOrderCol(ic),iOrderRow(ic)) == iINACTIVE_CELL) cycle

    call model_DownstreamCell(pGrd,iOrderRow(ic),iOrderCol(ic),iTgt_Row,iTgt_Col)

#ifdef STREAM_INTERACTIONS
    cel%rStreamCapture = rZERO
#endif

    ! Compute the runoff
    cel%rOutFlow = rf_model_CellRunoff(pConfig, cel, iDayOfYear)

    ! Now, route the water
    if ( iTgt_Row == iROUTE_LEFT_GRID .or. iTgt_Col == iROUTE_LEFT_GRID ) then
      cel%rFlowOutOfGrid = cel%rOutflow
      cel%rOutFlow = rZERO
      cycle
    elseif ( iTgt_Row == iROUTE_DEPRESSION  .or. iTgt_Col == iROUTE_DEPRESSION ) then
      ! Don't route any further; the water pools here.
      cel%rOutFlow = rZERO
      cycle
    endif

    ! MUST screen target values to ensure we don't start attempting
    ! manipulation of memory that is out of bounds!!
    call Assert(LOGICAL(iTgt_Row>0 .and. iTgt_Row <= pGrd%iNY,kind=c_bool), &
      "iTgt_Row out of bounds: Row = "//int2char(iOrderRow(ic)) &
      //"  Col = "//int2char(iOrderCol(ic)), &
      trim(__FILE__),__LINE__)
    call Assert(LOGICAL(iTgt_Col>0 .and. iTgt_Col <= pGrd%iNX,kind=c_bool), &
      "iTgt_Col out of bounds: Row = "//int2char(iOrderRow(ic)) &
      //"  Col = "//int2char(iOrderCol(ic)), &
      trim(__FILE__),__LINE__)

    target_cel => pGrd%Cells(iTgt_Col,iTgt_Row)

    !> if target cell is inactive, assume that the water should be tracked
    !> as flow out of grid
    if ( pGrd%iMask(iTgt_Col,iTgt_Row) == iINACTIVE_CELL) then

      cel%rFlowOutOfGrid = cel%rOutflow
      cel%rOutFlow = rZERO
      cycle

    endif
    
    if (target_cel%iLandUse == pConfig%iOPEN_WATER_LU &
    .or. target_cel%rSoilWaterCap<rNEAR_ZERO) then
      ! Don't route any further; the water has joined a generic
      ! surface water feature. We assume that once the water hits a
      ! surface water feature that the surface water drainage
      ! network transports the bulk of it
      ! out of the model domain quite rapidly
      cel%rFlowOutOfGrid = cel%rOutflow
      cel%rOutFlow = rZERO

    else
      ! add cell outflow to target cell inflow
      target_cel%rInFlow = &
        target_cel%rInFlow + cel%rOutFlow * cel%rRouteFraction
      cel%rFlowOutOfGrid = cel%rOutflow * (rONE - cel%rRouteFraction)
      cel%rOutflow = cel%rOutflow * cel%rRouteFraction
    end if

  end do

end subroutine model_RunoffDownhill

!!***

!--------------------------------------------------------------------------
!!****s* model/model_Runoff_NoRouting
! NAME
!   model_Runoff_NoRouting - Removes any computed runoff directly from the
!                            model domain.
! SYNOPSIS
!   This subroutine makes a single pass through all grid cells in the
!   model domain in order to calculate surface water runoff. The grid
!   cells farthest upstream are solved first. Calculation of runoff amounts
!   proceeds from upstream to downstream until a calculation has been made
!   for all grid cells.
!
!   NOTE that this version performs *NO* routing, but simply removes any
!   runoff directly from the model domain.
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

subroutine model_Runoff_NoRouting(pGrd, pConfig, iDayOfYear, iMonth)

  ! [ ARGUMENTS ]
  type ( T_GENERAL_GRID ),pointer :: pGrd          ! pointer to model grid
  type (T_MODEL_CONFIGURATION), pointer :: pConfig ! pointer to data structure that contains
    ! model options, flags, and other settings
  integer (kind=c_int),intent(in) :: iDayOfYear
  integer (kind=c_int), intent(in) :: iMonth       ! Integer month value (1-12)
  ! [ LOCALS ]
  integer (kind=c_int) :: iCol,iRow, iFrac
  real (kind=c_float) :: rR
  type (T_CELL),pointer :: cel
  ! [ CONSTANTS ]

  do iRow=1,pGrd%iNY
    do iCol=1,pGrd%iNX
      cel => pGrd%Cells(iCol,iRow)

  ! Compute the runoff for each cell
  rR = rf_model_CellRunoff(pConfig, cel, iDayOfYear)

  ! Now, remove any runoff from the model grid
!      call stats_UpdateAllAccumulatorsByCell(REAL(rR,kind=c_double), &
!         iRUNOFF_OUTSIDE,iMonth,iZERO)

  cel%rFlowOutOfGrid = rR
!       cel%rOutFlow = rR

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! What is the point of this? If we aren't routing,
!! only a small amount of water (generated from a
!! cell directly beneath a stream segment) will
!! be captured...
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#ifdef STREAM_INTERACTIONS
  ! Capture into streams or fractures
  cel%rStreamCapture = rZERO
  if ( cel%iStreamIndex /= 0 ) then
    ! Compute the amount of fracture recharge
    cel%rStreamCapture = cel%rInFlow * pconfig%rStreamMaxCapture(cel%iStreamIndex) &
      / pconfig%rStreamMaxInflow(cel%iStreamIndex)
    if (cel%rStreamCapture < rZERO) then
      print *, "Negative!", cel%rInFlow, cel%rStreamCapture
    endif
    cel%rOutFlow = cel%rOutFlow - cel%rStreamCapture
  end if
#endif

  ! we've removed the water from the grid; it shouldn't be included in
  ! "outflow" water
  cel%rOutFlow = rZERO

  end do
end do

  return

end subroutine model_Runoff_NoRouting
