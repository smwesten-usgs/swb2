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


