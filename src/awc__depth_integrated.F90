!> @file
!>  Contains the module awc__depth_integrated, which populates
!! the available water content by reading in and depth-averaging
!! soil available water contents over multiple soil horizons.


!> Populate the available water content by reading in and depth-averaging
!! soil available water contents over multiple soil horizons.

module awc__depth_integrated

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use exceptions
  use parameters
  use fstring
  use fstring_list

  implicit none

  private

  public :: awc_depth_integrated_read, awc_depth_integrated_initialize, AVAILABLE_WATER_CONTENT

  type (DATA_CATALOG_ENTRY_T), pointer :: pSOILS_CODE_GRID
  real (c_float), allocatable     :: AVAILABLE_WATER_CONTENT(:,:)

contains

  subroutine awc_depth_integrated_read( fRooting_Depth )

    real (c_float), intent(inout)       :: fRooting_Depth(:,:)

    ! [ LOCALS ]

    integer (c_int), allocatable  :: iLanduse_Code(:)
    integer (c_int), allocatable  :: iSoils_Table_Code(:)
    integer (c_int), allocatable  :: iSoils_Components(:)
    integer (c_int), allocatable  :: iSoils_Horizons(:)    
    real (c_float), allocatable   :: fSoils_AWC(:) 
    real (c_float), allocatable   :: fSoils_Top_Depth(:)
    real (c_float), allocatable   :: fSoils_Bottom_Depth(:)
    real (c_float), allocatable   :: fSoils_Component_Fraction(:)
    real (c_float), allocatable   :: fSoils_Horizon_Thickness(:)
                   
    integer (c_int)               :: iNumberOfLanduses
    integer (c_int)               :: iNumberOfSoils
    integer (c_int)               :: iNumberOfSoilsComponents
    integer (c_int)               :: iNumberOfSoilsHorizons
    integer (c_int)               :: iNumberOfSoilsAWC
    integer (c_int)               :: iNumberOfSoilsTopDepths
    integer (c_int)               :: iNumberOfSoilsBottomDepths
    integer (c_int)               :: iNumberOfSoilsComponentFractions     

    real (c_float)                :: fTemp_AWC               
    real (c_float)                :: fDepthOfDeepestHorizon
    real (c_float)                :: fFinal_AWC
    integer (c_int)               :: iDeepestSoilHorizon
    logical (c_bool)              :: lFirst

    type (FSTRING_LIST_T)               :: slList
    integer (c_int)               :: iStat
    integer (c_int)               :: iIndex, iIndex2
    integer (c_int)               :: iIndex_x, iIndex_y
    real (c_float)                :: fRooting_Depth_inches
    real (c_float)                :: fSoil_Thickness_Total

   call slList%append("LU_Code")
   call slList%append("Landuse_Code")
   call slList%append("Landuse_Lookup_Code")

   call PARAMS%get_parameters( slKeys=slList, iValues=iLanduse_Code )
   iNumberOfLanduses = count( iLanduse_Code >= 0 )
   call slList%clear()


   call slList%append("Soils_Code")
   call slList%append("Soils_Lookup_Code")
   call slList%append("Soil_Code")
   call slList%append("Soil_Lookup_Code")

   call PARAMS%get_parameters( slKeys=slList, iValues=iSoils_Table_Code )
   iNumberOfSoils = count( iSoils_Table_Code >= 0 )
   call slList%clear()

   call slList%append("Soils_Component")
   call slList%append("Soils_Component_Number")
   call slList%append("Soil_Component")
   call slList%append("Soil_Component_Number")

   call PARAMS%get_parameters( slKeys=slList, iValues=iSoils_Components )
   iNumberOfSoilsComponents = count( iSoils_Components >= 0 )
   call slList%clear()

   call slList%append("Soils_Top_Depth")
   call slList%append("Soil_Top_Depth")   
   call slList%append("Soils_Z_Top")
   call slList%append("Soils_Top_of_Horizon")

   call PARAMS%get_parameters( slKeys=slList, fValues=fSoils_Top_Depth )
   iNumberOfSoilsTopDepths = count( fSoils_Top_Depth >= 0 )
   call slList%clear()

   call slList%append("Soils_Bottom_Depth")
   call slList%append("Soil_Bottom_Depth")   
   call slList%append("Soils_Z_Bottom")
   call slList%append("Soils_Bottom_of_Horizon")

   call PARAMS%get_parameters( slKeys=slList, fValues=fSoils_Bottom_Depth )
   iNumberOfSoilsBottomDepths = count( fSoils_Bottom_Depth >= 0 )
   call slList%clear()

   call slList%append("Soils_Horizon")
   call slList%append("Soils_Horizon_Number")
   call slList%append("Soil_Horizon")
   call slList%append("Soil_Horizon_Number")

   call PARAMS%get_parameters( slKeys=slList, iValues=iSoils_Horizons )
   iNumberOfSoilsHorizons = count( iSoils_Horizons >= 0 )
   call slList%clear()

   call slList%append("Soils_Component_Fraction")
   call slList%append("Soil_Component_Fraction")

   call PARAMS%get_parameters( slKeys=slList, fValues=fSoils_Component_Fraction )
   iNumberOfSoilsComponentFractions = count( fSoils_Component_Fraction >= 0 )
   call slList%clear()


   call slList%append("Soils_Available_Water_Content")
   call slList%append("Soils_AWC")
   call slList%append("Soil_Available_Water_Content")
   call slList%append("Soil_AWC")
   call slList%append("Available_Water_Content") 
   call slList%append("AWC")
 
   call PARAMS%get_parameters( slKeys=slList, fValues=fSoils_AWC )
   iNumberOfSoilsAWC = count( fSoils_AWC >= 0 )
   call slList%clear()

   !> @todo Need a *LOT* more data validation and guard code here to prevent execution in the case
   !! where only some data fields are found, or are only partially populated.

    ! locate the data structure associated with the gridded rainfall zone entries
    pSOILS_CODE_GRID => DAT%find("SOILS_CODE")
    call assert( associated(pSOILS_CODE_GRID), &
      "A SOILS_CODE grid must be supplied in order to make use of this option.", __SRCNAME__, __LINE__)

    call pSOILS_CODE_GRID%getvalues( )

    allocate (fSoils_Horizon_Thickness( ubound( fSoils_Bottom_Depth, 1) ), stat=iStat )

    allocate(AVAILABLE_WATER_CONTENT( ubound(fRooting_Depth, 1), ubound(fRooting_Depth, 2) ), stat=iStat )

    ! this should be in units of inches
    fSoils_Horizon_Thickness = fSoils_Bottom_Depth - fSoils_Top_Depth

    do iIndex_x=lbound( fRooting_Depth, 1 ), ubound( fRooting_Depth, 1 )

      do iIndex_y=lbound( fRooting_Depth, 2 ), ubound( fRooting_Depth, 2 )

        ! rooting depth in feet * 12 in/ft = rooting depth in inches
        fRooting_Depth_inches = fRooting_Depth( iIndex_x, iIndex_y ) * 12.0_c_float

        fTemp_AWC = 0.0_c_float
        lFirst = lTRUE
        iDeepestSoilHorizon = 0_c_int
        fDepthOfDeepestHorizon = 0.0_c_float
        fFinal_AWC = 0.0_c_float

        do iIndex=1, iNumberOfSoils
          if ( pSOILS_CODE_GRID%pGrdBase%iData( iIndex_x, iIndex_y ) == iSoils_Table_Code( iIndex ) ) then

            ! look for current soil code; calculate the component-weighted mean AWC for the deepest horizon
            if ( lFirst ) then

              lFirst = lFALSE

              ! find deepest soil horizon for the current soils code
              do iIndex2=iIndex, iNumberOfSoils
                if ( pSOILS_CODE_GRID%pGrdBase%iData( iIndex_x, iIndex_y ) == iSoils_Table_Code( iIndex2 ) )  &
                  iDeepestSoilHorizon = max( iDeepestSoilHorizon, iSoils_Horizons( iIndex2 ) )
              enddo

              ! if we are in the deepest horizon of the current soils code, calculate the composite averaged AWC
              do iIndex2=iIndex, iNumberOfSoils
                if ( ( pSOILS_CODE_GRID%pGrdBase%iData( iIndex_x, iIndex_y ) == iSoils_Table_Code( iIndex2 ) )  &
                  .and. ( iDeepestSoilHorizon == iSoils_Horizons( iIndex2) ) )   &
                  fFinal_AWC = fFinal_AWC + fSoils_AWC( iIndex2 ) * fSoils_Component_Fraction( iIndex2 )
                  fDepthOfDeepestHorizon = fSoils_Bottom_Depth( iIndex2 )
              enddo

            endif  

            ! rooting depth never reaches to this horizon; ignore
            if ( fRooting_Depth_inches < fSoils_Top_Depth( iIndex ) ) cycle

            ! rooting depth exceeds lower bound of current horizon; entire horizon
            ! depth used as weight factor
            if ( fRooting_Depth_inches > fSoils_Bottom_Depth( iIndex ) ) then

              fTemp_AWC = fTemp_AWC + fSoils_AWC( iIndex ) * fSoils_Component_Fraction( iIndex )     &
                                    * fSoils_Horizon_Thickness( iIndex )

            ! rooting depth falls in between the upper and lower bounds of the current soil horizon
            elseif ( fRooting_Depth_inches >= fSoils_Top_Depth( iIndex )                    &
                   .and. fRooting_Depth_inches <= fSoils_Bottom_Depth( iIndex ) ) then
            
              fTemp_AWC = fTemp_AWC + fSoils_AWC( iIndex ) * fSoils_Component_Fraction( iIndex )     &
                                    * ( fRooting_Depth_inches - fSoils_Top_Depth( iIndex ) )

            endif                         

          endif

        enddo  

        ! if the soil data from the table does not extend to the rooting depth, extrapolate
        if (fRooting_Depth_inches > fDepthOfDeepestHorizon )  &

              fTemp_AWC = fTemp_AWC + fFinal_AWC * ( fRooting_Depth_inches - fDepthOfDeepestHorizon )


        if ( fRooting_Depth_inches > 0.0_c_float ) then
          fTemp_AWC = fTemp_AWC / fRooting_Depth_inches  ! divide by total rooting depth to obtain weighted average
        else
          fTemp_AWC = 0.0_c_float
        endif    

        ! table values are in/in; multiply by 12 in/ft to return AWC in inches/foot
        AVAILABLE_WATER_CONTENT(iIndex_x, iIndex_y) = fTemp_AWC * 12.0_c_float 
 
      enddo

    enddo
 

! Original HWB code...

! c........compute area-weighted soil-moisture storage capacity and account for
! c        depth-varying AWC
!          smca(i,k)=0.
!          do 420 j=1,nseq(i)
!          do 400 l=1,nlay(i,j)
!             if(z.ge.zt(i,j,l).and.z.le.zb(i,j,l))then
! c...........bottom of root zone is within current layer
!                smca(i,k)=smca(i,k)+pct(i,j,l)*awc(i,j,l)*(z-zt(i,j,l))
!             elseif(z.gt.zb(i,j,l).and.l.eq.nlay(i,j))then
! c...........bottom of root zone is below current layer, which is the
! c           bottom layer; extrapolate bottom layer down to root depth
!                smca(i,k)=smca(i,k)+pct(i,j,l)*awc(i,j,l)*(z-zt(i,j,l))
!             elseif(z.gt.zb(i,j,l).and.l.lt.nlay(i,j))then
! c...........bottom of root zone is below current layer, which is not the
! c           bottom layer; include entire current layer
!                smca(i,k)=smca(i,k)+pct(i,j,l)*awc(i,j,l)*
!      1         (zb(i,j,l)-zt(i,j,l))
! c...........exit if bottom of root zone is above current layer
!             elseif(z.lt.zt(i,j,l))then
!                goto 440
!             endif
!  400     continue
!  420     continue
!  440     continue




  end subroutine awc_depth_integrated_read

!--------------------------------------------------------------------------------------------------

  subroutine awc_depth_integrated_initialize( lActive, fAWC, iSoils_Code )

    logical (c_bool), intent(in)     :: lActive(:,:)
    real (c_float), intent(inout)    :: fAWC(:)
    integer (c_int), intent(inout)   :: iSoils_Code(:)

    iSoils_Code = pack( pSOILS_CODE_GRID%pGrdBase%iData, lActive )

    fAWC = pack( AVAILABLE_WATER_CONTENT, lActive )

  end subroutine awc_depth_integrated_initialize

end module awc__depth_integrated
