!> @file
!>  Contains a single module, et_gridded_values, which
!>  populates the reference et by applying the monthly value obtained from a reference grid.


!>  Populate potential evapotranspiration by substituting in 
!>  the daily average ET from a gridded data source.

module awc__table_values

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog
  use data_catalog_entry
  use parameters
  use strings
  use string_list

  implicit none

  private

  public :: awc_table_values_read, awc_table_values_initialize, AVAILABLE_WATER_CONTENT

  type (DATA_CATALOG_ENTRY_T), pointer :: pSOILS_CODE_GRID
  real (kind=c_float), allocatable     :: AVAILABLE_WATER_CONTENT(:,:)

contains

  subroutine awc_table_values_read( fRooting_Depth )

    real (kind=c_float), intent(inout)       :: fRooting_Depth(:,:)

    ! [ LOCALS ]

    integer (kind=c_int), allocatable  :: iLanduse_Code(:)
    integer (kind=c_int), allocatable  :: iSoils_Table_Code(:)
    integer (kind=c_int), allocatable  :: iSoils_Components(:)
    integer (kind=c_int), allocatable  :: iSoils_Horizons(:)    
    real (kind=c_float), allocatable   :: fSoils_AWC(:) 
    real (kind=c_float), allocatable   :: fSoils_Top_Depth(:)
    real (kind=c_float), allocatable   :: fSoils_Bottom_Depth(:)
    real (kind=c_float), allocatable   :: fSoils_Component_Fraction(:)
    real (kind=c_float), allocatable   :: fSoils_Horizon_Thickness(:)
                   
    integer (kind=c_int)               :: iNumberOfLanduses
    integer (kind=c_int)               :: iNumberOfSoils
    integer (kind=c_int)               :: iNumberOfSoilsComponents
    integer (kind=c_int)               :: iNumberOfSoilsHorizons
    integer (kind=c_int)               :: iNumberOfSoilsAWC
    integer (kind=c_int)               :: iNumberOfSoilsTopDepths
    integer (kind=c_int)               :: iNumberOfSoilsBottomDepths
    integer (kind=c_int)               :: iNumberOfSoilsComponentFractions     

    real (kind=c_float)                :: fTemp_AWC               

    type (STRING_LIST_T)               :: slList
    integer (kind=c_int)               :: iStat
    integer (kind=c_int)               :: iIndex
    integer (kind=c_int)               :: iIndex2
    integer (kind=c_int)               :: iIndex_x, iIndex_y
    real (kind=c_float)                :: fRooting_Depth_inches


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
      "A SOILS_CODE grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

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

        do iIndex2=1, iNumberOfSoils
          if ( pSOILS_CODE_GRID%pGrdBase%iData( iIndex_x, iIndex_y ) == iSoils_Table_Code( iIndex2 ) ) then
            if ( fRooting_Depth_inches < fSoils_Top_Depth( iIndex2 ) ) cycle
            if ( fRooting_Depth_inches > fSoils_Bottom_Depth( iIndex2 ) ) then

              fTemp_AWC = fTemp_AWC + fSoils_AWC( iIndex2 ) * fSoils_Component_Fraction( iIndex2 )     &
                                    * fSoils_Horizon_Thickness( iIndex2 )

            elseif ( fRooting_Depth_inches >= fSoils_Top_Depth( iIndex2 )                    &
                   .and. fRooting_Depth_inches <= fSoils_Bottom_Depth( iIndex2 ) ) then
            
              fTemp_AWC = fTemp_AWC + fSoils_AWC( iIndex2 ) * fSoils_Component_Fraction( iIndex2 )     &
                                    * ( fSoils_Bottom_Depth( iIndex2 ) - fRooting_Depth_inches )

            endif                         

          endif

        enddo  

        fTemp_AWC = fTemp_AWC / fRooting_Depth_inches  ! divide by total rooting depth to obtain weighted average

        ! table values are in/in; multiply by 12 in/ft to return AWC in inches/foot
        AVAILABLE_WATER_CONTENT(iIndex_x, iIndex_y) = fTemp_AWC * 12.0_c_float 
 
      enddo

    enddo
    
  end subroutine awc_table_values_read

!--------------------------------------------------------------------------------------------------

  subroutine awc_table_values_initialize( lActive, fAWC, iSoils_Code )

    logical (kind=c_bool), intent(in)     :: lActive(:,:)
    real (kind=c_float), intent(inout)    :: fAWC(:)
    integer (kind=c_int), intent(inout)   :: iSoils_Code(:)

    iSoils_Code = pack( pSOILS_CODE_GRID%pGrdBase%iData, lActive )
    fAWC = pack( AVAILABLE_WATER_CONTENT, lActive )
    
  end subroutine awc_table_values_initialize

end module awc__table_values
