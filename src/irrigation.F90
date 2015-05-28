!> @file
!>  Contains a single module, \ref irrigation, which
!>  provides support for estimating irrigation amounts

!> Provides support for assessing the effect of irrigation on recharge
!> values by estimating the irrigation required to maintain soil moisture levels
!> for specific crop types.
module irrigation

  use iso_c_binding, only          : c_short, c_int, c_float, c_double
  use constants_and_conversions
  use data_catalog, only           : DAT
  use data_catalog_entry, only     : DATA_CATALOG_ENTRY_T
  use parameters, only             : PARAMS
  use simulation_datetime, only    : SIM_DT
  use string_list, only            : STRING_LIST_T

  implicit none

  private 

  public :: irrigation__initialize, irrigation__calculate


  real (kind=c_float), allocatable   :: MAXIMUM_ALLOWABLE_DEPLETION(:)
  real (kind=c_float), allocatable   :: IRRIGATION_FROM_GROUNDWATER(:)
  real (kind=c_float), allocatable   :: IRRIGATION_FROM_SURFACE_WATER(:) 
  real (kind=c_float), allocatable   :: IRRIGATION_TOTAL(:)

  real (kind=c_float), allocatable   :: FRACTION_OF_IRRIGATION_FROM_GW(:)   
  integer (kind=c_int), allocatable  :: FIRST_DAY_OF_IRRIGATION(:)
  integer (kind=c_int), allocatable  :: LAST_DAY_OF_IRRIGATION(:)

  type (DATA_CATALOG_ENTRY_T), pointer :: pIRRIGATION_MASK

contains

!> Estimate the irrigation water required to sustain plant growth.
!!
!! Estimate the irrigation water required in order to
!! keep soil moisture values above the maximum allowable depletion (MAD)
!! for each gridcell.
!!
!! @param[inout] pGrd Pointer to the model grid object
!! @param[in] pConfig Pointer to the configuration data structure (type T_CONFIG).
!!

  subroutine irrigation__initialize( iNumActiveCells, iLanduseIndex )

    integer (kind=c_int), intent(in)   :: iNumActiveCells
    integer (kind=c_int), intent(in)   :: iLanduseIndex(:)

    ! [ LOCALS ]
    type (STRING_LIST_T)              :: slList
    integer (kind=c_int)              :: iNumberOfLanduses
    integer (kind=c_int)              :: iIndex
    integer (kind=c_int)              :: iStat

    call slList%append("Fraction_irrigation_from_GW")
    call slList%append("Frac_irr_fm_GW")
    call slList%append("Fraction_irrigation_from_groundwater")
    call slList%append("Frac_irrigation_from_GW")
    call slList%append("Fraction_of_irrigation_from_GW")
    call slList%append("Fraction_of_irrigation_from_groundwater")

    call PARAMS%get_parameters( slKeys=slList, fValues=FRACTION_OF_IRRIGATION_FROM_GW, lFatal=lTRUE )
    call slList%clear()

    call slList%append("Max_allowable_depletion")
    call slList%append("Maximum_allowable_depletion")
    call slList%append("MAD")

    call PARAMS%get_parameters( slKeys=slList, fValues=MAXIMUM_ALLOWABLE_DEPLETION, lFatal=lTRUE ) 
    call slList%clear()

    ! locate the data structure associated with the gridded irrigation mask entries
    pIRRIGATION_MASK => DAT%find("IRRIGATION_MASK")
    if ( .not. associated(pIRRIGATION_MASK) ) &
        call die("A IRRIGATION_MASK grid must be supplied in order to make use of this option.", __FILE__, __LINE__)

  end subroutine irrigation__initialize

!--------------------------------------------------------------------------------------------------  

  subroutine irrigation__calculate( iLanduseIndex, fSoilStorage_Max, lActive )

    integer (kind=c_int), intent(in)  :: iLanduseIndex(:)
    real (kind=c_float), intent(in)   :: fSoilStorage_Max(:)
    logical (kind=c_bool), intent(in) :: lActive(:,:)

    ! [ LOCALS ]
    real (kind=c_float)  :: fDepletionFraction(:)
    real (kind=c_double) :: rDepletionAmount
    real (kind=c_double) :: rIrrigationAmount

    integer (kind=c_int)              :: iMonth
    integer (kind=c_int)              :: iDay
    integer (kind=c_int)              :: iYear
    integer (kind=c_int)              :: iJulianDay
    integer (kind=c_int)              :: iDayOfYear
    integer (kind=c_int)              :: iDaysInMonth
    integer (kind=c_int)              :: iNumDaysFromOrigin
    integer (kind=c_int), allocatable :: iIrrigation_Mask(:)

    ! zero out Irrigation term
    IRRIGATION_FROM_GROUNDWATER = rZERO
    IRRIGATION_FROM_SURFACE_WATER = rZERO
    IRRIGATION_TOTAL = rZERO

    associate ( dt => SIM_DT%curr )

      iJulianDay = dt%getJulianDay()
      iMonth = asInt( dt%iMonth )
      iDay = asInt( dt%iDay )
      iYear = dt%iYear
      iDaysInMonth = SIM_DT%iDaysInMonth
      iDayOfYear = SIM_DT%iDOY
      iNumDaysFromOrigin = SIM_DT%iNumDaysFromOrigin

    end associate  

    call pIRRIGATION_MASK%getvalues( iMonth, iDay, iYear, iJulianDay )
    iIrrigation_Mask = pack( pIRRIGATION_MASK%pGrdBase%iData, lActive )

    ! for each cell, add water if soil storage zone is below the
    ! maximum allowable depletion

    ! now we run the gauntlet of tests to ensure that we really need
    ! to perform all of the irrigation calculations

    where ( iDayOfYear < FIRST_DAY_OF_IRRIGATION( iLanduseIndex )     &
       .or. iDayOfYear > LAST_DAY_OF_IRRIGATION( iLanduseIndex ) )

       ! do nothing 

    else where ( MAXIMUM_ALLOWABLE_DEPLETION( iLanduseIndex ) > 0.99         &
         .or. fSoilStorage_Max < rNEAR_ZERO                                  &
         .or. iIrrigation_Mask == 0 )              

      ! do nothing

    else where ( fDepletionFraction > MAXIMUM_ALLOWABLE_DEPLETION_VECTOR ) 

        !> NEW as of 4/23/2014: irrigation amount can either be the amount
        !! of the current soil moisture deficit *or* a specified maximum
        !! daily amount
!         if (pIRRIGATION%rIrrigationAmount <= rNEAR_ZERO) then
!           rIrrigationAmount = cel%rSoilWaterCap - cel%rSoilMoisture
!         else
!           rIrrigationAmount = pIRRIGATION%rIrrigationAmount
!         endif

!         cel%rIrrigationFromGW = REAL(pIRRIGATION%rFractionOfIrrigationFromGW &
!                                     * rIrrigationAmount, kind=c_double )

!         cel%rIrrigationFromSW = real((1.0 - pIRRIGATION%rFractionOfIrrigationFromGW) &
!                                     * rIrrigationAmount, kind=c_double )

        !> @todo Must difinitively figure out what to do with water that
        !! is calculated to be used as part of the inefficiency in the
        !! delivery system.

        ! rIrrigationAmount is the value that actually enters the mass balance
        ! NOTE!! Currently we are assuming that the amounts from GW and SW are the amounts a grower
        !        would estimate based on pumping rates and times; it is assumed that the inefficiencies
        !        in delivery result in water that bypasses the root zone.
!         cel%rIrrigationAmount = cel%rIrrigationFromGW * REAL(pIRRIGATION%rIrrigationEfficiency_GW, kind=c_double ) &
!                               + cel%rIrrigationFromSW * REAL(pIRRIGATION%rIrrigationEfficiency_SW, kind=c_double )


!      endif

    end where  

  end subroutine irrigation__calculate

end module irrigation
