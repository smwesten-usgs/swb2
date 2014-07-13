!> @file
!> Contains a single module, @ref sm_thornthwaite_mather, which estimates runoff by
!> means of the NRCS/SCS curve number method.


module soil_moisture__thornthwaite_mather

  use iso_c_binding, only : c_short, c_int, c_float, c_double
  use exceptions
  use parameters
  use strings
  use string_list
  implicit none

  private

  public :: initialize_soil_moisture__thornthwaite_mather
  public :: calculate_soil_moisture__thornthwaite_mather

  ! parameters that allow the Thornthwaite-Mather tables (1957) to be
  ! represented by a single equation
  real (kind=c_double), parameter :: TM_SLOPE_TERM = 0.478769194198665_c_double
  real (kind=c_double), parameter :: TM_EXP_TERM   = -1.03678439421169_c_double

  real (kind=c_float), allocatable, public  :: APWL(:)
  integer (kind=c_int), allocatable         :: iLanduseCodes(:)
  real (kind=c_float), allocatable          :: ROOTING_DEPTH(:,:)

contains

!--------------------------------------------------------------------------------------------------

  subroutine initialize_soil_moisture__thornthwaite_mather( fSoilStorage_Max, iLanduseIndex, iSoilsGroup )

    real (kind=c_float), intent(inout)    :: fSoilStorage_Max(:)
    integer (kind=c_int), intent(in)      :: iLanduseIndex(:)
    integer (kind=c_int), intent(in)      :: iSoilsGroup(:)

    ! [ LOCALS ]
    integer (kind=c_int)              :: iNumActiveCells
    integer (kind=c_int)              :: iStat
    integer (kind=c_int)              :: iNumberOfLanduses
    integer (kind=c_int)              :: iNumberOfSoilGroups
    integer (kind=c_int)              :: iSoilsIndex
    integer (kind=c_int)              :: iLUIndex
    type (STRING_LIST_T)              :: slList
    type (STRING_LIST_T)              :: slRZ
    integer (kind=c_int), allocatable :: iRZ_SeqNums(:) 
    real (kind=c_float), allocatable  :: RZ(:)
    character (len=:), allocatable    :: sText


    iNumActiveCells = ubound(fSoilStorage_Max,1)

    allocate( APWL( iNumActiveCells ), stat=iStat )
    call assert( iStat == 0, "Problem allocating memory", __FILE__, __LINE__ )

    APWL = 0.0_c_float 

    call slList%append("LU_Code")
    call slList%append("Landuse_Lookup_Code")

    !> Determine how many soil groups are present

    ! retrieve a string list of all keys associated with root zone depth (i.e. RZ_1, RZ_2, RZ_3, etc.)
    slRZ = PARAMS%grep_keys("RZ")
    ! Convert the string list to an vector of integers; this call strips off the "RZ_" part of label
    iRZ_SeqNums = slRZ%asInt()
    ! count how many items are present in the vector; this should equal the number of soils groups
    iNumberOfSoilGroups = count( iRZ_SeqNums > 0 )

    !> Determine how many landuse codes are present
    call PARAMS%get_values( slList, iLanduseCodes )
    iNumberOfLanduses = count( iLanduseCodes > 0 )

    allocate( ROOTING_DEPTH(iNumberOfLanduses, iNumberOfSoilGroups), stat=iStat )
    call assert( iStat == 0, "Failed to allocate memory for maximum rooting depth table", &
      __FILE__, __LINE__)

    ! we should have the max rooting depth table fully filled out following this block
    do iSoilsIndex = 1, iNumberOfSoilGroups
      sText = "RZ_"//asCharacter(iSoilsIndex)
      call PARAMS%get_values( sText, RZ )
      ROOTING_DEPTH(:, iSoilsIndex) = RZ
    enddo  

    do iSoilsIndex = 1, iNumberOfSoilGroups
      do iLUIndex = 1, iNumberOfLanduses

        where ( iLanduseIndex == iLUIndex .and. iSoilsGroup == iSoilsIndex )

          fSoilStorage_Max = ROOTING_DEPTH( iLUIndex, iSoilsIndex ) * 2.

        end where

      enddo
    enddo

  end subroutine initialize_soil_moisture__thornthwaite_mather

!--------------------------------------------------------------------------------------------------

  elemental function calc_soil_storage_given_current_APWL( fSWC, fAPWL)  result(fSoilStorage)

    real (kind=c_float), intent(in) :: fSWC     ! max soil-water capacity (inches)
    real (kind=c_float), intent(in) :: fAPWL    ! accum pot. water loss (inches)
    real (kind=c_float)             :: fSoilStorage

    ! equation as implemented in R;
    ! sm.df$y = maximum soil-water capacity
    ! sm.df$x = APWL
    ! 10^(log10(sm.df$y) - (s.opt[[1]]*sm.df$y^e.opt[[1]]) * sm.df$x)

    if (fSWC > 0.0_c_float ) then

      fSoilStorage = 10.0_c_float**( log10( fSWC )  - ( abs( fAPWL ) * TM_SLOPE_TERM * fSWC**TM_EXP_TERM ) )
   
    else
   
      fSoilStorage = 0.0_c_float
  
    endif

  end function calc_soil_storage_given_current_APWL

!--------------------------------------------------------------------------------------------------

  elemental function calc_APWL_given_soil_storage(fSWC, fSoilStorage)  result(fAPWL)

    real (kind=c_float), intent(in) :: fSWC            ! max soil-water capacity (inches)
    real (kind=c_float), intent(in) :: fSoilStorage  ! curr soil storage (inches)
    real (kind=c_float)             :: fAPWL

    ! equation as implemented in R;
    ! sm.df$y = maximum soil-water capacity
    ! sm.df$x = APWL
    ! (log10(sm.df$y) - log10(sm.df$pred)) / (s.opt[[1]] * sm.df$y^e.opt[[1]])

    if (fSWC > 0.0_c_float .and. fSoilStorage > 0.0_c_float ) then

      fAPWL = -( log10(fSWC) - log10(fSoilStorage)) / ( TM_SLOPE_TERM * fSWC**TM_EXP_TERM )

    else
    
      fAPWL = 0.0_c_float

    endif  

  end function calc_APWL_given_soil_storage

!--------------------------------------------------------------------------------------------------

  elemental subroutine calculate_soil_moisture__thornthwaite_mather(fAPWL, fSoilStorage, fSoilStorage_Excess,                &
                                            fSoilStorage_Max, fInfiltration, fReference_ET)

    real (kind=c_float), intent(inout)   :: fAPWL
    real (kind=c_float), intent(inout)   :: fSoilStorage
    real (kind=c_float), intent(out)     :: fSoilStorage_Excess
    real (kind=c_float), intent(in)      :: fSoilStorage_Max
    real (kind=c_float), intent(in)      :: fInfiltration
    real (kind=c_float), intent(in)      :: fReference_ET

    ! [ LOCALS ]
    real (kind=c_float) :: fP_minus_PE

    fP_minus_PE = fInfiltration - fReference_ET

    ! P - PE < 0: soil is losing moisture; ET exceeds infiltration/precip
    ! must update new APWL, then back-calculate new soil moisture
    if ( fP_minus_PE < 0.0_c_float ) then

      fAPWL = fAPWL - fP_minus_PE
      fSoilStorage = calc_soil_storage_given_current_APWL( fSWC=fSoilStorage_Max, fAPWL=fAPWL )
    
    else ! P > PE: soil is gaining moisture; add P - PE directly to soil moisture, then
         ! beck-calculate a new APWL that corresponds to the new soil moisture

      fSoilStorage = fSoilStorage + fP_minus_PE

      if (fSoilStorage > fSoilStorage_Max ) then

        fSoilStorage_Excess = fSoilStorage - fSoilStorage_Max
        fSoilStorage = fSoilStorage_Max

      else

        fSoilStorage_Excess = 0.0_c_float

      endif

      fAPWL = calc_APWL_given_soil_storage(fSWC=fSoilStorage_Max, fSoilStorage=fSoilStorage)

    endif

  end subroutine calculate_soil_moisture__thornthwaite_mather

end module soil_moisture__thornthwaite_mather
