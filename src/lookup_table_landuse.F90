module table_record_landuse
  
  use iso_c_binding, only : c_short, c_float
  implicit none

  type, public :: TABLE_RECORD_LANDUSE_T

    integer (kind=c_short) :: iLandUseType
    integer (kind=c_short) :: iSoilsGroup
    character (len=:), allocatable :: sLandUseDescription
    character (len=:), allocatable :: sAssumedPercentImperviousness
    real (kind=c_float) :: rCN_base
    real (kind=c_float) :: rRootingDepth
    real (kind=c_float) :: rMaxInfiltration
    real (kind=c_float) :: rIntercept_GrowingSeason
    real (kind=c_float) :: rIntercept_NonGrowingSeason

  end type TABLE_RECORD_LANDUSE_T

end module table_record_landuse

module lookup_table_landuse

  use iso_c_binding, only : c_int, c_bool, c_short, c_float, c_double
  use exceptions
  use data_file
  use data_column
  use data_frame
  use constants_and_conversions, only : sTAB, lFALSE, lTRUE
  use strings
  use string_list
  use types_new
  use table_record_landuse, LOOKUP_TABLE_DATA_T => TABLE_RECORD_LANDUSE_T
  implicit none

  private

  type, public :: LOOKUP_TABLE_T

    type (LOOKUP_TABLE_DATA_T), allocatable    :: data(:)
    
  contains

    procedure :: read_lookup_table_sub
    generic :: readfile => read_lookup_table_sub
 
    procedure :: map_columns_to_fields_sub
    generic :: map => map_columns_to_fields_sub

  end type LOOKUP_TABLE_T

  type (DATA_FRAME_T) :: DF
  type (DATA_FILE_T)  :: FILE
  
contains

!--------------------------------------------------------------------------------------------------

  subroutine read_lookup_table_sub(this, sFilename, sCommentChars, sDelimiters)

    class (LOOKUP_TABLE_T) :: this
    character (len=*), intent(in)  :: sFilename
    character (len=*), intent(in)  :: sCommentChars
    character (len=*), intent(in)  :: sDelimiters
    
    ! [ LOCALS ]
    character (len=MAX_STR_LEN) :: sBuf
    integer (kind=c_int)        :: iCount

    call FILE%open(sFilename = sFilename,    &
                   sCommentChars = sCommentChars,      &
                   sDelimiters = sDelimiters )

    FILE%slColNames = FILE%readHeader()

    print *, __FILE__, ": ", __LINE__


    iCount = FILE%slColNames%count


    print *, "  => FILE%slColNames%count = ", FILE%slColNames%count

    call DF%initialize( slColNames=FILE%slColNames, &
        iRecordCount = FILE%iNumberOfRecords )


    print *, "  => FILE%iNumberOfRecords = ", FILE%iNumberOfRecords

    do while ( FILE%isOpen() )

      sBuf = FILE%readLine()

      call DF%putrow( sBuf, sDelimiters )

    enddo

  end subroutine read_lookup_table_sub

!--------------------------------------------------------------------------------------------------

  subroutine map_columns_to_fields_sub(this)

    class (LOOKUP_TABLE_T)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iLUIndex 
    integer (kind=c_int) :: iSoilsIndex 
    integer (kind=c_int) :: iNumberOfRecords
    integer (kind=c_int) :: iNumberOfSoilGroups
    integer (kind=c_int) :: iCount
    integer (kind=c_int) :: iStat
    type (T_DATA_COLUMN), pointer         :: pCOL
    real (kind=c_float), allocatable      :: rValues(:)
    integer (kind=c_int), allocatable     :: iValues(:)
    integer (kind=c_int), allocatable     :: iRootingDepthSeqNums(:)
    integer (kind=c_int), allocatable     :: iCurveNumberSeqNums(:)
    integer (kind=c_int), allocatable     :: iMaxRechargeSeqNums(:)
    type (STRING_LIST_T)                  :: slRootingDepth
    type (STRING_LIST_T)                  :: slCurveNumber
    type (STRING_LIST_T)                  :: slMaxRecharge
    integer (kind=c_int)                  :: iNumberOfRootingDepthColumns
    integer (kind=c_int)                  :: iNumberOfCurveNumberColumns
    integer (kind=c_int)                  :: iNumberOfMaxRechargeColumns
    real (kind=c_float), allocatable      :: fRZ(:,:)
    real (kind=c_float), allocatable      :: fCN(:,:)
    real (kind=c_float), allocatable      :: fMaxRecharge(:,:)        
    character (len=:), allocatable        :: sText


    iNumberOfRecords = FILE%iNumberOfRecords

    !> First task: figure out how many soil groups are in the table
    !! To do this, assume that user has logically named the rooting depths with
    !! sequential numbers attached to the column name (e.g. RZ_1, RZ_2, etc.)
    slRootingDepth = DF%slColNames%grep("RZ")
    iRootingDepthSeqNums = slRootingDepth%asInt()
    iNumberOfRootingDepthColumns = maxval(iRootingDepthSeqNums)

    slCurveNumber = DF%slColNames%grep("CN")
    iCurveNumberSeqNums = slCurveNumber%asInt()
    iNumberOfCurveNumberColumns = maxval(iCurveNumberSeqNums)
    
    slMaxRecharge = DF%slColNames%grep("Max_Recharge")
    iMaxRechargeSeqNums = slMaxRecharge%asInt()
    iNumberOfMaxRechargeColumns = maxval(iMaxRechargeSeqNums)
    
    associate( a => iNumberOfRootingDepthColumns, b => iNumberOfCurveNumberColumns, c => iNumberOfMaxRechargeColumns)

      if ( .not. (a==b .and. b==c) )  &
         call warn(sMessage="Number of soil group-related parameters in landuse lookup table is unequal", lFatal=lTRUE)

    end associate

    iNumberOfSoilGroups = iNumberOfRootingDepthColumns

    if (.not. allocated( this%data) ) then
      allocate( this%data( iNumberOfRecords * iNumberOfSoilGroups ), stat=iStat)
      if( iStat /= 0)  call die("Failed to allocate memory for data table.", __FILE__, __LINE__)
    endif        

    allocate( fCN( iNumberOfRecords, iNumberOfSoilGroups), stat=iStat)
    if( iStat /= 0)  call die("Failed to allocate memory for data table.", __FILE__, __LINE__)

    print *, "Initial allocation: ", iNumberOfRecords, iNumberOfSoilGroups

    do iSoilsIndex = 1, iNumberOfSoilGroups

      sText = "CN_"//asCharacter(iSoilsIndex)
      pCOL => DF%getcol( sText )

      if (associated(pCOL) ) then

        iCount = pCol%count()
!        fCN(:, iSoilsIndex) = pCOL%getColumnFloatVals()

         print *, iNumberOfRecords, size(rValues), iCount
         rValues = pCOL%getColumnFloatVals()
!         fCN(:, iSoilsIndex) = rValues

         print *, size(rValues)

        print *, "CN VALUES FOR: "//sText
        print *, rValues

      endif  

    enddo


!     allocate( fRZ( iNumberOfRecords, iNumberOfSoilGroups), stat=iStat)
!     if( iStat /= 0)  call die("Failed to allocate memory for data table.", __FILE__, __LINE__)

!     do iSoilsIndex = 1, iNumberOfSoilGroups

!       sText = "RZ_"//asCharacter(iSoilsIndex)
!       pCOL => DF%getcol( sText )

!       if (associated(pCOL) ) then

!         iCount = pCol%count()
!         !rValues = pCOL%getColumnFloatVals()
!         fRZ(:, iSoilsIndex) = pCOL%getColumnFloatVals()

!         print *, "RZ VALUES FOR: "//sText
!         print *, fRZ

!       endif  

!     enddo



  end subroutine map_columns_to_fields_sub  


end module lookup_table_landuse  
