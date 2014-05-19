
!> Type controlling how the landuse table is read in, parsed, checked for basic
!! range violations, and ultimately used to populate the table structure used to 
!! run SWB. Note that the design is such that it should be possible to run the 
!! basic checks separately from the actual SWB program execution. It would be
!! nice to generalize this code somehow. 

module lookup_table_landuse

  use iso_c_binding, only : c_int, c_bool, c_short, c_float, c_double
  use exceptions
  use data_file
  use data_column
  use data_frame
  use constants_and_conversions, only : sTAB, lFALSE, lTRUE, rTINYVAL
  use strings
  use string_list
  use types_new
  use table_record_landuse, LOOKUP_TABLE_DATA_T => TABLE_RECORD_LANDUSE_T
  implicit none

  private

  !> enumerated constants for working with LANDUSE LOOKUP TABLE objects
  enum, bind(c)
    enumerator :: LU_LANDUSE_CODE = 1, LU_SOILS_GROUP = 2
  end enum

  !> enumerated constants for working with LANDUSE LOOKUP TABLE objects
  enum, bind(c)
    enumerator :: LU_CN_BASE = 1, LU_ROOTING_DEPTH = 2, LU_MAX_RECHARGE = 3,   &
                  LU_INTERCEPTION_GROW = 4, LU_INTERCEPTION_NONGROW = 5
  end enum


  type, public :: TABLE_RECORD_LANDUSE_T

    character (len=:), allocatable :: sLandUseDescription
    integer (kind=c_int) :: i(2)
    real (kind=c_float)  :: f(5)

  end type TABLE_RECORD_LANDUSE_T


  type, public :: LOOKUP_TABLE_LANDUSE_T

    type (TABLE_RECORD_LANDUSE_T), pointer    :: table(:) => null()
    
  contains

    procedure :: read_lookup_table_sub
    generic :: readfile => read_lookup_table_sub
 
    procedure :: map_columns_to_fields_sub
    generic :: map => map_columns_to_fields_sub

    procedure :: get_pointer_fn
    generic :: getpointer => get_pointer_fn

  end type LOOKUP_TABLE_LANDUSE_T

  type (DATA_FRAME_T) :: DF
  type (DATA_FILE_T)  :: FILE
  
contains

!--------------------------------------------------------------------------------------------------

  subroutine read_lookup_table_sub(this, sFilename, sCommentChars, sDelimiters)

    class (LOOKUP_TABLE_LANDUSE_T) :: this
    character (len=*), intent(in)  :: sFilename
    character (len=*), intent(in)  :: sCommentChars
    character (len=*), intent(in)  :: sDelimiters
    
    ! [ LOCALS ]
    character (len=MAX_STR_LEN) :: sBuf
    integer (kind=c_int)        :: iCount

    call FILE%open(sFilename = sFilename,              &
                   sCommentChars = sCommentChars,      &
                   sDelimiters = sDelimiters )

    FILE%slColNames = FILE%readHeader()

    iCount = FILE%slColNames%count

    !> Set up a local table structure to hold the raw table table
    call DF%initialize( slColNames=FILE%slColNames, &
        iRecordCount = FILE%iNumberOfRecords )

    !> Read in the table values line by line
    do while ( .not. FILE%isEOF() )

      sBuf = FILE%readLine()

      if (len_trim(sBuf) > 0 )  call DF%putrow( sBuf, sDelimiters )

    enddo

  end subroutine read_lookup_table_sub

!--------------------------------------------------------------------------------------------------

  subroutine map_columns_to_fields_sub(this)

    class (LOOKUP_TABLE_LANDUSE_T)  :: this

    ! [ LOCALS ]
    integer (kind=c_int) :: iLUIndex 
    integer (kind=c_int) :: iSoilsIndex 
    integer (kind=c_int) :: iNumberOfRecords
    integer (kind=c_int) :: iNumberOfSoilGroups
    integer (kind=c_int) :: iCount
    integer (kind=c_int) :: iStat
    type (DATA_COLUMN_T), pointer         :: pLU
    type (DATA_COLUMN_T), pointer         :: pCOL
    type (DATA_COLUMN_T), pointer         :: pInterception_grow
    type (DATA_COLUMN_T), pointer         :: pInterception_nongrow


    character (len=:), allocatable        :: sBuf 
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

    pLU => null()
    pCol => null()
    pInterception_grow => null()
    pInterception_nongrow => null()

    !> First task: figure out how many soil groups are in the table
    !! To do this, assume that user has logically named the rooting depths with
    !! sequential numbers attached to the column name (e.g. RZ_1, RZ_2, etc.)

    !> First, obtain a string list for all columns whose names contain "RZ"
    slRootingDepth = DF%slColNames%grep("RZ")
    !> Next extract the integer portion of those column names
    iRootingDepthSeqNums = slRootingDepth%asInt()
    !> Finally, find the maximum value for the numbered "RZ" columns
    iNumberOfRootingDepthColumns = maxval(iRootingDepthSeqNums)

    !> Repeat for curve numbers (CN)
    slCurveNumber = DF%slColNames%grep("CN")
    iCurveNumberSeqNums = slCurveNumber%asInt()
    iNumberOfCurveNumberColumns = maxval(iCurveNumberSeqNums)
    
    !> Repeat for maximum recharge values
    slMaxRecharge = DF%slColNames%grep("Max_Recharge")
    iMaxRechargeSeqNums = slMaxRecharge%asInt()
    iNumberOfMaxRechargeColumns = maxval(iMaxRechargeSeqNums)
    
    associate( a => iNumberOfRootingDepthColumns, b => iNumberOfCurveNumberColumns, c => iNumberOfMaxRechargeColumns)

      !> check to ensure that the number of columns indicated for these soil group-related items is the same
      if ( .not. (a==b .and. b==c) )  &
         call warn(sMessage="Number of soil group-related parameters in landuse lookup table is unequal", lFatal=lTRUE)

    end associate

    ! OK. We know the number of soil groups
    iNumberOfSoilGroups = iNumberOfRootingDepthColumns

    if (.not. associated( this%table) ) then
      allocate( this%table( iNumberOfRecords * iNumberOfSoilGroups ), stat=iStat)
      if( iStat /= 0)  call die("Failed to allocate memory for table table.", __FILE__, __LINE__)
    else
      call die("Data table already allocated", __FILE__, __LINE__)

    endif        

    !> Next, allocate room for the temporary local variables to hold the curve numbers,
    !> rooting depths, and max recharge values

    allocate( fCN( iNumberOfRecords, iNumberOfSoilGroups), stat=iStat)
    if( iStat /= 0)  call die("Failed to allocate memory for table table.", __FILE__, __LINE__)

    do iSoilsIndex = 1, iNumberOfSoilGroups

      sText = "CN_"//asCharacter(iSoilsIndex)
      pCOL => DF%getcol( sText )

      if (associated(pCOL) ) then

        fCN(:, iSoilsIndex) = pCOL%asFloat()

      endif  

    enddo


    allocate( fRZ( iNumberOfRecords, iNumberOfSoilGroups), stat=iStat)
    if( iStat /= 0)  call die("Failed to allocate memory for table table.", __FILE__, __LINE__)

    do iSoilsIndex = 1, iNumberOfSoilGroups

      sText = "RZ_"//asCharacter(iSoilsIndex)
      pCOL => DF%getcol( sText )

      if (associated(pCOL) ) then

        fRZ(:, iSoilsIndex) = pCOL%asFloat()

      endif  

    enddo


    allocate( fMaxRecharge( iNumberOfRecords, iNumberOfSoilGroups), stat=iStat)
    if( iStat /= 0)  call die("Failed to allocate memory for table table.", __FILE__, __LINE__)

    do iSoilsIndex = 1, iNumberOfSoilGroups

      sText = "Max_Recharge_"//asCharacter(iSoilsIndex)
      pCOL => DF%getcol( sText )

      if (associated(pCOL) ) then

        fMaxRecharge(:, iSoilsIndex) = pCOL%asFloat()

      endif  

    enddo


    !> OK, now we have all of the table in place. Time to map it from the table frame to the 
    !! table structure.

    iCount = 0
    pLU => DF%getcol("LU_Code")
    pInterception_grow => DF%getcol("Interception_growing")
    pInterception_nongrow => DF%getcol("Interception_nongrowing")

    do iLUIndex=1, iNumberOfRecords

      do iSoilsIndex=1, iNumberOfSoilGroups

        iCount = iCount + 1

        associate( lu => this%table(iCount) )
          
          call pLU%get(iLUIndex, lu%i(LU_LANDUSE_CODE) )
          lu%i(LU_SOILS_GROUP) = iSoilsIndex

          pCol => DF%getcol("Description")
          if (associated(pCol) ) then
            call pCol%get( iLUIndex, sBuf )
            lu%sLandUseDescription = trim(adjustl(clean(sBuf, '"')))
          else
            lu%sLandUseDescription = "NA"
          endif  

          lu%f(LU_CN_BASE) = fCN( iLUIndex, iSoilsIndex )
          lu%f(LU_ROOTING_DEPTH) = fRZ( iLUIndex, iSoilsIndex )
          lu%f(LU_MAX_RECHARGE) = fMaxRecharge( iLUIndex, iSoilsIndex )

          if (associated(pInterception_grow) ) then
            call pInterception_grow%get(iLUIndex, lu%f(LU_INTERCEPTION_GROW))
          else
            lu%f(LU_INTERCEPTION_GROW) = rTINYVAL
          endif
              
          if (associated(pInterception_nongrow) ) then
            call pInterception_grow%get(iLUIndex, lu%f(LU_INTERCEPTION_NONGROW))
          else
            lu%f(LU_INTERCEPTION_NONGROW) = rTINYVAL
          endif

        end associate 
        
      enddo
      
    enddo     

  end subroutine map_columns_to_fields_sub  


  function get_pointer_fn(this, iLanduseCode, iSoilsGroup)   result(pRec)

    class (LOOKUP_TABLE_LANDUSE_T)           :: this
    integer (kind=c_int)                     :: iLandUseCode
    integer (kind=c_int)                     :: iSoilsGroup
    type (TABLE_RECORD_LANDUSE_T), pointer   :: pRec

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    pRec => null()

    do iIndex = 1, uBound(this%table, 1)
      associate ( rec => this%table(iIndex) )
        
        if ( ( rec%i(LU_LANDUSE_CODE) == iLanduseCode ) .and. ( rec%i(LU_SOILS_GROUP) == iSoilsGroup ) ) then
          pRec => this%table(iIndex)
          exit
        endif

      end associate

    enddo 

    if ( .not. associated( pRec ) ) &
      call warn( sMessage = "Failed to find a landuse table entry corresponding to landuse code "     &
        //asCharacter(iLanduseCode)//" and soils group "//asCharacter(iSoilsGroup),               &
        lFatal = lTRUE, sHints = "Make sure that a landuse table entry exists for all combinations " &
        //"of landuse and soil type values found in the input grids.") 


  end function get_pointer_fn

end module lookup_table_landuse  
