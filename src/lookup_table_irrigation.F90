
!> Type controlling how the irrigation table is read in, parsed, checked for basic
!! range violations, and ultimately used to populate the data structure used to 
!! run SWB. Note that the design is such that it should be possible to run the 
!! basic checks separately from the actual SWB program execution. It would be
!! nice to generalize this code somehow. 

module lookup_table_irrigation

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

 !> enumerated constants for working with integer IRRIGATION LOOKUP TABLE objects
  enum, bind(c)
    enumerator :: IRR_LANDUSE_CODE = 1, IRR_L_PLANT = 2, IRR_L_INIT = 3, IRR_L_MID = 4, &
                  IRR_L_LATE = 5, IRR_BEGIN_IRRIGATION = 6, IRR_END_IRRIGATION = 7
  end enum


 !> enumerated constants for working with logical IRRIGATION LOOKUP TABLE objects
  enum, bind(c)
    enumerator :: IRR_UNITS_ARE_DOY = 1
  end enum

 !> enumerated constants for working with real IRRIGATION LOOKUP TABLE objects
  enum, bind(c)
    enumerator :: IRR_CROP_COEF_KCB_INIT = 1, IRR_CROP_COEF_KCB_MID = 2, IRR_CROP_COEF_KCB_END = 3, &
                  IRR_CROP_COEF_KCB_MIN = 4, IRR_GDD_BASETEMP = 5, IRR_GDD_MAXTEMP = 6,             &
                  IRR_DEPLETION_FRACTION = 7, IRR_MAX_ALLOWABLE_DEPLETION = 8,                      &
                  IRR_IRRIGATION_AMOUNT = 9, IRR_FRAC_IRRIGATION_FM_GW = 10,                        &
                  IRR_IRRIGATION_EFFICIENCY_GW = 11, IRR_IRRIGATION_EFFICIENCY_SW = 12,             &
                  IRR_MEAN_PLANT_HEIGHT_FT = 13
  end enum

  character (len=19)         :: sRealVarNames(13) = &
    ["Kcb_init           ", "Kcb_mid            ", "Kcb_end            ", "Kcb_min            ", &
     "GDD_Basetemp       ", "GDD_Maxtemp        ", "Depletion_fraction ", "Max_allow_depletion", &
     "Irrigation_amount  ", "Frac_irrig_fm_GW   ", "Irr_efficiency_GW  ", "Irr_efficiency_SW  ", &
     "Mean_plant_height  " ]

  integer (kind=c_int), parameter :: NUMBER_OF_REAL_COLUMNS     = 13
  integer (kind=c_int), parameter :: NUMBER_OF_INTEGER_COLUMNS  = 7
  integer (kind=c_int), parameter :: NUMBER_OF_LOGICAL_COLUMNS  = 1

  type, public :: TABLE_RECORD_IRRIGATION_T

    character (len=:), allocatable :: sCropDescription

    integer (kind=c_int)           :: i(:,:)
    real (kind=c_float)            :: f(:,:)
    logical (kind=c_bool)          :: l(:,:)

  end type TABLE_RECORD_IRRIGATION_T


  type, public :: LOOKUP_TABLE_IRRIGATION_T

    type (TABLE_RECORD_IRRIGATION_T), pointer    :: table(:) => null()
    
  contains

    procedure :: read_lookup_table_sub
    generic :: readfile => read_lookup_table_sub
 
    procedure :: map_columns_to_fields_sub
    generic :: map => map_columns_to_fields_sub

    procedure :: get_pointer_fn
    generic :: getpointer => get_pointer_fn

  end type LOOKUP_TABLE_IRRIGATION_T

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

    call FILE%open(sFilename = sFilename,    &
                   sCommentChars = sCommentChars,      &
                   sDelimiters = sDelimiters )

    FILE%slColNames = FILE%readHeader()

    iCount = FILE%slColNames%count

    !> Set up a local data structure to hold the raw table data
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
    integer (kind=c_int) :: iNumberOfRecords
    integer (kind=c_int) :: iCount
    integer (kind=c_int) :: iStat

    type (DATA_COLUMN_T), pointer         :: pIRR
    type (DATA_COLUMN_T), pointer         :: pCOL


    character (len=:), allocatable        :: sBuf 

    character (len=64), allocatable  :: sCropDescription(:)    

    iNumberOfRecords = FILE%iNumberOfRecords

    pIRR => null()
    pCol => null()

    if (.not. associated( this%table) ) then
      allocate( this%table( iNumberOfRecords ), stat=iStat)
      if( iStat /= 0)  call die("Failed to allocate memory for data table.", __FILE__, __LINE__)
    else
      call die("Data table already allocated", __FILE__, __LINE__)

    endif        

    !> OK, now we have all of the data in place. Time to map it from the data frame to the 
    !! data structure.

    allocate( this%table%i(iNumberOfRecords, NUMBER_OF_INTEGER_COLUMNS), stat=iStat)
    if (iStat /= 0)   call die("Failed to allocate memory for table object", __FILE__, __LINE__)

    allocate( this%table%f(iNumberOfRecords, NUMBER_OF_REAL_COLUMNS), stat=iStat)
    if (iStat /= 0)   call die("Failed to allocate memory for table object", __FILE__, __LINE__)

    allocate( this%table%l(iNumberOfRecords, NUMBER_OF_LOGICAL_COLUMNS), stat=iStat)
    if (iStat /= 0)   call die("Failed to allocate memory for table object", __FILE__, __LINE__)



    pCol => DF%getcol("Crop_Description")

    do iLUIndex=1, iNumberOfRecords

      associate( irr=> this%table(iLUIndex) )

        if (associated(pCol) ) then
          call pCol%get( iLUIndex, sBuf )
          irr%sCropDescription = trim(adjustl(clean(sBuf, '"')))
        else
          irr%sCropDescription = "NA"
        endif 

      end associate   
      
    enddo     


    do iIndex=1, ubound(sRealVarNames, 1)

      pCol => DF%getcol( trim( sRealVarNames(iIndex) ) )

      if ( associated(pCol) ) then

        this%table%f(:, iIndex) = pCol%asFloat()

      else

        this%table%f(:, iIndex) = fTINYVAL

      endif
      
    enddo  





  end subroutine map_columns_to_fields_sub  


  function get_pointer_fn(this, iLanduseCode)   result(pRec)

    class (LOOKUP_TABLE_IRRIGATION_T)           :: this
    integer (kind=c_int)                        :: iLandUseType
    type (TABLE_RECORD_IRRIGATION_T), pointer   :: pRec

    ! [ LOCALS ]
    integer (kind=c_int) :: iIndex

    pRec => null()

    do iIndex = 1, uBound(this%table, 1)
      associate ( rec => this%table(iIndex) )
        
        if ( (rec%i(IRR_LANDUSE_CODE) == iLanduseCode) ) then
          pRec => this%table(iIndex)
          exit
        endif

      end associate

    enddo 

    if ( .not. associated( pRec ) ) &
      call warn( sMessage = "Failed to find a irrigation table entry corresponding to landuse code "     &
        //asCharacter(iLanduseType), lFatal = lTRUE,  &
        sHints = "Make sure that a irrigation table entry exists for all " &
        //"landuses found in the input grids.") 


  end function get_pointer_fn

end module lookup_table_irrigation 
