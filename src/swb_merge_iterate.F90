module model_iterate

  use iso_c_binding, only         : c_bool, c_int, c_size_t, c_ptrdiff_t
  use constants_and_conversions
  use data_catalog, only          : DATA_CATALOG_T, DAT
  use data_catalog_entry, only    : DATA_CATALOG_ENTRY_T
  use grid
  use logfiles, only              : LOGS, LOG_ALL
  use model_domain, only          : MODEL_DOMAIN_T
  use simulation_datetime, only   : SIM_DT
  use string_list, only           : STRING_LIST_T
  use strings
  use netcdf4_support
  use output, only                : write_output
  implicit none

  private

  public :: iterate_over_simulation_days

contains

  subroutine iterate_over_simulation_days(cells)

    class (MODEL_DOMAIN_T), intent(inout)  :: cells

    ! [ LOCALS ]
    type (DATA_CATALOG_ENTRY_T), pointer   :: pENTRY
    integer (kind=c_int)                   :: index_val
    character (len=256)                    :: sBuf

    type (T_NETCDF4_FILE ), pointer        :: pNCFILE
    integer (kind=c_int)                   :: row, col


    allocate( pNCFILE )

    pENTRY => DAT%get(1)

    cells%pGrdOut%rData = NC_FILL_FLOAT
    call grid_set_nodata_value( pGrd=cells%pGrdOut, fValue=NC_FILL_FLOAT )

    call netcdf_open_and_prepare_as_output( NCFILE=pNCFILE,                         &
      sVariableName=pENTRY%sVariableName_z, sVariableUnits="inches_per_day",       &
      iNX=cells%number_of_columns, iNY=cells%number_of_rows,                       &
      fX=cells%X, fY=cells%Y, StartDate=SIM_DT%start, EndDate=SIM_DT%end,          &
      dpLat=cells%Y_lat, dpLon=cells%X_lon, fValidMin=0.0, fValidMax=2000.0 )

      cells%dont_care = NC_FILL_FLOAT

    do while ( SIM_DT%curr <= SIM_DT%end )

      call LOGS%write("Merging grids: "//SIM_DT%curr%prettydate(), iLogLevel=LOG_ALL, lEcho=.true._c_bool )

      call netcdf_put_variable_vector(NCFILE=pNCFILE, &
         iVarID=pNCFILE%iVarID(NC_TIME), &
         iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t)], &
         iCount=[1_c_size_t], &
         iStride=[1_c_ptrdiff_t], &
         dpValues=[real(SIM_DT%iNumDaysFromOrigin, kind=c_double)])

      index_val = 1

      pENTRY => DAT%get(1)

      cells%pGrdOut%rData = NC_FILL_FLOAT

      do while ( associated( pENTRY ) )

        call pENTRY%getvalues( iMonth=int(SIM_DT%curr%iMonth), iDay=int(SIM_DT%curr%iDay),   &
          iYear=SIM_DT%curr%iYear, iJulianDay=SIM_DT%curr%getJulianDay() )

        do col=1, cells%number_of_columns
          do row=1, cells%number_of_rows

            if ( pENTRY%pGrdBase%rData(col, row) < NC_FILL_FLOAT ) then
              cells%pGrdOut%rData(col, row) = pENTRY%pGrdBase%rData(col, row)

!               if ( pENTRY%pGrdBase%rData(col, row) > 0.0 ) &
!               print *, col, row, pENTRY%pGrdBase%rData(col, row), cells%pGrdOut%rData(col, row)

            endif
          enddo  
        enddo

        index_val = index_val + 1
        pENTRY => DAT%get( index_val )

      enddo  

      call netcdf_put_variable_array(NCFILE=pNCFILE,                                            &
            iVarID=pNCFILE%iVarID(NC_Z),                                                        &
            iStart=[ int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t),0_c_size_t, 0_c_size_t ],    &
            iCount=[ 1_c_size_t, int(cells%number_of_rows, kind=c_size_t),                       &
                                int(cells%number_of_columns, kind=c_size_t) ],                   &
            iStride=[ 1_c_ptrdiff_t, 1_c_ptrdiff_t, 1_c_ptrdiff_t ],                            &
            rValues=cells%pGrdOut%rData )


!      call write_output( cells )
      call SIM_DT%addDay( )

    enddo 

  end subroutine iterate_over_simulation_days


  subroutine minmaxmean_float( variable , varname, nodata_value )

    real (kind=c_float), dimension(:,:)  :: variable
    character (len=*), intent(in)        :: varname
    real (kind=c_float), intent(in)      :: nodata_value

    ! [ LOCALS ] 
    integer (kind=c_int) :: iCount
    character (len=20)   :: sVarname
    character (len=14)   :: sMin
    character (len=14)   :: sMax
    character (len=14)   :: sMean
    character (len=10)   :: sCount

    write (sVarname, fmt="(a20)") adjustl(varname)

    if (size( variable, 1) > 0 ) then
      write (sMin, fmt="(g14.3)")   minval(variable, variable < nodata_value )
      write (sMax, fmt="(g14.3)")   maxval(variable, variable < nodata_value )
      write (sMean, fmt="(g14.3)")  sum(variable, variable < nodata_value ) / count( variable < nodata_value )
      write (sCount, fmt="(i10)") count( variable < nodata_value )
    else
      write (sMin, fmt="(g14.3)")   -9999.
      write (sMax, fmt="(g14.3)")   -9999.
      write (sMean, fmt="(g14.3)")  -9999.
      write (sCount, fmt="(i10)")       0
    endif


    print *, adjustl(sVarname)//" | "//adjustl(sMin)//" | "//adjustl(sMax) &
       //" | "//adjustl(sMean)//" | "//adjustl(sCount)


  end subroutine minmaxmean_float




end module model_iterate