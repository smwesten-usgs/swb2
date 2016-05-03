module swb_merge_iterate

  use iso_c_binding, only         : c_bool, c_int, c_size_t, c_ptrdiff_t
  use ieee_arithmetic, only       : ieee_is_nan
  use constants_and_conversions
  use data_catalog, only          : DATA_CATALOG_T, DAT
  use data_catalog_entry, only    : DATA_CATALOG_ENTRY_T
  use grid
  use logfiles, only              : LOGS, LOG_ALL
  use swb_merge_domain, only      : MODEL_DOMAIN_T
  use swb_merge_initialize, only  : NCFILES, bbox
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
    integer (kind=c_int)                   :: row, col
    integer (kind=c_int)                   :: mcol, mrow
    integer (kind=c_int)                   :: indx
    integer (kind=c_int)                   :: julian_day
    logical (kind=c_bool)                  :: found_date


    cells%pGrdOut%rData = NC_FILL_FLOAT
    call grid_set_nodata_value( pGrd=cells%pGrdOut, fValue=NC_FILL_FLOAT )
    cells%nodata_fill_value = NC_FILL_FLOAT

    do while ( SIM_DT%curr <= SIM_DT%end )

      julian_day = nint( SIM_DT%curr%dJulianDate, kind=c_int )

      call LOGS%write("Merging grids: "//SIM_DT%curr%prettydate(), iLogLevel=LOG_ALL, lEcho=.true._c_bool )

      call netcdf_put_variable_vector(NCFILE=cells%NCFILE, &
         iVarID=cells%NCFILE%iVarID(NC_TIME), &
         iStart=[int(SIM_DT%iNumDaysFromOrigin, kind=c_size_t)], &
         iCount=[1_c_size_t], &
         iStride=[1_c_ptrdiff_t], &
         dpValues=[real(SIM_DT%iNumDaysFromOrigin, kind=c_double)])

!      $omp parallel private( col, row, pENTRY )

      cells%pGrdOut%rData = NC_FILL_FLOAT

      do indx=1, ubound( NCFILES, 1)

        found_date = netcdf_update_time_starting_index(NCFILE=NCFILES( indx ),     &
                                               iJulianDay=julian_day  ) 

        call netcdf_get_variable_slice(NCFILE=NCFILES( indx ), rValues=bbox( indx )%data_grid )

        do col=1, ubound( bbox(indx)%merge_column, 1 )
          do row=1, ubound( bbox(indx)%merge_column, 2 )
            if ( bbox( indx )%data_grid(col,row) > NC_FILL_FLOAT ) then
              mcol = bbox(indx)%merge_column(col,row)
              mrow = bbox(indx)%merge_row(col,row)              
              cells%pGrdOut%rData( mcol, mrow ) = bbox( indx )%data_grid(col,row)
            endif  
          enddo  
        enddo

      enddo  

!      $omp end parallel

      call netcdf_put_variable_array(NCFILE=cells%NCFILE,                                            &
            iVarID=cells%NCFILE%iVarID(NC_Z),                                                        &
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




end module swb_merge_iterate