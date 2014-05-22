subroutine model_EndOfRun(pGrd, pConfig, pGraph)

  ![ARGUMENTS]
  type (T_GENERAL_GRID),pointer :: pGrd      ! Grid of model cells
  type (T_MODEL_CONFIGURATION), pointer :: pConfig  ! pointer to data structure that contains
    ! model options, flags, and other settings
  type (T_GRAPH_CONFIGURATION), dimension(:), pointer :: pGraph
    ! pointer to data structure that holds parameters for creating
    ! DISLIN plots

  ![LOCALS]
  integer (kind=c_int) :: iIndex
  type (DATA_CATALOG_ENTRY_T), pointer :: current

  ! clean up
  close ( unit=LU_TS )
  if ( pConfig%lReportDaily ) then
    close ( unit=LU_MSB_REPORT )
    close ( unit=LU_CSV_MIN )
    close ( unit=LU_CSV_MEAN )
    close ( unit=LU_CSV_MAX )
    close ( unit=LU_CSV_ANNUAL )
  end if

  ! close any binary output files
  call stats_CloseBinaryFiles()

  ! trigger the call to reconstitute the output grids and plots from the
  ! compressed binary files, if desired

  if(.not. pConfig%lUseSWBRead) &
    call stats_RewriteGrids(pGrd%iNX, pGrd%iNY, pGrd%rX0, pGrd%rY0, pGrd%rX1, &
      pGrd%rY1, pConfig, pGraph)

  ! destroy model grid to free up memory
  call grid_Destroy(pGrd)
  call grid_Destroy(pGenericGrd_int)
  call grid_Destroy(pGenericGrd_sgl)

  do iIndex=1,DAT%count
    current => DAT%get(iIndex)
    if (associated(current) ) then
      if (current%iNC_ARCHIVE_STATUS == NETCDF_FILE_OPEN ) then
        call netcdf_close_file(NCFILE=current%NCFILE_ARCHIVE)
        call netcdf_deallocate_data_struct(NCFILE=current%NCFILE_ARCHIVE)
      endif
    endif  
  enddo

  ! how long did all this take, anyway?
  call cpu_time(rEndTime)
  print "(//1x,'SWB run completed in: ',f10.2,' minutes')", &
    (rEndTime - rStartTime) / 60.0_c_float
  write(unit=LU_LOG,fmt="(//1x,'SWB run completed in: ',f10.2, ' minutes')"), &
    (rEndTime - rStartTime) / 60.0_c_float

end subroutine model_EndOfRun
