module version_control

  use iso_c_binding, only             : c_bool
  implicit none

  character (len=20), parameter :: GIT_COMMIT_HASH_STRING = @GIT_HASH_STRING@
  character (len=30), parameter :: GIT_BRANCH_STRING = @GIT_BRANCH_STRING@

  character (len=*), parameter :: MAJOR_VERSION_NUMBER = @SWB_MAJOR_VERSION_STRING@
  character (len=*), parameter :: MINOR_VERSION_NUMBER = @SWB_MINOR_VERSION_STRING@
  character (len=*), parameter :: PATCH_VERSION_NUMBER = @SWB_PATCH_VERSION_STRING@
  character (len=*), parameter :: BUILD_NUMBER = @BUILD_NUMBER_STRING@

  character (len=*), parameter :: SWB_VERSION =                                   &
                                       trim(adjustl(MAJOR_VERSION_NUMBER))//"."   &
                                       //trim(adjustl(MINOR_VERSION_NUMBER))//"." &
                                       //trim(adjustl(PATCH_VERSION_NUMBER))      &
                                       //", build "                               &
                                       //trim(adjustl(BUILD_NUMBER))

  character (len=*), parameter :: SYSTEM_NAME = adjustl(trim(@PLATFORM_NAME@))
  character (len=*), parameter :: COMPILE_DATE = trim(__DATE__)
  character (len=*), parameter :: COMPILE_TIME = trim(__TIME__)
  character (len=*), parameter :: COMPILATION_TIMESTAMP = trim(COMPILE_DATE)//"  "//trim(COMPILE_TIME)
end module version_control
