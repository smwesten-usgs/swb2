module version_control

  use iso_c_binding, only             : c_bool
  implicit none

#ifndef VERSION_H
#define VERSION_H

#define QUOTE(str) "str"
#define EXPAND_AND_QUOTE(str) QUOTE(str)

#define PLATFORM_NAME "@CMAKE_SYSTEM@"
#define WINDOWS_SYSTEM_BOOL "@WIN32"
#define GIT_BRANCH_VALUE "@GIT_BRANCH@"
#define GIT_COMMIT_HASH_VALUE "@GIT_COMMIT_HASH@"
#define GIT_COMMITS_ON_BRANCH_VALUE "@GIT_COMMITS_ON_BRANCH@"
#define SWB_MAJOR_VERSION_VALUE "@SWB_MAJOR_VERSION@"
#define SWB_MINOR_VERSION_VALUE "@SWB_MINOR_VERSION@"
#define SWB_PATCH_VERSION_VALUE "@SWB_PATCH_VERSION@"

  character (len=20), parameter :: GIT_COMMIT_HASH_STRING = GIT_COMMIT_HASH_VALUE
  character (len=30), parameter :: GIT_BRANCH_STRING = GIT_BRANCH_VALUE

  character (len=*), parameter :: MAJOR_VERSION_NUMBER = SWB_MAJOR_VERSION_VALUE
  character (len=*), parameter :: MINOR_VERSION_NUMBER = SWB_MINOR_VERSION_VALUE
  character (len=*), parameter :: PATCH_VERSION_NUMBER = SWB_PATCH_VERSION_VALUE
  character (len=*), parameter :: BUILD_NUMBER = GIT_COMMITS_ON_BRANCH_VALUE

  character (len=*), parameter :: SWB_VERSION =                                   &
                                       trim(adjustl(MAJOR_VERSION_NUMBER))//"."   &
                                       //trim(adjustl(MINOR_VERSION_NUMBER))//"." &
                                       //trim(adjustl(PATCH_VERSION_NUMBER))      &
                                       //", build "                               &
                                       //trim(adjustl(BUILD_NUMBER))

  character (len=*), parameter :: SYSTEM_NAME = adjustl(trim(PLATFORM_NAME))
  character (len=*), parameter :: COMPILE_DATE = trim(__DATE__)
  character (len=*), parameter :: COMPILE_TIME = trim(__TIME__)
  character (len=*), parameter :: COMPILATION_TIMESTAMP = trim(COMPILE_DATE)//"  "//trim(COMPILE_TIME)
  logical (c_bool), parameter  :: WINDOWS_SYSTEM = (trim(WINDOWS_SYSTEM_BOOL) == "True")

#endif

end module version_control
