# CMake project for swb2

# This script expects that the following configuration variables are
# set and passed during CMake invocation:

# CMAKE_BINARY_DIR
# DESIRED_BUILD_TYPE     | possible values: "debug", "release", "profile"
# CMAKE_INSTALL_PREFIX   | ex: "D:/DOS"
# Fortran_FLAGS_DEBUG
# Fortran_FLAGS_RELEASE
# Fortran_FLAGS_PROFILE
# PATH_TO_R
# OS                     | possible values: "win_x86", "win_x64", "max_osx", "linux_x64"
# Fortran_COMPILER_NAME  | ex: "gfortran", "ifort"
# COMPILER_VERSION       | ex: "4.8.2"
# LIB_PATH1
# LIB_PATH2
# LIB_PATH3
# LIB_PATH4

message("Processing top-level CMakelists.txt for project swb2")

include( CMakeForceCompiler )

#------------------------------------------------------------------------------------#
#                           Build directory check                                    #
#------------------------------------------------------------------------------------#
if(${CMAKE_SOURCE_DIR}/src STREQUAL ${CMAKE_BINARY_DIR})
  message(FATAL_ERROR "Cannot be built in the source directory. Use out-of-source build instead.")
  message(FATAL_ERROR "  cd /some/work/dir/build")
  message(FATAL_ERROR "  cmake -i /path/to/source")
endif()

#------------------------------------------------------------------------
# Set basic project settings
#------------------------------------------------------------------------
#
project (swb2 Fortran C)
enable_language (Fortran C)

SET(CPACK_GENERATOR "TGZ")
include(CPack)

set(CMAKE_INCLUDE_CURRENT_DIR OFF)

set(PACKAGE_BUGREPORT "smwesten@usgs.gov")
set(PACKAGE_NAME "swb2")
set(PACKAGE_STRING "swb2 2.0 ALPHA")
set(PACKAGE_TARNAME "swb2")
set(PACKAGE_VERSION "2.0 ALPHA")

cmake_minimum_required(VERSION 2.8)

include( cmake/version_control.cmake )

set( EXTERNAL_LIBS ${SWB_EXTERNAL_LIBS})

#if(MSVC)
  # Automatically adds compiler definitions to all subdirectories too.
  add_definitions(/D_CRT_SECURE_NO_DEPRECATE /DWIN32_LEAN_AND_MEAN)
  # # Disable all warnings
  # string (REPLACE "/W3" "" CMAKE_C_FLAGS "${CMAKE_C_FLAGS}")
  # set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /w")
#endif(MSVC)

#libvcruntime.lib libucrt.lib libifport.lib

link_libraries( ${EXTERNAL_LIBS} )
include_directories( ${INCLUDE_DIRECTORIES} "${PROJECT_SOURCE_DIR}/src/proj4" )

if ("${SYSTEM_TYPE}" STREQUAL "win_x64" OR "${SYSTEM_TYPE}" STREQUAL "win_x86")
  add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb2.exe ${CMAKE_INSTALL_PREFIX}/swb2.exe )
  add_custom_target(copy_swbstats ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats2.exe ${CMAKE_INSTALL_PREFIX}/swbstats2.exe )
else()
  add_custom_target(copy ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swb2 ${CMAKE_INSTALL_PREFIX}/swb2 )
  add_custom_target(copy_swbstats ${CMAKE_COMMAND} -E copy ${CMAKE_BINARY_DIR}/src/swbstats2 ${CMAKE_INSTALL_PREFIX}/swbstats2 )
endif()

include( cmake/print_summary.cmake )

add_subdirectory(src)
add_subdirectory(test/unit_tests)
