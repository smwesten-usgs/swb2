# set build type; can be "Release", "Debug", or "Profile"
if (BUILD_TYPE STREQUAL "Debug")

  set (CMAKE_BUILD_TYPE "Debug")

elseif (DESIRED_BUILD_TYPE STREQUAL "Profile")

  set (CMAKE_BUILD_TYPE "Profile")

else()

  set(CMAKE_BUILD_TYPE "Release")

endif()

# define which portions of swb to build (i.e. swbstats? as library?)
set( TARGET__SWB_EXECUTABLE "TRUE" )
set( TARGET__SWB_LIBRARY "FALSE" )

