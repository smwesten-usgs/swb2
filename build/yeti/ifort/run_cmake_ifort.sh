#!/bin/bash
#remove existing Cmake cache and directories
# remove existing Cmake cache and directories
rm -fr CMake*
rm -rf Testing
rm -rf src
rm -rf tests
rm -f CPack*
rm -f *.txt

module purge
module load intel/psxe-2018u3 
#module load gcc/6.1
#module load hdf5-serial/1.10.1-gcc6.1.0
#module load netcdf-serial/4.4.1.1-hdf51.10.1-gcc6.1.0
#module load zlib/1.2.11-gcc
module load cmake/3.14.2

export GFORTRAN=ifort
export GCC=icc
export LD_LIBRARY_PATH=/home/smwesten/Source_Code/swb2/build/yeti/ifort/static_libs

# define where 'make copy' will place executables
export INSTALL_PREFIX=/usr/local/bin

# define other variables for use in the CMakeList.txt file
# options are "Release" or "Debug"
export BUILD_TYPE="Release"

# define platform and compiler specific compilation flags
export CMAKE_Fortran_FLAGS_DEBUG="-O0 -cpp -g -ggdb -traceback -check bounds -extend_source"
export CMAKE_Fortran_FLAGS_RELEASE="-O2 -cpp -traceback -extend_source -xHost -no-cxxlib -heap-arrays 64"

# set important environment variables
export FC=$GFORTRAN
export CC=$GCC

cmake "../../.." -G "Unix Makefiles"                         \
-DCMAKE_BUILD_TYPE="$BUILD_TYPE "                            \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALL_PREFIX "               \
-DCMAKE_Fortran_FLAGS_DEBUG="$CMAKE_Fortran_FLAGS_DEBUG "    \
-DLD_LIBRARY_PATH="$LD_LIBRARY_PATH"                         \
-DCMAKE_Fortran_FLAGS_RELEASE="$CMAKE_Fortran_FLAGS_RELEASE"
