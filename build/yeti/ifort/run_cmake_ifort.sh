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
module load tools/netcdf-c-4.6.2-intel
module load tools/hdf5-1.8.15-intel
module load cmake/3.14.2
module load zlib/1.2.11-gcc

export GFORTRAN=ifort
export GCC=icc

# define where 'make copy' will place executables
export INSTALL_PREFIX=/usr/local/bin

# define other variables for use in the CMakeList.txt file
# options are "Release" or "Debug"
export BUILD_TYPE="Release"

# define platform and compiler specific compilation flags
export CMAKE_Fortran_FLAGS_DEBUG="-O0 -cpp -g -ggdb -traceback -check bounds -extend_source"
export CMAKE_Fortran_FLAGS_RELEASE="-O2 -cpp -traceback -extend_source"

# set important environment variables
export FC=$GFORTRAN
export CC=$GCC

cmake "../../.." -G "Unix Makefiles"                         \
-DCMAKE_BUILD_TYPE="$BUILD_TYPE "                            \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALL_PREFIX "               \
-DCMAKE_Fortran_FLAGS_DEBUG="$CMAKE_Fortran_FLAGS_DEBUG "    \
-DCMAKE_Fortran_FLAGS_RELEASE="$CMAKE_Fortran_FLAGS_RELEASE"
