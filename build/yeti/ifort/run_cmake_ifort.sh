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
module load cmake/3.14.2

export GFORTRAN=ifort
export GCC=icc

# define where 'make copy' will place executables
export INSTALL_PREFIX=/usr/local/bin

export HDF5_LIB_DIR=build/yeti/ifort/static_libs
export NETCDF_LIB_DIR=$HDF5_LIB_DIR
export ZLIB_LIB_DIR=$HDF5_LIB_DIR

# define other variables for use in the CMakeList.txt file
# options are "Release" or "Debug"
export BUILD_TYPE="Release"

# define platform and compiler specific compilation flags
export CMAKE_Fortran_FLAGS_DEBUG="-O0 -cpp -g -ggdb -traceback -check bounds -extend_source"
export CMAKE_Fortran_FLAGS_RELEASE="-O2 -cpp -traceback -extend_source -xHost -no-cxxlib"

# set important environment variables
export FC=$GFORTRAN
export CC=$GCC

cmake "../../.." -G "Unix Makefiles"                         \
-DCMAKE_BUILD_TYPE="$BUILD_TYPE "                            \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALL_PREFIX "               \
-DCMAKE_Fortran_FLAGS_DEBUG="$CMAKE_Fortran_FLAGS_DEBUG "    \
-DHDF5_LIB_DIR="$HDF5_LIB_DIR"                               \
-DNETCDF_LIB_DIR="$NETCDF_LIB_DIR"                           \
-DZLIB_LIB_DIR="$ZLIB_LIB_DIR"                               \
-DCMAKE_Fortran_FLAGS_RELEASE="$CMAKE_Fortran_FLAGS_RELEASE"
