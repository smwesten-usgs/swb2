#!/bin/bash
#remove existing Cmake cache and directories
# remove existing Cmake cache and directories
rm -fr CMake*
rm -rf Testing
rm -rf src
rm -rf tests
rm -f CPack*
rm -f *.txt

module switch PrgEnv-cray/6.0.5 PrgEnv-gnu
module load cray-hdf5/1.10.5.2 
module load cray-netcdf/4.6.3.2

export GFORTRAN=gfortran
export GCC=gcc

# define where 'make copy' will place executables
export INSTALL_PREFIX=/usr/local/bin

# define other variables for use in the CMakeList.txt file
# options are "Release" or "Debug"
export BUILD_TYPE="Debug"

# define platform and compiler specific compilation flags
export CMAKE_Fortran_FLAGS_DEBUG="-O0 -g -ggdb -Wuninitialized -static-libgfortran -fbacktrace -fcheck=all -fexceptions -fmax-errors=6 -fbackslash -ffree-line-length-none -Wno-maybe-uninitialized"
export CMAKE_Fortran_FLAGS_RELEASE="-O2 -mtune=native -ffree-line-length-512 -static-libgfortran -fbackslash -ffpe-summary='none' -Wno-maybe-uninitialized"

# set important environment variables
export FC=$GFORTRAN
export CC=$GCC

cmake "../../.." -G "Unix Makefiles"                         \
-DCMAKE_BUILD_TYPE="$BUILD_TYPE "                            \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALL_PREFIX "               \
-DCMAKE_Fortran_FLAGS_DEBUG="$CMAKE_Fortran_FLAGS_DEBUG "    \
-DCMAKE_Fortran_FLAGS_RELEASE="$CMAKE_Fortran_FLAGS_RELEASE"
