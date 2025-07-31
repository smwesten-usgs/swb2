#!/bin/bash
#remove existing Cmake cache and directories
# remove existing Cmake cache and directories
rm -fr CMake*
rm -rf Testing
rm -rf src
rm -rf test
rm -rf tests
rm -f CPack*
rm -f *.txt

export PATH=/usr:/usr/local:/usr/local/bin:/usr/local/lib:/usr/bin/cmake:/usr/bin:/bin:/usr/sbin:/sbin:/Applications/CMake.app/Contents/bin

# define other variables for use in the CMakeList.txt file
# options are "Release" or "Debug"
export BUILD_TYPE="Release"

# define platform and compiler specific compilation flags
export CMAKE_Fortran_FLAGS_DEBUG="-O0 -g -gfull -ggdb -Wuninitialized -fbacktrace -fcheck=all -fexceptions -fsanitize=null -fsanitize=leak -fmax-errors=6 -fbackslash -ffree-line-length-none -Wno-maybe-uninitialized"
# "-fpe-trap=overflow,zero"
export CMAKE_Fortran_FLAGS_RELEASE="-O2 -g -march=native -ffree-line-length-512 -fbackslash -ffpe-summary='none' -Wno-maybe-uninitialized"

# set important environment variables
export FC=gfortran-10
export CC=gcc-10
export CXX=g++-10

cmake "../../.." -G "Unix Makefiles"                         \
-DCMAKE_BUILD_TYPE="$BUILD_TYPE "                            \
-DCMAKE_Fortran_FLAGS_DEBUG="$CMAKE_Fortran_FLAGS_DEBUG "    \
-DCMAKE_Fortran_FLAGS_RELEASE="$CMAKE_Fortran_FLAGS_RELEASE"
