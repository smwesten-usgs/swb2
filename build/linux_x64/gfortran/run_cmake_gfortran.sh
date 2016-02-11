#!/bin/sh
#remove existing Cmake cache and directories
# remove existing Cmake cache and directories
rm -fr CMake*
rm -f *.txt

# set CMAKE-related and build-related variables
export CMAKEROOT=/share/apps/cmake/3.4.2
export GCC_VERSION=5.3.0
export COMPILER_TRIPLET=x86_64-unknown-linux-gnu
export Fortran_COMPILER_NAME=gfortran
export R_HOME=/usr/bin/R

# define where 'make copy' will place executables
export INSTALL_PREFIX=/usr/local/bin

# define other variables for use in the CMakeList.txt file
# options are "Release" or "Debug"
export BUILD_TYPE="Release"
# options are "x86" (32-bit) or "x64" (64-bit)
export PLATFORM_TYPE="x64"

# define platform and compiler specific compilation flags
export CMAKE_Fortran_FLAGS_DEBUG="-O0 -g -ggdb -fcheck=all -fexceptions -ffree-line-length-none -static-libgcc -static-libgfortran"
#set CMAKE_Fortran_FLAGS_RELEASE="-O2 -mtune=native -floop-parallelize-all -flto -ffree-line-length-none -static-libgcc -static-libgfortran"
export CMAKE_Fortran_FLAGS_RELEASE="-O3 -mtune=native -ftree-parallelize-loops=8 -floop-parallelize-all -ffree-line-length-none -static -static-libgcc -static-libgfortran -I/share/apps/gcc/5.3.0/include -L/share/apps/gcc/5.3.0/lib"

# set important environment variables
export FC=/share/apps/gcc/5.3.0/bin/gfortran
export CC=/share/apps/gcc/5.3.0/bin/gcc
export CXX=/share/apps/gcc/5.3.0/bin/g++
export AR=/share/apps/gcc/5.3.0/bin/gcc-ar
export NM=/share/apps/gcc/5.3.0/bin/gcc-nm

set LIBRARY_PATH=/share/apps/gcc/5.3.0/lib

# set compiler-specific link and compile flags
export LDFLAGS="-flto"
export CPPFLAGS="-DgFortran"

/share/apps/cmake/3.4.2/bin/cmake ../../.. -G "Unix Makefiles" \
-DGCC_VERSION="$GCC_VERSION " \
-DCOMPILER_TRIPLET="$COMPILER_TRIPLET " \
-DFortran_COMPILER_NAME="$Fortran_COMPILER_NAME " \
-DFC="$FC "                       \
-DCC="$CC "                       \
-DPLATFORM_TYPE="$PLATFORM_TYPE " \
-DCMAKE_BUILD_TYPE="$BUILD_TYPE " \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALL_PREFIX " \
-DLIB_PATH1="/share/apps " \
-DCMAKE_Fortran_FLAGS_DEBUG="$CMAKE_Fortran_FLAGS_DEBUG " \
-DCMAKE_Fortran_FLAGS_RELEASE="$CMAKE_Fortran_FLAGS_RELEASE"

