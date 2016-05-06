#!/bin/sh
#remove existing Cmake cache and directories
# remove existing Cmake cache and directories
rm -fr CMake*
rm -f *.txt

# set CMAKE-related and build-related variables
export CMAKEROOT=/usr/bin
export GCC_VERSION=5.3.1
export COMPILER_TRIPLET=x86_64-redhat-linux
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
export CMAKE_Fortran_FLAGS_DEBUG="-O0 -g -ggdb -fbacktrace -fcheck=all -fexceptions -ffree-line-length-none -finit-local-zero"
#set CMAKE_Fortran_FLAGS_RELEASE="-O2 -mtune=native -floop-parallelize-all -flto -ffree-line-length-none -static-libgcc -static-libgfortran"
export CMAKE_Fortran_FLAGS_RELEASE="-O2 -mtune=native -g -fbacktrace -ffree-line-length-none -finit-local-zero"

# set important environment variables
export FC=/usr/bin/gfortran
export CC=/usr/bin/gcc
export CXX=/usr/bin/g++
export AR=/usr/bin/gcc-ar
export NM=/usr/bin/gcc-nm

set LIBRARY_PATH=/usr/lib

# set compiler-specific link and compile flags
export LDFLAGS="-flto -ldl"
export CPPFLAGS="-DgFortran"

/usr/bin/cmake ../../.. -G "Unix Makefiles" \
-DGCC_VERSION="$GCC_VERSION " \
-DCOMPILER_TRIPLET="$COMPILER_TRIPLET " \
-DFortran_COMPILER_NAME="$Fortran_COMPILER_NAME " \
-DFC="$FC "                       \
-DCC="$CC "                       \
-DLIBGCC_PATH="/usr/lib/gcc/x86_64-redhat-linux/5.3.1/"  \
-DPLATFORM_TYPE="$PLATFORM_TYPE " \
-DBUILD_TYPE="$BUILD_TYPE " \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALL_PREFIX " \
-DLIB_PATH1="/usr/lib64" \
-DLIB_PATH2="/usr/local/lib" \
-DLIB_PATH3="/usr/local/lib64" \
-DCMAKE_Fortran_FLAGS_DEBUG="$CMAKE_Fortran_FLAGS_DEBUG " \
-DCMAKE_Fortran_FLAGS_RELEASE="$CMAKE_Fortran_FLAGS_RELEASE"

