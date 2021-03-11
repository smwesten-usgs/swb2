#!/bin/bash
#remove existing Cmake cache and directories
# remove existing Cmake cache and directories
rm -fr CMake*
rm -rf Testing
rm -rf src
rm -rf tests
rm -f CPack*
rm -f *.txt

# set CMAKE-related and build-related variables
export GCC=/opt/local/bin/gcc-mp-5
export GFORTRAN=/opt/local/bin/gfortran-mp-5
export GPP=/opt/local/bin/g++-mp-5
export CMAKEROOT=/usr/bin/cmake
export R_HOME=/usr/bin/R

export LIB_HDF5_HL=$( locate hdf5_hl.a | grep opt )
export LIB_HDF5=$( locate hdf5.a | grep opt )
export LIB_NETCDF=$( locate netcdf.dylib | grep opt )
export LIB_Z=$( locate libz.a | grep opt )
export LIB_GCC=$( locate libgcc.a | grep opt )
export LIB_GFORTRAN=$( locate libgfortran.a | grep opt )

export PATH=/opt/local:/opt/local/bin:/opt/local/lib:$PATH

# define where 'make copy' will place executables
export INSTALL_PREFIX=/usr/local/bin

# define other variables for use in the CMakeList.txt file
# options are "Release" or "Debug"
export BUILD_TYPE="RELEASE"
export OS="mac_osx"

# define platform and compiler specific compilation flags
export CMAKE_Fortran_FLAGS_DEBUG="-O0 -g -ggdb -Wuninitialized -fbacktrace -fcheck=all -fexceptions -fsanitize=null -fsanitize=leak -fmax-errors=6 -fbackslash -ffree-line-length-none"
#set CMAKE_Fortran_FLAGS_RELEASE="-O2 -mtune=native -floop-parallelize-all -flto -ffree-line-length-none -static-libgcc -static-libgfortran"
#export CMAKE_Fortran_FLAGS_RELEASE="-O3 -mtune=native -ffree-line-length-none -ffpe-summary='none' -fopenmp"

export CMAKE_Fortran_FLAGS_RELEASE="-O2 -mtune=native -ffree-line-length-512 -fbackslash -ffpe-summary='none'"


# set important environment variables
export FC=$GFORTRAN
export CC=$GCC
export CXX=$GPP
export AR=gcc-ar-mp-5
export NM=gcc-nm-mp-5
export LD=/usr/bin/ld
export STRIP=/usr/bin/strip
export CMAKE_RANLIB=gcc-ranlib-$COMPILER_MAJ_VERSION

cmake ../../.. -G "Unix Makefiles"                           \
-DSYSTEM_TYPE="$SYSTEM_TYPE "                                \
-DLIB_HDF5_HL="$LIB_HDF5_HL "                                \
-DLIB_HDF5="$LIB_HDF5 "                                      \
-DLIB_Z="$LIB_Z "                                            \
-DLIB_NETCDF="$LIB_NETCDF "                                  \
-DLIB_GCC="$LIB_GCC "                                        \
-DLIB_GFORTRAN="$LIB_GFORTRAN "                              \
-DCMAKE_BUILD_TYPE="$BUILD_TYPE "                            \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALL_PREFIX "               \
-DCMAKE_Fortran_FLAGS_DEBUG="$CMAKE_Fortran_FLAGS_DEBUG "    \
-DCMAKE_Fortran_FLAGS_RELEASE="$CMAKE_Fortran_FLAGS_RELEASE"
