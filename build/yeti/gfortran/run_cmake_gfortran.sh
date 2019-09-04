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
module load gcc/6.1
module load hdf5-serial/1.10.1-gcc6.1.0
module load netcdf-serial/4.4.1.1-hdf51.10.1-gcc6.1.0 
module load cmake/3.14.2
#export LIB_HDF5_HL=$( locate hdf5_hl.a | grep usr | grep -v data)
#export LIB_HDF5=$( locate hdf5.a | grep usr | grep -v data)
#export LIB_NETCDF=$( locate netcdf.so.11 | grep usr | grep -v data | grep -v netcdf.so.11.)

#export LIB_Z=$( locate libz.a | grep usr | grep -v data)
#export LIB_SZ=$( locate libsz.a | grep /usr/lib64 )
#export LIB_DL=$(locate libdl.so | grep -v 32 | grep -v libdl.so.)
#export LIB_CURL=$(locate libcurl.so | grep usr | grep -v libcurl.so. )
export LIB_GCC=$( locate libgcc.a | grep -v 32 | grep -v gnu/4/ | grep -v gnu/5/ | grep -v gnu/6/ | grep -v /7/)
export LIB_GFORTRAN=$( locate libgfortran.a | grep -v 32 | grep -v gnu/4/ | grep -v gnu/5/ | grep -v gnu/6/ | grep -v /7/)

export PATH=/usr/local:/usr/local/bin:/usr/local/lib:/usr/bin/cmake:$PATH

#export SWB_EXTERNAL_LIBS="$LIB_NETCDF;$LIB_HDF5_HL;$LIB_HDF5;$LIB_Z;$LIB_DL;$LIB_GCC;$LIB_GFORTRAN;$LIB_CURL"
export SWB_EXTERNAL_LIBS="$LIB_NETCDF;$LIB_GCC;$LIB_GFORTRAN"

# define where 'make copy' will place executables
export INSTALL_PREFIX=/usr/local/bin

# define other variables for use in the CMakeList.txt file
# options are "Release" or "Debug"
export BUILD_TYPE="Debug"
export SYSTEM_TYPE="Linux"

# define platform and compiler specific compilation flags
export CMAKE_Fortran_FLAGS_DEBUG="-O0 -g -ggdb -Wuninitialized -fbacktrace -fcheck=all -fexceptions -fmax-errors=6 -fbackslash -ffree-line-length-none -Wno-maybe-uninitialized"
export CMAKE_Fortran_FLAGS_RELEASE="-O2 -mtune=native -ffree-line-length-512 -fbackslash -ffpe-summary='none' -Wno-maybe-uninitialized"

# set important environment variables
export FC=$GFORTRAN
export CC=$GCC

cmake "../../.." -G "Unix Makefiles"                         \
-DSYSTEM_TYPE="$SYSTEM_TYPE "                                \
-DCMAKE_BUILD_TYPE="$BUILD_TYPE "                            \
-DSWB_EXTERNAL_LIBS="$SWB_EXTERNAL_LIBS "                    \
-DCMAKE_INSTALL_PREFIX:PATH="$INSTALL_PREFIX "               \
-DCMAKE_Fortran_FLAGS_DEBUG="$CMAKE_Fortran_FLAGS_DEBUG "    \
-DCMAKE_Fortran_FLAGS_RELEASE="$CMAKE_Fortran_FLAGS_RELEASE"
