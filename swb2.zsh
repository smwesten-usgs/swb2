#!/bin/bash

# module switch PrgEnv-cray PrgEnv-intel
# module load cray-hdf5
# module load cray-netcdf
# module load intel-oneapi
# module load meson ninja

# point Meson to intel compilers
export FC=gfortran
export CC=clang
export CXX=clang

rm -rf build
mkdir build
meson setup build && cd build && meson compile

