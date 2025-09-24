#!/bin/bash

module switch PrgEnv-cray/6.0.10 PrgEnv-intel/6.0.10
module load cray-hdf5/1.12.1.1
module load cray-netcdf/4.8.1.1
module load intel/19.1.0.166
module load meson ninja

# point Meson to intel compilers
export FC=ifort
export CC=icc
export CXX=icpc

rm -rf builddir
mkdir builddir
meson setup builddir ../.. && cd builddir && meson compile
cd src
cp swb2 ../../../../bin/linux_x64/swb2
cp swbstats2 ../../../../bin/linux_x64/swbstats2

cd ../test/unit_tests
cp swbtest ../../../../../test/unit_tests/swbtest
