#!/bin/bash

module load cray-hdf5
module load cray-netcdf
module load intel-oneapi
module load meson ninja

# point Meson to intel compilers
export FC=ifx
export CC=icx
export CXX=icx

rm -rf builddir
mkdir builddir
meson setup builddir ../.. && cd builddir && meson compile
cd src
cp swb2 ../../../../bin/linux_x64/swb2
cp swbstats2 ../../../../bin/linux_x64/swbstats2

cd ../test/unit_tests
cp swbtest ../../../../../test/unit_tests/swbtest
