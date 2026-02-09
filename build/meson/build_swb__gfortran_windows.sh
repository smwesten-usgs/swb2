#!/bin/sh
# point Meson to new LLVM versions of intel compilers on Windows

rm -rf builddir
mkdir builddir

export LATEST_NETCDF=/ucrt64/bin

export FC=gfortran && export CC=gcc && export CXX=g++ && meson setup -Dnetcdf_root=${LATEST_NETCDF} builddir ../.. && cd builddir && meson compile
cd src
cp swb2 /usr/local/bin/swb2
cp swbstats2 /usr/local/bin/swbstats2

cd ../test/unit_tests
cp swbtest ../../../../../test/unit_tests/swbtest
