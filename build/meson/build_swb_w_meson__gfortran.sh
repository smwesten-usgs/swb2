#!/bin/sh
# point Meson to new LLVM versions of intel compilers on Windows

rm -rf builddir
mkdir builddir
export FC=gfortran && export CC=gcc && export CXX=g++ && meson setup builddir ../.. && cd builddir && meson compile
cd src
sudo cp swb2 /usr/local/bin/swb2
sudo cp swbstats2 /usr/local/bin/swbstats2

cd ../test/unit_tests
cp swbtest ../../../../../test/unit_tests/swbtest
