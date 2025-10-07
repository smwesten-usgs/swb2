#!/bin/bash

export CC=gcc-8
export FC=gfortran-8
export LD=g++-8
export CXXFLAGS="-ggdb -g"

# gfortran-8 -g -O2 -fno-align-commons -finit-local-zero -fdiagnostics-color=always \
#  -ffixed-line-length-none -fbacktrace -fcheck=all -Warray-bounds                \
#   -c ../../src/kiss_random_number_generator.F90

gfortran-8 -g -O2 -fno-align-commons -finit-local-zero -fdiagnostics-color=always \
 -ffixed-line-length-none -fbacktrace -fcheck=all -Warray-bounds                \
  HI_WB_3_10_frag_seq.f -o HI_WB_3_10

cp HI_WB_3_10 /usr/local/bin
