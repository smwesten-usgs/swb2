set CC=gcc
set FC=gfortran
set LD=g++
set CXXFLAGS="-ggdb -g"

gfortran -g -O2 -static -static-libgfortran -static-libgcc -fno-align-commons -finit-local-zero -fdiagnostics-color=always -ffixed-line-length-none -fbacktrace -fcheck=all -Warray-bounds HI_WB_3_10_frag_seq.f -o HI_WB_3_10__gfortran.exe
