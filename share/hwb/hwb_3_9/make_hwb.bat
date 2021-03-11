set CC=gcc
set FC=gfortran
set LD=g++
set CXXFLAGS="-ggdb -g"

::gfortran -ggdb -g -O0 -fno-align-commons -finit-local-zero -fdiagnostics-color=always -ffixed-line-length-none -fbacktrace -fcheck=all -Warray-bounds HI_WB_3_9.f -o HI_WB_3_9__smw.exe
gfortran -g -O2 -static -static-libgfortran -static-libgcc -fno-align-commons -finit-local-zero -fdiagnostics-color=always -ffixed-line-length-none -fbacktrace -fcheck=all -Warray-bounds -c ..\..\src\kiss_random_number_generator.F90
gfortran -g -O2 -static -static-libgfortran -static-libgcc -fno-align-commons -finit-local-zero -fdiagnostics-color=always -ffixed-line-length-none -fbacktrace -fcheck=all -Warray-bounds kiss_random_number_generator.o HI_WB_3_9.f -o HI_WB_3_9__gfortran.exe
