set FORTRAN_CMD=ifort /O2 /QxHost /static /fpp

%FORTRAN_CMD% /compile-only ..\..\src\kiss_random_number_generator.F90
%FORTRAN_CMD% /extend-source:132 kiss_random_number_generator.obj HI_WB_3_9.f /exe:HI_WB_3_9__ifort.exe
