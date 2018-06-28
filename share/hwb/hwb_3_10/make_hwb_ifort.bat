set FORTRAN_CMD=ifort /O2 /QxHost /static /fpp

%FORTRAN_CMD% /extend-source:132 kiss_random_number_generator.obj HI_WB_3_10_frag_seq.f /exe:HI_WB_3_10__ifort.exe
