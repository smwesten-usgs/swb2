gfortran -O strings.F90 string_list.F90 data_file.F90 lookup_table_landuse.F90 test_landuse_table.F90 ^
data_column.F90 data_frame.F90 datetime.F90 ^
types_new.F90 exceptions.F90 constants_and_conversions.F90 -fexceptions -fbounds-check -o lu_test
