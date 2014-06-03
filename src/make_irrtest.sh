gfortran-4.9 -O -Wunused-variable strings.F90 string_list.F90 data_file.F90 lookup_table_irrigation.F90 test_irrigation_table.F90 \
data_column.F90 data_frame.F90 datetime.F90 \
types_new.F90 exceptions.F90 constants_and_conversions.F90 -fexceptions -fbounds-check -o irr_test
