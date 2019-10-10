find_library(HDF5_HL_LIBRARY
  NAMES hdf5_hl.a hdf5_hl hdf5_hl.so libhdf5_serial_hl.so
  HINTS /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib "NetCDF"
  DOC "hdf5_hl library")

if(HDF5_HL_LIBRARY)
  add_library(HDF5_HL_LIBRARY UNKNOWN IMPORTED)
endif()
