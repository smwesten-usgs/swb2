find_library(HDF5_LIBRARY
  NAMES hdf5 libhdf5_serial
  HINTS ${HDF5_LIB_DIR} /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib "NetCDF" lib
  DOC "hdf5 library")

if(HDF5_LIBRARY)
  add_library(HDF5_LIBRARY UNKNOWN IMPORTED)
endif()
