find_library(HDF5_HL_LIBRARY
  NAMES hdf5_hl.a hdf5_hl hdf5_hl.so
  HINTS /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib "NetCDF"
  DOC "hdf5_hl library")

find_path(HDF5_INCLUDE_DIR
  NAMES hdf5_hl.h
  HINTS /usr "c:/Program Files"
  PATH_SUFFIXES lib/x86_64-linux-gnu/ local/lib/ local/lib64 "NetCDF"
  DOC "hdf5_hl include dir")

add_library(HDF5_HL_LIBRARY UNKNOWN IMPORTED)
set_target_properties(HDF5_HL_LIBRARY PROPERTIES
  IMPORTED_LOCATION "${HDF5_HL_LIBRARY}"
  INTERFACE_INCLUDE_DIRECTORIES "${HDF5_HL_INCLUDE_DIR}")
