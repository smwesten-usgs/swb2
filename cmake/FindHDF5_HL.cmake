find_library(HDF5_HL_LIBRARY
  NAMES hdf5_hl.a hdf5_hl libhdf5_serial_hl
  HINTS ${HDF5_LIB_DIR} /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib "NetCDF"
  DOC "hdf5_hl library")

add_library(HDF5_HL_LIBRARY UNKNOWN IMPORTED)
set_target_properties(HDF5_HL_LIBRARY PROPERTIES
  IMPORTED_LOCATION "${HDF5_HL_LIBRARY}" )

set(EXTERNAL_LIBRARIES ${HDF5_HL_LIBRARY} ${EXTERNAL_LIBRARIES})
