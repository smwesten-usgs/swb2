find_library(HDF5_HL_LIBRARY
  NAMES hdf5_hl.a hdf5_hl libhdf5_serial_hl
  HINTS ${LD_LIBRARY_PATH} /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib/x86_64-linux-gnu/ local/lib/ lib/x86_64-linux-gnu/hdf5/serial/  local/lib64 x86_64-w64-mingw32/lib "netCDF"
  DOC "hdf5_hl library")

if(HDF5_HL_LIBRARY)
  add_library(HDF5_HL_LIBRARY UNKNOWN IMPORTED
              GLOBAL
  )
endif()

add_dependencies(HDF5_HL_LIBRARY HDF5_LIBRARY ZLIB_LIBRARY)
