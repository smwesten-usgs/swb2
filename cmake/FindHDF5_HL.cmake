find_library(HDF5_HL_LIBRARY
  NAMES hdf5_hl libhdf5_hl libhdf5_serial_hl
  PATHS ENV LD_LIBRARY_PATH /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES  lib lib64 lib/x86_64-linux-gnu/ local/lib/ 
  DOC "hdf5_hl library")

if(HDF5_HL_LIBRARY)
  add_library(HDF5_HL_LIBRARY UNKNOWN IMPORTED
              GLOBAL
  )
  add_dependencies(HDF5_HL_LIBRARY HDF5_LIBRARY ZLIB_LIBRARY)
endif()
