find_library(HDF5_HL_LIBRARY
  NAMES hdf5_hl libhdf5_hl hdf5_serial_hl
  PATHS ${HDF5_HL_LIB_PATH} ${LIBRARY_PATH} $ENV{HDF5_ROOT} /usr
  PATH_SUFFIXES lib lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib
  DOC "hdf5_hl library"
  NO_DEFAULT_PATH
)

if(HDF5_HL_LIBRARY)
  add_library(HDF5_HL_LIBRARY UNKNOWN IMPORTED
              GLOBAL
  )
  add_dependencies(HDF5_HL_LIBRARY HDF5_LIBRARY ZLIB_LIBRARY)
endif()
