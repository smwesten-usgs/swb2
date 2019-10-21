find_library(HDF5_LIBRARY
  NAMES hdf5.a hdf5 libhdf5_serial
  HINTS ${LD_LIBRARY_PATH} /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib lib/x86_64-linux-gnu/hdf5/serial/ "NetCDF" lib
  DOC "hdf5 library"
  NO_DEFAULT_PATH
)


if(HDF5_LIBRARY)
  add_library(HDF5_LIBRARY UNKNOWN IMPORTED
              GLOBAL
  )
endif()

add_dependencies(HDF5_LIBRARY ZLIB_LIBRARY)
