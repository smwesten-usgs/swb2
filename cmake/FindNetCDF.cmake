find_library(NETCDF_LIBRARY
  NAMES netcdf libnetcdf
  PATHS ${NETCDF_LIB_PATH} ${LIBRARY_PATH} $ENV{NETCDF_DIR} /usr
  PATH_SUFFIXES  lib lib64 lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib
  DOC "netcdf library"
  NO_DEFAULT_PATH
)

if(NETCDF_LIBRARY)
  add_library(NETCDF_LIBRARY UNKNOWN IMPORTED
             GLOBAL
  )
  add_dependencies(NETCDF_LIBRARY HDF_LIBRARY HDF5_HL_LIBRARY ZLIB_LIBRARY SZIP_LIBRARY CURL_LIBRARY)
endif()
