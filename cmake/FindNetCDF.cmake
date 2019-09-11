find_library(NetCDF_LIBRARY
  NAMES netcdf.a netcdf netcdf.so libnetcdf11 libnetcdf11.so
  HINTS /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib "NetCDF"
  DOC "netcdf library")

add_library(NetCDF_LIBRARY UNKNOWN IMPORTED)
set_target_properties(NetCDF_LIBRARY PROPERTIES
  IMPORTED_LOCATION "${NetCDF_LIBRARY}")

set(EXTERNAL_LIBRARIES ${EXTERNAL_LIBRARIES} ${NetCDF_LIBRARY})
