find_library(NetCDF_LIBRARY
  NAMES netcdf.a netcdf netcdf.so libnetcdf11 libnetcdf11.so
  HINTS /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib "NetCDF"
  DOC "netcdf library")

find_path(NetCDF_INCLUDE_DIR
  NAMES netcdf.h netcdf
  HINTS /usr "c:/Program Files" /usr/include
  PATH_SUFFIXES lib/x86_64-linux-gnu/ local/lib/ local/lib64 "NetCDF"
  DOC "netcdf include dir")

add_library(NetCDF_LIBRARY UNKNOWN IMPORTED)
set_target_properties(NetCDF_LIBRARY PROPERTIES
  IMPORTED_LOCATION "${NetCDF_LIBRARY}"
  INTERFACE_INCLUDE_DIRECTORIES "${NetCDF_INCLUDE_DIR}")

set(INCLUDE_DIRECTORIES ${INCLUDE_DIRECTORIES} ${NetCDF_INCLUDE_DIR})
set(EXTERNAL_LIBRARIES ${EXTERNAL_LIBRARIES} ${NetCDF_LIBRARY})
