find_library(NETCDF_LIBRARY
  NAMES netcdf.a netcdf netcdf.so libnetcdf11 libnetcdf11.so
  HINTS /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib "NetCDF"
  DOC "netcdf library")

if(NETCDF_LIBRARY)
  add_library(NETCDF_LIBRARY UNKNOWN IMPORTED)
endif()
