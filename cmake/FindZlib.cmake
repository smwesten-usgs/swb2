find_library(ZLIB_LIBRARY
  NAMES zlib.a zlibstatic.lib zlib zlib.so
  HINTS /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib "NetCDF"
  DOC "zlib library")

find_path(ZLIB_INCLUDE_DIR
  NAMES zlib.h
  HINTS /usr "c:/Program Files"
  PATH_SUFFIXES lib/x86_64-linux-gnu/ local/lib/ local/lib64 "NetCDF"
  DOC "zlib include dir")

add_library(ZLIB_LIBRARY UNKNOWN IMPORTED)
set_target_properties(ZLIB_LIBRARY PROPERTIES
  IMPORTED_LOCATION "${ZLIB_LIBRARY}"
  INTERFACE_INCLUDE_DIRECTORIES "${ZLIB_INCLUDE_DIR}")

if( ZLIB_LIBRARY )
  set(EXTERNAL_LIBRARIES ${ZLIB_LIBRARY} ${EXTERNAL_LIBRARIES})
endif()
