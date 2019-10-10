find_library(ZLIB_LIBRARY
  NAMES zlib.a zlibstatic z libz 
  HINTS ${LIBRARY_PATH} /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib "NetCDF"
  DOC "zlib library")

add_library(ZLIB_LIBRARY UNKNOWN IMPORTED)

if( ZLIB_LIBRARY )
  set(EXTERNAL_LIBRARIES ${ZLIB_LIBRARY} ${EXTERNAL_LIBRARIES})
endif()
