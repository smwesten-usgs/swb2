find_library(ZLIB_LIBRARY
  NAMES zlib.a zlibstatic z libz zlib 
  HINTS ${ZLIB_LIB_DIR} /usr "c:/MinGW64" "c:/Program Files"
  PATH_SUFFIXES lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib "NetCDF"
  DOC "zlib library")

if( ZLIB_LIBRARY )
  add_library(ZLIB_LIBRARY UNKNOWN IMPORTED)
endif()
