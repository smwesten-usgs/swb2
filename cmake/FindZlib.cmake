find_library(ZLIB_LIBRARY
  NAMES zlib.a libz.a zlibstatic.lib z zlib libz libzlib
  PATHS ${Z_LIB_PATH} ${LIBRARY_PATH} /usr/local/opt/zlib /usr
  PATH_SUFFIXES lib lib/x86_64-linux-gnu/ local/lib/ local/lib64 x86_64-w64-mingw32/lib
  DOC "zlib library"
  NO_DEFAULT_PATH 
)

if( ZLIB_LIBRARY )
  message("-- adding ZLIB_LIBRARY to project")
  add_library(ZLIB_LIBRARY UNKNOWN IMPORTED
             GLOBAL
  )
endif()
