find_library(QUADMATH_LIBRARY
  NAMES libquadmath.a quadmath.a quadmath
  HINTS ${QUADMATH_LIB_PATH} ${LIBRARY_PATH} /usr
  PATH_SUFFIXES lib local/lib64 x86_64-w64-mingw32/lib lib/gcc/x86_64-w64-mingw32/8.1.0
  DOC "quadmath library"
  NO_DEFAULT_PATH
)

if(QUADMATH_LIBRARY)
  message("-- adding QUADMATH_LIBRARY to project")
  add_library(QUADMATH_LIBRARY UNKNOWN IMPORTED
              GLOBAL
  )
endif()

