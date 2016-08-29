set( FILE_UTILS_TEST_SRC
  ${PROJECT_SOURCE_DIR}/src/constants_and_conversions.F90
  ${PROJECT_SOURCE_DIR}/src/get_libc_error_number.c
  ${PROJECT_SOURCE_DIR}/src/exceptions.F90
  ${PROJECT_SOURCE_DIR}/src/logfiles.F90
  ${PROJECT_SOURCE_DIR}/src/generated/version_control.F90
  ${PROJECT_SOURCE_DIR}/src/file_utilities.F90
  ${PROJECT_SOURCE_DIR}/src/strings.F90

  test__file_utils.F90
  )

add_executable( test__file_utils ${FILE_UTILS_TEST_SRC} )

foreach(f IN LISTS FILE_UTILS_TEST_SRC )
  get_filename_component(b ${f} NAME)
  set_source_files_properties(${f} PROPERTIES
  COMPILE_DEFINITIONS "__SRCNAME__=\"${b}\"")
endforeach()

set_target_properties(test__file_utils
  PROPERTIES COMPILE_DEFINITIONS "${PREPROCESSOR_DEFINES}")

set( PROJ4_TEST_SRC
  ${PROJECT_SOURCE_DIR}/src/constants_and_conversions.F90
  ${PROJECT_SOURCE_DIR}/src/logfiles.F90
  ${PROJECT_SOURCE_DIR}/src/exceptions.F90
  ${PROJECT_SOURCE_DIR}/src/string_list.F90
  ${PROJECT_SOURCE_DIR}/src/strings.F90
  ${PROJECT_SOURCE_DIR}/src/proj4_support.F90
  ${PROJECT_SOURCE_DIR}/src/generated/version_control.F90

  test__proj4_support.F90
  )

add_executable(test__proj4_support ${PROJ4_TEST_SRC})

foreach(f IN LISTS PROJ4_TEST_SRC )
  get_filename_component(b ${f} NAME)
  set_source_files_properties(${f} PROPERTIES
    COMPILE_DEFINITIONS "__SRCNAME__=\"${b}\"")
endforeach()


set_target_properties(test__proj4_support
          PROPERTIES COMPILE_DEFINITIONS "${PREPROCESSOR_DEFINES}")
