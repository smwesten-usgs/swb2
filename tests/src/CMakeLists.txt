add_executable(test__file_utils
          ${PROJECT_SOURCE_DIR}/src/constants_and_conversions.F90
          ${PROJECT_SOURCE_DIR}/src/get_libc_error_number.c
          ${PROJECT_SOURCE_DIR}/src/exceptions.F90
          ${PROJECT_SOURCE_DIR}/src/logfiles.F90
          ${PROJECT_SOURCE_DIR}/src/generated/version_control.F90
          ${PROJECT_SOURCE_DIR}/src/file_utilities.F90
          ${PROJECT_SOURCE_DIR}/src/strings.F90

          test__file_utils.F90
          )

set_target_properties(test__file_utils
          PROPERTIES COMPILE_DEFINITIONS "${PREPROCESSOR_DEFINES}")

target_link_libraries(test__file_utils ${EXTRA_LIBS})
