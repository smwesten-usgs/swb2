
execute_process(
  COMMAND ${PYTHON_CMD} make_test_suite.py
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

file(GLOB fortran_files
     RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
     *.F90 )

#get_property(moduleDir TARGET swblib PROPERTY Fortran_MODULE_DIRECTORY)

add_executable(swbtest
              ${fortran_files})

target_link_libraries(swbtest swb_lib proj4_lib)
target_include_directories(swbtest PUBLIC ${CMAKE_BINARY_DIR}/src)

if ("${SYSTEM_TYPE}" STREQUAL "win_x64" OR "${SYSTEM_TYPE}" STREQUAL "win_x86")
  install(TARGETS swbtest "${PROJECT_SOURCE_DIR}\test\unit_tests")
else()
  install(TARGETS swbtest DESTINATION "${PROJECT_SOURCE_DIR}/test/unit_tests")
endif()
