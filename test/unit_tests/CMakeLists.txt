# Unit tests using the vendored test-drive framework
# Requires: swb_lib and proj4_lib targets from parent CMakeLists.txt

set(TEST_SRCS
  testdrive.F90
  test_fixtures.F90
  test_timer.F90
  test_allocatable_string.F90
  test_exceptions.F90
  test_gash.F90
  test_datetime.F90
  test_fao56.F90
  tester.F90
)

# Assign __SRCNAME__ for each source file (used by __FILE__ alternatives)
foreach(f IN LISTS TEST_SRCS)
  get_filename_component(b ${f} NAME)
  set_source_files_properties(${f} PROPERTIES COMPILE_DEFINITIONS "__SRCNAME__=\"${b}\"")
endforeach()

add_executable(swbtest ${TEST_SRCS})
target_link_libraries(swbtest swb_lib proj4_lib ${EXTERNAL_LIBRARIES})

# Register tests with CTest
enable_testing()

# Run all suites
add_test(NAME "All unit tests"
         COMMAND swbtest
         WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})

# Register individual suites for selective execution
foreach(suite timer allocatable_string exceptions gash datetime fao56)
  add_test(NAME ${suite}
           COMMAND swbtest ${suite}
           WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
endforeach()
