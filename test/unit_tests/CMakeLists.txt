execute_process(
  COMMAND ${PYTHON_CMD} make_test_suite.py
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

file(GLOB fortran_files
     RELATIVE ${CMAKE_CURRENT_SOURCE_DIR}
     *.F90 )

     # the following snippet assigns the local filename to __SRCNAME__
 # for each of the files contained in the 'SRCS' list
 #
 # i.e. rather than using __FILE__, which might expand to
 # d:/Some_dir/some_subdir/filename.F90, we can use
 # __SRCNAME__, which expands to 'filename.F90'

foreach(f IN LISTS fortran_files)
  get_filename_component(b ${f} NAME)
  set_source_files_properties(${f} PROPERTIES COMPILE_DEFINITIONS "__SRCNAME__=\"${b}\"")
endforeach()

add_executable(swbtest
  ${fortran_files})

target_link_libraries(swbtest swb_lib proj4_lib)

# see https://stackoverflow.com/questions/43917443/fortran-module-files-not-found-by-cmake
# target_include_directories(swbtest PUBLIC $<TARGET_PROPERTY:swb_lib,Fortran_MODULE_DIRECTORY>)
include_directories(${CMAKE_CURRENT_BINARY_DIR}/src/generated)