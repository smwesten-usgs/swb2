
#if(TARGET__SWB_TESTS)

  add_executable(test__FAO56
            ${PROJECT_SOURCE_DIR}/src/constants_and_conversions.F90
		${PROJECT_SOURCE_DIR}/src/datetime.F90
            ${PROJECT_SOURCE_DIR}/src/dictionary.F90
            ${PROJECT_SOURCE_DIR}/src/exceptions.F90
            ${PROJECT_SOURCE_DIR}/src/file_operations.F90
            ${PROJECT_SOURCE_DIR}/src/logfiles.F90
            ${PROJECT_SOURCE_DIR}/src/parameters.F90
            ${PROJECT_SOURCE_DIR}/src/soil_moisture__FAO_56.F90
            ${PROJECT_SOURCE_DIR}/src/simulation_datetime.F90
            ${PROJECT_SOURCE_DIR}/src/strings.F90
            ${PROJECT_SOURCE_DIR}/src/string_list.F90
            test__FAO56.F90
            )

  set_target_properties(test__FAO56
            PROPERTIES COMPILE_DEFINITIONS "${PREPROCESSOR_DEFINES}")

  target_link_libraries(test__FAO56 ${EXTRA_LIBS})


  add_executable(test__GDD
            ${PROJECT_SOURCE_DIR}/src/constants_and_conversions.F90
        ${PROJECT_SOURCE_DIR}/src/datetime.F90
            ${PROJECT_SOURCE_DIR}/src/dictionary.F90
            ${PROJECT_SOURCE_DIR}/src/exceptions.F90
            ${PROJECT_SOURCE_DIR}/src/file_operations.F90
            ${PROJECT_SOURCE_DIR}/src/logfiles.F90
            ${PROJECT_SOURCE_DIR}/src/parameters.F90
            ${PROJECT_SOURCE_DIR}/src/strings.F90
            ${PROJECT_SOURCE_DIR}/src/string_list.F90
            test__GDD.F90
            )

  set_target_properties(test__GDD
            PROPERTIES COMPILE_DEFINITIONS "${PREPROCESSOR_DEFINES}")

  target_link_libraries(test__GDD ${EXTRA_LIBS})


#endif()
