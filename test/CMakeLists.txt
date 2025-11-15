include(CTest)

if(BUILD_TESTING)
  # Generate README test file from README.md
  set(README_MD "${CMAKE_SOURCE_DIR}/README.md")
  set(README_CPP "${CMAKE_CURRENT_BINARY_DIR}/readme_test.cpp")
  set(GEN_SCRIPT "${CMAKE_CURRENT_SOURCE_DIR}/gen_readme_cpp.py")

  add_custom_command(
    OUTPUT ${README_CPP}
    COMMAND ${Python3_EXECUTABLE} ${GEN_SCRIPT} ${README_MD} ${README_CPP}
    DEPENDS ${README_MD} ${GEN_SCRIPT}
    COMMENT "Generating README test from README.md")

  # Add generated file to test sources
  file(GLOB TESTS "*_utest.cpp")
  list(APPEND TESTS ${README_CPP})

  add_executable(ecor_tests ${TESTS})
  target_include_directories(ecor_tests PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
  target_link_libraries(ecor_tests PRIVATE ecor)
  add_test(NAME ecor_tests COMMAND ecor_tests)
endif()
