include(CTest)

if(BUILD_TESTING)
  file(GLOB TESTS "*_utest.cpp")

  add_executable(ecor_tests ${TESTS})
  target_link_libraries(ecor_tests PRIVATE ecor)
  add_test(NAME ecor_tests COMMAND ecor_tests)
endif()
