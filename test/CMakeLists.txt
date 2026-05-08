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

  # GDB pretty-printer tests (three-phase: gen → run → eval)
  set(GDB_PPRINTER ${CMAKE_SOURCE_DIR}/pprinter.py)
  set(GDB_SCRIPT ${CMAKE_BINARY_DIR}/ecor_gdb_test.gdb)
  set(GDB_LOG ${CMAKE_BINARY_DIR}/ecor_gdb_test.log)

  add_executable(ecor_gdb_test ${CMAKE_CURRENT_SOURCE_DIR}/gdb_test.cpp)
  target_include_directories(ecor_gdb_test PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})
  target_link_libraries(ecor_gdb_test PRIVATE ecor)
  target_compile_options(ecor_gdb_test PRIVATE -g -O0)
  target_compile_definitions(ecor_gdb_test PRIVATE ECOR_DEBUG_PARENT)

  add_test(NAME ecor_gdb_gen COMMAND ecor_gdb_test gen ${GDB_PPRINTER}
                                     ${GDB_SCRIPT} ${GDB_LOG})
  set_tests_properties(ecor_gdb_gen PROPERTIES FIXTURES_SETUP ecor_gdb_script)

  add_test(NAME ecor_gdb_run
           COMMAND gdb ${CMAKE_CURRENT_BINARY_DIR}/ecor_gdb_test -x
                   ${GDB_SCRIPT} --batch)
  set_tests_properties(
    ecor_gdb_run PROPERTIES FIXTURES_REQUIRED ecor_gdb_script FIXTURES_SETUP
                            ecor_gdb_log)

  add_test(NAME ecor_gdb_eval COMMAND ecor_gdb_test eval ${GDB_LOG})
  set_tests_properties(ecor_gdb_eval PROPERTIES FIXTURES_REQUIRED ecor_gdb_log)
endif()
