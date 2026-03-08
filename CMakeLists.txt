cmake_minimum_required(VERSION 3.19)

option(ECOR_TESTS_ENABLED "Enable tests" OFF)

project(ecor)

include(CMakePackageConfigHelpers)
include(FetchContent)

# Allow the env var set in .envrc (or CI) to redirect FetchContent to a local
# checkout.
if(DEFINED ENV{FETCHCONTENT_SOURCE_DIR_ZLL}
   AND NOT DEFINED CACHE{FETCHCONTENT_SOURCE_DIR_ZLL})
  set(FETCHCONTENT_SOURCE_DIR_ZLL
      "$ENV{FETCHCONTENT_SOURCE_DIR_ZLL}"
      CACHE
        PATH
        "Local zll source override (from FETCHCONTENT_SOURCE_DIR_ZLL env var)")
endif()

FetchContent_Declare(
  zll
  GIT_REPOSITORY https://github.com/koniarik/zll.git
  GIT_TAG 6159cad0af1e44062ba857a100525326fc6e8781)
FetchContent_MakeAvailable(zll)

add_library(ecor INTERFACE include/ecor/ecor.hpp)
target_include_directories(
  ecor INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
                 $<INSTALL_INTERFACE:include>)
target_compile_features(ecor INTERFACE cxx_std_20)
target_link_libraries(ecor INTERFACE zll)

add_library(ecor::ecor ALIAS ecor)

install(DIRECTORY include/ecor/ DESTINATION include/ecor/)

install(
  TARGETS ecor zll
  EXPORT ecor
  INCLUDES
  DESTINATION include/ecor/)
install(
  EXPORT ecor
  FILE ecor-config.cmake
  NAMESPACE ecor::
  DESTINATION lib/cmake/ecor/)

option(ECOR_SIZE_PROBE_ENABLED "Build the ARM code-size probe binary" OFF)

if(ECOR_SIZE_PROBE_ENABLED)
  add_executable(ecor_size_probe test/size_probe.cpp)
  target_link_libraries(ecor_size_probe PRIVATE ecor)
  target_compile_features(ecor_size_probe PRIVATE cxx_std_20)

  # Post-build: print section totals with arm-none-eabi-size
  find_program(ARM_SIZE arm-none-eabi-size)
  if(ARM_SIZE)
    add_custom_command(
      TARGET ecor_size_probe
      POST_BUILD
      COMMAND ${ARM_SIZE} -A $<TARGET_FILE:ecor_size_probe>
      COMMENT "Section sizes of ecor_size_probe"
      VERBATIM)
  endif()
endif()

if(ECOR_TESTS_ENABLED)
  find_package(
    Python3
    COMPONENTS Interpreter
    REQUIRED)
  enable_testing()
  add_subdirectory(test)
endif()
