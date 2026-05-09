cmake_minimum_required(VERSION 3.19)

option(ECOR_TESTS_ENABLED "Enable tests" OFF)

project(ecor)

include(CMakePackageConfigHelpers)
include(FetchContent)

FetchContent_Declare(
  zll
  GIT_REPOSITORY https://github.com/koniarik/zll.git
  GIT_TAG v1.0.0)
FetchContent_MakeAvailable(zll)

add_library(ecor INTERFACE include/ecor/ecor.hpp)
target_include_directories(
  ecor INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
                 $<INSTALL_INTERFACE:include>)
target_compile_features(ecor INTERFACE cxx_std_23)
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

if(ECOR_TESTS_ENABLED)
  find_package(
    Python3
    COMPONENTS Interpreter
    REQUIRED)
  enable_testing()
  add_subdirectory(test)
endif()
