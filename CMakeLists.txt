cmake_minimum_required(VERSION 3.19)

option(ECOR_TESTS_ENABLED "Enable tests" OFF)

project(ecor)

include(CMakePackageConfigHelpers)

add_library(zll INTERFACE dep/zll/zll.hpp)
target_include_directories(
  zll INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/dep/zll>
                $<INSTALL_INTERFACE:include/zll>)

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

if(ECOR_TESTS_ENABLED)
  add_subdirectory(test)
endif()
