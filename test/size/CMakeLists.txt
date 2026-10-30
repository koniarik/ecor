cmake_minimum_required(VERSION 3.25)
project(ecor_size CXX)

# Code-size probes, compiled for Cortex-M with arm-none-eabi-g++ at -Os. size.py
# writes MATRIX, a file of size_probe() calls, one per probe, header variant and
# core, then builds this project and measures the objects. Configure with the
# toolchain file next to this one.

set(MATRIX
    ""
    CACHE FILEPATH "probe list written by size.py")
set(ECOR_SIZE_EXTRA_FLAGS
    ""
    CACHE STRING "extra compile options for every probe")

include(FetchContent)
file(READ ${CMAKE_CURRENT_SOURCE_DIR}/../../CMakeLists.txt _top_level)
string(REGEX MATCH "GIT_TAG +([^ )\n]+)" _unused "${_top_level}")
FetchContent_Declare(
  zll
  GIT_REPOSITORY https://github.com/koniarik/zll.git
  GIT_TAG ${CMAKE_MATCH_1})
FetchContent_MakeAvailable(zll)

set(ECOR_SIZE_FLAGS
    -Os
    -mthumb
    -fno-exceptions
    -fno-rtti
    -ffunction-sections
    -fdata-sections
    -fno-threadsafe-statics)

# size_probe(<target> <source> <header dir> <cpu> [DEFINE=VALUE ...]): one
# object file.
function(size_probe target source header_dir cpu)
  add_library(${target} OBJECT ${source})
  target_include_directories(${target} PRIVATE ${header_dir})
  target_link_libraries(${target} PRIVATE zll)
  target_compile_features(${target} PRIVATE cxx_std_23)
  target_compile_definitions(${target} PRIVATE ${ARGN})
  target_compile_options(${target} PRIVATE ${ECOR_SIZE_FLAGS} -mcpu=${cpu}
                                           ${ECOR_SIZE_EXTRA_FLAGS})
endfunction()

# size_image(<target> <header dir> <cpu> <lto 0|1> <source> ...): a linked
# executable with no startup code, entered at `_start`, with unreferenced
# sections dropped, so that the figure is what a program pays after linking.
function(size_image target header_dir cpu lto)
  add_executable(${target} ${ARGN})
  set_target_properties(
    ${target} PROPERTIES RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/images
                         SUFFIX .elf)
  target_include_directories(${target} PRIVATE ${header_dir})
  target_link_libraries(${target} PRIVATE zll)
  target_compile_features(${target} PRIVATE cxx_std_23)
  target_compile_options(${target} PRIVATE ${ECOR_SIZE_FLAGS} -mcpu=${cpu}
                                           ${ECOR_SIZE_EXTRA_FLAGS})
  target_link_options(
    ${target}
    PRIVATE
    -Os
    -mcpu=${cpu}
    -mthumb
    -nostartfiles
    --specs=nano.specs
    --specs=nosys.specs
    -Wl,--gc-sections
    -Wl,-e,_start)
  if(lto)
    target_compile_options(${target} PRIVATE -flto=auto)
    target_link_options(${target} PRIVATE -flto=auto)
  endif()
endfunction()

if(MATRIX)
  include(${MATRIX})
endif()
