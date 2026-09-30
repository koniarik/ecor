# Builds against an installed ecor; point CMAKE_PREFIX_PATH at the install
# prefix.
cmake_minimum_required(VERSION 3.19)

project(ecor_install_consumer CXX)

find_package(ecor REQUIRED)

add_executable(consumer main.cpp)
target_link_libraries(consumer ecor::ecor)
