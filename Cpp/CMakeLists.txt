cmake_minimum_required(VERSION 3.10)
project(magnetar_cpp CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find packages
find_package(Drogon REQUIRED)
find_package(SQLite3 REQUIRED)
find_package(Threads REQUIRED)

# Include directories
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/src)

# Source files
file(GLOB_RECURSE SRC_FILES src/*.cc)

# Executable
add_executable(magnetar_cpp ${SRC_FILES})
target_link_libraries(magnetar_cpp PRIVATE Drogon::Drogon SQLite3::SQLite3 Threads::Threads)

# Tests
enable_testing()
add_subdirectory(test)
