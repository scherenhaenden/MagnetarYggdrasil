find_package(GTest REQUIRED)

# Include directories
include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../src)

# Define a variable for source files to exclude main.cc (if necessary)
# For simplicity, we'll just link the implementation files
file(GLOB_RECURSE TEST_SRC_FILES *.cc)

# Re-list source files from src/ but exclude main.cc to avoid duplicate main symbol
file(GLOB_RECURSE SRC_FILES ${CMAKE_CURRENT_SOURCE_DIR}/../src/*.cc)
list(FILTER SRC_FILES EXCLUDE REGEX "main.cc")

add_executable(magnetar_test ${TEST_SRC_FILES} ${SRC_FILES})
target_link_libraries(magnetar_test PRIVATE Drogon::Drogon SQLite3::SQLite3 Threads::Threads GTest::GTest GTest::Main)
add_test(NAME magnetar_test COMMAND magnetar_test)
