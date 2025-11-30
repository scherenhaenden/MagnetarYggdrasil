# C++ (Drogon) Implementation

## Prerequisites

*   **CMake** >= 3.10
*   **C++ Compiler** supporting C++17 (GCC 8+, Clang 6+, MSVC)
*   **Drogon** framework installed.
*   **SQLite3** development libraries.
*   **Jsoncpp**
*   **UUID** library (libuuid on Linux)

## Build

```bash
mkdir build
cd build
cmake ..
make -j$(nproc)
```

## Run

```bash
./magnetar_cpp
```

## Test

```bash
cd build
./magnetar_test
```

## Structure

*   `src/`: Source code
    *   `controllers/`: API endpoints
    *   `services/`: Business logic
    *   `repositories/`: Data access
    *   `models/`: Data structures
    *   `db/`: Database initialization
*   `test/`: Tests
