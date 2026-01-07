# How to Run AdaSPARK

This project uses GNAT and `gprbuild` for compilation.

## Prerequisites

*   GNAT toolchain (including `gprbuild`)
*   `make` (optional, if Makefile is present)

## Build

To build the project, run:

```bash
gprbuild -P magnetar_ada.gpr
```

## Run

After building, the executable will be in the `obj` or `bin` directory (depending on project configuration).

```bash
./obj/magnetar_ada
```

(Note: Adjust path if `magnetar_ada.gpr` specifies a different `Exec_Dir`)

## Test

To run tests (if implemented):

```bash
./obj/test_runner
```
