# How to Run

## Prerequisites
- Java installed (JDK 11+ recommended).
- Clojure CLI tools installed.

## Running the Server
```bash
clj -M -m clojure-ring.core
```

## Running Tests
```bash
clj -M:test -e "(require 'clojure.test) (clojure.test/run-tests 'clojure-ring.service-test 'clojure-ring.handler-test)"
```
