# C (Kore.io) Implementation

## Prerequisites
*   `kore` (Install via package manager or source: https://kore.io/)
*   `sqlite3` (libsqlite3-dev)
*   `gcc`

## Build
```bash
kodev build
# OR manually
make
```

## Run
```bash
kore run
# OR
kodev run
```

The application will listen on port 8080 (TLS enabled by default in Kore, make sure you have certs or disable TLS in config for local testing if needed, though Kore insists on TLS usually).
Note: This configuration expects `cert.pem` and `key.pem` in the root or configured path.
For development without TLS, you might need `kore_source` tweak or use `kodev` which generates dev certs.

## Tests
To run the test suite:
```bash
make test
```
