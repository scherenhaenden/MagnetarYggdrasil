# How to Run

## Prerequisites

1.  **Rust & Cargo**: [Install Rust](https://rustup.rs/)
2.  **Wasm Target**: `rustup target add wasm32-wasi`
3.  **Spin CLI**: [Install Spin](https://developer.fermyon.com/spin/install)

## Build

```bash
cargo build --target wasm32-wasi --release
```

## Run

```bash
spin up --build
```

The server will start at `http://127.0.0.1:3000`.

## Testing

Run unit tests (standard Rust tests):

```bash
cargo test
```

Note: Integration tests involving the full Spin runtime are typically done by running the server and hitting it with `curl` or using `spin test` (if configured), but standard `cargo test` covers the logic layers.
