# Testing

## Strategy
- **Unit Tests**: Test Services and Models using standard `cargo test`.
- **Repository Tests**: Mocking `spin_sdk::sqlite` is difficult in unit tests without a host, so we rely on abstracting the connection or integration tests.
- **Integration Tests**: Spin provides testing capabilities, but for now we focus on logic correctness.

## Tools
- `cargo test`
- `spin test` (future)
