# Rules

1.  **Strict Layer Separation**: Handlers cannot call Repository directly.
2.  **Immutability**: Use immutable data structures where possible.
3.  **Error Handling**: Use exceptions for errors, caught by middleware or handlers to return appropriate HTTP codes.
4.  **Testing**: All public functions must be tested.
