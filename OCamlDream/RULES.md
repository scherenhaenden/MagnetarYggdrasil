# Rules

1.  **Strict Layer Separation**: Handlers cannot talk to DB directly.
2.  **No Logic in Handlers**: Only parsing and formatting.
3.  **100% Test Coverage**: Mock everything.
4.  **Error Handling**: Use Result types, handle all edge cases.
