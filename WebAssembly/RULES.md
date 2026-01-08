# Rules

1.  **No Logic in Handlers**: Handlers only parse HTTP and call Services.
2.  **No SQL in Services**: Services only call Repositories.
3.  **No HTTP in Repositories**: Repositories only deal with Data.
4.  **100% Test Coverage**: All logic must be tested.
5.  **Strict Error Handling**: Use `Result` and `Option` correctly.
