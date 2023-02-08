# ClientTypes

Maintains types used to communicate with the `client`/editor:

- API request/response payloads
- requests/responses used in Analysis via WebAssembly-compiled code
- Pusher.com payloads
- data injected into `ui.html`

This project intentionally has no dependencies on other Dark projects (other than
Prelude), to sensure no internal domain types are referenced.

Translation between these types and "domain types" is provided via separate
`ClientTypes2___Types` projects.

## Note