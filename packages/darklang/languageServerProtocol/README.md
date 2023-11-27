# Language Server Protocol support

The `@Darklang.LanguageServerProtocol` module contains many types (and functions?)
towards supporting the Language Server Protocol. This was written per the 3.17.0
spec, found here: https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification
and is largely organized similarly to the spec.

Base types are found in the `common.dark` file, most the rest of the files are
organized by the LSP spec sections, and `io.dark` is the main entry point for handling
incoming messages and dispatching responses and notifications to the client.
(TODO nothing is actually in io.dark - it's a place for future abstraction)

Note: this is framed largely as an LSP _Server_ specifically, although the spec
notes that both parties may act as both client and server - we're ignoring that, as
it's not really ever done in reality.

Many types were borrowed from the most 'official' sample LSP server:
https://github.com/microsoft/vscode-languageserver-node. The organization of that
project is quite hard to follow, though, so we've reorganized things here.

## Some terms

- **client**: the editor that is using the LSP server
- **server**: the LSP server
- **client capabilities**: the capabilities that the client has, which the server
  can use to determine what features to support (i.e. what messages to send).
  These are sent by the client during the `initialize` handshake.
- **server capabilities**: the capabilities that the server has, which the client
  can use to determine what features to use (i.e. what messages to send).
  These are sent by the server during the `initialize` handshake.
- **document**: a file that is open in the client
- **workspace**: the set of documents that are open in the client
- **request**: a message sent from one LSP party to another, which expects a result (i.e. contains an `id`)
- **result**: a message sent from one LSP party to another, in response to a request
  (i.e. contains an `id` corresponding to an earlier request)
- **notification**: a message sent from one LSP party to another, which does not expect a result
- **message**: a general term for either a notification, request, or response (error or result)

## Some patterns

- try to keep type names the same as the TypeScript ones in the spec
- per-file organization
  - supporting types at the top
  - types around requests, results, and notifications in the middle
  - client/server capabilities at the end of the file
- many types are defined as interfaces with `extends` such as:

  ```js
  export interface CompletionParams extends TextDocumentPositionParams {
    context?: CompletionContext;
  }
  ```

  We could represent this in Darklang by inlining those properties in this type, resulting in something like

  ```fsharp
  type CompletionParams = {
    textDocument: TextDocumentIdentifier;
    position: Position;

    context: CompletionContext;
  }
  ```

  or we could add fields per each `extends`:

  ```fsharp
  type CompletionParams = {
    textDocumentPosition: TextDocumentPositionParams
    context: Option<CompletionContext>
  }
  ```

  The latter is easier to understand, but won't work out of the box if we end up
  switching to our usage-ignorant Json parser/serializer. I'm going to use the
  latter for now, and we can re-evaluate if/when needed. That said, for each case
  where we do this, we should add a comment roughly of `@extends TextDocumentPositionParams`

## TODOs:

- deal with @proposed
- deal with @deprecated
- any more @since?
- deal with io.dark
