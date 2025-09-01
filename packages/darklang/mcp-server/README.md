# Darklang MCP Server

This directory contains the implementation of a Model Context Protocol (MCP) server for Darklang.

## Overview

The Model Context Protocol (MCP) is a protocol for communication between AI models and external tools/resources. It allows AI models to access external data, execute code, and interact with various services.

This MCP server implementation allows AI models to interact with Darklang, providing access to resources and tools that can be used to extend the capabilities of the AI model.

## Structure

- `mcp-server.dark`: Main server implementation, handles communication with clients
- `initialize.dark`: Handles initialization of the MCP server
- `handleIncomingMessage.dark`: Handles incoming messages from clients

## Usage

The MCP server can be run as a CLI application, reading from stdin and writing to stdout. It follows the MCP specification, using JSON-RPC for communication.

### Resources

Resources represent data that can be accessed by the AI model. The MCP server can provide access to various types of resources, such as:

- Files
- Database records
- API responses
- And more

Resources are identified by URIs, and can be accessed using the `readResource` request.

### Tools

Tools represent functionality that can be executed by the AI model. The MCP server can provide access to various types of tools, such as:

- Code execution
- API calls
- Database queries
- And more

Tools are identified by names, and can be executed using the `callTool` request.

## Extending

To add new resources or tools to the MCP server, you can modify the `initialize.dark` file to include them in the initial state.

### Adding a Resource

To add a new resource, add it to the `resources` dictionary in the initial state:

```darklang
{ resources = Dict {
    "resource://example" ->
      { uri = "resource://example"
        name = "Example Resource"
        description = Stdlib.Option.Option.Some "An example resource"
        mimeType = Stdlib.Option.Option.Some "text/plain" }
  }
}
```

### Adding a Tool

To add a new tool, add it to the `tools` dictionary in the initial state:

```darklang
{ tools = Dict {
    "exampleTool" ->
      { name = "exampleTool"
        description = "An example tool"
        inputSchema = Json.Object [
          ("type", Json.String "object"),
          ("properties", Json.Object [
            ("param1", Json.Object [
              ("type", Json.String "string"),
              ("description", Json.String "A parameter")
            ])
          ]),
          ("required", Json.Array [Json.String "param1"])
        ]
        outputSchema = Stdlib.Option.Option.None }
  }
}
```

Then implement the tool's functionality in the `executeTool` function in `handleIncomingMessage.dark`.