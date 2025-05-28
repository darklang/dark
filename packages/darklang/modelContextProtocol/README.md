# Model Context Protocol (MCP) for Darklang

This directory contains the Darklang implementation of the Model Context Protocol (MCP).

The Model Context Protocol is a protocol for communication between AI models and external tools/resources. It allows AI models to access external data, execute code, and interact with various services.

Similar to the Language Server Protocol (LSP), MCP uses JSON-RPC for communication between clients and servers.

## Structure

- `common.dark`: Common types and functions used throughout the MCP implementation
- `io.dark`: Communication with clients, message parsing and handling
- `tracing.dark`: Tracing and logging utilities
- Lifecycle: Initialization, shutdown, and other lifecycle events
- Resources: Access to external resources
- Tools: Execution of external tools

## Usage

This protocol implementation can be used to create MCP servers that provide tools and resources to AI models.
