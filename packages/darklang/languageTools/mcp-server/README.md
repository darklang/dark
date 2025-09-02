# Darklang MCP Server

This directory contains the implementation of a Model Context Protocol (MCP) server for Darklang.

## Overview

The Model Context Protocol (MCP) is a protocol for communication between AI models and external tools/resources. It allows AI models to access external data, execute code, and interact with various services.

This MCP server implementation allows AI models to interact with Darklang, providing access to resources and tools that can be used to extend the capabilities of the AI model.

## Structure

- `mcp-server.dark`: Main server entry point
- `state.dark`: Server state management
- `logging.dark`: Logging utilities
- `io.dark`: Input/output handling
- `initialize.dark`: Server initialization logic
- `handleIncomingMessage.dark`: Central message routing
- `toolHandlers.dark`: Tool request handling
- `resourceHandlers.dark`: Resource request handling
- `promptHandlers.dark`: Prompt request handling
- `tools/`: Individual tool implementations
- `resources/`: Individual resource implementations
- `prompts/`: Individual prompt implementations

## Usage

The MCP server can be run as a CLI application, reading from stdin and writing to stdout.
It follows the MCP specification (2025-06-18), using JSON-RPC for communication.

So far, we've only really been testing it against Claude Code, within the devcontainer.