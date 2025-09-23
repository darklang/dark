# Darklang Internal MCP Server

This is an MCP server that provides internal development tools, resources, and prompts for Darklang development. It includes tools for executing Darklang code, searching functions, typechecking, and accessing various development resources.

## Features

### Tools

- **executeDarklang**: Execute Darklang code directly
- **searchFunctions**: Search for functions in the codebase
- **typecheck**: Perform type checking on Darklang code
- **listPackages**: List available Darklang packages

### Resources

- **builtins**: Access to Darklang builtin functions
- **stdlib**: Standard library documentation
- **packages**: Package information and documentation
- **types**: Type system information
- **config**: Configuration details
- **logs**: Development logs access
- **examples**: Code examples and patterns
- **syntax**: Darklang syntax reference

### Prompts

- **codeReview**: Get code review assistance
- **documentFunction**: Generate function documentation
- **convertTypes**: Help with type conversions
- **debugHelper**: Debugging assistance
- **patternMatch**: Pattern matching guidance

## Installation

To add this MCP server to Claude Code, run:

```bash
claude mcp add darklang-internal ./scripts/run-cli run @Darklang.Internal.DarklangInternalMcpServer.main
```
