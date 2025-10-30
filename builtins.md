# Darklang Builtins

Complete reference of all builtin function modules in Darklang, organized by execution context.

## BuiltinExecution
Standard library functions available in all Darklang code execution contexts.

### Core Types
- **NoModule** - Top-level utility functions
- **Bool** - Boolean operations

### Numeric Types
- **Int8**, **UInt8** - 8-bit signed/unsigned integers
- **Int16**, **UInt16** - 16-bit signed/unsigned integers
- **Int32**, **UInt32** - 32-bit signed/unsigned integers
- **Int64**, **UInt64** - 64-bit signed/unsigned integers
- **Int128**, **UInt128** - 128-bit signed/unsigned integers
- **Float** - Floating-point operations
- **Math** - Mathematical functions

### Data Types
- **Bytes** - Binary data operations
- **Char** - Character operations
- **String** - String manipulation
- **List** - List operations
- **Dict** - Dictionary/map operations
- **DateTime** - Date and time handling
- **Uuid** - UUID generation and manipulation

### Encoding & Serialization
- **Base64** - Base64 encoding/decoding
- **Json** - JSON parsing and serialization
- **AltJson** - Alternative JSON handling

### Network & I/O
- **HttpClient** - HTTP client operations

### Language Tools
- **LanguageTools** - Language manipulation utilities
- **Parser** - Parsing functions

### Security
- **Crypto** - Cryptographic operations
- **X509** - X.509 certificate handling

### Reflection
- **Reflection** - Runtime reflection capabilities

## BuiltinCli
CLI-specific functions for command-line tools.

- **Directory** - Directory operations
- **Environment** - Environment variable access
- **File** - File system operations
- **Execution** - Process execution
- **Output** - Console output
- **Stdin** - Standard input handling
- **Time** - Timing and delays

## BuiltinDarkInternal
Internal-only functions restricted to the dark-internal canvas. All functions wrapped with access control that checks `exeState.program.internalFnsAllowed`.

- **Canvases** - Canvas management
- **DBs** - Database administration
- **Domains** - Domain configuration
- **F404** - 404 handling
- **Infra** - Infrastructure operations
- **Secrets** - Secret management
- **Users** - User administration
- **Workers** - Worker/queue management

## BuiltinCloudExecution
Backend execution functions for cloud-hosted Darklang code.

- **DB** - Database operations
- **Event** - Event handling

## BuiltinPM
Package management functions for working with Darklang packages.

- **Packages** - Package CRUD operations
- **PackageOps** - Package operation handling
- **Instances** - Instance management
- **Branches** - Branch operations
- **Sync** - Synchronization utilities

## BuiltinCliHost
CLI host interface functions.

- **Cli** - CLI application interface

## Access Patterns

### Public
BuiltinExecution modules are available to all Darklang code.

### CLI-Only
BuiltinCli and BuiltinCliHost modules are only available when running in CLI mode.

### Cloud-Only
BuiltinCloudExecution modules are only available in cloud-hosted execution.

### Package-Manager-Only
BuiltinPM modules are only available in package management contexts.

### Internal-Only
BuiltinDarkInternal modules are restricted to the dark-internal canvas via runtime access control.
