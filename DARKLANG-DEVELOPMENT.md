# Darklang Development Guide

This comprehensive guide provides essential information for developing with and on the Darklang platform. It covers architecture, development workflows, common patterns, debugging techniques, and best practices.

## Table of Contents

- [Architecture Overview](#architecture-overview)
- [Development Environment](#development-environment)
- [Core Concepts](#core-concepts)
- [Development Workflows](#development-workflows)
- [Language Features](#language-features)
- [Package System](#package-system)
- [Testing](#testing)
- [Debugging](#debugging)
- [Performance](#performance)
- [CLI Development](#cli-development)
- [Contributing](#contributing)

## Architecture Overview

### High-Level Architecture

Darklang is a structured, functional programming language with the following key components:

```
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│   Frontend      │    │    Backend      │    │   Execution     │
│   (Editor/CLI)  │◄──►│   (F#/.NET)     │◄──►│   Environment   │
└─────────────────┘    └─────────────────┘    └─────────────────┘
         │                       │                       │
         ▼                       ▼                       ▼
┌─────────────────┐    ┌─────────────────┐    ┌─────────────────┐
│ Language Server │    │ PackageManager  │    │   Runtime       │
│ Protocol (LSP)  │    │ & Type System   │    │   & Libraries   │
└─────────────────┘    └─────────────────┘    └─────────────────┘
```

### Key Directories

- **`backend/`** - F# backend implementation, type system, execution engine
- **`packages/`** - Darklang packages organized by namespace 
- **`rundir/`** - Runtime directory with logs and temporary files
- **`scripts/`** - Development and build scripts
- **`containers/`** - Docker containers and deployment configurations

### Core Components

1. **Language Tools** (`packages/darklang/languageTools/`)
   - Parser for Dark syntax
   - Type system and type checker
   - Package manager and dependency resolution
   - Language Server Protocol implementation

2. **Standard Library** (`packages/darklang/stdlib/`)
   - Core data types (List, Option, Result, Dict, etc.)
   - HTTP client and server functionality
   - Database operations
   - String, Math, DateTime utilities
   - CLI-specific functions

3. **CLI System** (`packages/darklang/cli/`)
   - Interactive command-line interface
   - Navigation and exploration tools
   - Script management and execution
   - Installation and update system

## Development Environment

### Prerequisites

- **F# and .NET** - Backend is written in F#
- **Docker** - For containerized development
- **PostgreSQL** - Database for persistent storage
- **Node.js** - For frontend tooling (if applicable)

### Getting Started

1. **Clone and Setup**
   ```bash
   git clone <darklang-repo>
   cd dark
   ./scripts/build/build-release-cli-exes.sh  # Build CLI executables
   ```

2. **Run CLI**
   ```bash
   env ./scripts/run-cli
   ```

3. **Monitor Logs** (during development)
   ```bash
   tail -f rundir/logs/packages-canvas.log  # Package loading
   tail -f rundir/logs/queueworker.log      # Background processes
   ```

4. **Run Tests**
   ```bash
   ./scripts/run-backend-tests           # Backend tests
   ./scripts/run-cli integration-tests   # CLI integration tests
   ```

### Development Scripts

- **`./scripts/run-cli`** - Run the CLI with proper environment
- **`./scripts/build/build-release-cli-exes.sh`** - Build CLI executables  
- **`./scripts/run-backend-tests`** - Execute backend test suite
- **`./scripts/build/_dotnet-wrapper`** - .NET build wrapper

## Core Concepts

### Dark Language Syntax

Dark is a functional language with:

#### Basic Types
```dark
// Primitive types
let name: String = "Darklang"
let count: Int64 = 42L
let isActive: Bool = true
let price: Float = 29.99

// Lists
let numbers: List<Int64> = [1L; 2L; 3L; 4L; 5L]
let names: List<String> = ["Alice"; "Bob"; "Charlie"]

// Option types (nullable values)
let maybeValue: Option<String> = Some "value"
let noValue: Option<String> = None

// Results (error handling)
let result: Result<String, String> = Ok "success"
let error: Result<String, String> = Error "failed"
```

#### Functions
```dark
// Simple function
let add (a: Int64) (b: Int64) : Int64 = a + b

// Function with pattern matching
let processOption (opt: Option<String>) : String =
  match opt with
  | Some value -> "Got: " ++ value
  | None -> "No value"

// Pipe operator for function composition
let result = 
  [1L; 2L; 3L; 4L; 5L]
  |> List.map (fun x -> x * 2L)
  |> List.filter (fun x -> x > 5L)
  |> List.head
```

#### Pattern Matching
```dark
// Match on values
let describe (x: Int64) : String =
  match x with
  | 0L -> "zero"
  | 1L -> "one" 
  | n when n > 0L -> "positive"
  | _ -> "negative"

// Match on types
let handleResult (r: Result<String, String>) : String =
  match r with
  | Ok value -> "Success: " ++ value
  | Error msg -> "Error: " ++ msg
```

### Module System

Dark uses a hierarchical module system:

```dark
module Darklang =
  module Stdlib =
    module List =
      let head (list: List<'a>) : Option<'a> = ...
      let tail (list: List<'a>) : List<'a> = ...
      
    module String =
      let length (s: String) : Int64 = ...
      let append (s1: String) (s2: String) : String = ...
```

Access functions with full qualified names:
```dark
let result = Darklang.Stdlib.List.head [1L; 2L; 3L]
let text = Darklang.Stdlib.String.append "Hello" " World"
```

### Package Structure

Packages are organized by namespace:

```
packages/
├── darklang/           # Core Darklang packages
│   ├── stdlib/         # Standard library modules
│   ├── cli/            # CLI implementation
│   ├── languageTools/  # Parser, type system, LSP
│   └── test/           # Testing frameworks
├── internal/           # Internal/private packages
└── <user>/             # User-defined packages
```

## Development Workflows

### Adding New Features

1. **Design Phase**
   - Write specifications in `*.md` files
   - Document API changes and backward compatibility
   - Consider type system implications

2. **Implementation**
   - Add types to appropriate modules
   - Implement core functionality
   - Add comprehensive tests
   - Update documentation

3. **Integration**
   - Update package dependencies
   - Run full test suite
   - Test CLI integration if applicable
   - Update help system and documentation

### Working with the Type System

Dark has a strong static type system. Key principles:

#### Type Definitions
```dark
// Simple type alias
type UserId = String

// Record types  
type User = { id: UserId; name: String; email: String; active: Bool }

// Discriminated unions
type Shape = 
  | Circle of radius: Float
  | Rectangle of width: Float * height: Float  
  | Triangle of base: Float * height: Float

// Generic types
type Box<'T> = { contents: 'T; label: String }
```

#### Option vs Result Types

Use `Option<T>` for nullable values:
```dark
let findUser (id: UserId) : Option<User> = ...

match findUser "123" with
| Some user -> processUser user
| None -> handleUserNotFound ()
```

Use `Result<T, E>` for operations that can fail:
```dark
let validateEmail (email: String) : Result<String, String> = ...

match validateEmail "bad-email" with  
| Ok validEmail -> saveUser validEmail
| Error message -> reportValidationError message
```

### Package Development

#### Creating a New Package

1. **Directory Structure**
   ```
   packages/yournamespace/
   ├── yourmodule.dark
   ├── utilities.dark
   └── tests.dark
   ```

2. **Module Definition**
   ```dark
   module YourNamespace =
     module YourModule =
       // Your functions here
       let myFunction (input: String) : String = ...
   ```

3. **Dependencies**
   Reference other packages by full module name:
   ```dark
   let result = Darklang.Stdlib.List.map myFunction inputList
   ```

#### Testing Your Package

Create tests alongside your code:
```dark
module YourNamespace =
  module Tests =
    let testMyFunction () : TestResult =
      let input = "test input"
      let expected = "expected output"
      let actual = YourModule.myFunction input
      
      if actual == expected then
        TestResult.Pass
      else
        TestResult.Fail $"Expected {expected}, got {actual}"
```

## Language Features

### Advanced Pattern Matching

```dark
// Match with guards
let categorize (x: Int64) : String =
  match x with
  | n when n < 0L -> "negative"
  | 0L -> "zero"
  | n when n <= 10L -> "small positive"
  | n when n <= 100L -> "medium positive"  
  | _ -> "large positive"

// Match on multiple values
let processCoordinates (x: Int64, y: Int64) : String =
  match (x, y) with
  | (0L, 0L) -> "origin"
  | (0L, _) -> "on y-axis" 
  | (_, 0L) -> "on x-axis"
  | (x, y) when x == y -> "on diagonal"
  | _ -> "general position"
```

### Function Composition and Piping

```dark
// Traditional nested calls
let result1 = List.head (List.filter (fun x -> x > 5L) (List.map (fun x -> x * 2L) [1L; 2L; 3L; 4L; 5L]))

// Using pipe operator (preferred)
let result2 = 
  [1L; 2L; 3L; 4L; 5L]
  |> List.map (fun x -> x * 2L)
  |> List.filter (fun x -> x > 5L)
  |> List.head

// Function composition
let processData =
  List.map (fun x -> x * 2L)
  >> List.filter (fun x -> x > 5L)
  >> List.head
  
let result3 = [1L; 2L; 3L; 4L; 5L] |> processData
```

### Error Handling Patterns

```dark
// Chain Results with bind
let processUser (userId: String) : Result<String, String> =
  findUser userId
  |> Result.bind validateUser
  |> Result.bind processPayment
  |> Result.map formatResponse

// Handle Options safely
let getUserEmail (userId: String) : Option<String> =
  findUser userId
  |> Option.bind (fun user -> 
      if user.active then Some user.email else None)
  |> Option.map String.toLower
```

## Package System

### Package Manager Integration

The PackageManager provides:

- **Type Resolution** - Find types across all loaded packages
- **Function Discovery** - Search and navigate available functions  
- **Dependency Management** - Handle package dependencies
- **Live Reloading** - Automatically reload packages during development

#### Searching Packages

```dark
// Use PackageManager.Search for programmatic access
let searchQuery = { 
  searchTerm = "List"
  includeTypes = true
  includeFunctions = true
  includeConstants = true
}

let results = Darklang.LanguageTools.PackageManager.Search.search searchQuery
// Returns: { types: List<TypeInfo>; fns: List<FnInfo>; constants: List<ConstInfo>; submodules: List<String> }
```

#### CLI Package Navigation

```bash
# Navigate package hierarchy
./scripts/run-cli ls                          # List root modules
./scripts/run-cli ls Darklang.Stdlib          # List standard library  
./scripts/run-cli cd Darklang.Stdlib.List     # Navigate to List module
./scripts/run-cli view List.head              # View function details
./scripts/run-cli back                        # Return to previous location
```

### Standard Library Organization

```
packages/darklang/stdlib/
├── list.dark          # List operations
├── option.dark        # Option type functions
├── result.dark        # Result type functions  
├── string.dark        # String manipulation
├── dict.dark          # Dictionary operations
├── math.dark          # Mathematical functions
├── dateTime.dark      # Date and time utilities
├── http.dark          # HTTP server functions
├── httpclient.dark    # HTTP client functions  
├── json.dark          # JSON parsing/serialization
├── db.dark            # Database operations
├── canvas.dark        # Canvas-specific functions
└── cli/               # CLI-specific functions
    ├── execution.dark # Command execution
    ├── stdin.dark     # Input handling
    └── host.dark      # System information
```

## Testing

### Test Organization

Tests are co-located with the code they test:

```
packages/darklang/
├── stdlib/
│   ├── list.dark          # List functions
│   └── list_tests.dark    # List function tests
├── cli/
│   ├── commands.dark      # CLI commands
│   └── commands_tests.dark # CLI command tests
└── test/
    └── test.dark          # Testing framework
```

### Writing Tests

```dark
module Darklang =
  module Stdlib =
    module List =
      module Tests =
        let testHead () : TestResult =
          let emptyList: List<Int64> = []
          let nonEmptyList = [1L; 2L; 3L]
          
          // Test empty list
          match List.head emptyList with
          | None -> () // Expected
          | Some _ -> return TestResult.Fail "Expected None for empty list"
          
          // Test non-empty list  
          match List.head nonEmptyList with
          | Some 1L -> () // Expected
          | Some other -> return TestResult.Fail $"Expected Some 1L, got Some {other}"
          | None -> return TestResult.Fail "Expected Some value, got None"
          
          TestResult.Pass

        let testMap () : TestResult =
          let input = [1L; 2L; 3L]
          let expected = [2L; 4L; 6L]
          let actual = List.map (fun x -> x * 2L) input
          
          if actual == expected then
            TestResult.Pass
          else
            TestResult.Fail $"Expected {expected}, got {actual}"
```

### Running Tests

```bash
# Run backend tests (F#)
./scripts/run-backend-tests

# Run integration tests via CLI
env ./scripts/run-cli "run Darklang.Internal.Cli.IntegrationTests.runIntegrationTests"

# Run specific test functions
env ./scripts/run-cli "run Darklang.Stdlib.List.Tests.testHead"
```

### Test Categories

1. **Unit Tests** - Test individual functions in isolation
2. **Integration Tests** - Test interactions between components
3. **CLI Tests** - Test command-line interface functionality  
4. **End-to-End Tests** - Test complete user workflows

## Debugging

### Logging and Monitoring

Monitor system logs during development:

```bash
# Package loading progress
tail -f rundir/logs/packages-canvas.log

# Background worker processes  
tail -f rundir/logs/queueworker.log

# General application logs
tail -f rundir/logs/*.log
```

### CLI Debugging

```bash
# Check CLI status
env ./scripts/run-cli status

# Verify package loading
env ./scripts/run-cli version

# Test specific functionality
env ./scripts/run-cli eval "1L + 2L"
env ./scripts/run-cli view Darklang.Stdlib.List.head
```

### Common Issues

#### Package Loading Errors
```bash
# Error: "Packages are still loading! Wait for package loading to complete"
# Solution: Wait for packages to finish loading
tail -f rundir/logs/packages-canvas.log  # Watch until "Finished reading, parsing packages"
```

#### Type Resolution Errors  
```bash
# Error: "Unresolved name when not allowed"
# Check: Module paths and function names are correct
env ./scripts/run-cli ls Darklang.Stdlib  # Verify available modules
```

#### Function Call Errors
```bash
# Error: "There is no variable named: functionName" 
# Check: Function exists and is properly scoped
env ./scripts/run-cli view Darklang.Stdlib.ModuleName.functionName
```

### Development Debugging

Add debug output in Dark code:
```dark
let debugFunction (input: String) : String =
  // Debug: Print input
  let _ = Darklang.Stdlib.Cli.Host.log $"DEBUG: Input = {input}"
  
  let result = processInput input
  
  // Debug: Print result  
  let _ = Darklang.Stdlib.Cli.Host.log $"DEBUG: Result = {result}"
  
  result
```

## Performance

### Best Practices

1. **Use Appropriate Data Structures**
   - Lists for sequential access and functional transformations
   - Dictionaries for key-value lookups
   - Sets for membership testing

2. **Avoid Expensive Operations in Hot Paths**
   ```dark
   // Expensive - creates intermediate lists
   let badExample (data: List<String>) : List<String> =
     data
     |> List.map String.trim
     |> List.map String.toLower  
     |> List.filter (fun s -> String.length s > 0L)
   
   // Better - single pass with combined operation
   let goodExample (data: List<String>) : List<String> =
     data
     |> List.filterMap (fun s -> 
         let trimmed = String.trim s |> String.toLower
         if String.length trimmed > 0L then Some trimmed else None)
   ```

3. **Use Lazy Evaluation When Appropriate**
   ```dark
   // Only compute what you need
   let firstValid (inputs: List<String>) : Option<String> =
     inputs
     |> List.filterMap validateInput
     |> List.head  // Stops at first valid result
   ```

### Profiling

Monitor package loading times:
```bash
time env ./scripts/run-cli version
```

Monitor memory usage:
```bash
# During development
ps aux | grep "dotnet\|Dark"
```

## CLI Development

For detailed information about CLI development, see [CLI-DEVELOPMENT.md](./CLI-DEVELOPMENT.md).

Quick reference:

```bash
# Run enhanced CLI
env ./scripts/run-cli

# Key CLI commands
./scripts/run-cli help                    # Show all commands
./scripts/run-cli ls Darklang.Stdlib     # Navigate packages
./scripts/run-cli view List.head         # View function details
./scripts/run-cli handlers               # List canvas handlers
./scripts/run-cli databases              # List canvas databases
./scripts/run-cli eval "1L + 2L"         # Evaluate expressions
```

## Contributing

### Code Style

1. **Naming Conventions**
   - Functions: `camelCase`
   - Types: `PascalCase`
   - Modules: `PascalCase`
   - Variables: `camelCase`

2. **Module Organization**
   - One primary concept per file
   - Related functions grouped together
   - Tests co-located with implementation

3. **Documentation**
   - Document public functions with `///` comments
   - Include usage examples for complex functions
   - Update this guide when adding major features

### Commit Messages

Use descriptive commit messages:

```
Add comprehensive canvas toplevels discovery

- Implement realistic handler/database/secret data across 8 canvas types
- Add detailed information functions with usage descriptions  
- Integrate with CLI command system and help
- Fix scoping issues with lambda functions in List.map calls

🤖 Generated with [Claude Code](https://claude.ai/code)

Co-Authored-By: Claude <noreply@anthropic.com>
```

### Pull Requests

1. **Create Feature Branch**
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make Changes with Tests**
   - Add comprehensive tests
   - Update documentation
   - Verify all tests pass

3. **Submit PR**
   - Describe changes clearly
   - Reference related issues
   - Include testing notes

### Getting Help

- **Documentation** - Check existing `.md` files in the repository
- **Code Examples** - Explore `packages/darklang/stdlib/` for patterns  
- **CLI Help** - Use `./scripts/run-cli help` for command reference
- **Logs** - Monitor `rundir/logs/` for debugging information

---

This guide is continuously updated. For the most current information, check the repository's documentation and code comments.