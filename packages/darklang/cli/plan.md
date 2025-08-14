# CLI MVU Architecture Plan

## What We Just Built
- Ultra-minimal working CLI with proper interactive mode
- Clean MVU foundation: Types, Commands, main Cli modules
- Working recursive input loop that stays interactive until `quit`
- Command parsing and execution with state management

## What's Next

### 1. Command Registry Pattern
- Dynamic command registration system
- Modules can contribute their own commands
- Self-generating help system
- Commands contribute to central help automatically

### 2. Proper MVU Components
- Enhanced `Msg` type for all possible actions
- `Update` module with pure state transformations
- `View` module for consistent output formatting
- Effects system for side effects (file I/O, network, etc)

### 3. Composable Command Modules
Each command module follows MVU pattern:
- Own Model/State for command-specific data
- Own Msg types for command actions
- Own Update functions
- Own View functions
- Registers itself with central command registry

### 4. Universal Argument System
- Shared argument parsing/validation
- Type-safe argument handling
- Auto-completion support
- Help generation from argument specs

### 5. Example Command Modules
- `view` module (browse packages/modules)
- `search` module (find functions/types)
- `docs` module (show documentation)
- `repl` module (expression evaluation)

## Design Goals
- Each module feels like a mini-MVU app
- Modules compose naturally
- Central help system aggregates from all modules
- Easy to add new commands
- Type-safe throughout
- Minimal boilerplate

## Old CLI Reference
The `cli_old_backup/` folder contains the original 1000+ line implementation for reference on features and patterns that worked well.