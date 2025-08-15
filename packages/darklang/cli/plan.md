# CLI MVU Architecture Plan

## ✅ Completed
- Ultra-minimal working CLI with proper interactive mode
- Clean MVU foundation: Types, Commands, main Cli modules
- Working recursive input loop that stays interactive until `quit`
- Command parsing and execution with state management
- Command registry pattern with dynamic registration
- Real package search integration (no fake data!)
- Package navigation (cd, ls, pwd, back) with real data
- Tab completion for cd command showing actual packages
- Installation commands reorganized into proper module structure
- Fixed ls to show only direct children, not all descendants
- Robust cd/ls navigation: .., ../.., relative/absolute paths, ls with args

## 🐛 Known Bugs
- Dot-notation paths not supported yet (cd Darklang.Stdlib)

## 📋 Next Up

### Quick Wins (can do now)
- Port `clear` command - simple screen clearing
- Port `eval` command - expression evaluation
- Port `run` command - execute package functions
- Fix cd relative path bug

### Medium Tasks
- Port `scripts` command for script management
- Port `mode` command for interaction modes
- Add tests.dark module
- Support dot-notation paths
- Implement `tree` command with two modes:
  - `tree --plain`: ASCII tree view with cute icons (📦 modules, 🔧 fns, 📋 types, 💎 values)
  - `tree` (interactive): expandable/collapsible tree like `lf` file manager

### Major Features (need keystroke input first)
- Upgrade to keystroke-based input for true interactive mode
- Command history navigation with arrow keys
- Interactive `view` command with entity browsing
- Syntax highlighting and colors module

## 📝 Session Context (for resuming work)

### Key Background
- We're rewriting the CLI from monolith (`cli_old_backup/`) to MVU architecture
- Old CLI in `packages/darklang/cli_old_backup` has all features we need to port
- New CLI in `packages/darklang/cli` is the rewrite in progress
- The CLI runs in 2 modes:
  - Interactive: no args, REPL-like, commands entered one by one
  - Non-interactive: args provided, returns result and exits

### Testing
- Test with: `./scripts/run-cli args`
- Interactive mode is hard to test directly, ask user to test complex interactions
- Build system auto-rebuilds, just be patient and check logs:
  - Package reloads: `./rundir/logs/packages-canvas.log` (~10s)
  - .NET builds: `build-server.log` (up to 1 min)

### Important Architectural Notes
- Keep MVU-style composable architecture
- Each CLI subsection needs its own state where feasible
- No fake data - use real package search via `LanguageTools.PackageManager.Search`
- Installation commands moved to `installation/commands.dark`

### Current State Types
- `PackagePath`: Root | Module owner moduleName
- Package exploration tracks current path and history for 'back' command

## Design Goals
- Each module feels like a mini-MVU app
- Modules compose naturally
- Central help system aggregates from all modules
- Easy to add new commands
- Type-safe throughout
- Minimal boilerplate

## Old CLI Reference
The `cli_old_backup/` folder contains the original 1000+ line implementation for reference on features and patterns that worked well.