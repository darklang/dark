# Darklang Monorepo - Active Trimming Sprint

This document serves as the primary prompt for AI-assisted development sessions. The goal is to **massively trim** this codebase to enable faster iteration on Darklang as a product.

## FIRST: Read the Todo System

**Before doing anything else**, read `todos/README.md` to understand current progress.

The todo system is a tree of markdown files:
```
todos/
  README.md                              <- START HERE - shows overall progress
  phase1-remove-product-features/        <- Do these first
    01-delete-prodexec.md
    02-delete-queueworker.md
    ...
  phase2-trim-dev-environment/           <- Then these
    01-remove-deployment-terraform.md
    ...
    final/
      01-clean-ports.md
      ...
  phase2.5-dev-cycle-analysis/           <- Research phase
    01-analyze-compile-scripts.md
```

Each file has:
- `[ ]` checkboxes for individual tasks
- Detailed instructions for what to do
- Search commands to find related code
- Commit message templates

## Session Workflow

1. **Read `todos/README.md`** - Find the first unchecked `[ ]` item
2. **Open that todo file** - Read its full instructions
3. **Complete the work** - Follow the steps in the file
4. **Run tests**: `./scripts/run-backend-tests` - Must pass
5. **Wait for build** - Check `rundir/logs/build-server.log`
6. **Commit** - Use the commit message template from the todo file
7. **Mark checkbox `[x]`** - Update both the todo file AND `todos/README.md`
8. **Commit the checkbox update** - Can be same commit or separate
9. **Repeat** - Move to next unchecked item

## Current Sprint: Codebase Trimming

**Objective**: Remove unused product features and dev environment bloat to create a lean, fast development experience focused on: the CLI, VS Code extension, and single-exe local-first experience.

## Commit Discipline

**Critical**: Every checked checkbox = one commit. Follow this pattern:
```bash
# Before committing:
./scripts/run-backend-tests  # Must pass
# Wait for build to succeed (check rundir/logs/build-server.log)

# Commit format - keep it SHORT, no attribution:
git commit -m "trim: [brief description]"
```

## External Resources
- Team notes: ~/vaults/Darklang Dev
- WIP website: wip.darklang.com
- Blog: blog.darklang.com
- Website repo: ~/code/darklang.com
- Docs (outdated): ~/code/docs

## Key Directories

- **`backend/`** - F# backend: type system, execution engine
- **`packages/`** - Darklang packages by namespace (bulk of user-facing code)
- **`rundir/`** - Runtime directory with logs and temp files
- **`scripts/`** - Development and build scripts
- **`todos/`** - Sprint task tracking (read this to know what to work on)

## Build System

**NEVER manually rebuild** - everything happens automatically via `./scripts/build/compile` running in background.

**Logs to monitor:**
- `rundir/logs/build-server.log` - .NET builds (up to 1 min)
- `rundir/logs/packages-canvas.log` - Package reloads (~10s)

**Wait for builds**: After changing F# files, poll logs until build completes. Don't rush.

## Testing

Before any commit:
```bash
./scripts/run-backend-tests
```

This runs the Expecto test suite. All tests must pass before committing.

**After tests pass**: Report the test count (e.g., "Tests passed: 142/142") so we can track the count decreasing as we trim.

## Darklang Syntax Reference

### Critical Rules
- Whitespace/indentation-sensitive - proper formatting is critical
- No nested function definitions - extract to module level
- LHS of `|>` needs parentheses if complex: `(Stdlib.List.range 0L 100L) |> Stdlib.List.map fn`
- List items separated by `;`
- No `/` for Int64 division - use `Stdlib.Int64.divide`
- No `-` for Float subtraction - use `Stdlib.Float.subtract`
- "function" is reserved in F# - use "fn" for field names
- `++` for string concat; `@` doesn't exist - use `Stdlib.List.append`

### Record Construction
```
// Correct:
RecordType { field = value }

// Multi-line:
RecordType
  { field1 = value1
    field2 = value2 }

// Wrong - { to left of type name:
{ field = value } : RecordType
```

### Enum Construction vs Matching
```
// Construction - need type prefix:
EnumType.CaseName

// Match - no type prefix:
match x with
| CaseName -> ...
```

### Function Arguments
- Check parameter order carefully
- `Stdlib.String.join` expects list first, then separator
- `Stdlib.List.range` expects start and end, both inclusive

---

## What's Being Removed (Context for AI Sessions)

### Phase 1: Product Features to Delete
These are features we haven't used and don't need for MVP:

| Component | Location | Purpose (being removed) |
|-----------|----------|------------------------|
| **ProdExec** | `backend/src/ProdExec/` | One-off production tasks |
| **QueueWorker** | `backend/src/QueueWorker/` | Background event queue processing |
| **CronChecker** | `backend/src/CronChecker/` | Scheduled cron job execution |
| **BuiltinDarkInternal** (partial) | `backend/src/BuiltinDarkInternal/Libs/` | F404s, Infra, Workers, Secrets |
| **LibClientTypes** | `backend/src/LibClientTypes/` | Pusher client message types |
| **LibClientTypesToCloudTypes** | `backend/src/LibClientTypesToCloudTypes/` | Event serialization |
| **WASM** | `backend/src/Wasm/` | Browser editor via Blazor WASM |
| **backend/static** (most) | `backend/static/` | WASM JS files (keep favicon) |

### Phase 2: Dev Environment to Trim
| Item | Why Remove |
|------|-----------|
| Terraform (`tf/`) | No production deployment needed now |
| Production containers (`containers/`) | Not deploying to cloud |
| CircleCI jobs (many) | Simplify CI to essentials |
| Docker CE in container | Don't need to build Docker in Docker |
| Honeycomb/OpenTelemetry | Observability not needed in dev |
| Google Cloud SDK/PubSub | No cloud deployment |
| PostgreSQL/YugaByte volumes | Using SQLite only |
| Java (OpenJDK) | Only needed for PubSub emulator |
| Chisel | SSH tunneling for ProdExec |
| Second instance scripts | Sync testing not needed now |
| Datatests | Data integrity tests for cloud |

### Phase 2.5: Dev Cycle Analysis
Research `scripts/build/compile` and related scripts to identify opportunities to make dev cycles faster.

---

## When Stuck

- Ask for clarification rather than guessing
- If a removal breaks things unexpectedly, investigate before proceeding
- Document any surprises in the todo file for future reference

## Using Sub-Agents

When delegating to sub-agents, always include:
1. The specific task from the todo file
2. Relevant context from this CLAUDE.md
3. Testing requirements
4. Commit message format

Don't overwhelm sub-agents with full context - give them just what they need.

---

## Files That Should NOT Be Modified

Unless explicitly required by a todo item:
- `.dark` files in `packages/` (Darklang source)
- Core type system files in `backend/src/LibExecution/`
- Parser files in `tree-sitter-darklang/`
- The CLI entry point `backend/src/Cli/`
- VS Code extension in `vscode-extension/`

---

## Quick Reference: Test Commands

```bash
# Full backend tests
./scripts/run-backend-tests

# Check build status
tail -f rundir/logs/build-server.log

# Check package reload status
tail -f rundir/logs/packages-canvas.log
```

---

*This document is part of an active sprint. Check `todos/README.md` for current progress.*
