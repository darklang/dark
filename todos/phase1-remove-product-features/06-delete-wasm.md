# Delete WASM

**Status**: [ ] Not started

## What is WASM?

The Wasm project compiles F# to WebAssembly via Blazor for a browser-based Dark editor. This includes:
- Editor state management
- WASM-JS interop bindings
- Browser-side code execution

We're focusing on CLI and VS Code - browser editor is not needed for MVP.

## Files to Delete

```
backend/src/Wasm/
  Builtin.fs
  DarkEditor.fs
  EvalHelpers.fs
  Init.fs
  Program.fs
  WasmHelpers.fs
  Libs/
    Editor.fs
  Wasm.fsproj
```

## Other References to Remove

1. **Solution file**: Remove from `backend/fsdark.sln`
2. **NuGet**: `Microsoft.AspNetCore.Components.WebAssembly` - removed in Phase 2
3. **Static files**: WASM JS files in `backend/static/` - handled in next task

## Search Commands

```bash
grep -r "Wasm" --include="*.fs" --include="*.fsproj" --include="*.sln" backend/
grep -r "WebAssembly\|Blazor" --include="*.fs" --include="*.fsproj" backend/
```

## Steps

1. [ ] Delete `backend/src/Wasm/` directory
2. [ ] Remove from `backend/fsdark.sln`
3. [ ] Remove `Microsoft.AspNetCore.Components.WebAssembly` from `backend/paket.dependencies`
4. [ ] Search for any references to Wasm project
5. [ ] Run `./scripts/run-backend-tests`
6. [ ] Wait for build
7. [ ] Commit: `trim: delete WASM/Blazor project`

## Commit Message Template

```
trim: delete WASM/Blazor project

- Remove backend/src/Wasm/ directory
- Remove from fsdark.sln
- Remove WebAssembly NuGet dependency

WASM project provided browser-based editor via Blazor.
Not needed for CLI/VS Code focused architecture.
```

## Notes

- The static JS files (dark-wasm-webworker.js, etc.) are handled in the next task
- This is a significant removal - may reveal other dependencies
