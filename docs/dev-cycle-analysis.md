# Dev Cycle Analysis

Analysis of the build system to identify optimization opportunities.

## Build System Architecture

### Key Components

1. **`scripts/builder`** - Entry point, starts Docker container with the build server
2. **`scripts/build/_build-server`** - Python script that watches for file changes
3. **`scripts/build/compile`** - Python script that determines what to build based on changed files
4. **`scripts/build/_dotnet-wrapper`** - Wrapper around dotnet CLI that handles server kill on copy errors

### Build Flow

```
File change detected (watchfiles)
    -> _build-server.compile(changed_files)
        -> compile script marks what should build
        -> execute() runs build phases in sequence:
            1. Parser build (if grammar.js changed)
            2. Tool restore (if global.json changed)
            3. Paket restore/install (if dependencies changed)
            4. F# build (quick or full)
            5. Reload backend server
            6. Run migrations
            7. Reload packages
            8. Run tests (if --test flag)
            9. Linting (shellcheck, yamllint)
```

## Current Timings (approximate)

| Phase | Duration | Notes |
|-------|----------|-------|
| F# incremental build | 15-60s | Depends on what changed |
| F# full rebuild | 60-120s | After dependency changes |
| Package reload | ~10s | Parses all .dark files |
| Server restart | ~2s | Kill + restart BwdServer |
| Migrations | ~1s | Usually no-op |

## Identified Bottlenecks

### 1. Sequential Build Steps
**Problem**: Build phases run sequentially even when independent.
**Example**: Package reload waits for server restart, but they could run in parallel if the server isn't needed for reload.
**Impact**: Medium

### 2. Always Reload Packages After F# Build
**Problem**: `reload_all_packages` triggers after every F# build, even if no .dark files changed.
**Code**: `compile:298,306` - both quick and full builds set `should.reload_all_packages = True`
**Impact**: High (~10s per F# change)

### 3. Server Restart Forces Package Reload
**Problem**: When .dark files change, the server is restarted AND packages are reloaded separately.
**Code**: `compile:370-371` - `.dark` file changes trigger both `reload_all_packages` and `reload_backend_server`
**Impact**: Low (necessary for cache invalidation)

### 4. Copy Error Recovery
**Problem**: When dotnet build encounters copy errors (file in use), the wrapper kills all servers.
**Code**: `_dotnet-wrapper:14-17`
**Impact**: Low (necessary workaround)

### 5. No Parallel dotnet Builds
**Problem**: Multiple projects build sequentially via `&&` chaining.
**Code**: `compile:182-189` - joins builds with `&&`
**Impact**: Medium (could parallelize independent projects)

## Recommendations

### Quick Wins

1. **Don't reload packages on F# changes**
   - Remove `should.reload_all_packages = True` from backend_quick_build and backend_full_build paths
   - Only reload when .dark files actually change
   - **Savings**: ~10s per F# file change

2. **Skip server restart when only running tests**
   - If only test files changed, no need to restart server
   - **Savings**: ~2s for test-only changes

### Medium Effort

3. **Parallel package reload and server restart**
   - Use threading to run these concurrently when both are needed
   - **Savings**: ~8s (overlap package reload with server restart)

4. **Incremental package reload**
   - Only parse and update changed .dark files instead of all
   - Requires tracking package dependencies
   - **Savings**: Variable, most impactful for small changes

### Larger Effort

5. **Use dotnet watch instead of custom watcher**
   - `dotnet watch` has better incremental build support
   - Would require restructuring build scripts
   - **Potential**: Faster incremental builds

6. **Build caching layer**
   - Cache parsed ASTs, type-checked results
   - Complex to implement correctly
   - **Potential**: Sub-second rebuilds for small changes

## Questions for Discussion

1. Is the 10s package reload time acceptable, or should we prioritize incremental reload?
2. Should we invest in `dotnet watch` integration?
3. Are there other workflows (besides F# and .dark changes) that are slow?
4. Is the current server restart approach (kill all) necessary, or can we do graceful restarts?

## Measurements to Collect

To validate these findings, we should measure:
- Time from save to "build complete" message
- Time from save to server ready
- CPU/memory during builds
- How often each build phase is triggered

## Next Steps

1. Implement quick win #1 (don't reload packages on F# changes)
2. Measure baseline timings more precisely
3. Discuss priorities with team
