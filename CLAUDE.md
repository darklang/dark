# This is the main Darklang monorepo. Please assist in the development of this language+platform.

## CURRENT PROJECT: Faster Package Reloading

**Goal**: Make `./scripts/build/reload-packages` complete in **under 1 second** (currently ~15s).

### Quick Reference
- **Plan**: See `FASTER_RELOAD_PLAN.md` for full optimization strategy
- **Measure**: `time ./scripts/build/reload-packages`
- **Logs**: `./rundir/logs/packages-canvas.log`
- **Ralph loop**: Run `./ralph` to start automated optimization loop

### Key Files to Modify
| File | Purpose |
|------|---------|
| `backend/src/LocalExec/LoadPackagesFromDisk.fs` | Main loading - add incremental support |
| `backend/src/LibPackageManager/Purge.fs` | Clearing - make selective |
| `backend/src/LibPackageManager/Inserts.fs` | Insertion - batch operations |
| `backend/src/LibPackageManager/PackageOpPlayback.fs` | Application - lazy RT conversion |

### Optimization Priority
1. **Incremental reload** - Only reload changed .dark files (10x impact)
2. **Parallelize parsing** - Change `mapSequentially` to parallel (2x impact)
3. **Lazy RT conversion** - Don't convert PT→RT upfront (1.5x impact)
4. **Batch DB ops** - Bulk inserts (1.3x impact)

### Testing After Changes
```bash
# Rebuild F# (wait for build-server to complete)
tail -f ./rundir/logs/build-server.log

# Then measure reload time
time ./scripts/build/reload-packages
```

---

## External resources:
- team notes: ~/vaults/Darklang Dev
- in-progress website: wip.darklang.com
- posts on blog.darklang.com
- most recent post on stachu.net
- other source code
  - website (WIP) ~/code/darklang.com
  - docs (outdated) ~/code/docs

### Key Directories

- **`backend/`** - F# backend implementation, type system, execution engine
- **`packages/`** - Darklang packages organized by namespace -- the bulk of user-facing code is here.
- **`rundir/`** - Runtime directory with logs and temporary files
- **`scripts/`** - Development and build scripts

## Regarding Builds
you should never try to manually rebuild code or reload packages.
All of these things happen automatically, thanks mostly to ./scripts/build/compile running all the time in the background, building stuff and logging as it does.

just be patient, poll those logs, and your changes will take effect. eventually

package-reloads are higher level, happen whenever you change a .dark file, take about 10s, and log to ./rundir/logs/packages-canvas.log.

.net builds are lower level, happen whenever you change an F# file, and take up to a min to load, and log to build-server.log. when they finish, they trigger a package reload too, 'just in case'

## Regarding Darklang Syntax

### Critical Rules
- Darklang is whitespace- and indentation-sensitive - proper indentation and line breaks are critical
- No nested function definitions allowed - extract all functions to module level
- The LHS of a |> needs parentheses if it's complex: `(Stdlib.List.range 0L 100L) |> Stdlib.List.map fn`
- List items are separated by `;`
- Cannot use `/` operator for Int64 division - use `Stdlib.Int64.divide`
- Cannot use `-` operator for Float subtraction - use `Stdlib.Float.subtract`
- Reserved keyword: "function" is reserved in F#, use "fn" instead for field names
- ++ is for string concat; @ doesn't exist - use Stdlib.List.append to combine lists

### Record Construction
- When constructing records, ensure the `{` is never to the left of the type name
- Correct: `RecordType { field = value }` or multi-line with proper indentation
- Wrong: Type name and opening brace misaligned

### Enum Construction
- When constructing enums, need typename before case name: `EnumType.CaseName`
- When deconstructing in match expressions, use only case name: `| CaseName ->`

### Function Arguments
- Check parameter order carefully - e.g., `Stdlib.String.join` expects list first, then separator
- `Stdlib.List.range` expects start and end values, both inclusive
