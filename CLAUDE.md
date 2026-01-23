<!-- TASK CONTEXT START -->
# Active Task - Planning Phase

You have been given a task to complete. Your job is to:

1. **Research** the codebase to understand what needs to be done
2. **Create a plan** with specific, actionable todos
3. **Signal** when planning is complete

## The Task

Sqlite Spike -- Access, DSL

We embed Sqlite in our CLI/Runtime.
Internally, we access a data.db to store various data, in our host language, F#, using (library).

We'd like to avail access to this DB, as well as to user .db files, in our language Darklang, with a minimal set of Builtins.

Separately, a DSL for querying the DB might be cool.
Hack on that, but keep it separate/'above' the main solution, so I can remove it in case it's ugly.

Test what you can, ideally in .dark tests.
Update CLI and VS Code editing experience if relevant.

## Your Instructions

1. Read and understand the relevant code
2. Create `.claude-task/todos.md` with a detailed checklist of specific tasks
3. **When planning is complete**, write "ready" to `.claude-task/phase`:
   ```bash
   echo "ready" > .claude-task/phase
   ```
   This signals the TUI that you're done planning.

4. Tell the user: "Planning complete. Review .claude-task/todos.md. Execution will start automatically."

## What happens next

The automated loop will:
- Run you repeatedly until all todos are done
- You read CLAUDE.md and .claude-task/todos.md each iteration
- Mark todos as [x] when complete
- Write "done" to .claude-task/phase when ALL todos complete

## Important

- Be thorough in research before creating the plan
- Keep todos specific and actionable
- Include testing in the plan
- You can interact with the user now during planning
- **COMMIT your plan** before signaling ready (git add . && git commit -m "plan: <task summary>")

<!-- TASK CONTEXT END -->

# This is the main Darklang monorepo. Please assist in the development of this language+platform.

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
