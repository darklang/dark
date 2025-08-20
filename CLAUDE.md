# This is the main Darklang monorepo. Please help me work on it.

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
- **`packages/`** - Darklang packages organized by namespace
- **`rundir/`** - Runtime directory with logs and temporary files
- **`scripts/`** - Development and build scripts

## Regarding Builds
you should never try to manually rebuild code or reload packages.
All of these things happen automatically, thanks mostly to ./scripts/build/compile running all the time in the background, building stuff and logging as it does.

just be patient, poll those logs, and your changes will take effect. eventually

package-reloads are higher level, happen whenever you change a .dark file, take about 10s, and log to ./rundir/logs/packages-canvas.log.

.net builds are lower level, happen whenever you change an F# file, and take up to a min to load, and log to build-server.log. when they finish, they trigger a package reload too, 'just in case'

## Regarding Darklang Syntax
- when you construct enums, you need the typename before the case name. when you deconstruct - enums, you're only allowed the case name
- oh you can't use / for Int64s. use Stdlib.Int64.divide
- same for `-`; can only be used for
- list items are separated by ;
- the LHS of a |> may need to be wrapped by () if it's "complex"

