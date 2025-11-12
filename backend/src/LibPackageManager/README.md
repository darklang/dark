# LibPM / LibMatter

This project organizes our implementations of RT. and PT.PackageManager
- a set of each wholly in memory using F# collections etc.
  - and some mechanism to compose to this
- a set of each wholly bound to a sql database, with ops applied eventually, and syncing etc.
  - TODO we need some sort of magical syncing server that _just does this_
  - FOR NOW, we are assuming: user is always connected to the Internet and can access https://matter.darklang.com - if that breaks, CLI breaks.
- some me


Use cases vary:
- run-time
- pre-dogfooding parsings
  - (all 2 phases, maybe with 'id stabilization')
  - canvases from disk
  - packages from disk
  - test module/file from disk
- dev-time parsings (2 phase)

the SQL-bound one syncs, magically, automatically.

LibSync?

Syncer.exe?


hmm how many of these things do we need in both F# _and_ Darklang? The whole setup?
is the syncer written in F# or Darklang?