# `darklang` CLI executable

This is a project that yields a `darklang` executable artifact.
It builds to many platforms -- see the .fsproj file for details.

## Usage

- create script with `#!/usr/bin/env darklang`
- runs same as if it was bash

## Structure

- `Cli/Cli.fs` — entry point + builtin wiring; binary that runs Dark code.
- The CLI command surface itself lives in Dark, under
  `packages/darklang/cli/` (commands, dispatch, help).
- POSIX-flavored builtins (filesystem, process, stdin/stdout) come from
  `Builtins.Cli` and `Builtins.CliHost`.