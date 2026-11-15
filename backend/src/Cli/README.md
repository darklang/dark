# `dark` CLI executable

This is a project that yields the `dark` executable.
It builds to many platforms -- see the .fsproj file for details.

## Usage

- `dark run <file.dark>` runs a script

## Structure

- `Cli/Cli.fs` — entry point + builtin wiring; binary that runs Dark code.
- The CLI command surface itself lives in Dark, under
  `packages/darklang/cli/` (commands, dispatch, help).
- POSIX-flavored builtins (filesystem, process, stdin/stdout) come from
  `Builtins.Cli` and `Builtins.CliHost`.