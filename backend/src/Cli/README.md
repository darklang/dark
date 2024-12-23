# `darklang` CLI executable

This is a project that yields a `darklang` executable artifact.
It builds to many platforms -- see the .fsproj file for details.

## Usage

- create script with `#!/usr/bin/env darklang`
- runs same as if it was bash

## Structure

- `Cli`
  - binary that runs dark code
  - supported greatly by `LibCli`
- `LibCli`
  - stdlib fns and types for filesystem and other posix stuff
  - print to stdout (and stderr?)
  - read stdin (start processing once it closes)
  - `Directory.\*`
  - `File.\*`