# `darklang` CLI executable

TODO improve this readme

## Usage

- create script with `#!/usr/bin/env darklang`
- runs same as if it was bash

## Structure

- LibCli
  - stdlib fns and types for filesystem and other posix stuff
  - print to stdout (and stderr?)
  - read stdin (start processing once it closes)
  - Directory.\*
  - File.\*
- Cli
  - binary that runs dark programs, compiled with libcli
- prompt.txt
  - used in addition to the user prompt
