# darklang-cli

## Usage

- create script with `#!/usr/bin/env darklang`
- runs same as if it was bash

- darklang infer --prompt "A file that prints 'hello' 100 times in a loop"
  - creates and saves a script that does what the prompt asks with the name creates by the AI

## Structure

- LibCLI
  - stdlib fns and types for filesystem and other posix stuff
  - print to stdout (and stderr?)
  - read stdin (start processing once it closes)
  - Directory.\*
  - File.\*
- CLI
  - binary that runs dark programs, compiled with LibCLI
- InternalCLI
  - binary that runs dark programs, compiled with LibCLI
  - has access to additional "internal" Dark types/fns (e.g. access to add packages)
- prompt.txt
  - used in addition to the user prompt
