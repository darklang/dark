# Contributing to Dark

We are committed to make Dark easy to contribute to. Our
[contributor docs](https://docs.darklang.com/contributing/getting-started)
will help guide you through your first PR, find good projects to contribute to,
and learn about the code base.

That said, we are still in the early stages of building out the new version of
Dark, and are a bit behind on our docs -- much of the content in those docs is
geared towards dark-classic. If you have any questions, please touch base
[in Discord](https://darklang.com/discord-invite).

## Getting a local development environment set up

- please refer to the [dev setup docs](docs/dev-setup/README.md)

## Try the locally-built CLI

- `scripts/dev/status` should say `status: ok` and `tree: up to date`
- `./scripts/run-cli help` should then print the CLI's help
- `./scripts/run-cli docs for-ai` is the best documentation in the repo: language
  syntax, naming, and the conventions we actually follow

If `run-cli` tells you the tree has moved on since the last build, run
`scripts/dev/build`. Builds are explicit here; nothing rebuilds behind your back
unless you ask for it with `scripts/dev/watch`.

## Review the Coding Guide

- [Coding Guide](/CODING-GUIDE.md)

## Run tests

- `scripts/run-backend-tests` runs all of them; `--groups` and `--find` help you run
  part of them and print the command to do it
- [Running unit tests](docs/unittests.md), for the filter flags and their traps
- [Dark unit tests](backend/testfiles/README.md), for the `.dark` test files

### Using Dark scripts

The [`scripts/`](/scripts) directory is full of scripts. They automatically execute
in the dev container, even if they are run on the host (see
[`scripts/devcontainer/_assert-in-container`](/scripts/devcontainer/_assert-in-container)
for how this works). Scripts starting with an underscore are primarily intended to be
run by other scripts. Scripts without an underscore are usually intended to be called
by a human, though they are often also called by other scripts as well.

## Read the contributor docs

If you've gotten this far, you're now ready to [contribute your first PR](https://darklang.github.io/docs/contributing/getting-started#first-contribution).

## Running unix commands in the container

- `scripts/run-in-docker bash`

## Accessing the local db

- `scripts/run-in-docker psql -d devdb`

## Config files

Config files ([config/](config)) define all env vars that you can use in Dark code.
All config vars must start with `DARK_CONFIG`. Changing a config variable in
`config/dev` requires restaring the devcontanier.

## Debugging dotnet

### Debugger

We've had mixed success with getting the VS Code debugger to work with Dark.

When it works, it supports breakpoints, stepping, inspecting the stack, etc.

You must launch the executable from VS Code for this to work -- attaching does not
currently seem to work. You can edit [`launch.json`](.vscode/launch.json) to change
what tests are run or how other binaries are started up, which should be straightforward.

That said, running tests with debugger support _does_ seem to work -- again, adjust
`launch.json` to adjust which tests are run, and then run the "Tests" configuration.

## Other docs worth reviewing

- [Contributor docs](https://docs.darklang.com/contributing/getting-started)
- [Writing docstrings in the Dark Standard library](docs/writing-docstrings.md)
