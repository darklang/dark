# Dark

This is the main repo for [Dark](https://darklang.com), a combined language, editor,
and infrastructure to make it easy to build backends.

This repo is intended to help Dark users solve their needs by fixing bugs,
expanding features, or otherwise contributing. Dark is [source available, not
open source](https://github.com/darklang/dark/blob/main/LICENSE.md).

Note that the production version of Darklang is not in this repo. That is
currently in https://github.com/darklang/classic-dark.

Darklang is currently undergoing an experimental shift to discern how AI
technologies will change what Darklang does and how it operates. This is being
tracked in https://darklang.com/magicbox.

See also:

- [Darklang Community Discord](https://darklang.com/discord-invite)
- [Contributor guide](https://docs.darklang.com/contributing/getting-started)
- [Darklang Classic login](https://darklang.com/login)
- See our [guide to the repo](https://docs.darklang.com/contributing/repo-layout) for help browsing.

## Contributing

We are committed to make Dark easy to contribute to. Our
[contributor docs](https://docs.darklang.com/contributing/getting-started)
will help guide you through your first PR, find good projects to contribute to,
and learn about the code base.

### Getting started

The easiest way to get to get started is to [follow our VSCode setup instructions](docs/vscode-setup.md).

Note that there are contribution instructions - if you just want to try darklang out,
sign up at https://darklang.com.

We try to make it really easy to get started. If you have any problems, please ask in
[Discord](https://darklang.com/discord-invite) and we'll work to fix any issues you have.

### Install dependencies

We develop Dark within a docker container, so there is not a lot of setup.
However, we do need to setup the host system in a few ways to support running
scripts, and Docker.

#### OSX

To build and run the server you must have the following installed (and running):

- [Homebrew for Mac](https://brew.sh/)
- [Docker for Mac](https://docs.docker.com/docker-for-mac/install/)
- Bash 4 or later: `brew install bash`

#### Linux

Everything should just work on Linux, so long as you have docker installed and
you are using bash 4 or later.

#### Windows

On Windows, you can run Dark in WSL2 (Windows Subsystem for Linux):

- You must be on at least Windows 10 Version 2004, and you must run WSL 2
  (docker does not work in WSL 1)
- Follow the [WSL 2 installation
  instructions](https://docs.microsoft.com/en-us/windows/wsl/install-win10#update-to-wsl-2)
- Follow the [Docker for WSL 2 installation
  instructions](https://docs.docker.com/docker-for-windows/wsl/)
- You need to clone the dark repo with the git `core.autocrlf` setting set to
  `false`. You can configure this by running `git config --global core.autocrlf false`. If you have already cloned dark, you will need to reclone it.
- This section of the guide is incomplete. Please [create an
  issue](https://github.com/darklang/dark/issues) if you find something doesn't work.

### Building and running for the first time

#### Running the build script

Now that the pre-requisites are installed, we should be able to build the
development container in Docker, which has the exact right versions of all the
tools we use.

- If you're using VSCode, we run our build scripts in the VSCode devcontainer. See
  [the VSCode instructions](docs/vscode-setup.md) for instructions.
- Otherwise, simply run `scripts/builder --compile --watch --test`

Note: Running `scripts/builder --compile --watch --test` manually is recommended the first time, since it will show you the output of the build process.

These steps apply for all builds, VSCode or using `scripts/builder`:

- Wait until the terminal says "Initial compile succeeded" - this means the
  build server is ready. The `builder` script will sit open, waiting for file
  changes in order to recompile
- If you see "initial compile failed", it may be a memory issue. Sometimes
  trying again will work. If not, ensure you have Docker configured to provide
  4GB or more of memory, then try again.

### Trying out the AI prompt

For more info check [the CanvasHack readme](backend/experiments/CanvasHack/README.md). But the main steps are:

- Run `./scripts/run-canvas-hack load-from-disk`
- Open http://dark-editor.dlio.localhost:11003/

### Using Dark scripts

The [`scripts/`](/scripts) directory is full of scripts. They automatically execute
in the dev container, even if they are run on the host (see
[`scripts/devcontainer/_assert-in-container`](/scripts/devcontainer/_assert-in-container)
for how this works). Scripts starting with an underscore are primarily intended to be
run by other scripts. Scripts without an underscore are usually intended to be called
by a human, though they are often also called by other scripts as well.

## Read the contributor docs

If you've gotten this far, you're now ready to [contribute your first PR](https://darklang.github.io/docs/contributing/getting-started#first-contribution).

## Testing

- `scripts/run-backend-tests`

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

The VSCode debugger works out of the box with Dark, supporting stepping, breakpoints,
inspecting the stack, etc. You must launch the executable from VSCode for this to
work--attaching does not currently seem to work. You can edit
[`launch.json`](.vscode/launch.json) to change what tests are run or how other
binaries are started up, which should be straightforward.

### REPL (fsi)

You can get a REPL with all of the Dark libraries loaded by running:

- [`scripts/build/dotnet-fsi`](scripts/build/dotnet-fsi)

## Production Services

These are compiled to create libraries and binaries.

These are put into containers, whose definitions are in [containers/](containers). We also
have some containers which are defined entirely in their directory (typically,
these have a self-contained codebase).

## Other important docs

- [Contributor docs](https://docs.darklang.com/contributing/getting-started)
- [Other ways to run the dev container](docs/builder-options.md)
- [Setting up your editor](docs/editor-setup.md)
- [Running unit tests](docs/unittests.md)
- [Dark unit tests](backend/testfiles/README.md)

### Less important docs

- [Docs around running Dark in production](docs/production)
- [Writing docstrings in the Dark Standard library](docs/writing-docstrings.md)
- [Logging and telemetry](docs/logging-and-telemetry.md)
