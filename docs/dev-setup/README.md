# Getting a local development environment set up

Our development is primarily done in VS Code, and we have a devcontainer setup to
make it easy to get started. It's possible to work on Darklang without VS Code,
but it's a bit more involved, and not as well-documented or supported.

## Install dependencies

We develop Dark within a docker container, so there is not a lot of setup.
However, we do need to setup the host system in a few ways to support running
scripts, and Docker.

### Linux

Everything should just work on Linux, so long as you have docker installed and
you are using bash 4 or later.

### OSX

To build and run the server you must have the following installed (and running):

- [Homebrew for Mac](https://brew.sh/)
- [Docker for Mac](https://docs.docker.com/docker-for-mac/install/)
- Bash 4 or later: `brew install bash`

### Windows

Admidittedly, our Windows support is not as good as our support for other
platforms. We would love to improve it, and welcome feedback + contributions.

That said, you can run Dark in WSL2 (Windows Subsystem for Linux):

- You must be on at least Windows 10 Version 2004, and you must run WSL 2
  (docker does not work in WSL 1)
- Follow the [WSL 2 installation
  instructions](https://docs.microsoft.com/en-us/windows/wsl/install-win10#update-to-wsl-2)
- Follow the [Docker for WSL 2 installation
  instructions](https://docs.docker.com/docker-for-windows/wsl)
- You need to clone the dark repo with the git `core.autocrlf` setting set to
  `false`. You can configure this by running `git config --global core.autocrlf false`.
  If you have already cloned dark, you will need to reclone it.

## Building and running for the first time

### Running the build script

Now that the pre-requisites are installed, we should be able to build the
development container in Docker, which has the exact right versions of all the
tools we use.

Start the container:

```
scripts/dev/start
```

Safe to re-run; it does nothing if the container is already up. It also picks this
clone's host ports and prints them, so several clones can run side by side. Pass
`--recreate` after changing `.devcontainer/devcontainer.json`, since `devcontainer up`
otherwise reuses the container that already exists.

This needs the devcontainer CLI on the host: `npm install -g @devcontainers/cli`.

If you'd rather watch the build in your terminal, `scripts/builder` starts the container
and then follows the build log. In VS Code, opening the folder in the container does the
same thing, and you can skip both.

### Ensure all built OK

Wait until the log says "Initial compile succeeded" - that means the build server is
ready and watching for changes.

### In case of error

If you see "initial compile failed," there are a few things to try:

- It may be a memory issue. Ensure you have Docker configured to provide 4GB or
  more of memory, then try again.
- Sometimes, simply trying again will work
  -- rebuild the container or re-run `scripts/builder` manually again.
- If setting up in VS Code, try navigating to the `global.json` at the root of the
  repo, and saving it (unchanged).
- Go to a random `.dark` file in the `packages` directory and save it (unchanged).
- If you're still stuck, please ask for help in [Discord](https://darklang.com/discord-invite) or create a [GitHub issue](https://github.com/darklang/dark/issues).

## Formatting

You will also want to support formatting in your client. Dark uses Prettier for
JS/HTML/CSS and Fantomas for F# and Darklang code. The script
`script/formatting/format` can be used to format or check formatting, and there is a
pre-commit hook you can use to run it automatically. Formatting is checked in CI.

We use yapf for python formatting, though it's not checked in CI.

## Pre-commit hook

You probably also want to install a pre-commit hook that runs the formatters for
you.
`cp scripts/formatting/pre-commit-hook.sh .git/hooks/pre-commit && chmod +x .git/hooks/pre-commit`

## Running several clones at once

Each clone gets its own container, its own `backend/Build` volume, and its own block of
host ports: the first clone up takes 9090-9099, the next 9100-9109. `scripts/dev/start`
picks the block and prints it, and `scripts/dev/host-port` tells you later.

`./scripts/*` commands enter the container belonging to the clone you're standing in. If
that clone has no container, they stop and say so rather than running in another one.

### Rebuilding the dev container

Pulling a Dockerfile change and running `scripts/dev/start --recreate` rebuilds as much
as Docker's cache allows. Without `--recreate` the existing container is reused, which is
usually what you want.

You can use `export CURRENTLY_REBUILDING_DOCKER=1` to make your run-in-docker
invocations, use the old+running container as opposed to attempting to use the
container that has an in progress build.
