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

`scripts/builder` does the same and then prints the build's status. In VS Code, opening
the folder in the container does the same thing, and you can skip both.

Starting the container also builds once, so it's usable when it comes up.

### Your first fifteen minutes

It shouldn't take fifteen. Measured on a fresh clone with a fresh container and no
build cache, these four came to **144 seconds**, of which 92 was the container coming
up and building and 51 was the test run. That was with the Docker image already
built; the very first time you'll also pay for that, which is the "few minutes" the
start script warns you about.

Four commands, ending in one that proves the environment works:

```
scripts/dev/start            # start the container; prints your host ports
scripts/dev/status           # "status: ok" and "tree: up to date"
scripts/run-cli help         # the CLI runs
scripts/dev/build            # a no-op now, but this is the loop from here on
```

Then `scripts/run-cli docs for-ai` for the language and the conventions. It's the best
documentation in the repo and it's easy to miss.

### The loop from then on

Builds are explicit. Edit however many files you like, then build once:

```
scripts/dev/build            # everything changed since the last good build
scripts/dev/plan             # what that would do, without doing it
scripts/dev/status           # did it work, and has the tree moved on since?
```

`build` blocks, prints the steps it chose, and exits nonzero if any of them fails.
`status` reads `rundir/build-state.json`, which every build path writes, so nothing has
to guess by reading logs.

If you'd rather it rebuilt on save, that still exists and is one command:

```
scripts/dev/watch            # Ctrl+C to stop
```

It's off by default because a five-file change under a watcher pays for five rebuilds,
four of them on states you didn't ask for, each producing failures that look real.

### In case of error

If the build fails:

- `scripts/dev/status` says which step failed; `rundir/logs/build.log` has the output.
- It may be a memory issue. Ensure you have Docker configured to provide 4GB or
  more of memory, then `scripts/dev/build` again.
- Sometimes simply trying again will work.
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
