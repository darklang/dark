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
- This section of the guide is incomplete. Please [create an
  issue](https://github.com/darklang/dark/issues) if you find something doesn't work.

## Building and running for the first time

### Running the build script

Now that the pre-requisites are installed, we should be able to build the
development container in Docker, which has the exact right versions of all the
tools we use.

- If you're using VS Code, we run our build scripts in the VS Code devcontainer. See
  [the VS Code instructions](docs/vscode-setup.md) for instructions.
- Otherwise, simply run `scripts/builder --compile --watch --test`,
  and watch the output of the build process.

### Ensure all built OK

These steps apply for all builds, in VS Code or if manually running `scripts/builder`:

Wait until the terminal says "Initial compile succeeded" - this means the build
server is ready. The `builder` script will sit open, waiting for file changes in
order to recompile.

### In case of error

If you see "initial compile failed," there are a few things to try:

- It may be a memory issue. Ensure you have Docker configured to provide 4GB or
  more of memory, then try again.
- Sometimes, simply trying again will work
  -- rebuild the container or re-run `scripts/builder` manually again.
- If setting up in VS Code, try navigating to the `global.json` at the root of the
  repo, and saving it (unchanged).
- Go to a random `.dark` file in the `packages` directory and save it (unchanged).
- If you're still stuck, please ask for help in Discord or create a GitHub issue.

## Formatting

You will also want to support formatting in your client. Dark uses Prettier for
JS/HTML/CSS and Fantomas for F# and Darklang code. The script
`script/formatting/format` can be used to format or check formatting, and there is a
pre-commit hook you can use to run it automatically. Formatting is checked in CI.

We use yapf for python formatting, though it's not checked in CI.

## Pre-commit hook

You probably also want to install a pre-commit hook that the formatters for
you.
`cp scripts/formatting/pre-commit-hook.sh .git/hooks/pre-commit && chmod +x .git/hooks/pre-commit`

## Other ways to run the dev container

### Just serve it, not constantly recompiling

- Run `scripts/builder --compile --serve`

Note that if you're using a VS Code devcontainer, this will happen automatically and
you don't need to run it.

### (Not) Rebuilding the dev container

If you pull a commit with a Dockerfile update, and then restart your
`scripts/builder` script -- it will rebuild as much of the container as possible.

If you don't want to rebuild the container, use `NEVER_REBUILD_DOCKER=1 scripts/builder ...`
to make the the build script use the last built one.

In another shell you can now kick off a `scripts/builder --compile` to rebuild the container
in parallel with your currently working one.

You can use `export CURRENTLY_REBUILDING_DOCKER=1` to make your run-in-docker
invocations, use the old+running container as opposed to attempting to use the
container that has an in progress build.
