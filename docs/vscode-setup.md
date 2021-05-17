## Setting up VSCode

VSCode's Remote Containers feature works very well with Dark. However, we need
to run things slightly differently to make it work. Instead of using
`./scripts/builder` to run the build, we run the build within the devcontainer.

## Getting started

Install the [Remote Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)

Open the Dark directory, and when prompted to open it in a container, do so. The container will build, hopefully without problem.

When asked, install the suggested extensions as they are used to build Dark. We
plan to automatically install the right extensions to build Dark, however right
now we are missing the extensions for ReScript and OCaml.

## Issues

We are very interested in any issues you might have. First class support for
VSCode is relatively new, but we probably use it more than any other editor.
Please report any issues in Slack.

## Using Dark scripts

The `scripts/` directory is full of scripts mostly designed to run within the
dev container. These should work just fine when run in the devcontainer or on
the host.
