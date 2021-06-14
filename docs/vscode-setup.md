## Setting up VSCode

VSCode's Remote Containers feature works very well with Dark. However, we need
to run things slightly differently to make it work. Instead of using
`./scripts/builder` to run the build, we run the build within the devcontainer.

## Getting started

Install the [Remote Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)

Open the Dark directory, and when prompted to open it in a container, do so.
The container will build, hopefully without problem.

When asked, install the suggested extensions as they are used to build Dark. We
plan to automatically install the right extensions to build Dark, however right
now we are missing the extensions for ReScript and OCaml.

## Build output

You should see build output in the terminal, under the heading "Configuring".
If you have to reload the VSCode window, the build output will no longer
appear. Fortunately, it is logged to `rundir/logs/build-server.txt`, so you can
see it by opening a terminal and running:

- `tail -f rundir/logs/build-server.txt`

## Extensions

VSCode extensions are installed on a docker volume in the container, which caches them for the next time we need to restart the container. This means that new extensions added to devcontainer.json are not automatically built. If that's needed, you can delete the extensions volume with:

- `docker volume rm darklang-dark-extension-volume darklang-dark-extension-volume-insiders`

## Issues

We are very interested in any issues you might have. First class support for
VSCode is relatively new, but we probably use it more than any other editor.
Please report any issues in Slack.
