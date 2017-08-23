# Scripts used to build Dark


## Building
- `builder`: the build script. Build a docker container, then runs a build in the container. It watches for any file changes and will compile on change.

- `run-in-docker`: run a command in the running container. Expects builder to be running.

- `run-full-build`: run the full build. Expects builder to be running

## Deploying

- `heroku-push`: push to heroku

## Editor support

- `compile-in-docker`: if you want to compile a file directly, use this. This is useful when you want to get errors in your editor.

- `ocamlmerlin`: a script to run ocamlmerlin within the container. This exists because Emacs needs an executable name, and doesn't allow you to say `run-in-docker ocamlmerlin`. Note this script is not specific to ocamlmerlin at all, and can be copied directly to other scripts.

- `ocp-indent`: exact copy of ocamlmerlin

