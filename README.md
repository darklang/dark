# How to build and run

- Install Docker for Mac
- Run `brew install bash`
- Run `brew install fswatch`
- Run `scripts/builder --compile --watch --test`
- Wait til the terminal says "Finished initial compile" - this means the build server is ready
- Open your browser to http://localhost:8000/admin/ui
- Edit code normally - on each save in your filesystem, the app will be rebuilt and the browser will reload as necessary

## Just serve it, not constantly recompiling

- Run `scripts/builder --compile --serve`

# Deploy it to prod

The GCP docs for what we use (a docker container in a Google Compute
Engine instance) is at
https://cloud.google.com/compute/docs/containers/deploying-containers.

Build the production container (assumes that the build has succeeded):

- `./scripts/run-in-docker ./scripts/gcp-build-container`

Run it locally (well, inside the dev container):

- `./scripts/run-in-docker ./scripts/gcp-run-locally`

Run integration tests on it:

- `./scripts/run-in-docker ./integration-tests/run.sh --gcp`

Then deploy it:

- `./scripts/run-in-docker ./script/gcp-deploy`

To connect to the host via SSH (you can use regular docker commands from there):

- `gcloud beta compute ssh instance-2`


# Testing

To run unit tests:

- `scripts/builder --compile --test`

Integration tests (on the dev environment):

- `./scripts/run-in-docker ./integration-tests/run.sh`

Integration tests (on the gcp environment):

- `./scripts/run-in-docker ./integration-tests/run.sh --gcp`

Read me about integration testing in integration-tests/README.


# Config files

Config files are in config/. Simple rule: anything that runs inside the
container must use a DARK_CONFIG value set in config/, and cannot use
any other env var.

# Setting up your editor

Ideally, you'd be able to use elm-make and merlin inside the container.
We have this kinda working, but not fully. You can use ocamlmerlin in
the container, but it needs some vim/emacs scripts locally, which
require the whole toolchain to get installed unless you want to hack it.

To get elm to work (in vim at least), you need elm-make locally.

- Install merlin:
  - `brew install opam`
  - `opam init`
    - copy snippet to your bashrc/shell config
  - `opam install merlin`
  - `opam install ocp-indent`

- Install elm tools:
  - `brew install elm`

# (Not) Rebuilding the dev container

If you pull a commit with a Dockerfile update, and then restart your
`scripts/builder` script -- it will rebuild as much of the container as possible.

If you don't want to rebuild the container, use `NEVER_REBUILD_DOCKER=1 scripts/builder ...`
to make the the build script use the last built one.

In another shell you can now kick off a `scripts/builder --compile` to rebuild the container
in parallel with your currently working one.

You can use `export CURRENTLY_REBUILDING_DOCKER=1` to make your run-in-docker invocations, including say ocamlmerlin, use the old+running container as opposed to attempting to use the container
that has an in progress build.

# Debugging elm

By default, debugging in the client is disabled. This is because it is
impossibly slow. However, it's a great debugger, so there's a simple way
to enable it:

- `scripts/start_client_debug`

Stop debugging with

- `scripts/stop_client_debug`

# Versioning Dark files

Dark files are versioned by the hash of the "shape" of their structure.
If you change the structure of an Op (including the nested structure),
then the binary version will no longer load.

By default, the server will also save a json version. You can manually
edit this to the new version. Check out server/canvas.ml to see how it
all works.


# Backing up darkfiles

Darkfiles are backed up daily, so long as the build container is running
at 4pm. They are backed up to persistdir/backup_appdata.

# Debugging ppx stuff

PPX is an ocaml preprocessor we use. The ppx libraries are all pretty
opaque. To read the prprocessed output, run jbuilder with the --verbose
flag to see the commands run. You'll see something that looks like this:

```
./.ppx/landmarks.ppx+lwt.ppx+ppx_bin_prot+ppx_deriving.std+ppx_deriving_yojson+ppx_fields_conv+ppx_sexp_conv+ppx_pipebang/ppx.exe --dump-ast --cookie 'library-name="dark"' -o lib/types.pp.ml --impl lib/types.ml
```

Run that without the --dump-ast and then look at the .pp.ml file to find
the preprocessed version.

# Running conduit

- Run `./scripts/run-conduit-frontend.sh`
- Go to `http://localhost:8001`
- This will use http://conduit.localhost:8000 as its server, so use http://conduiit.localhost:8000/admin/ui to create the UI.

# How to profile (old info, much may have changed)
- make compilation and run use OCAML_LANDMARKS="auto,format=json,output=profile.json,allocation", in scripts/support/compile (enable the `profile` global)
- rm server/_build
- run the builder
- refresh the page 10 times, then go to localhost:8000/admin/api/shutdown (shuts
  down the server and saves the profiling info)
  - if the time is all spent in twitter, delete server/profile.json and
    try again
- upload the profile.json to https://lexifi.github.io/landmarks/viewer.html
- look in "Source Tree Time"


