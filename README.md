# How to build and run

- Install Docker for Mac
- Install Homebrew for Mac from https://brew.sh/
- Run `brew install bash`
- Run `brew install fswatch`
- Run `scripts/builder --compile --watch --test` (if don't have time to debug docker issues, `NEVER_REBUILD_DOCKER=1 scripts/builder --compile --watch --test` is your friend)
- Wait til the terminal says "Finished initial compile" - this means the build server is ready
- Open your browser to http://YOURNAME.localhost:8000/admin/ui
- Edit code normally - on each save in your filesystem, the app will be rebuilt and the browser will reload as necessary

## Just serve it, not constantly recompiling

- Run `scripts/builder --compile --serve`

# Deploy it to prod

We're running in kubernetes on GKE.

Build the production container (assumes that the build has succeeded):

- `./scripts/gcp-build-container`

Run it locally (well, inside the dev container):

- `./scripts/gcp-run-locally`

Run integration tests on it:

- `./integration-tests/run.sh --gcp`

To deploy, you'll need `gcloud` installed:

- `curl -s https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-192.0.0-darwin-x86_64.tar.gz | tar xz && ./google-cloud-sdk/install.sh`.

then authenticate with gcloud:

- `gcloud auth login`

(Note: you might need to restart your shell for gcloud to appear in your $PATH, or run `exec $SHELL`)

You should restart your development container at this point, as it pulls in your currently authenticated
user at start time.

and then run:

- `./script/gke-deploy`

If you still have authentication problems (eg. `denied: Unable to access the repository, please check that you have permission to access it.` from a GCR push),
and you've confirmed that you've logged into gcloud and restarted your container, then check that you've accepted the invite to the Google Developer Project
in your email. If you have and it's still not working, or you don't have an invitation, then ping Paul or Ian.


# Testing

To run unit tests:

- `scripts/builder --compile --test`

Integration tests (on the dev environment):

- `./integration-tests/run.sh`

Integration tests (on the gcp environment):

- `./integration-tests/run.sh --gcp`

Read me about integration testing in integration-tests/README.

# Accessing the container

- `./scripts/run-in-docker bash`

# Accessing the local db

- `./scripts/run-in-docker psql -d devdb`

# Pull the prod db locally

- `./scripts/download-gcp-db`

This pulls all the data from gcp and puts it in a db named "postgres". In
development, we usually run against a db called "devdb". You
can switch to run against the "prodclone" db by changing the
DARK_CONFIG_DB_DBNAME variable in config/dev.


# Config files

Config files are in config/. Simple rule: anything that runs inside the
container must use a DARK_CONFIG value set in config/, and cannot use
any other env var.

# Setting up your editor

Ideally, you'd be able to use elm-make and merlin inside the container.
We have this kinda working, but not fully. You can use ocamlmerlin in
the container, but it needs some vim/emacs scripts locally, which
require the whole toolchain to get installed unless you want to hack it.

- Install merlin:
  - `brew install opam`
  - `opam init`
    - copy snippet to your bashrc/shell config
  - `opam install merlin`
  - `opam install ocp-indent`

To get elm to work, we added scripts/elm-make-vim, which handles how ALE uses
elm-make. It's possible this may need changes, or it may work out of the
box.

# (Not) Rebuilding the dev container

If you pull a commit with a Dockerfile update, and then restart your
`scripts/builder` script -- it will rebuild as much of the container as possible.

If you don't want to rebuild the container, use `NEVER_REBUILD_DOCKER=1 scripts/builder ...`
to make the the build script use the last built one.

In another shell you can now kick off a `scripts/builder --compile` to rebuild the container
in parallel with your currently working one.

You can use `export CURRENTLY_REBUILDING_DOCKER=1` to make your run-in-docker invocations, including say ocamlmerlin, use the old+running container as opposed to attempting to use the container
that has an in progress build.

# Preserving battery life

The're a poll during building that is a great drain on battery life. To
reduce the frequency of the poll, run script/builder using
POLL_FREQUENCY, which is the number of times per second to check.

- `POLL_FREQUENCY=0.1 scripts/builder --etc`

You can also disable the polling (ans consequently the building):

- `scripts/builder --compile --serve`

# Debugging elm

Go to `http://localhost:8000/admin/ui-debug` instead of `/admin/ui`.

# Versioning oplists

Oplists are versioned by the hash of the "shape" of their structure.
If you change the structure of an Op (including the nested structure),
then the binary version will no longer load.

By default, the server will also save a json version in json_oplists. You can migrate this to the new version. Check out server/serialize.ml to see how it all works.


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

# Importing packages from npm

npm dependencies are managed in development, bundled up via browserify, tracked in git, and included in `ui.html`.

* `yarn install`
* $`yarn add PACKAGE_NAME`
* Use the package in `server/src/main.js` or whatever
* $`yarn build` manages `/server/static/bundle.js`

