# How to build and run

- Install Docker for Mac
  - Set it to use 4 CPUs and 2GB of RAM
    - More than that significantly increases build time
- Install Homebrew for Mac from https://brew.sh/
- Run `brew install bash`
- Run `brew install fswatch`
- Run `pip install livereload`
- Run `scripts/builder --compile --watch --test`
- Wait til the terminal says "Finished initial compile" - this means the build server is ready
- Open your browser to http://builtwithdark.localhost:8000/YOURNAME/
- Edit code normally - on each save in your filesystem, the app will be rebuilt and the browser will reload as necessary

## If you're interviewing:

- run `scripts/builder --compile --test` and leave it for 30-60mins to compile the Docker image.
- If there's any issues, let us know when we start and we can fix it in the background.

## Just serve it, not constantly recompiling

- Run `scripts/builder --compile --serve`

# Deploy it to prod

We're running in kubernetes on GKE.

The production containers are deployed as part of the CI build on master.

## How to build production containers

Build the production container (assumes that the build has succeeded):

- `./scripts/gcp-build-containers`

Run it locally (well, inside the dev container):

- `./scripts/gcp-run-locally`

Run integration tests on it:

- `./integration-tests/run.sh --gcp`

## How to deploy manually.

You'll need `gcloud` installed:

- `curl -s https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-192.0.0-darwin-x86_64.tar.gz | tar xz && ./google-cloud-sdk/install.sh`.

then authenticate with gcloud:

- `gcloud auth login`

(Note: you might need to restart your shell for gcloud to appear in your $PATH,
or run `exec $SHELL`)

You should restart your development container at this point, as it pulls in
your currently authenticated user at start time.

Push the production container to Google Cloud Registry:

- `./scripts/gcp-push-images-to-gcr`

Trigger the deploy:

- `./script/gke-deploy`

## How to rollback a deploy (or pause deploys):
Use:
- `kubectl rollout history <deployment>`
- `kubectl rollout undo <deployment> --to-revision=N`
- `kubectl rollout pause <deployment>` (keeps CI from deploying while you work on a fix)
- `kubectl rollout resume <deployment>` (to resume after pausing)

See `docs/kubectl-rollout.md` for details.

## Troubleshooting GCP/GKE:

If gcloud auth is hanging, you can pass `--no-launch-browser` to `gcloud auth login` to have a CLI based workflow.

If you have authentication problems (eg. `denied: Unable to access the
repository, please check that you have permission to access it.` from a GCR
push), and you've confirmed that you've logged into gcloud and restarted your
container, then check that you've accepted the invite to the Google Developer
Project in your email. If you have and it's still not working, or you don't
have an invitation, then ping Paul or Ian.


## Troubleshooting container issues:

If don't have time to debug docker issues, try:

- `NEVER_REBUILD_DOCKER=1 scripts/builder ...`

# Adding an account to the DB

Accounts upserted to the database in the `init` function in `accounts.ml`.

There are two types of accounts, admins and regular users.

Regular users get access to canvases under their username only ie. a user with the username 'foo' gets access to foo-bar.builtwithdark.com, foo-foo.buildwithdark.com etc.  These can be given to anyone for user testing etc.

Admins can do the above normally, and also access anyone's canvases. These should only be given to employees and contractors who have signed contracts.

 Admins are added via `Account.upsert_admin` and regular users are added via `Account.upsert_account`. Both functions take an `Account.account` record as a parameter. To add a new user, add a call to one of the aforementioned functions in the body of the `init` function in accounts.ml.

Please commit and push the change. You must build a new production container and deploy it for the new account
to be usable on builtwithdark.com.

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

This pulls all the data from gcp and puts it in a db named "prodclone".

Access it:

- `./scripts/builder --compile --watch [etc] --prodclone`

You can reset the clone to a pristine production clone (at the time of
download) with:

- `./scripts/reset-prodclone`

And access it directly with:

- `./scripts/run-in-docker psql -d prodclone`

You can also access the real DB in production:

- `./scripts/gcp-prod`

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

Go to `http://builtwithdark.localhost:8000/YOURNAME/ui-debug` instead of `YOURNAME/`.

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
- This will use http://conduit.builtwithdark.localhost:8000 as its server, so use http://darklang.localhost:8000/a/conduit/ to create the UI.

# How to profile (old info, much may have changed)
- make compilation and run use OCAML_LANDMARKS="auto,format=json,output=profile.json,allocation", in scripts/support/compile (enable the `profile` global)
- rm server/_build
- run the builder
- refresh the page 10 times, then post to localhost:8000/api/shutdown (shuts
  down the server and saves the profiling info)
  - if the time is all spent in twitter, delete server/profile.json and
    try again
- upload the profile.json to https://lexifi.github.io/landmarks/viewer.html
- look in "Source Tree Time"

# Importing packages from npm

When writing JS, we use some packages from NPM. To add a new package:

- Add the package to package.json, perhaps using `yarn add`
- Add the library to module.exports in server/src/main.js.
- Run `yarn install` and `yarn build`. This puts the code in bundle.js.
- Check in bundle.js

For example, to add js-sha512:
- `yarn add js-sha512@^0.8.0`
- `yarn install`
- in server/src/main.js, add `var sha512 = require('js-sha512');`
- `yarn build`
- then use `sha512.sha384("test value")` in js.

# Important docs which we believe are up-to-date:

- [docs/add-user.md](docs/add-user.md)
- [docs/oplist-serialization.md](docs/oplist-serialization.md)
