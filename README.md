# Dark

## Getting Started

### Requirements

To build and run the server you must have the following installed (and running):

- Homebrew for Mac (https://brew.sh/)
- Docker for Mac (https://docs.docker.com/docker-for-mac/install/)
- The latest version of bash (`brew install bash`)
- fswatch `brew install fswatch`)
- PIP (`brew install pip`)
- live reload (`pip install livereload`)
- Dnsmasq (`brew install dnsmasq`)

### First time setup

#### Docker
The app and its dependencies are all held within the container. While code is edited on your machine, the application is compiled and run inside of the container.

Ensure that docker:
- set to use 4 CPUs and 2GB of RAM
- more than that significantly increases build time

#### Dnsmasq

A local DNS server is needed (on OSX) to access the application via a `.localhost` TLD. Follow the [dnsmasq quick start guide](./docs/dnsmasq-quick-start.md)

#### Building and running

- Run `scripts/builder --compile --watch --test`
- Wait til the terminal says "Finished initial compile" - this means the build server is ready
- Edit code normally - on each save in your filesystem, the app will be rebuilt and the browser will reload as necessary

#### Create an admin account for yourself

Follow the [adding a user guide](./docs/add-user.md#adding-yourself-as-an-admin)

#### Ensure the system is working

- Open your browser to http://darklang.localhost:8000/a/YOURNAME/

#### If you're interviewing:

- run `scripts/builder --compile --test` and leave it for 30-60mins to compile the Docker image.
- If there's any issues, let us know when we start and we can fix it in the background.

## Development

### Third party services

- [Trello](https://trello.com) for product backlog
- [CircleCI](https://circleci.com) for continuous integration
- [Rollbar](https://rollbar.com) for continuous integration
- [GCP](https://cloud.google.com) for hosting

### Git conventions

- `master` is deployed to production after every commit (and successful CI run)
- Create feature branches from `master` (naming convention: <features|chores|bugs|spikes>/trello-ticket-title-TRELLO_TICKET_ID)
- Rebase your feature branch before merging into `master` to produce clean/compact merge bubbles.
- Always retain merge commits when merging into `master` (e.g. git merge --no-ff branchname).
- Craft atomic commits that make sense on their own and can be easily cherry-picked or reverted if necessary. (please no WIP commits)
- Please delete branches once they have been merged into `master`.

### Testing

To run unit tests:

- `scripts/builder --compile --test`

Integration tests (on the dev environment):

- `./integration-tests/run.sh`

Integration tests (on the gcp environment):

- `./integration-tests/run.sh --gcp`

Read me about integration testing in [integration-tests/README](./integration-tests/README.md).

### Debugging the client

Click the "EnableDebugger" button on the bottom bar of the app.

### Debugging ppx stuff

PPX is an ocaml preprocessor we use. The ppx libraries are all pretty
opaque. To read the preprocessed output, run jbuilder with the --verbose
flag to see the commands run. You'll see something that looks like this:

```
./.ppx/landmarks.ppx+lwt.ppx+ppx_bin_prot+ppx_deriving.std+ppx_deriving_yojson+ppx_fields_conv+ppx_sexp_conv+ppx_pipebang/ppx.exe --dump-ast --cookie 'library-name="dark"' -o lib/types.pp.ml --impl lib/types.ml
```

Run that without the --dump-ast and then look at the .pp.ml file to find
the preprocessed version.

### Config files

Config files are in config/. Simple rule: anything that runs inside the
container must use a DARK_CONFIG value set in config/, and cannot use
any other env var.

### Code style

You will also want to support ocamlformat. For emacs, see [the
readme](https://github.com/ocaml-ppx/ocamlformat#emacs-setup). For vim:
- install [ALE](https://github.com/w0rp/ale)
- Add to your `.vimrc` (with an appropriate path-to-dark replacement):
```
set rtp+=~/[path to dark]/dark/scripts/ocamlformat
let g:ale_fixers = {'ocaml':['ocamlformat']}
```

### Setting up your editor

Ideally, you'd be able to use merlin inside the container.
We have this kinda working, but not fully. You can use ocamlmerlin in
the container, but it needs some vim/emacs scripts locally, which
require the whole toolchain to get installed unless you want to hack it.

- Install merlin:
  - `brew install opam`
  - `opam init`
    - copy snippet to your bashrc/shell config
  - `opam install merlin`
  - `opam install ocp-indent`

### Troubleshooting container issues

If don't have time to debug docker issues, try:

- `NEVER_REBUILD_DOCKER=1 scripts/builder ...`

#### Accessing the container

- `./scripts/run-in-docker bash`

#### Accessing the local db

- `./scripts/run-in-docker psql -d devdb`

#### (Not) Rebuilding the dev container

If you pull a commit with a Dockerfile update, and then restart your
`scripts/builder` script -- it will rebuild as much of the container as possible.

If you don't want to rebuild the container, use `NEVER_REBUILD_DOCKER=1 scripts/builder ...`
to make the the build script use the last built one.

In another shell you can now kick off a `scripts/builder --compile` to rebuild the container
in parallel with your currently working one.

You can use `export CURRENTLY_REBUILDING_DOCKER=1` to make your run-in-docker invocations, including say ocamlmerlin, use the old+running container as opposed to attempting to use the container
that has an in progress build.

#### Preserving battery life

The're a poll during building that is a great drain on battery life. To
reduce the frequency of the poll, run script/builder using
POLL_FREQUENCY, which is the number of times per second to check.

- `POLL_FREQUENCY=0.1 scripts/builder --etc`

You can also disable the polling (ans consequently the building):

- `scripts/builder --compile --serve`

### Pull the prod db locally

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

## Production deployments

We're running in kubernetes on GKE.

The production containers are deployed as part of the CI build on master.

### How to build production containers

Build the production container (assumes that the build has succeeded):

- `./scripts/gcp-build-containers`

Run it locally (well, inside the dev container):

- `./scripts/gcp-run-locally`

Run integration tests on it:

- `./integration-tests/run.sh --gcp`

### How to deploy manually.

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

### How to rollback a deploy (or pause deploys):
Use:
- `kubectl rollout history <deployment>`
- `kubectl rollout undo <deployment> --to-revision=N`
- `kubectl rollout pause <deployment>` (keeps CI from deploying while you work on a fix)
- `kubectl rollout resume <deployment>` (to resume after pausing)

See `docs/kubectl-rollout.md` for details.

### Troubleshooting GCP/GKE:

If gcloud auth is hanging, you can pass `--no-launch-browser` to `gcloud auth login` to have a CLI based workflow.

If you have authentication problems (eg. `denied: Unable to access the
repository, please check that you have permission to access it.` from a GCR
push), and you've confirmed that you've logged into gcloud and restarted your
container, then check that you've accepted the invite to the Google Developer
Project in your email. If you have and it's still not working, or you don't
have an invitation, then ping Paul or Ian.

## Running conduit

- Run `./scripts/run-conduit-frontend.sh`
- Go to `http://localhost:8001`
- This will use http://conduit.builtwithdark.localhost:8000 as its server, so use http://darklang.localhost:8000/a/conduit/ to create the UI.

## Important docs which we believe are up-to-date:

- [docs/add-user.md](docs/add-user.md)
- [docs/oplist-serialization.md](docs/oplist-serialization.md)
