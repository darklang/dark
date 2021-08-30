# Running scripts/one-off binaries in production

_We're rethinking how this works after we switch to F#. Maybe .fsx files?_

Sometimes, hopefully rarely, you want to interactively run some code against the production
database. This is generally to facilitate/backfill a data migration. The most common
method for doing so is adding an OCaml file with a `let () = ...` section (ie. a `main`)
to `backend/bin`, and a compilation stanza for it to `backend/bin/dune` so that it gets built.
The program will be built to `_build/default/backend/bin/` (note: this is _inside_ the attached
volume, so you can only see this from _inside_ the container).

Once you have a binary built locally, you have two options for running it in production:

## Copying via `kubectl`

This approach should be used for irregular, non-repeating scripts like a data migration.

Before you begin: You should pause deployments (see `docs/kubectl-rollout.md`), so the machine you're
working on doesn't get terminated.

Get an active pod name via `kubectl get pods`. You should use one of the
`bwd-deployment` or `editor-deployment` pods as they're set up to run any
backend application (ie. they definitely have the runtime libraries to support
any dependency specified in the backend).

To do the copy:
`kubectl cp _build/default/backend/bin/foo.exe $PODNAME:/home/dark/bin -c bwd-ctr`

To execute the copied binary:
`kubectl exec -it $PODNAME -c bwd-ctr bin/foo.exe`

REMINDER: Unpause deployments once done (see `docs/kubectl-rollout.md`)

## Adding it to every container

You should use this approach if you have a script that you _always_ want available on every production
box, maybe to aid debugging a live process.

Simply copy the binary into the directory that gets mounted into the containers
when being built in `scripts/deployment/gcp-build-containers`. It'll look something like a

`cp -f _build/default/backend/bin/foo.exe "$DIR/bin"` command before the invocation of `docker build` that's relevant to you. At time of writing, there's an
example in `gcp-build-containers` of `emergency_login_script.exe` doing this.

You'll also need to add the built filepath (eg. `_build/default/backend/bin/foo.exe`) to the `paths` array in the `persist_to_workspace` step of `build-backend` in `.circleci/config.yml` to ensure the file makes it from the initial compile stage to the build/deploy containers stage.
