# Running Dark in production

The docs in this directory are used by Dark employees to run the production Dark infrastructure.

## Overview

We're running in kubernetes on GKE.

The production containers are deployed as part of the CI build on the main branch.

### How to build production containers

Build the production container (assumes that the build has succeeded):

- `./scripts/gcp-build-containers`

### How to deploy manually.

You'll need `gcloud` installed:

- `curl -s https://dl.google.com/dl/cloudsdk/channels/rapid/downloads/google-cloud-sdk-192.0.0-darwin-x86_64.tar.gz | tar xz && ./google-cloud-sdk/install.sh`

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

## How to pull the prod db locally

- `scripts/download-gcp-db`

This pulls all the data from gcp and puts it in a db named "prodclone".

Access it:

- `scripts/builder --compile --watch [etc] --prodclone`

You can reset the clone to a pristine production clone (at the time of
download) with:

- `scripts/reset-prodclone`

And access it directly with:

- `scripts/run-in-docker psql -d prodclone`

You can also access the real DB in production:

- `./scripts/gcp-prod`



### Troubleshooting GCP/GKE:

If gcloud auth is hanging, you can pass `--no-launch-browser` to `gcloud auth login` to have a CLI based workflow.

If you have authentication problems (eg. `denied: Unable to access the
repository, please check that you have permission to access it.` from a GCR
push), and you've confirmed that you've logged into gcloud and restarted your
container, then check that you've accepted the invite to the Google Developer
Project in your email. If you have and it's still not working, or you don't
have an invitation, then ping Paul or Ian.

