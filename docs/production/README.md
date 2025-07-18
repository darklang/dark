# Running Dark in production

The docs in this directory are used by Dark employees to run the production Dark infrastructure.

**Almost nothing in this file is true.**

## Overview

The production containers are deployed as part of the CI build on the main branch.

### How to build production containers

Build the production container (assumes that the build has succeeded):

- `./scripts/production/gcp-build-containers`

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

- `./scripts/deployment/_gcp-push-images-to-gcr`

Trigger the deploy:

- `./script/deployment/gke-deploy`

## How to pull the prod db locally

TODO update this now that we've migrated to sqlite.

### Troubleshooting GCP/GKE:

If gcloud auth is hanging, you can pass `--no-launch-browser` to `gcloud auth login` to have a CLI based workflow.

If you have authentication problems (eg. `denied: Unable to access the repository, please check that you have permission to access it.` from a GCR
push), and you've confirmed that you've logged into gcloud and restarted your
container, then check that you've accepted the invite to the Google Developer
Project in your email. If you have and it's still not working, or you don't
have an invitation, then ping Paul or Ian.
