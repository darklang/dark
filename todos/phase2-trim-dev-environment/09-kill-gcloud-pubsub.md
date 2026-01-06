# Kill Google Cloud SDK and PubSub

**Status**: [ ] Not started

## What's Being Removed?

Google Cloud SDK and PubSub emulator:
- `google-cloud-sdk` - Full GCP CLI
- `google-cloud-sdk-pubsub-emulator` - Local PubSub emulator
- `google-cloud-sdk-gke-gcloud-auth-plugin` - GKE authentication

PubSub was used by QueueWorker (now deleted). GCloud SDK was for deployment.

## Dockerfile Changes

Remove from apt install:
```dockerfile
google-cloud-sdk \
google-cloud-sdk-pubsub-emulator \
google-cloud-sdk-gke-gcloud-auth-plugin \
```

Remove the apt source:
```dockerfile
RUN echo "deb http://packages.cloud.google.com/apt cloud-sdk main" > /etc/apt/sources.list.d/google-cloud-sdk.list
```

Remove GPG key:
```dockerfile
RUN curl -sSL https://packages.cloud.google.com/apt/doc/apt-key.gpg | apt-key add -
```

Remove PubSub env var:
```dockerfile
ENV PUBSUB_EMULATOR_HOST=localhost:8085
```

## NuGet Dependencies

Remove from `backend/paket.dependencies`:
```
nuget Google.Cloud.PubSub.V1 = 3.8.0
```

## Scripts to Remove

Already deleted in earlier tasks:
- `scripts/run-pubsub-emulator` (from QueueWorker deletion)

Check for others:
```bash
grep -r "gcloud\|pubsub" --include="*.sh" scripts/
```

## Devcontainer Mounts

In `.devcontainer/devcontainer.json`, remove:
```json
"type=volume,src=gcloud_creds,dst=/home/dark/.config/gcloud",
```

## Steps

1. [ ] Edit `Dockerfile`:
   - Remove google-cloud-sdk apt source and GPG key
   - Remove google-cloud-sdk packages from apt install
   - Remove PUBSUB_EMULATOR_HOST env var
2. [ ] Remove `Google.Cloud.PubSub.V1` from `backend/paket.dependencies`
3. [ ] Run `paket install`
4. [ ] Remove gcloud_creds mount from `.devcontainer/devcontainer.json`
5. [ ] Search for remaining gcloud/pubsub references and clean
6. [ ] Run `./scripts/run-backend-tests`
7. [ ] Wait for build
8. [ ] Commit: `trim: remove Google Cloud SDK and PubSub`

## Commit Message Template

```
trim: remove Google Cloud SDK and PubSub

- Remove google-cloud-sdk and pubsub-emulator from Dockerfile
- Remove Google.Cloud.PubSub.V1 NuGet package
- Remove gcloud credentials mount from devcontainer
- Remove PUBSUB_EMULATOR_HOST env var

GCloud SDK and PubSub were for cloud deployment and QueueWorker.
Neither needed for local development.
```

## Notes

- Significant Dockerfile change - will need container rebuild
- PubSub code in LibCloud may need cleanup if not already removed
- The `config/dev` env file may reference PUBSUB - check and clean
