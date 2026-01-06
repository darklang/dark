# Kill Docker CE in Container

**Status**: [ ] Not started

## What's Being Removed?

Docker CE (Community Edition) is installed in the dev container to enable building Docker containers inside the container (Docker-in-Docker). Since we're not building/pushing production containers, this is unnecessary.

## Changes to Dockerfile

In `/Dockerfile`, remove:

```dockerfile
# Remove this apt source
RUN echo "deb [arch=${TARGETARCH}] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" > /etc/apt/sources.list.d/docker.list

# Remove from apt install
docker-ce \
docker-buildx-plugin \
```

Also remove the docker GPG key import:
```dockerfile
RUN curl -sSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
```

## Changes to devcontainer.json

In `.devcontainer/devcontainer.json`, the Docker socket mount is already commented out:
```json
// "type=bind,src=/var/run/docker.sock,dst=/var/run/docker.sock",
```

Can also remove:
- `ms-azuretools.vscode-docker` extension (optional - might still be useful for reading Dockerfiles)

## Search Commands

```bash
grep -r "docker" --include="Dockerfile" --include="*.json" .devcontainer/
grep -r "DOCKER" --include="*.sh" scripts/
```

## Steps

1. [ ] Edit `Dockerfile` to remove Docker GPG key import
2. [ ] Edit `Dockerfile` to remove docker.list apt source
3. [ ] Edit `Dockerfile` to remove `docker-ce` and `docker-buildx-plugin` from apt install
4. [ ] Optionally remove Docker VS Code extension from devcontainer.json
5. [ ] Search for scripts that might use docker commands
6. [ ] Run `./scripts/run-backend-tests`
7. [ ] Commit: `trim: remove Docker CE from dev container`

## Commit Message Template

```
trim: remove Docker CE from dev container

- Remove docker-ce and docker-buildx-plugin from apt install
- Remove Docker apt repository and GPG key
- No longer need Docker-in-Docker for container builds

We're not building production containers in the dev environment.
```

## Notes

- This will require rebuilding the dev container
- The `containers/` directory still exists but will be deleted in task 07
- Some scripts may reference docker - they'll be cleaned up in final cleanup
