# Kill Terraform Tooling

**Status**: [ ] Not started

## What's Being Removed?

Terraform CLI and related tooling. The `tf/` directory was already deleted, but the tooling remains in the Dockerfile.

## Dockerfile Changes

Remove Hashicorp apt source:
```dockerfile
RUN curl -sSL https://apt.releases.hashicorp.com/gpg | apt-key add -
RUN echo "deb https://apt.releases.hashicorp.com $(lsb_release -cs) main" > /etc/apt/sources.list.d/hashicorp.list
```

Remove Terraform installation:
```dockerfile
############################
# Terraform
############################
RUN /home/dark/install-targz-file \
  --arm64-sha256=... \
  --amd64-sha256=... \
  --url=https://releases.hashicorp.com/terraform/1.6.0/terraform_1.6.0_linux_${TARGETARCH}.zip \
  --extract-file=terraform \
  --target=/usr/bin/terraform
```

## Devcontainer Changes

Remove terraform credentials mount from `.devcontainer/devcontainer.json`:
```json
"type=volume,src=terraform_creds,dst=/home/dark/.terraform.d",
```

Also remove the terraform directory creation in Dockerfile:
```dockerfile
RUN mkdir -p .terraform.d/
```

## Steps

1. [ ] Edit `Dockerfile`:
   - Remove Hashicorp apt source and GPG key
   - Remove Terraform installation section
   - Remove `.terraform.d/` directory creation
2. [ ] Edit `.devcontainer/devcontainer.json`:
   - Remove terraform_creds volume mount
3. [ ] Run `./scripts/run-backend-tests`
4. [ ] Commit: `trim: remove Terraform tooling`

## Commit Message Template

```
trim: remove Terraform tooling

- Remove Terraform installation from Dockerfile
- Remove Hashicorp apt repository
- Remove terraform credentials mount from devcontainer

Terraform config (tf/) already deleted. Tooling no longer needed.
```

## Notes

- VS Code Terraform extension was already removed in task 04
- This completes the terraform removal
