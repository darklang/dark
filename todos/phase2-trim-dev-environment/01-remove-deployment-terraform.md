# Remove Deployment Infrastructure (Terraform)

**Status**: [x] Complete

## What's Being Removed?

The entire `tf/` directory contains Terraform configuration for Google Cloud Platform deployment:
- Cloud Run services (BwdServer, ProdExec)
- VPC networking
- IAM roles and service accounts
- Pub/Sub configuration
- DNS and certificate management
- Artifact Registry settings
- Secret Manager integration

We're not deploying to cloud - all this infrastructure config is unnecessary.

## Directory to Delete

```
tf/
  main.tf
  locals.tf
  cloudrun.tf
  darklangio.tf
  bwdserver.tf
  iam.tf
  vpc.tf
  apis.tf
  pubsub.tf
  workload_identity_pool.tf
  artifact_registry.tf
  cloudstorage.tf
  custom-domains.tf
  secrets.tf
  service_env_vars.tf
  [and any other .tf files]
```

## Other References to Clean

1. **Scripts**: `scripts/deployment/` - many scripts reference terraform
2. **CircleCI**: Terraform validation jobs
3. **Devcontainer**: Terraform credentials mount

## Search Commands

```bash
grep -r "terraform\|\.tf" --include="*.sh" --include="*.yml" --include="*.json" scripts/ .circleci/ .devcontainer/
```

## Steps

1. [ ] Delete the entire `tf/` directory
2. [ ] Check `scripts/deployment/` for terraform-dependent scripts (note for later cleanup)
3. [ ] Note terraform references in CircleCI (will be cleaned in 02-trim-circleci.md)
4. [ ] Run `./scripts/run-backend-tests`
5. [ ] Commit: `trim: delete tf/ terraform directory`

## Commit Message Template

```
trim: delete tf/ terraform directory

- Remove entire tf/ directory with GCP infrastructure config
- Includes Cloud Run, VPC, IAM, Pub/Sub, and other GCP resources

We're not deploying to cloud infrastructure. Terraform config
can be recreated if needed in the future.
```

## Notes

- This is a big deletion but should have no impact on local dev
- Terraform tooling in Dockerfile will be removed in task 12
- Deployment scripts will be cleaned in final/02-clean-scripts.md
