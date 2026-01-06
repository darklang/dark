# Trim CircleCI Configuration

**Status**: [x] Complete

## Current CircleCI Jobs

From `.circleci/config.yml`:
- `static-checks` - Lint, Terraform validation, formatting
- `build-parser` - Tree-sitter parser compilation
- `build-backend` - .NET publish and tests
- `build-cli` - CLI executable builds
- `gcp-containers-test` - Test container builds
- `publish-github-release` - Release CLI on main
- `publish-vs-code-extension` - VS Code extension publishing
- `push-containers-to-gcp` - Push containers to Artifact Registry
- `push-assets-to-gcp` - Push assets to Cloud Storage
- `deploy-lock` - Deploy lock management
- `deploy` - Deployment to GKE

## Jobs to KEEP

- `static-checks` (but remove terraform validation)
- `build-parser`
- `build-backend`
- `build-cli`
- `publish-github-release`
- `publish-vs-code-extension`

## Jobs to REMOVE

- `gcp-containers-test` - Not deploying containers
- `push-containers-to-gcp` - Not pushing to GCP
- `push-assets-to-gcp` - Not pushing assets
- `deploy-lock` - No deployment locking needed
- `deploy` - No deployment

## Things to Trim from Remaining Jobs

- Remove terraform validation from `static-checks`
- Remove GCP authentication where not needed
- Remove Honeycomb/Rollbar notification steps
- Simplify workflows

## Steps

1. [ ] Read current `.circleci/config.yml` thoroughly
2. [ ] Remove `gcp-containers-test` job
3. [ ] Remove `push-containers-to-gcp` job
4. [ ] Remove `push-assets-to-gcp` job
5. [ ] Remove `deploy-lock` job
6. [ ] Remove `deploy` job
7. [ ] Remove terraform validation from `static-checks`
8. [ ] Remove deployment notification steps (Honeycomb, Rollbar)
9. [ ] Simplify workflows to only use remaining jobs
10. [ ] Validate config: `circleci config validate`
11. [ ] Run `./scripts/run-backend-tests`
12. [ ] Commit: `trim: simplify CircleCI to essentials`

## Commit Message Template

```
trim: simplify CircleCI to essentials

- Remove GCP container and asset push jobs
- Remove deploy-lock and deploy jobs
- Remove terraform validation from static-checks
- Remove deployment notification steps
- Keep: static-checks, build-parser, build-backend, build-cli,
  publish-github-release, publish-vs-code-extension

Simplified CI to only build/test/release - no cloud deployment.
```

## Notes

- Keep the CircleCI extension in VS Code for now (useful for config validation)
- May also want to remove `.circleci/gcp-workload-identity-config.json` if no GCP auth needed
- The base docker image reference may need updating if we change the Dockerfile significantly
