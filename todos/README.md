# Codebase Trimming Sprint - Task Tracker

This directory contains the task breakdown for trimming the Darklang codebase. Each checkbox represents a discrete, committable unit of work.

## How to Use This System

1. **Find the next unchecked item**: Go through phases in order. Within each phase, work top-to-bottom.
2. **Complete the task**: Follow the instructions in the specific todo file.
3. **Test**: Run `./scripts/run-backend-tests` - must pass before committing.
4. **Commit**: One commit per checkbox. Use format: `trim: [brief description]`
5. **Mark complete**: Change `[ ]` to `[x]` in the todo file.
6. **Commit the checkbox update**: Include it in the same commit or make a separate "progress" commit.

## Current Progress

### Phase 1: Remove Product Features
- [ ] [01-delete-prodexec.md](phase1-remove-product-features/01-delete-prodexec.md)
- [ ] [02-delete-queueworker.md](phase1-remove-product-features/02-delete-queueworker.md)
- [ ] [03-delete-cronchecker.md](phase1-remove-product-features/03-delete-cronchecker.md)
- [ ] [04-trim-builtin-dark-internal.md](phase1-remove-product-features/04-trim-builtin-dark-internal.md)
- [ ] [05-delete-libclienttypes.md](phase1-remove-product-features/05-delete-libclienttypes.md)
- [ ] [06-delete-wasm.md](phase1-remove-product-features/06-delete-wasm.md)
- [ ] [07-clean-backend-static.md](phase1-remove-product-features/07-clean-backend-static.md)

### Phase 2: Trim Dev Environment
- [ ] [01-remove-deployment-terraform.md](phase2-trim-dev-environment/01-remove-deployment-terraform.md)
- [ ] [02-trim-circleci.md](phase2-trim-dev-environment/02-trim-circleci.md)
- [ ] [03-kill-docker-ce.md](phase2-trim-dev-environment/03-kill-docker-ce.md)
- [ ] [04-trim-vscode-extensions.md](phase2-trim-dev-environment/04-trim-vscode-extensions.md)
- [ ] [05-kill-second-instance.md](phase2-trim-dev-environment/05-kill-second-instance.md)
- [ ] [06-kill-honeycomb-otel.md](phase2-trim-dev-environment/06-kill-honeycomb-otel.md)
- [ ] [07-kill-prod-containers.md](phase2-trim-dev-environment/07-kill-prod-containers.md)
- [ ] [08-delete-datatests.md](phase2-trim-dev-environment/08-delete-datatests.md)
- [ ] [09-kill-gcloud-pubsub.md](phase2-trim-dev-environment/09-kill-gcloud-pubsub.md)
- [ ] [10-kill-postgres-yugabyte.md](phase2-trim-dev-environment/10-kill-postgres-yugabyte.md)
- [ ] [11-kill-java.md](phase2-trim-dev-environment/11-kill-java.md)
- [ ] [12-kill-terraform-tooling.md](phase2-trim-dev-environment/12-kill-terraform-tooling.md)
- [ ] [13-add-dockerignore.md](phase2-trim-dev-environment/13-add-dockerignore.md)
- [ ] [14-trim-apt-installs.md](phase2-trim-dev-environment/14-trim-apt-installs.md)
- [ ] [15-kill-nuget-deps.md](phase2-trim-dev-environment/15-kill-nuget-deps.md)
- [ ] [16-kill-chisel.md](phase2-trim-dev-environment/16-kill-chisel.md)
- [ ] [final/01-clean-ports.md](phase2-trim-dev-environment/final/01-clean-ports.md)
- [ ] [final/02-clean-scripts.md](phase2-trim-dev-environment/final/02-clean-scripts.md)
- [ ] [final/03-clean-docs.md](phase2-trim-dev-environment/final/03-clean-docs.md)

### Phase 2.5: Dev Cycle Analysis
- [ ] [01-analyze-compile-scripts.md](phase2.5-dev-cycle-analysis/01-analyze-compile-scripts.md)

## Notes

- **Testing is mandatory**: Never commit without `./scripts/run-backend-tests` passing.
- **Wait for builds**: After F# changes, check `rundir/logs/build-server.log` for build completion.
- **One thing at a time**: Complete one checkbox fully before moving to the next.
- **Ask when uncertain**: If a removal has unexpected consequences, investigate before proceeding.

## Phases Overview

| Phase | Goal | Estimated Items |
|-------|------|-----------------|
| 1 | Remove unused product features (ProdExec, QueueWorker, etc.) | 7 |
| 2 | Trim dev environment (terraform, docker, cloud deps) | 19 |
| 2.5 | Analyze and optimize dev cycles | 1 |

---

*Last updated: Session start. Update this when phases are completed.*
