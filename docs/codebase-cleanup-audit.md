# Codebase Cleanup Audit Report

**Date**: 2026-01-06
**Scope**: Review for CLI and VS Code extension focus only
**Status**: Post-trimming sprint audit

---

## Executive Summary

The trimming sprint successfully removed most production infrastructure (ProdExec, QueueWorker, CronChecker, Honeycomb, Rollbar, LaunchDarkly, PubSub, Terraform, etc.). However, this audit found:

- **3 major backend projects** that could be removed for CLI-only builds
- **12+ stale documentation/config references** to deleted features
- **~10 unused Dockerfile dependencies** adding bloat
- **Several scripts** with dead code or stale references

---

## Part 1: Backend Code - Potential Removals

### High Priority - Remove Entirely (CLI doesn't need these)

| Project | Location | Size | Reason |
|---------|----------|------|--------|
| **BwdServer** | `backend/src/BwdServer/` | 14KB | HTTP server for cloud handlers - CLI uses LocalExec |
| **LibHttpMiddleware** | `backend/src/LibHttpMiddleware/` | 1 file | Only used by BwdServer |
| **BuiltinCloudExecution** | `backend/src/BuiltinCloudExecution/` | 2 files | `emit` is now a no-op stub; DB.fs is cloud-only |

### Medium Priority - Refactor/Audit

| Project | Issue | Action |
|---------|-------|--------|
| **LibCloud** | Contains both needed (Canvas, Account) and unneeded (Tracing, SqlCompiler, Stats) code | Extract only what LocalExec needs |
| **LibService** | Web server infrastructure (Kestrel, Kubernetes, HSTS) | Only needed for LocalExec server mode |
| **BuiltinDarkInternal** | Mix of CLI-useful and cloud-only functions | Audit which functions are actually called |

### Dead Code in Active Files

| File | Line(s) | Issue |
|------|---------|-------|
| `backend/src/Prelude/NonBlockingConsole.fs` | 18, 44 | WASM checks (`isWasm`) - WASM removed |
| `backend/src/Prelude/Exception.fs` | 37 | `sendRollbarError` stub remains |
| `backend/src/Cli/Cli.fs` | 88 | Commented Rollbar code |
| `backend/src/LocalExec/Builtins.fs` | 20 | TODO comment: "do we need this?" |
| `backend/src/LibCloud/SqlCompiler.fs` | 32-45, 140-148, 446 | Commented PostgreSQL/Npgsql code |

### Test Files Dependent on Removable Projects

If BwdServer is removed, these tests become orphaned:
- `backend/tests/Tests/BwdServer.Tests.fs`
- `backend/tests/Tests/Routing.Tests.fs`
- `backend/tests/Tests/SqlCompiler.Tests.fs`

---

## Part 2: Stale References to Deleted Features

### Properly Cleaned (No Action Needed)

| Feature | Status |
|---------|--------|
| ProdExec | Completely removed |
| QueueWorker | Removed, `emit` properly stubbed as no-op |
| CronChecker | Completely removed |
| Honeycomb/OpenTelemetry | Completely removed |
| Pusher | Completely removed |
| PubSub/Google Cloud | Completely removed |
| WASM/Blazor directories | Completely removed |

### Incomplete Cleanup (Action Needed)

| File | Line(s) | Issue |
|------|---------|-------|
| `.github/dependabot.yml` | 22-26 | References deleted `tf/` Terraform directory |
| `CONTRIBUTING.md` | 89-95 | References deleted `containers/` directory |
| `config/production` | 1 | References deleted `tf/service_env_vars.tf` |
| `scripts/run-backend-tests` | 56, 87 | Sets `DARK_CONFIG_ROLLBAR_ENABLED=n` and `DARK_CONFIG_LAUNCHDARKLY_SDK_API_KEY=none` - features deleted |
| `scripts/devcontainer/_setup-hosts` | 5-9 | `dark-editor.dlio.localhost`, `dark-repl.dlio.localhost` - web UI removed |

---

## Part 3: Scripts and Config Cleanup

### Scripts with Dead Code

| File | Line(s) | Issue |
|------|---------|-------|
| `scripts/build/compile` | 77-89 | Commented WASM copy code |
| `scripts/build/compile` | 411 | Ignores `containers/` directory (deleted) |
| `scripts/build/build-parser` | 72-74 | Commented WASM compilation |
| `scripts/builder` | 74-77 | `--prodclone` flag - no `config/dev_prodclone` exists |
| `.circleci/config.yml` | 197-202 | Commented integration test code |

### Scripts Potentially Removable

| File | Reason |
|------|--------|
| `scripts/contributors/checkout-pull-request` | Developer convenience, not in core workflow |
| `scripts/linting/_check-linked-libs` | No references found |
| `scripts/devcontainer/_allow-docker-access` | Docker-in-Docker not needed for CLI |
| `scripts/devcontainer/chrome-seccomp.json` | Browser security profile - web UI removed |

### Config Files to Update/Remove

| File | Action |
|------|--------|
| `config/production` | DELETE - points to non-existent `tf/` |

---

## Part 4: Dockerfile Bloat

### Unused APT Packages

| Package | Line | Reason Unused |
|---------|------|---------------|
| `rsync` | 86 | No cross-machine sync needed |
| `vim` | 96 | Editor preference, not automated |
| `pv` | 105 | Pipe viewer not used in scripts |
| `htop` | 106 | Manual monitoring only |
| `dnsutils` | 109 | Never called in scripts |
| `bash-completion` | 108 | Interactive only |

### Unused Python Packages (line 181)

| Package | Status |
|---------|--------|
| `yq` | Not used |
| `watchfiles` | Not used |
| `yapf==0.40.1` | Not used |

### Unused NPM Packages

| Package | Line | Reason |
|---------|------|--------|
| `prettier@3.0.2` | 169 | No JS/CSS files to format |
| `@vscode/vsce` | 294 | Could move to CI-only |

---

## Part 5: Documentation Issues

### Stale Documentation

| File | Issue |
|------|-------|
| `CONTRIBUTING.md` lines 89-95 | "Production Services" section references deleted `containers/` |
| `docs/dnsmasq.md` | Still valid but references `dlio.localhost` subdomains |

### Missing Documentation

- No documentation for the trimmed CLI-focused architecture
- `docs/release.md` may need updates for new release process

---

## Recommended Actions

### Phase 1: Quick Fixes (Low Risk)

1. **Delete** `config/production` (1 line pointing to deleted tf/)
2. **Delete** `.github/dependabot.yml` lines 22-26 (Terraform section)
3. **Update** `CONTRIBUTING.md` to remove "Production Services" section
4. **Remove** commented WASM code from `scripts/build/compile` and `build-parser`
5. **Remove** unused Dockerfile packages: `rsync vim pv htop dnsutils bash-completion`
6. **Remove** unused Python packages: `yq watchfiles yapf`
7. **Remove** `prettier` npm package (no JS/CSS files)

### Phase 2: Medium Effort (Moderate Risk)

1. **Delete** `scripts/devcontainer/chrome-seccomp.json` and update references
2. **Simplify** `scripts/devcontainer/_setup-hosts` (remove editor/repl hosts)
3. **Remove** `--prodclone` support from `scripts/builder`
4. **Clean up** Rollbar/LaunchDarkly env var overrides in scripts

### Phase 3: Significant Work (Higher Risk)

1. **Remove BwdServer project** and dependent tests
2. **Remove LibHttpMiddleware project**
3. **Audit BuiltinCloudExecution** - potentially remove entirely
4. **Refactor LibCloud** to extract only what LocalExec needs
5. **Remove WASM checks** from `NonBlockingConsole.fs`

---

## Impact Assessment

### If All Recommendations Implemented

| Metric | Before | After (Est.) |
|--------|--------|--------------|
| Backend projects | 15+ | ~10 |
| Dockerfile apt packages | 25+ | ~18 |
| Lines of F# code | ~50K | ~40K |
| Docker image size | ~2GB | ~1.5GB |
| Build time | ~60s | ~45s |

### Risk Assessment

| Phase | Risk Level | Reason |
|-------|------------|--------|
| Phase 1 | Very Low | Config/docs only, no code changes |
| Phase 2 | Low | Script cleanup, well-isolated |
| Phase 3 | Medium | Backend refactoring requires careful testing |

---

## Verification Checklist

Before implementing any removal, verify:

- [ ] `./scripts/run-backend-tests` passes
- [ ] CLI builds successfully: `./scripts/build/build-release-cli-exes.sh`
- [ ] VS Code extension builds: `./scripts/package-extension.sh`
- [ ] LocalExec works: `./scripts/run-local-exec --help`

---

## Appendix: Files Safe to Keep

These were audited and are actively used:
- All `scripts/build/` scripts (core build system)
- All `scripts/formatting/` scripts
- All `scripts/linting/` scripts (except `_check-linked-libs`)
- `config/dev` and `config/circleci`
- `.vscode/` configuration
- `tree-sitter-darklang/` (parser is used by CLI)
- `vscode-extension/` (in scope)
- `packages/` (Darklang standard library)
