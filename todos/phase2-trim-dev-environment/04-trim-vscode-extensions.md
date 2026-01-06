# Trim VS Code Extensions

**Status**: [ ] Not started

## Current Extensions

From `.devcontainer/devcontainer.json`:
```json
"extensions": [
  "ionide.ionide-fsharp",      // KEEP - F# support
  "esbenp.prettier-vscode",    // KEEP - formatting
  "ms-python.python",          // KEEP - Python scripts
  "ms-azuretools.vscode-docker", // REMOVE - no Docker builds
  "editorconfig.editorconfig", // KEEP - editor config
  "hashicorp.terraform",       // REMOVE - no terraform
  "CircleCI.circleci"          // REVIEW - useful for config validation?
]
```

## Extensions to Remove

| Extension | Reason |
|-----------|--------|
| `hashicorp.terraform` | No terraform config anymore |
| `ms-azuretools.vscode-docker` | Not building containers |
| `CircleCI.circleci` | Optional - keep if useful for validating config |

## Extensions to Keep

| Extension | Reason |
|-----------|--------|
| `ionide.ionide-fsharp` | Core F# development |
| `esbenp.prettier-vscode` | Code formatting |
| `ms-python.python` | Python scripts |
| `editorconfig.editorconfig` | Editor configuration |

## Steps

1. [ ] Edit `.devcontainer/devcontainer.json`
2. [ ] Remove `hashicorp.terraform` from extensions list
3. [ ] Remove `ms-azuretools.vscode-docker` from extensions list
4. [ ] Decide on `CircleCI.circleci` - remove if not useful
5. [ ] Run `./scripts/run-backend-tests`
6. [ ] Commit: `trim: remove unused VS Code extensions`

## Commit Message Template

```
trim: remove unused VS Code extensions

- Remove hashicorp.terraform (no terraform config)
- Remove ms-azuretools.vscode-docker (no container builds)
- [Remove CircleCI.circleci if decided]

Keep essential extensions: F#, Python, Prettier, EditorConfig
```

## Notes

- Changes require rebuilding dev container to take effect
- Could add extensions back later if needed
- CircleCI extension provides config validation which might still be useful
