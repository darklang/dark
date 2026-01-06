# Add .dockerignore

**Status**: [x] Complete

## Purpose

Add a `.dockerignore` file to speed up Docker builds by excluding unnecessary files from the build context.

## Suggested .dockerignore Content

```dockerignore
# Git
.git
.gitignore

# Build outputs
backend/Build/
rundir/

# Node modules (tree-sitter)
tree-sitter-darklang/node_modules/
tree-sitter-darklang/build/
node_modules/

# IDE
.vscode/
.idea/

# Documentation (not needed in container)
docs/

# Logs
*.log
rundir/logs/

# Test artifacts
*.test.db

# Local config
config/local

# CI (not needed in container)
.circleci/

# Todos (meta)
todos/
pre-prompt.md
```

## Steps

1. [ ] Create `.dockerignore` file with appropriate exclusions
2. [ ] Test that `docker build` still works
3. [ ] Verify container can run properly
4. [ ] Run `./scripts/run-backend-tests`
5. [ ] Commit: `trim: add .dockerignore for faster builds`

## Commit Message Template

```
trim: add .dockerignore for faster builds

- Exclude git, build outputs, node_modules
- Exclude docs, logs, test artifacts
- Exclude IDE and CI files

Reduces Docker build context size for faster container builds.
```

## Notes

- Be careful not to exclude files needed at container build time
- The `scripts/installers/` are needed and should NOT be ignored
- Test the build after adding to ensure nothing breaks
