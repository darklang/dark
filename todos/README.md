# Task Tracking System

This directory is used for organizing and tracking discrete units of work during focused development sprints.

## How to Use This System

1. **Create a phase directory**: `phase1-description/`, `phase2-description/`, etc.
2. **Create task files**: Each `.md` file represents one committable unit of work
3. **Work top-to-bottom**: Go through phases in order, tasks within each phase sequentially
4. **Test before commit**: Run `./scripts/run-backend-tests` - must pass
5. **Commit format**: `trim: [brief description]` or `feat: [description]`
6. **Mark complete**: Change `[ ]` to `[x]` in the task file
7. **Track in README**: Keep a progress list with checkboxes here

## Task File Template

```markdown
# Task Name

**Status**: [ ] Not started

## What This Task Does

Brief description of the goal.

## Steps

1. [ ] First step
2. [ ] Second step
3. [ ] Run tests
4. [ ] Commit

## Search Commands

```bash
# Commands to find relevant code
grep -r "pattern" --include="*.fs" backend/
```

## Commit Message Template

```
type: brief description

- Detail 1
- Detail 2
```

## Notes

- Any gotchas or considerations
```

## Completed Sprints

### Codebase Trimming Sprint (Jan 2026)

Removed production infrastructure to focus on CLI and VS Code extension:

- **Phase 1** (7 tasks): Removed ProdExec, QueueWorker, CronChecker, WASM, LibClientTypes
- **Phase 2** (19 tasks): Removed Terraform, Docker CE, Honeycomb, PubSub, PostgreSQL, Java, etc.
- **Phase 2.5** (1 task): Analyzed build system for optimization opportunities

See `docs/codebase-cleanup-audit.md` for follow-up opportunities identified.

---

*Keep task files until sprint is complete, then archive or delete them.*
