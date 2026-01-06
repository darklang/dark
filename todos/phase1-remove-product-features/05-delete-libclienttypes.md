# Delete LibClientTypes and LibClientTypesToCloudTypes

**Status**: [ ] Not started

## What are these?

- **LibClientTypes**: Defines payload types sent to the client via Pusher.com (NewTrace, New404)
- **LibClientTypesToCloudTypes**: Conversion layer between client types and cloud domain types

These are for real-time client notifications via Pusher - not needed for local-first CLI experience.

## Files to Delete

```
backend/src/LibClientTypes/
  ClientPusherTypes.fs
  LibClientTypes.fsproj
  README.md
  paket.references

backend/src/LibClientTypesToCloudTypes/
  Pusher.fs
  LibClientTypesToCloudTypes.fsproj
  README.md
  paket.references
```

## Other References to Remove

1. **Solution file**: Remove both from `backend/fsdark.sln`
2. **Project references**: Search for projects that reference these
3. **PusherServer NuGet**: Will be removed in Phase 2 (nuget deps task)

## Search Commands

```bash
grep -r "LibClientTypes\|LibClientTypesToCloudTypes" --include="*.fs" --include="*.fsproj" --include="*.sln" backend/
grep -r "Pusher\|ClientPusher" --include="*.fs" backend/
```

## Steps

1. [ ] Search for references to understand dependencies
2. [ ] Delete `backend/src/LibClientTypes/` directory
3. [ ] Delete `backend/src/LibClientTypesToCloudTypes/` directory
4. [ ] Remove both from `backend/fsdark.sln`
5. [ ] Remove project references from any .fsproj files that referenced them
6. [ ] Search for any Pusher-related code that may break
7. [ ] Run `./scripts/run-backend-tests`
8. [ ] Wait for build
9. [ ] Commit: `trim: delete LibClientTypes and LibClientTypesToCloudTypes`

## Commit Message Template

```
trim: delete LibClientTypes and LibClientTypesToCloudTypes

- Remove backend/src/LibClientTypes/ directory
- Remove backend/src/LibClientTypesToCloudTypes/ directory
- Remove from fsdark.sln
- Remove project references

These provided Pusher.com client notification types for real-time
updates. Not needed for local-first CLI architecture.
```

## Potential Complications

- LibCloud or BwdServer might reference Pusher types - may need to stub out or remove that code
- The PusherServer NuGet package will be removed in Phase 2
