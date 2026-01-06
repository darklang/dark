# Kill Unused NuGet Dependencies

**Status**: [ ] Not started

## Dependencies to Remove

From `backend/paket.dependencies`:

| Package | Reason to Remove |
|---------|-----------------|
| `Rollbar` | Error tracking for prod |
| `Rollbar.NetCore.AspNet` | Error tracking for prod |
| `LaunchDarkly.ServerSdk` | Feature flags for prod |
| `PusherServer` | Real-time notifications (LibClientTypes removed) |
| `Google.Cloud.PubSub.V1` | Already removed with GCloud |
| `Honeycomb.OpenTelemetry` | Already removed with OTEL |
| `OpenTelemetry.*` | Already removed with OTEL |

## Already Removed in Other Tasks

- `Microsoft.AspNetCore.Components.WebAssembly` (WASM task)
- `Google.Cloud.PubSub.V1` (GCloud task)
- `OpenTelemetry.*` and `Honeycomb.*` (OTEL task)

## Remaining to Remove

```
// Services - REMOVE
nuget PusherServer = 5.0.0
nuget Rollbar = 5.2.0
nuget Rollbar.NetCore.AspNet = 5.2.0
nuget LaunchDarkly.ServerSdk = 8.0.0
```

## F# Code to Update

Search for usage:
```bash
grep -r "Rollbar\|LaunchDarkly\|Pusher" --include="*.fs" backend/src/
```

Likely need to:
- Remove or stub Rollbar error reporting
- Remove or stub LaunchDarkly feature flag checks
- Remove Pusher notification code (if not already removed with LibClientTypes)

## Steps

1. [ ] Remove PusherServer from `backend/paket.dependencies`
2. [ ] Remove Rollbar packages from `backend/paket.dependencies`
3. [ ] Remove LaunchDarkly from `backend/paket.dependencies`
4. [ ] Run `paket install` to update lock file
5. [ ] Search for Rollbar usage in F# code and remove/stub
6. [ ] Search for LaunchDarkly usage and remove/stub
7. [ ] Search for Pusher usage and remove (if remaining)
8. [ ] Update any paket.references files in projects
9. [ ] Run `./scripts/run-backend-tests`
10. [ ] Wait for build
11. [ ] Commit: `trim: remove Rollbar, LaunchDarkly, Pusher dependencies`

## Commit Message Template

```
trim: remove Rollbar, LaunchDarkly, Pusher dependencies

- Remove PusherServer NuGet package
- Remove Rollbar and Rollbar.NetCore.AspNet packages
- Remove LaunchDarkly.ServerSdk package
- Remove/stub related F# code

These services were for production monitoring and feature flags.
Not needed for local development.
```

## Notes

- This may require significant F# code changes
- Rollbar might be integrated into error handling - need to stub gracefully
- LaunchDarkly might wrap feature checks - ensure features still work
- Be careful about removing code vs stubbing - don't break functionality
