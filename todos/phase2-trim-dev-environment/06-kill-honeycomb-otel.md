# Kill Honeycomb and OpenTelemetry

**Status**: [ ] Not started

## What's Being Removed?

Observability infrastructure:
- **Honeycomb**: Cloud observability platform for tracing
- **OpenTelemetry**: Instrumentation and telemetry export
- **Honeymarker**: CLI tool for marking deployments

Not needed for local development.

## Dockerfile Changes

Remove honeymarker installation:
```dockerfile
# DELETE this section
####################################
# Honeymarker installs
####################################

RUN /home/dark/install-exe-file \
  --arm64-sha256=... \
  --amd64-sha256=... \
  --url=https://github.com/honeycombio/honeymarker/releases/download/v0.2.10/honeymarker-linux-${TARGETARCH} \
  --target=/usr/bin/honeymarker
```

## NuGet Dependencies to Remove

In `backend/paket.dependencies`, remove:
```
// Tracing - DELETE ALL
nuget Honeycomb.OpenTelemetry = 1.3.1
nuget System.Diagnostics.DiagnosticSource = 8.0.0
nuget OpenTelemetry = 1.9.0
nuget OpenTelemetry.Exporter.OpenTelemetryProtocol = 1.9.0
nuget OpenTelemetry.Exporter.Console = 1.9.0
nuget OpenTelemetry.Instrumentation.AspNetCore = 1.8.1
nuget OpenTelemetry.Extensions.Hosting = 1.9.0
nuget OpenTelemetry.Instrumentation.Http = 1.8.1
```

## F# Code to Remove/Stub

Search for telemetry code:
```bash
grep -r "OpenTelemetry\|Honeycomb\|Telemetry" --include="*.fs" backend/
grep -r "DARK_CONFIG_TELEMETRY" --include="*.fs" --include="*.sh" backend/ scripts/
```

Likely locations:
- LibCloud or LibService telemetry initialization
- BwdServer startup
- Environment variable handling

## Scripts to Modify

- `scripts/deployment/_notify-deployment-honeycomb` - DELETE
- Any script using `honeymarker`

## Steps

1. [ ] Remove honeymarker from `Dockerfile`
2. [ ] Remove OpenTelemetry/Honeycomb NuGet packages from `backend/paket.dependencies`
3. [ ] Run `paket install` to update lock file
4. [ ] Search for telemetry code in F# and remove/stub it
5. [ ] Delete `scripts/deployment/_notify-deployment-honeycomb`
6. [ ] Remove telemetry env vars from config if present
7. [ ] Run `./scripts/run-backend-tests`
8. [ ] Wait for build (significant change)
9. [ ] Commit: `trim: remove Honeycomb and OpenTelemetry`

## Commit Message Template

```
trim: remove Honeycomb and OpenTelemetry

- Remove honeymarker from Dockerfile
- Remove OpenTelemetry NuGet packages
- Remove Honeycomb NuGet package
- Stub/remove telemetry initialization code
- Delete deployment notification script

Observability infrastructure not needed for local development.
```

## Notes

- This is a significant change affecting multiple areas
- The `DARK_CONFIG_TELEMETRY_EXPORTER=none` env var may still be referenced - search and clean
- May need to stub out telemetry calls rather than remove all code if deeply integrated
