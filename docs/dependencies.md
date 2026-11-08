# Dependencies

Every NuGet package and .NET tool the backend uses is pinned to an exact version, and
comes only from nuget.org.

- **Packages** are managed by Paket. `backend/paket.dependencies` names each direct
  dependency with `=`, an exact version. `backend/paket.lock` records the whole graph,
  transitive packages included. Both are checked in.
- **Tools** are in `backend/.config/dotnet-tools.json` (paket itself). fantomas and
  dotnet-trace are installed in the `Dockerfile`, each with `--version`.
- **The SDK** is `backend/global.json`, with `rollForward: disable`, and the `Dockerfile`
  installs that version.
- **Sources:** `backend/NuGet.Config` clears every other feed and maps all packages to
  nuget.org, so a feed configured elsewhere on a machine can't supply one.

## Bumping a package

1. Change its version in `backend/paket.dependencies`, or add a line for a new package
   and name it in the project's `paket.references`.
2. Run `scripts/dev/build`. It sees the change to `paket.dependencies` and runs
   `paket install`, which rewrites `paket.lock`, then clears Paket's restore caches. A
   bare `paket install` leaves those caches, and the build keeps the old versions.
3. Commit `paket.dependencies` and `paket.lock` together.

To move a transitive package, add it to `paket.dependencies` with the version you want.

## What CI checks

- CI restores with `paket restore --fail-on-checks`: if `paket.lock` disagrees with
  `paket.dependencies` or a `paket.references` file, the job fails rather than restoring
  the old lock with a warning. Locally, the build runs `paket install` instead.
- NU1603 is an error: if a version the lock names can't be found, NuGet won't
  substitute the next one up.

## Updates and advisories

Dependabot opens PRs for the tool manifest and the SDK, but it can't read Paket, so
nothing tells you about a newer package.

A weekly GitHub workflow, `.github/workflows/dependencies.yml`, covers advisories. It
sends `paket.lock`'s resolved versions to GitHub's dependency graph
(`scripts/deployment/paket-snapshot`), so Dependabot raises alerts for them, though it
opens no fix PRs. It also fails when the `--vulnerable` list below is not empty, and puts
`paket outdated` in the run's summary.

Restore runs NuGet's audit and prints NU1901 to
NU1904 for known advisories, transitive packages included. To list them, and packages
nuget.org marks deprecated, from `backend/` in the dev container:

    dotnet list fsdark.sln package --vulnerable --include-transitive
    dotnet list fsdark.sln package --deprecated --include-transitive
