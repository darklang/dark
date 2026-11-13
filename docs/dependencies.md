# Dependencies

Every NuGet package and .NET tool the backend uses is pinned to an exact version, and
comes only from nuget.org.

- **Packages** use NuGet central package management. `backend/Directory.Packages.props`
  has one `PackageVersion` per package, transitive packages included, and each `.fsproj`
  names what it uses with a versionless `PackageReference`. Transitive pinning is on, so a
  `PackageVersion` for a package nobody names directly sets its version too: NuGet otherwise
  takes the lowest version a dependency allows.
- **Locks:** each project has a `packages.lock.json` with its whole graph and a hash per
  package. `backend/src/Cli`'s covers every runtime the release publishes
  (`RuntimeIdentifiers` in `Cli.fsproj`). All of them are checked in.
- **FSharp.Core** is pinned like any other package (`DisableImplicitFSharpCoreReference`),
  not picked by the SDK.
- **Tools:** fantomas and dotnet-trace are installed in the `Dockerfile`, each with
  `--version`.
- **The SDK** is `backend/global.json`, with `rollForward: disable`, and the `Dockerfile`
  installs that version.
- **Sources:** `backend/NuGet.Config` clears every other feed and maps all packages to
  nuget.org, so a feed configured elsewhere on a machine can't supply one.

## Adding or bumping a package

1. Add or change its `PackageVersion` in `backend/Directory.Packages.props`. For a new
   package, also add `<PackageReference Include="Name" />` to the project that uses it.
2. Run `scripts/dev/build`. The restore rewrites the `packages.lock.json` files.
3. Commit `Directory.Packages.props` and the lock files together.

## What CI checks

- CI restores in locked mode (`RestoreLockedMode` is on when `CI=true`): if a lock
  disagrees with `Directory.Packages.props` or a project's references, the job fails
  rather than resolving again.
- A trimmed or AOT publish writes its lock under `backend/Build` instead, unchecked: it
  adds the SDK's ILLink and ILCompiler packages for the build host's runtime, so its graph
  differs per host. Their versions come with the pinned SDK.
- NU1603 is an error: if a pinned version can't be found, NuGet won't substitute the next
  one up.

## Updates and advisories

Dependabot reads `Directory.Packages.props` and opens PRs for packages and the SDK. Restore
runs NuGet's audit and prints NU1901 to NU1904 for known advisories, transitive packages
included. To list them, and packages nuget.org marks deprecated, from `backend/` in the dev
container:

    dotnet list fsdark.sln package --vulnerable --include-transitive
    dotnet list fsdark.sln package --deprecated --include-transitive
