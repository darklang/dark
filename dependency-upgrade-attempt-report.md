# Dependency Upgrade Attempt Report - 2026-01-28

## Summary
Attempted to upgrade from .NET 8 to .NET 10 (then 9) along with various package upgrades. The .NET framework upgrade failed due to Paket not correctly resolving package assemblies for newer target frameworks.

## What Went Well

### Successfully Upgraded (should keep in future attempt)
1. **Paket 9.0.2 → 10.3.1** - Required for .NET 10 support, worked correctly
2. **dotnet-trace 9.0.553101 → 9.0.661903** - Minor version bump, no issues
3. **Fantomas 6.2.3 → 7.0.3** - Should work fine

### Package Version Upgrades (validated as compatible)
- **NodaTime 3.2.2 → 3.3.0** - Safe upgrade
- **FSharpPlus 1.5.0 → 1.8.0** - Safe upgrade
- **SQLitePCLRaw.bundle_e_sqlite3** - Needs >= 2.1.11 for newer Microsoft.Data.Sqlite

### Tree-sitter Ecosystem (not tested, should be safe)
- nan: 2.17.0 → 2.23.0
- tree-sitter-cli: 0.20.8 → 0.26.3
- web-tree-sitter: 0.20.8 → 0.26.3
- tree-sitter-javascript: 0.19.0 → 0.23.2
- tree-sitter (Rust): 0.20.10 → 0.24
- cc (Rust): 1.0 → 1.2

### VSCode Extension (not tested, should be safe)
- TypeScript: 4.9.4 → 5.7.3
- ESLint: 8.26.0 → 8.57.1
- @typescript-eslint/*: 5.42.0 → 7.18.0
- @types/node: 16.11.7 → 22.10.7
- @types/mocha: 9.1.0 → 10.0.10
- vscode-languageclient: 7.0.0 → 9.0.1
- @types/vscode: 1.63.0 → 1.83.0

## What Failed

### .NET Framework Upgrade (net8.0 → net9.0/net10.0)
**Root Cause**: Paket generates `.paket.resolved` files listing the packages, but the build couldn't find the namespaces:
- `FSharp.Control.Tasks` (from Ply)
- `NodaTime`
- `FSharpPlus`
- `FSharpx`
- `FSharp.SystemTextJson` (`JsonFSharpConverter`)

**Symptoms**:
- Packages appeared in `paket.lock` with correct restriction (`== net9.0` or `== net10.0`)
- `.paket.resolved` files were generated with package references
- `dotnet restore` reported success
- Build failed with "namespace not defined" errors

**Likely Cause**: These packages only have netstandard2.0/netcoreapp3.x builds. Paket may not be correctly mapping these to net9.0/net10.0 compatible assemblies. See Paket issues:
- https://github.com/fsprojects/Paket/issues/4296 (.NET 10 support - merged)
- https://github.com/fsprojects/Paket/issues/4274 (transitive dependency gaps)

### Version Conflicts Encountered
1. **FSharp.Core**: FSharp.Compiler.Service 43.10.101 requires FSharp.Core 10.0.101 (not 10.0.102)
2. **SQLitePCLRaw.bundle_e_sqlite3**: Microsoft.Data.Sqlite 10.0.2 requires >= 2.1.11 (not 2.1.10)

## Recommendations for Next Attempt

1. **Wait for Paket fixes** - The .NET 10 support is recent (Jan 2026). May need updates for better netstandard→net10 resolution.

2. **Test incrementally**:
   - First: Upgrade Paket, tree-sitter, VSCode extension packages (no framework change)
   - Second: Try net9.0 with minimal package changes
   - Third: Try net10.0 once net9.0 works

3. **Consider alternatives to Ply** - It's from 2019 and unmaintained. F# 6+ has native `task { }` support.

4. **File Paket issue** if the problem persists - the resolved files are correct but assemblies aren't loaded.

## Files Modified (to revert)
- `backend/paket.dependencies`
- `backend/paket.lock`
- `backend/global.json`
- `backend/.config/dotnet-tools.json`
- `Dockerfile`
- `scripts/installers/install-dotnet9` (new file, delete)
- `scripts/installers/install-dotnet10` (new file, delete)
- All `*.fsproj` files (TargetFramework)
- Various scripts (net8.0 paths)
- `.circleci/config.yml`
- `tree-sitter-darklang/package.json`
- `tree-sitter-darklang/Cargo.toml`
- `vscode-extension/package.json`
- `vscode-extension/client/package.json`
