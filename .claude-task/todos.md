# Upgrade to .NET 10

## Overview
Upgrading from .NET 8.0.303 to .NET 10.0.102 (LTS, released Nov 2025).
All 24 F# projects currently use `net8.0` target framework.

## Implementation Tasks

### 1. Docker and Installation Infrastructure
- [ ] Update Dockerfile to use .NET 10.0.102
  - Change install-dotnet8 script call to use new version and checksums
  - Update .NET dependencies comments if needed
- [ ] Rename and update install script
  - Rename `scripts/installers/install-dotnet8` to `install-dotnet10`
  - Update version to 10.0.102
  - Update SHA512 checksums (x64: 7adf40e8e554..., arm64: 1254141153d2...)
- [ ] Update Dockerfile reference to call install-dotnet10

### 2. .NET SDK Configuration
- [ ] Update `backend/global.json`
  - Change SDK version from 8.0.303 to 10.0.102
  - Keep rollForward: disable

### 3. Project Files - Update all .fsproj files (24 files)
- [ ] Update backend/src projects (21 files):
  - Change `<TargetFramework>net8.0</TargetFramework>` to `<TargetFramework>net10.0</TargetFramework>`
  - Projects: BwdServer, Cli, BuiltinCli, BuiltinCliHost, BuiltinCloudExecution, BuiltinDarkInternal, BuiltinExecution, BuiltinPM, DvalReprDeveloper, LibBinarySerialization, LibCloud, LibCloudExecution, LibConfig, LibDB, LibExecution, LibHttpMiddleware, LibPackageManager, LibParser, LibService, LibTreeSitter, LocalExec, Prelude
- [ ] Update backend/tests projects (2 files):
  - TestUtils, Tests

### 4. Paket Dependencies
- [ ] Update `backend/paket.lock`
  - Change RESTRICTION from `== net8.0` to `== net10.0`
  - Update dependency versions if needed (especially Microsoft.Extensions.* packages which are already on 10.0.1)

### 5. Build and Verification
- [ ] Monitor build-server.log for successful compilation
  - Wait for automatic rebuild to complete
  - Check for any compatibility issues or warnings
- [ ] Monitor packages-canvas.log for package reload
  - Verify packages load successfully
- [ ] Run tests to verify functionality
  - Execute test suite
  - Verify all tests pass

### 6. Documentation
- [ ] Update any documentation that mentions .NET version
  - Check README files if they reference .NET 8
  - Update development setup docs if needed

## Notes
- .NET 10 is LTS (Long-Term Support), supported until Nov 2028
- F# 10.0 and C# 14.0 are included
- Build system will automatically rebuild when files change
- No manual rebuild or package reload needed

## Resources
- [.NET 10 Download Page](https://dotnet.microsoft.com/en-us/download/dotnet/10.0)
- [.NET 10 Release Notes](https://github.com/dotnet/core/blob/main/release-notes/10.0/README.md)
- [Announcing .NET 10 Blog Post](https://devblogs.microsoft.com/dotnet/announcing-dotnet-10/)
