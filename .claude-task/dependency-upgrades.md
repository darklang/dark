# Darklang Dependency Upgrade Report

Generated: January 23, 2026

This report identifies available upgrades for dependencies across the Darklang monorepo.

## Summary

The codebase uses three main package ecosystems:
- **.NET/F# packages** managed via Paket
- **Node.js/NPM packages** for VSCode extension and tree-sitter tooling
- **Rust crates** for tree-sitter parser bindings

---

## 1. .NET/F# Dependencies (Paket)

### High Priority Upgrades

#### FSharp.Core
- **Current**: 8.0.101
- **Latest**: 10.0.102 (January 13, 2026)
- **Change**: Major version upgrade (2 major versions)
- **Impact**: Follows .NET 10 release. Likely includes new language features and performance improvements.
- **Risk**: HIGH - Major version changes may include breaking changes
- **Recommendation**: Review F# 9.0 and 10.0 release notes before upgrading

#### System.Text.Json
- **Current**: 8.0.5
- **Latest**: 10.0.2 (January 13, 2026)
- **Change**: Major version upgrade
- **Impact**: Part of .NET 10 release with performance and feature improvements
- **Risk**: MEDIUM - Breaking changes possible but Microsoft maintains good compatibility
- **Recommendation**: Test serialization/deserialization thoroughly after upgrade

#### FSharp.Compiler.Service
- **Current**: 43.8.101
- **Latest**: 43.10.101
- **Change**: Minor version upgrade (43.8 → 43.10)
- **Impact**: Includes improvements to F# compiler service APIs
- **Risk**: LOW-MEDIUM - Minor version should be compatible
- **Recommendation**: Safe to upgrade, test parser functionality

#### Microsoft.Data.Sqlite
- **Current**: 8.0.1
- **Latest**: 10.0.2 (January 13, 2026)
- **Change**: Major version upgrade
- **Impact**: Database provider updates, performance improvements
- **Risk**: MEDIUM - Test all database operations
- **Recommendation**: Verify database compatibility and test queries

### Medium Priority Upgrades

#### FSharpPlus
- **Current**: 1.5.0
- **Latest**: 1.8.0 (October 28, 2025)
- **Change**: Minor version upgrade (1.5 → 1.8)
- **Impact**: Additional functional programming utilities and abstractions
- **Risk**: LOW - Minor version, backwards compatible
- **Recommendation**: Safe to upgrade

#### NodaTime
- **Current**: 3.2.2
- **Latest**: 3.3.0
- **Change**: Patch version upgrade
- **Impact**: Bug fixes and minor improvements to date/time handling
- **Risk**: LOW - Patch version is safe
- **Recommendation**: Upgrade recommended

#### System.IO.Hashing
- **Current**: 8.0.0
- **Latest**: 10.0.x (likely available with .NET 10)
- **Change**: Major version upgrade
- **Impact**: Part of .NET 10 release
- **Risk**: LOW - Hashing APIs are stable
- **Recommendation**: Upgrade with other .NET 10 packages

#### Microsoft.Extensions.Diagnostics.HealthChecks
- **Current**: 8.0.0
- **Latest**: 10.0.x (likely available with .NET 10)
- **Change**: Major version upgrade
- **Impact**: Part of .NET 10 release
- **Risk**: LOW - Health check APIs are stable
- **Recommendation**: Upgrade with other .NET 10 packages

### Low Priority / No Update Needed

#### Ply
- **Current**: 0.3.1
- **Latest**: 0.3.1
- **Status**: Up to date ✓

#### FsRegEx
- **Current**: 0.7.2
- **Latest**: 0.7.2
- **Status**: Up to date ✓

#### FSharpx.Extras
- **Current**: 3.1.0
- **Latest**: Not checked (lower priority library)
- **Recommendation**: Check if needed

#### FSharp.SystemTextJson
- **Current**: 1.3.13
- **Latest**: Not extensively checked
- **Recommendation**: May need update when upgrading System.Text.Json

#### NodaTime.Serialization.SystemTextJson
- **Current**: 1.3.0
- **Latest**: Not extensively checked
- **Recommendation**: May need update when upgrading NodaTime/System.Text.Json

#### SQLitePCLRaw.bundle_e_sqlite3
- **Current**: 2.1.10
- **Latest**: Not extensively checked
- **Recommendation**: Coordinate with Microsoft.Data.Sqlite upgrade

#### Fumble
- **Current**: 1.0.3
- **Latest**: Not extensively checked
- **Risk**: LOW - Stable library

#### Expecto (Testing)
- **Current**: 10.1.0
- **Latest**: Not extensively checked
- **Recommendation**: Check for latest test framework improvements

#### NReco.Logging.File
- **Current**: 1.1.7
- **Latest**: Not extensively checked
- **Risk**: LOW - Logging library

#### SimpleBase
- **Current**: 4.0.0
- **Latest**: Not extensively checked
- **Risk**: LOW

#### Sodium.Core
- **Current**: 1.3.4
- **Latest**: Not extensively checked
- **Recommendation**: Check for security updates

#### Legivel
- **Current**: 0.4.6
- **Latest**: Not extensively checked
- **Risk**: LOW

---

## 2. Node.js/NPM Dependencies

### VSCode Extension (`vscode-extension/package.json`)

#### High Priority Upgrades

##### VSCode Engine
- **Current**: ^1.83.0 (October 2023)
- **Latest**: Significantly newer versions available
- **Impact**: Access to newer VSCode APIs and features
- **Risk**: MEDIUM - May require code changes for API compatibility
- **Recommendation**: Test extension thoroughly with newer VSCode versions

##### TypeScript
- **Current**: ^4.9.4 (November 2022)
- **Latest**: 5.9 stable (TypeScript 6.0 and 7.0 in development)
- **Change**: Major version upgrade (4.9 → 5.9)
- **Impact**: New language features, better type inference, performance improvements
- **Risk**: MEDIUM - Breaking changes in TS 5.x
- **Recommendation**: Upgrade to TypeScript 5.x, review migration guide
- **Note**: TypeScript 7.0 (native/Go rewrite) is in preview for 10x performance but may have ecosystem compatibility issues

##### @typescript-eslint/*
- **Current**: ^5.42.0 (October 2022)
- **Latest**: 8.53.0 (published days ago)
- **Change**: Major version upgrade (5.x → 8.x)
- **Impact**: New linting rules, better TypeScript 5.x support
- **Risk**: MEDIUM - May require eslint config updates
- **Recommendation**: Upgrade in conjunction with TypeScript upgrade

##### ESLint
- **Current**: ^8.26.0
- **Latest**: 9.39.2 stable, 10.0.0-rc.0 in release candidate
- **Change**: Major version upgrade available (8.x → 9.x or 10.x)
- **Impact**: New rules, plugin system improvements
- **Risk**: MEDIUM - Config format changes in v9+
- **Recommendation**: Upgrade to ESLint 9.x (10.x expected January 2026)

#### Medium Priority Upgrades

##### esbuild
- **Current**: ^0.27.2
- **Latest**: Still 0.27.x (published recently)
- **Status**: Relatively up to date
- **Recommendation**: Check for latest 0.27.x patch version

##### Mocha
- **Current**: ^11.1.0
- **Latest**: 11.x series
- **Status**: Likely up to date
- **Recommendation**: Check for latest 11.x patch version

##### @types/node
- **Current**: ^16.11.7
- **Latest**: Much newer versions for Node 18, 20, 22+
- **Change**: Major version upgrade (Node 16 → newer)
- **Impact**: Type definitions for newer Node.js APIs
- **Risk**: LOW - Only affects types
- **Recommendation**: Upgrade to match target Node version

##### @types/mocha
- **Current**: ^9.1.0
- **Latest**: Likely newer versions available
- **Risk**: LOW - Only affects types
- **Recommendation**: Upgrade to match Mocha version

### VSCode Extension Client (`vscode-extension/client/package.json`)

#### High Priority Upgrades

##### vscode-languageclient
- **Current**: ^7.0.0
- **Latest**: 9.0.1 (published ~2 years ago, ~January 2024)
- **Change**: Major version upgrade (7.0 → 9.0)
- **Impact**: Improved Language Server Protocol support, bug fixes
- **Risk**: MEDIUM - May require LSP implementation changes
- **Recommendation**: Review migration guide, test language server communication

##### @types/vscode
- **Current**: ^1.63.0
- **Latest**: Much newer versions available
- **Impact**: Type definitions for newer VSCode APIs
- **Risk**: LOW - Only affects types
- **Recommendation**: Upgrade to match VSCode engine version

### Tree-sitter NPM Packages (`tree-sitter-darklang/package.json`)

#### High Priority Upgrades

##### tree-sitter-cli
- **Current**: ^0.20.8
- **Latest**: 0.26.3 (published ~1 month ago)
- **Change**: Minor version upgrade (0.20 → 0.26)
- **Impact**: Improved parser generation, bug fixes, new features
- **Risk**: MEDIUM - Parser generation may produce different output
- **Recommendation**: Regenerate parser and test thoroughly
- **Important Note**: Modern tree-sitter uses Node-API instead of NAN

##### web-tree-sitter
- **Current**: ^0.20.8
- **Latest**: 0.26.3 (published ~10 days ago)
- **Change**: Minor version upgrade (0.20 → 0.26)
- **Impact**: Web WASM bindings improvements
- **Risk**: MEDIUM - Test web-based parsing
- **Recommendation**: Upgrade alongside tree-sitter-cli

##### nan
- **Current**: ^2.17.0
- **Latest**: 2.23.0
- **Change**: Patch version upgrade (2.17 → 2.23)
- **Impact**: Node addon abstraction improvements
- **Risk**: LOW - Backwards compatible
- **Recommendation**: Upgrade, but note that modern tree-sitter uses Node-API instead
- **Important**: Consider migrating away from NAN to Node-API (tree-sitter 0.22+)

##### tree-sitter-javascript
- **Current**: ^0.19.0
- **Latest**: Likely 0.21.x or newer
- **Risk**: LOW - Dev dependency
- **Recommendation**: Check for latest version

---

## 3. Rust Dependencies

### Tree-sitter Rust Bindings (`tree-sitter-darklang/Cargo.toml`)

#### High Priority Upgrades

##### tree-sitter
- **Current**: ~0.20.10
- **Latest**: 0.25.6 (per search results showing versions through 0.25.x)
- **Change**: Minor version upgrade (0.20 → 0.25)
- **Impact**: Parser API improvements, performance enhancements, bug fixes
- **Risk**: MEDIUM - Breaking changes possible between 0.20 and 0.25
- **Recommendation**: Review tree-sitter changelog, regenerate Rust bindings, test parser
- **Note**: There was a known versioning conflict issue between 0.20 and 0.21 that required grammar updates

#### Medium Priority Upgrades

##### cc (build dependency)
- **Current**: 1.0 (exact version not specified, just "1.0")
- **Latest**: 1.0.97+ (in the 1.0.x series)
- **Change**: Patch version upgrade within 1.0.x series
- **Impact**: C/C++ build improvements
- **Risk**: LOW - Patch versions are backwards compatible
- **Recommendation**: Specify latest 1.0.x version (e.g., "1.0.97")

---

## Upgrade Strategy Recommendations

### Phase 1: Low-Risk Updates (Do First)
1. **NodaTime**: 3.2.2 → 3.3.0 (patch version)
2. **FSharpPlus**: 1.5.0 → 1.8.0 (minor version)
3. **nan**: 2.17.0 → 2.23.0 (patch version, though consider deprecating)
4. **cc**: 1.0 → 1.0.97+ (patch version)
5. **@types packages**: Upgrade all type definition packages

### Phase 2: Tree-sitter Ecosystem (Coordinate Together)
1. **tree-sitter-cli**: 0.20.8 → 0.26.3
2. **web-tree-sitter**: 0.20.8 → 0.26.3
3. **tree-sitter (Rust)**: ~0.20.10 → 0.25.6
4. Regenerate parser grammar after CLI upgrade
5. Test parsing thoroughly
6. Consider migrating from NAN to Node-API

### Phase 3: TypeScript/JavaScript Tooling (Coordinate Together)
1. **TypeScript**: 4.9.4 → 5.9 (or latest 5.x)
2. **@typescript-eslint/***: 5.42.0 → 8.53.0
3. **ESLint**: 8.26.0 → 9.39.2 (or 10.0.0 when released)
4. Update eslint configuration for new versions
5. Fix any new linting errors
6. Update @types/node to match target Node version

### Phase 4: VSCode Extension (Test Thoroughly)
1. **vscode-languageclient**: 7.0.0 → 9.0.1
2. **VSCode engine**: Update minimum version requirement
3. **@types/vscode**: Match new engine version
4. Test all language server features
5. Test extension in multiple VSCode versions

### Phase 5: .NET 10 Migration (Coordinate Together - Breaking Changes)
This is the most significant upgrade and should be done carefully:
1. **Review .NET 9 and 10 release notes** for breaking changes
2. **FSharp.Core**: 8.0.101 → 10.0.102
3. **System.Text.Json**: 8.0.5 → 10.0.2
4. **Microsoft.Data.Sqlite**: 8.0.1 → 10.0.2
5. **FSharp.Compiler.Service**: 43.8.101 → 43.10.101
6. **System.IO.Hashing**: 8.0.0 → 10.0.x
7. **Microsoft.Extensions.Diagnostics.HealthChecks**: 8.0.0 → 10.0.x
8. Update framework in paket.dependencies: `framework: net10.0`
9. Run full test suite
10. Check for deprecation warnings
11. Test all F# language features
12. Test database operations thoroughly

### Phase 6: Review Remaining Packages
Check for updates to lower-priority packages:
- FSharp.SystemTextJson
- NodaTime.Serialization.SystemTextJson
- SQLitePCLRaw.bundle_e_sqlite3
- Fumble
- Expecto
- NReco.Logging.File
- SimpleBase
- Sodium.Core (security implications - check for updates)
- Legivel
- FSharpx.Extras
- esbuild (latest patch)
- Mocha (latest patch)
- tree-sitter-javascript

---

## Breaking Changes to Watch For

### .NET 10 Upgrade
- **F# language changes**: Review F# 9.0 and 10.0 what's new
- **System.Text.Json**: API changes, new serialization options
- **Database**: Connection string changes, API updates
- **Performance characteristics**: May affect existing code

### TypeScript 5.x
- **Stricter type checking**: May reveal existing type errors
- **Decorators**: Updated decorator support
- **Module resolution**: Changes to module resolution algorithms
- **Breaking changes**: Review TypeScript 5.0, 5.1, 5.2, 5.3, 5.4, 5.5 release notes

### ESLint 9.x/10.x
- **Flat config**: New configuration format (legacy eslintrc deprecated)
- **Rule changes**: Some rules changed or removed
- **Plugin system**: Updates to plugin architecture

### tree-sitter 0.25
- **API changes**: Review tree-sitter 0.21-0.25 changelogs
- **Grammar changes**: May need grammar updates
- **Node-API migration**: Move away from NAN to Node-API

### VSCode Language Client 9.x
- **LSP version**: Updated Language Server Protocol version
- **API changes**: Review vscode-languageclient 8.0 and 9.0 changes
- **Middleware updates**: Check custom middleware implementations

---

## Testing Checklist

After each upgrade phase, verify:

### .NET/F# Testing
- [ ] All unit tests pass
- [ ] Parser functionality works
- [ ] Database operations succeed
- [ ] Serialization/deserialization works
- [ ] Date/time handling correct
- [ ] CLI tools function
- [ ] Build and compile succeed

### TypeScript/VSCode Extension Testing
- [ ] Extension builds without errors
- [ ] Extension loads in VSCode
- [ ] Language server starts and responds
- [ ] Syntax highlighting works
- [ ] Code completion works
- [ ] Go to definition works
- [ ] Error diagnostics display
- [ ] All extension commands function
- [ ] No console errors

### Tree-sitter Testing
- [ ] Parser generates correctly
- [ ] Grammar parses test files
- [ ] No parser crashes
- [ ] Correct parse trees produced
- [ ] Tree-sitter tests pass
- [ ] Web parsing works (if used)

---

## Resources

### .NET/F#
- [F# What's New](https://learn.microsoft.com/en-us/dotnet/fsharp/whats-new/)
- [.NET 10 Release Notes](https://learn.microsoft.com/en-us/dotnet/core/whats-new/dotnet-10)
- [Paket Documentation](https://fsprojects.github.io/Paket/)

### TypeScript/JavaScript
- [TypeScript Release Notes](https://www.typescriptlang.org/docs/handbook/release-notes/overview.html)
- [typescript-eslint Migration Guide](https://typescript-eslint.io/users/releases/)
- [ESLint Migration Guide](https://eslint.org/docs/latest/use/migrate-to-9.0.0)

### Tree-sitter
- [Tree-sitter Changelog](https://github.com/tree-sitter/tree-sitter/releases)
- [Tree-sitter Documentation](https://tree-sitter.github.io/tree-sitter/)

### VSCode Extension
- [VSCode Extension API](https://code.visualstudio.com/api)
- [vscode-languageclient Changelog](https://github.com/microsoft/vscode-languageserver-node/blob/main/client/CHANGELOG.md)

---

## Conclusion

The Darklang monorepo has several dependencies that could be upgraded, with the most significant being:

1. **.NET 10 migration** - A major upgrade that requires careful planning and testing
2. **TypeScript 5.x upgrade** - Substantial improvement in type checking and features
3. **Tree-sitter 0.26 upgrade** - Improved parser with 6 minor versions of fixes and features
4. **VSCode tooling updates** - Better extension development experience

**Recommended approach**: Start with low-risk updates, then tackle tree-sitter, then TypeScript tooling, then VSCode extension updates, and finally the .NET 10 migration as a comprehensive effort.

**Timeline estimate**:
- Low-risk updates: 1-2 days
- Tree-sitter: 2-3 days
- TypeScript/tooling: 3-5 days
- VSCode extension: 2-3 days
- .NET 10 migration: 1-2 weeks (with thorough testing)

Total: Approximately 3-4 weeks for all upgrades with proper testing.
