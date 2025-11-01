# Later.md - Features and work deferred for future implementation

## Package Renaming / Moving (`mv` command)

**Status**: Stub implementation removed

**Intent**: Allow users to rename/move package items (functions, types, values) from one location to another while preserving history.

**Proposed flow**:
1. Parse source location to get owner/modules/name
2. Find current item ID at that location (query package_ops for latest SetXName)
3. Parse destination location (supports relative paths based on current directory)
4. Create new SetXName op with same ID but new location
5. Ask for confirmation: "Rename Darklang.Foo.old to Darklang.Bar.new? [y/n]"
6. If yes: Call Darklang.SCM.PackageOps.addToBranch with the op
7. Show success message

**Examples**:
```
# Absolute paths:
mv fn /Darklang.Foo.oldName /Darklang.Bar.newName

# Relative paths (if you're in Darklang.Foo):
mv fn helper utils.helper
mv type User Models.User
```

**Note**: This creates a new SetXName op, preserving history - the old name stays in history.

---

## Package History Command

**Status**: CLI command removed, but underlying functionality exists

**Intent**: Show the history of what a package name has pointed to over time (tracking when names were reassigned to different items).

**Current implementation**:
- Backend: `scmGetNameHistory` builtin in `BuiltinPM/Libs/PackageHistory.fs`
- Package wrapper: `Darklang.SCM.History.getNameHistory`
- Returns list of HistoryEntry with: timestamp, branchID, itemId, opType

**Removed CLI command usage**:
```
history Darklang.Stdlib.List.map

# Output showed:
# 2024-10-28 10:30:00 | main | SetFnName -> abc123...
# 2024-10-28 11:45:00 | branch def456... | SetFnName -> 789xyz...
```

**Next steps**:
- The underlying functionality is fully implemented and working
- Decide on final UX for the CLI history command
- Consider: Should it show package_ops history? Or just name reassignment history? Both?
- Consider adding filtering by branch, time range, or item type

---

## Package Removal / Deprecation (`rm` command)

**Status**: Stub removed - not implementing at this time

**Intent**: Allow users to remove/deprecate package items (functions, types, values) by removing their name binding while preserving the item in history.

**Proposed flow**:
1. Parse location to get owner/modules/name
2. Find current item ID at that location (query package_ops for latest SetXName)
3. Create RemoveX op with that ID (or possibly SetXName op with empty/null location to "unname" it)
4. Ask for confirmation: "Remove Darklang.Foo.bar? [y/n]"
5. If yes: Call Darklang.SCM.PackageOps.addToBranch with the op
6. Show success message

**Examples**:
```
# Absolute path:
rm fn /Darklang.Foo.deprecated

# Relative path (if you're in Darklang.Foo):
rm fn deprecated
rm type OldModel
```

**Note**: The item remains in history, but loses its current name binding. This is different from `mv` which reassigns the name.

**Open questions**:
- Do we need RemoveX ops, or should we use SetXName with null location?
- How do we represent "unnamed" items in the package manager?
- Should removed items be queryable/restorable?

---

## LSP Server Architecture: Multi-Client Support

**Status**: Current design is "one server, one client"

**Issue**: The LSP server currently assumes a single client connection. However, the dev server needs to operate on separate LSP clients separately, tracking state (initialization, documentsInScope, branchID) per client.

**Proposed change**: Refactor LspState to support multiple clients:
- Track state per client connection
- Maintain separate branchID context for each client
- Keep separate documentsInScope for each client
- Handle initialization state per client

**Note**: This is probably part of a larger effort to move from stdio-based LSP server to HTTP-bound one.

---
