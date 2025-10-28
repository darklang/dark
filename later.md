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

**Status**: Needs consolidation based on recent package_ops changes

**Intent**: Show history of changes to packages, possibly filtering by branch, time range, or item.

**Next steps**: Review how this can be implemented using the package_ops table and determine the right UX.

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
