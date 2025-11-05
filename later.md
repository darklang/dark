# Later.md - Deferred work

## Package Renaming / Moving (`mv` command)

Allow renaming/moving package items while preserving history. Create new SetXName op with same ID but new location.

Examples:
```
mv fn /Darklang.Foo.oldName /Darklang.Bar.newName
mv fn helper utils.helper  # relative paths
```

---

## Package History Command

Decide on final CLI UX - show package_ops history? Just name reassignments? Both?

Backend exists (`scmGetNameHistory` in `BuiltinPM/Libs/PackageHistory.fs`).

---

## Package Removal / Deprecation (`rm` command)

Remove/deprecate package items by removing name binding while preserving in history.

Open questions:
- Use RemoveX ops or SetXName with null location?
- How to represent "unnamed" items in package manager?
- Should removed items be queryable/restorable?

---

## LSP Server: Multi-Client Support

Track state (initialization, documentsInScope, branchID) per client instead of single global client.

Probably part of larger effort to move from stdio-based to HTTP-bound LSP server.

---

## Optimize Package Tree Queries

Uses `AllDescendants` search, filters in Darklang to extract immediate children. Won't scale to millions of packages.

Options:
1. SQL-based extraction using `substr`/`instr` to return only immediate child names
2. Denormalized `module_hierarchy` table populated on package ops
3. Caching layer with TTL/invalidation
4. Dedicated `getImmediateChildren(owner, modulePath)` API

Recommend: Start with #4, add #2 if needed.

---

## Scripts in VSCode Extension

If we want scripts accessible in VSCode: create dedicated tree view/panel separate from Packages panel.

---
