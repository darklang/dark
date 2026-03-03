# Branch Cleanups: `hash-based-refs-finally`

- [ ] rename ContentHash to just Hash
  add a /// comment at `type Hash = ` to note that the type is intended to be used for hashes of the _structure_ of some other value, not relying on temporary things like the name, etc.
  'content hash' -> hash
  contentHash->hash
  look up every other use of 'content'
  make sure no 'id' bound to one of these
- [ ] **`StringSegment.fromDT` case mismatch** — `RuntimeTypesToDarkTypes.fs:400`
  writes `"Interpolated"` but line 406 matches `"Interpolation"`. Deserialization
  of interpolated string segments will always fail.
- [ ] **~180 lines of design-note comments in `ProgramTypes.fs`** — Lines 714-822
  and 1053-1130 are scratch-pad brainstorming about DB schema, sessions, etc.
  Move to a design doc.
- [ ] **Commented-out `Packages` type in `programTypes.dark:409-413`** — Dead code.
- [ ] **Commented-out test code in LibParser.Tests.fs** — A `"pipe without
  expr"` test (lines ~103-123) is commented out. May just need uncommenting.
- [ ] **O(n^2) list appends in `Propagation.fs:210-231`** — `repoints <- repoints
  @ [repoint]` and `allOps <- allOps @ ops` in a loop. Use `ResizeArray` or
  prepend-then-reverse.
- [ ] **O(n^2) list appends in `PackageManager.fs:194-214`** — `existing @ [loc]`
  inside a fold. Use `loc :: existing` and reverse.
- [ ] **Duplicated convergence loop** — `TestModule.parseTestFile` (lines 284-328)
  and `LoadPackagesFromDisk.load` (lines 55-113) share nearly identical iterative
  re-parse-until-stable logic. Extract a shared helper.
- [ ] **`getBranchChain` N+1 DB queries** — `Branches.fs:193-215` walks branch
  ancestry with one SQL query per ancestor. A recursive CTE would fetch the chain
  in a single query.
- [ ] **Mutable `statements` list in `PackageOpPlayback.applySetName`** —
  Classic imperative SQL statement building. Probably leave as-is.
- [ ] **`PackageLocation.toFQN()` with empty modules** — `ProgramTypes.fs:574`:
  if `modules` is `[]`, produces `"owner..name"` (double dot). Document the
  invariant or add validation.
---

## Separate PR

- [ ] **Tree experiment (`packages/stachu/tree-experiment/`)** — 821 lines
  of new code for an unrelated outliner experiment. Split out to its own
  branch/PR before merging `hash-based-refs-finally`.
