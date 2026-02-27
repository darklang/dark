# Branch Cleanups: `hash-based-refs-finally`

Remaining cleanup opportunities from reviewing the full branch diff.

---

## Bugs

- [ ] **`StringSegment.fromDT` case mismatch** — `RuntimeTypesToDarkTypes.fs:400`
  writes `"Interpolated"` but line 406 matches `"Interpolation"`. Deserialization
  of interpolated string segments will always fail.

- [ ] **`PropagateRepoint` and `PackageOp` still use UUID names/types for
  package item refs** — `programTypes.dark:423-427`: `PropagateRepoint` has
  `fromUUID: Uuid` / `toUUID: Uuid` but CLI consumers access `.toHash`.
  `PackageOp.PropagateUpdate` (lines 438-444): `fromSourceUUIDs: List<Uuid>`,
  `toSourceUUID: Uuid`. `RevertPropagation` (lines 446-452):
  `restoredSourceUUID: Uuid`. All of these refer to package item content hashes
  and should be `String` with hash-based names. (`propagationId` and `revertId`
  are operation IDs and can stay as `Uuid`.)

- [ ] **`cliScript.dark` accesses `.id` on package items** — Lines 258, 266, 274,
  289, 297, 305 do `{ newType with id = originalType.id }` but the types now have
  `.hash`, not `.id`.

- [ ] **`WipRefresh.fs:95-100` changedCount is 2x actual** — Uses symmetric
  difference of old/new hash sets, which counts each changed item twice (old hash
  + new hash). Should be `Set.count / 2` or count locations whose hash changed.

- [ ] **`evalConstantExpr` silently returns `DUnit` for unrecognized expressions**
  — `ProgramTypesToRuntimeTypes.fs:1198` has a catch-all `| _ -> RT.DUnit` that
  swallows unknown expression types. Should error instead.

---

## High Priority

- [ ] **Dead ResizeArrays in `PackageManager.fs:89-103`** — `createInMemory`
  populates `types`, `values`, `fns` ResizeArrays but never reads them. The actual
  maps are built by a separate loop at lines 138-181. Remove the dead arrays.

- [ ] **Regex compiled on every call in `DeferredResolver.fs:27-50`** —
  `parseTypeName` and `parseFnOrValueName` compile regex patterns on every
  invocation. Extract to module-level compiled `Regex` instances.

- [ ] **Dead `_lookup` mutable maps in `PackageRefs.fs:29-32, 684-688`** — Both
  `Type._lookup` and `Fn._lookup` are populated but never read anywhere. Remove
  or add a comment explaining if they're for debugging.

- [ ] **Dead `extractFromLetPattern` in `DependencyExtractor.fs:125`** — Always
  returns `[]`. Called at line 232 but contributes nothing. Remove.

- [ ] **`canvas.dark` (Dark-side) is broken** — Lines 131-133 pass wrong types to
  `withExtras`, lines 146/157/168 access `.name` which no longer exists on package
  types. File header says "CLEANUP this hasn't been used in a while". Decide:
  fix or delete.

- [ ] **`throwIfRelevant` in `nameResolver.dark:26-36` is a complete no-op** —
  Both branches return `err` unchanged. Either implement actual error throwing for
  `ThrowError` or remove the function.

- [ ] **~180 lines of design-note comments in `ProgramTypes.fs`** — Lines 714-822
  and 1053-1130 are scratch-pad brainstorming about DB schema, sessions, etc.
  Move to a design doc.

- [ ] **Commented-out `Packages` type in `programTypes.dark:409-413`** — Dead code.

---

## Medium Priority

- [ ] **Unify Add*/Set*Name op scanning** — Three sites scan ops looking for
  Add*/Set*Name pairs: `collectAddedHashes` (PackageOpPlayback.fs),
  `createInMemory` (PackageManager.fs), and `computeRealHashes`
  (HashStabilization.fs). Each uses the same mutable-pending-item pattern.
  Consider a shared `PackageOp.namedItems` helper.

- [ ] **Commented-out test code in LibParser.Tests.fs** — A `"pipe without
  expr"` test (lines ~103-123) is commented out. May just need uncommenting.

- [ ] **Duplicated AST walker: DeferredResolver.fs vs AstTransformer.fs** —
  `reResolveExpr`/`reResolveTypeRef`/etc. are structurally identical to
  `transformExpr`/`transformTypeRef` in AstTransformer.fs (~400 lines of
  near-duplicate code). Consider a generic walker parameterized by leaf operation.

- [ ] **Duplicated ID extraction: AstTransformer.fs vs DependencyExtractor.fs** —
  `getTypePackageId`/`getFnPackageId`/`getValuePackageId` are identical to
  `extractTypeId`/`extractFnId`/`extractValueId`. Extract to shared module.

- [ ] **Duplicated `ownerField`/`modulesField`/`nameField` helpers** —
  `ProgramTypesToDarkTypes.fs` and `RuntimeTypesToDarkTypes.fs` both define
  identical helper functions. Extract to `CommonToDarkTypes.fs`.

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

- [ ] **Stale UUID comment** — `Packages.fs:210` says "Evaluate a package value
  by its UUID" but now operates on content hashes.

- [ ] **Unused imports across BuiltinPM** — `Packages.fs:24-25` (`open Fumble`,
  `open LibDB.Db`), `Merge.fs:3-4`, `Scripts.fs:3-4`, `PackageOps.fs:3-4` all
  have unused `System.Threading.Tasks`/`FSharp.Control.Tasks` imports.
  `BuiltinCliHost/Utils.fs:12` has unused `DvalDecoder` alias.

- [ ] **Duplicated `pmFindType`/`pmFindValue`/`pmFindFn` pattern in
  `Packages.fs`** — Three near-identical functions (lines 77, 129, 244) and
  similarly for location lookups (lines 321, 343, 365, 389, 410, 432).
  Extract a helper.

- [ ] **Silent `ContentHash ""` fallback in `Packages.fs:538-540`** —
  `pmPropagate` converts non-string values to `ContentHash ""` instead of
  erroring.

- [ ] **Disabled golden-file test in `Serialization.Binary.Tests.fs:186-193`** —
  Skips binary format comparison with comment about changing format. Track
  re-enablement.

- [ ] **Massive duplication in `writtenTypesToProgramTypes.dark:811-1070`** —
  The `EApply` case repeats the same "parse typeArgs, parse args, construct
  EApply" block ~8 times. Factor into a helper.

---

## Low Priority / Style

- [ ] **Mutable `statements` list in `PackageOpPlayback.applySetName`** —
  Classic imperative SQL statement building. Probably leave as-is.

- [ ] **`computeOpHash` in `Inserts.fs` converts hash→UUID** — Lossy truncation
  for DB compatibility. Plan for a dedicated migration PR.

- [ ] **Stale CLEANUP comment in `RuntimeTypes.fs:8`** — "there's some useful
  'reference things by hash' work to be done" — this branch *is* that work.
  Remove or update.

- [ ] **Dead `[<Measure>] type register` in `RuntimeTypes.fs:278`** — Unused.

- [ ] **Commented-out `LetPattern` variants in `ProgramTypes.fs:177-183`** —
  `LPIgnored` and `LPParens` should be tracked as issues, not comments.

- [ ] **Commented-out `typeNamePattern` in `ProgramTypes.fs:12`** — Dead code.

- [ ] **Commented-out `deprecation` in `prettyPrinter/programTypes.dark:830-841`**
  — Dead code per comment.

- [ ] **Catch-all expr case in `prettyPrinter/programTypes.dark:824-828`** —
  Comment says "CLEANUP: remove this case before shipping to users".

- [ ] **`ContentHash.empty` edge case** — `Prelude.fs:61`: `ContentHash ""`
  is not a valid SHA-256 digest. `toShortString` at line 69 will crash on
  it. Consider removing or adding a guard.

- [ ] **`PackageLocation.toFQN()` with empty modules** — `ProgramTypes.fs:574`:
  if `modules` is `[]`, produces `"owner..name"` (double dot). Document the
  invariant or add validation.

- [ ] **Three TODO comments about renaming `typeName`/`valueName`/`fnName`** in
  `prettyPrinter/runtimeTypes.dark:66,93,122` — to `fqtypeName`/etc.

- [ ] **`prepareProcessCommand` duplication in `Execution.fs:57-68`** — Linux
  and OSX branches are identical. Collapse.

---

## Separate PR

- [ ] **Tree experiment (`packages/stachu/tree-experiment/`)** — 821 lines
  of new code for an unrelated outliner experiment. Split out to its own
  branch/PR before merging `hash-based-refs-finally`.
