# Branch Cleanups: `hash-based-refs-finally`


- [ ] rename ContentHash to just Hash
  add a /// comment at `type Hash = ` to note that the type is intended to be used for hashes of the _structure_ of some other value, not relying on temporary things like the name, etc.
  'content hash' -> hash
  contentHash->hash
  look up every other use of 'content'
  make sure no 'id' bound to one of these

- [x] I hate that NameLookup leaked, somehow, into ProgramTYpes.fs -- that's supposed to be for core types. why was this done? report back and tell me my options (tell me in this file)
  fixed: moved to `LibPackageManager/NameLookup.fs`. Both consumers (NameResolver,
  DeferredResolver) can see it since LibParser depends on LibPackageManager.

- [x] why do we still have getTypeLocation when we now have getTypeLocations?
  fixed: removed singular getLocation/getTypeLocation/getValueLocation/getFnLocation
  and the 3 singular builtins. All callers now use getLocations + pickLocation helper.

- [ ] let's break down Hashing.fs a bit -- create a dir and a few files - for base format,
relatedly -- so many fns in there are framed as 'cononicalWriteX' -- well, aren't they _all_ sort of canonical, and isn't it pretty much _always_ write (rather thanr ead)? Feels redundatn. I suspect there's some general cleanup taht can/should be done. maybe we should do it in the file first, and then tidy after.

- [ ] why is backend/tests/Tests/NewParser.Tests.fs have empty content hashes? I guess I'm slightly surprised things are working - if any by-hash refs are happening at runtime, they wouldn't be found, right?

- [ ] packages/darklang/cli/deps.dark still has some things named 'id' that should be 'hash'

- [ ] it seems packages/darklang/cli/packages/hash.dark has its own one-off pretty printing of hashes (formatHash) but can't/shouldn't we use something that exists in stdlib?

- [ ] packages/darklang/cli/packages/hash.dark has custom fallback for "look for type, otherwise look for value, otherwise look for fn" - isn't there a way to tidy this? I suspect we have something more gneeric like a stdlib fn that's "look for anything _at this location_ and in the reponse incldue what kind of thing it is" we could (re)use.

- [ ] research/report: what is the List<String> I've seen recently added in various bits of code in packages/darklang/languageTools/writtenTypesToProgramTypes.dark ? If we really need that, maybe let's create an alias for the type so reading things is more clear?

- [ ] **`StringSegment.fromDT` case mismatch** — `RuntimeTypesToDarkTypes.fs:400`
  writes `"Interpolated"` but line 406 matches `"Interpolation"`. Deserialization
  of interpolated string segments will always fail.

- [x] **`PropagateRepoint` and `PackageOp` still use UUID names/types for
  package item refs** — fixed: now uses `ContentHash` with hash-based names.

- [x] **`WipRefresh.fs:95-100` changedCount is 2x actual** — fixed: use
  `Set.difference newHashes oldHashes` instead of symmetric difference.

- [ ] **Dead ResizeArrays in `PackageManager.fs:89-103`** — `createInMemory`
  populates `types`, `values`, `fns` ResizeArrays but never reads them. The actual
  maps are built by a separate loop at lines 138-181. Remove the dead arrays.

- [x] **Regex compiled on every call in `DeferredResolver.fs:27-50`** —
  fixed: extracted to module-level compiled `Regex` instances.

- [x] **Dead `_lookup` mutable maps in `PackageRefs.fs:29-32, 684-688`** —
  intentional: infrastructure for future LocalExec hash-checking tool (see TODO comment).

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

- [x] **Duplicated `ownerField`/`modulesField`/`nameField` helpers** —
  fixed: moved to `CommonToDarkTypes.fs`, both files now delegate to `C2DT.*`.

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

- [x] **Massive duplication in `writtenTypesToProgramTypes.dark:811-1070`** —
  fixed: hoisted typeArgs/args parsing above the branching, reduced self-call
  detection to a single `isSelfCall` bool. ~260 lines → ~60.

- [ ] **Mutable `statements` list in `PackageOpPlayback.applySetName`** —
  Classic imperative SQL statement building. Probably leave as-is.

- [ ] **`computeOpHash` in `Inserts.fs` converts hash→UUID** — Lossy truncation
  for DB compatibility. Plan for a dedicated migration PR.

- [ ] **`PackageLocation.toFQN()` with empty modules** — `ProgramTypes.fs:574`:
  if `modules` is `[]`, produces `"owner..name"` (double dot). Document the
  invariant or add validation.

---

## Separate PR

- [ ] **Tree experiment (`packages/stachu/tree-experiment/`)** — 821 lines
  of new code for an unrelated outliner experiment. Split out to its own
  branch/PR before merging `hash-based-refs-finally`.
