module Builtins.Matter.Libs.PM.PackageOps

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module Builtin = LibExecution.Builtin
module PackageRefs = LibExecution.PackageRefs
module Dval = LibExecution.Dval
module VT = LibExecution.ValueType
module NR = LibExecution.RuntimeTypes.NameResolution

open Builtin.Shortcuts


let packageOpTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.ProgramTypes.packageOp ())

let packageOpKT () = KTCustomType(packageOpTypeName (), [])


/// Author a BranchEvent op so what happened to a branch travels the way everything else does.
///
/// The projections are already updated by the caller's own SQL; this is not how the local store learns
/// what happened. It is how the OTHER machine learns. The fold is idempotent for these events (each sets a
/// column only when it is still NULL), so the op landing here as well changes nothing locally.
let private recordBranchEvent
  (branchId : string)
  (event : PT.BranchEventKind)
  : Ply<unit> =
  uply {
    let at = System.DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ss.fffZ")
    let op = PT.PackageOp.BranchEvent(branchId, event, at)
    let! _ = LibDB.Inserts.insertAndApplyOps [ op ]
    return ()
  }


// TODO: review/reconsider the accessibility of these fns
let fns (pm : PT.PackageManager) : List<BuiltInFn> =
  [ { name = fn "pmStabilizeHashes" 0
      typeParams = []
      parameters =
        [ Param.make "ops" (TList(TCustomType(NR.ok (packageOpTypeName ()), []))) "" ]
      returnType = TList(TCustomType(NR.ok (packageOpTypeName ()), []))
      description =
        "Compute real content-addressed hashes for package ops (SCC-aware)."
      fn =
        (function
        | _, _, _, [ DList(_vt, ops) ] ->
          uply {
            let ptOps = ops |> List.choose PT2DT.PackageOp.fromDT
            let stabilized = LibDB.HashStabilization.computeRealHashes ptOps
            return
              Dval.list
                (packageOpKT ())
                (stabilized |> List.map PT2DT.PackageOp.toDT)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmUnresolvedNames" 0
      typeParams = []
      parameters =
        [ Param.make "ops" (TList(TCustomType(NR.ok (packageOpTypeName ()), []))) "" ]
      returnType = TList(TTuple(TString, TList TString, []))
      description =
        "For each op still holding unresolved name references, its content hash and those names."
      fn =
        (function
        | _, _, _, [ DList(_vt, ops) ] ->
          uply {
            // Reports; decides nothing. Whether an unresolved name should stop a commit is a decision, and
            // decisions live in Dark -- see `Cli.Commit`.
            let found =
              ops
              |> List.choose PT2DT.PackageOp.fromDT
              |> List.choose LibDB.UnresolvedCheck.inOp
              |> List.map (fun (hash, names) ->
                DTuple(
                  DString hash,
                  Dval.list KTString (names |> List.map DString),
                  []
                ))
            return Dval.list (KTTuple(VT.string, VT.list VT.string, [])) found
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "pmTypeMismatches" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TList(
          TTuple(
            TString,
            TypeReference.option TInt64,
            [ TCustomType(NR.ok (PT2DT.TypeReference.typeName ()), [])
              TCustomType(NR.ok (PT2DT.TypeReference.typeName ()), []) ]
          )
        )
      description =
        "For each live fn holding a type mismatch this can be CERTAIN of, a tuple of
        (fn hash, where, expected type, actual type).

        `where` is None for the body against the declared return type, or Some i for the argument at
        0-based position i.

        Sound rather than complete: it reports only pairs no substitution could reconcile, so silence is
        not a claim that the item type-checks. Reports; refuses nothing."
      fn =
        let siteKT = KTCustomType(Dval.optionType (), [ VT.int64 ])
        let typKT = PT2DT.TypeReference.knownType ()
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            // The cache lives with the check, in `TypeSurface.Cache`, so a hash already known clean under
            // this build is never re-walked and the reload can warm the whole tree in one go. It reads the
            // live set itself: handing it four thousand hashes so it could filter them back out was more
            // expensive than the check.
            let! raw = LibDB.TypeSurface.Cache.checkAll pm.getFn

            let found =
              raw
              |> List.map (fun (h, m : LibDB.TypeSurface.Mismatch) ->
                let where =
                  match m.site with
                  | LibDB.TypeSurface.ReturnValue -> Dval.optionNone KTInt64
                  | LibDB.TypeSurface.Argument i ->
                    Dval.optionSome KTInt64 (DInt64(int64 i))

                DTuple(
                  DString h,
                  where,
                  [ PT2DT.TypeReference.toDT m.expected
                    PT2DT.TypeReference.toDT m.actual ]
                ))

            return
              Dval.list
                (KTTuple(
                  VT.string,
                  ValueType.Known siteKT,
                  [ ValueType.Known typKT; ValueType.Known typKT ]
                ))
                found
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    { name = fn "scmAddOps" 0
      typeParams = []
      parameters =
        [ Param.make
            "branchId"
            TString
            "the branch these ops land on; \"\" is main. Passed rather than ambient so a caller can author onto a branch it isn't sitting on -- which is what sync does"
          Param.make "ops" (TList(TCustomType(NR.ok (packageOpTypeName ()), []))) "" ]
      returnType = TypeReference.result TInt TString
      description =
        "Add package ops to <branchId> (\"\" = main), uncommitted.
        Returns the number inserted; duplicates are skipped, since an op's id is its content."
      fn =
        let resultOk = Dval.resultOk KTInt KTString
        let resultError = Dval.resultError KTInt KTString
        (function
        | exeState, _, _, [ DString branchIdParam; DList(_vtTODO, ops) ] ->
          uply {
            try
              let ops = ops |> List.choose PT2DT.PackageOp.fromDT

              match (if branchIdParam = "" then None else Some branchIdParam) with
              // On a branch (this process was launched with --branch): the edit lands on the
              // BRANCH, not main -- store effective=0 + tag the frontier, no fold (branch ops are
              // inert in main until merge). Refresh the process overlay so a later eval in THIS
              // same process sees the new op. Main's fold/refresh/value-eval is skipped entirely.
              | Some branchId ->
                // Register the branch (idempotent; records parent=main). Stabilize hashes exactly
                // like the main path's WipRefresh, so a
                // merged value's package_values (keyed by AddValue's hash) and locations (SetName's
                // hash) AGREE -- without this they diverge and a merged value can't be found.
                do! LibDB.Branches.createBranch branchId "" "main"

                let stabilized = LibDB.HashStabilization.computeRealHashes ops
                let! stabilized = LibDB.Lineage.recordPrevious stabilized
                let! n = LibDB.Branches.storeDeltaOps branchId stabilized
                // Record the per-name base (the PARENT's current hash for each name this touches) so a
                // later merge can tell if the parent changed the same name (the reload-stable rebased
                // check). Parent is main unless `switch` pre-registered this branch off another.
                let! parentId = LibDB.Branches.parentOf branchId
                do! LibDB.Branches.recordNameBases branchId parentId stabilized
                // Fold this branch's CONTENT (Add*, never SetName) into the content-addressed tables.
                // Content is shared and immutable, so this is safe; the NAME layer is what a branch has to
                // keep to itself, and no locations row is written here. Two things need it:
                //   - an expression-valued branch value needs an rt_dval for eval to read
                //   - PROPAGATION needs the branch item's dependency edges in package_dependencies, or a
                //     branch-authored dependent is invisible to discovery and never repoints
                let contentOps =
                  stabilized
                  |> List.filter (fun op ->
                    match op with
                    | PT.PackageOp.AddValue _
                    | PT.PackageOp.AddFn _
                    | PT.PackageOp.AddType _ -> true
                    | _ -> false)
                if not (List.isEmpty contentOps) then
                  do! LibDB.PackageOpPlayback.applyOps contentOps
                  let builtins : Builtins =
                    { values = exeState.values.builtIn; fns = exeState.fns.builtIn }
                  let! _ =
                    LibDB.Seed.evaluateAllValues builtins LibDB.PackageManager.rt
                  ()
                // Refresh the process overlay ONLY when we just wrote to the branch this process is
                // sitting on. Authoring onto some other branch must not move what this process resolves
                // names against -- that would make a targeted write silently change the caller's world.
                //
                // It does have to stop serving a stale answer ABOUT that branch, though: reads of another
                // branch are memoized, and a branch write that folds nothing would otherwise never
                // invalidate them.
                if LibDB.PackageManager.currentBranchId () = Some branchId then
                  let! all = LibDB.Branches.loadDeltaOps branchId
                  LibDB.PackageManager.setBranchOverlay all
                else
                  LibDB.PackageManager.forgetBranch branchId
                return resultOk (Dval.int (bigint (int n)))

              | None ->
                // Stabilize hashes BEFORE inserting, exactly as the branch path does (storeDeltaOps stores
                // `computeRealHashes ops`). Previously main inserted raw `ops` whose SetName targets are still
                // provisional/empty, so WipRefresh.refresh below was the FIRST thing to assign real hashes --
                // which it did by rewriting the ENTIRE op log every author (~6s; see 30e-authoring-perf). With
                // the ops already stable at insert, refresh finds nothing changed and takes its fast-path (no
                // whole-log discard+reinsert). The persisted end-state is identical (refresh stabilized before
                // too); we just do it once, up front, over the new ops rather than over all ~10k.
                let stabilizedOps = LibDB.HashStabilization.computeRealHashes ops
                // What each of these bindings REPLACES, asked of the store while it still holds the old
                // answer. Must run before the insert for that reason.
                let! stabilizedOps = LibDB.Lineage.recordPrevious stabilizedOps

                // All ops are added as WIP - use scmCommitWipOpsByIds to commit them
                let! insertedCount =
                  LibDB.Inserts.insertAndApplyOpsAsWip stabilizedOps

                // Auto-refresh existing WIP items: re-resolve names and
                // recompute SCC-aware hashes now that new items exist (still needed for the forward-ref case:
                // an earlier WIP item that references THIS newly-authored one).
                let! _refreshed = LibDB.WipRefresh.refresh pm

                // Populate `rt_dval` for any package_values rows still
                // NULL after this insert+refresh. `applyAddValue` always
                // inserts NULL and Phase-3 `evaluateAllValues` only runs
                // at startup when there are unapplied ops. Without this
                // step, a CLI-added value that references another value
                // (qualified or bare) would fail at eval with a NULL
                // rt_dval until the next cold restart.
                let builtins : Builtins =
                  { values = exeState.values.builtIn; fns = exeState.fns.builtIn }
                let! _ =
                  LibDB.Seed.evaluateAllValues builtins LibDB.PackageManager.rt

                return resultOk (Dval.int (bigint insertedCount))
            with ex ->
              return resultError (Dval.string ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Which branch is THIS process on ("" = main). Set by `--branch <id>` or the persistent
    // `current_branch`, both resolved in the CLI entry point before any Dark runs. Dark can't read it any
    // other way: it's process state, not a row, and `configGet "current_branch"` misses the flag form.
    { name = fn "scmCurrentBranch" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "The branch this process is on, or \"\" for main."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            return
              DString(
                LibDB.PackageManager.currentBranchId () |> Option.defaultValue ""
              )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Turn a branch NAME into the id everything below the CLI refers to, starting the branch if that
    // name has none.
    //
    // The two are separate on purpose. A name is what a person types and reads, so it is renameable and
    // reusable: archive `fix-auth`, start another, and both want the label. An id is what op tags,
    // per-name bases, relay bundles and parent links point at, so it must survive a rename and must never
    // join two unrelated branches that happened to reuse a label -- including two machines that each
    // started a `fix-auth`, which sync has to keep apart.
    //
    // One implementation, called from both languages, because a second one that resolved names even
    // slightly differently would hand the same name two ids and split a branch in half.
    { name = fn "scmResolveBranch" 0
      typeParams = []
      parameters =
        [ Param.make "name" TString "the branch name a person typed"
          Param.make
            "parentId"
            TString
            "the branch id to parent a NEW branch to (\"main\" at top level)" ]
      returnType = TTuple(TString, TBool, [])
      description =
        "Resolves a branch name to its id, creating the branch if the name has no live one.
        Returns (id, wasCreated)."
      fn =
        (function
        | _, _, _, [ DString name; DString parentId ] ->
          uply {
            let! (id, created) = LibDB.Branches.resolveOrCreate name parentId
            return DTuple(DString id, DBool created, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // The name to SHOW for a branch id. Falls back to the id, which is all an imported branch that
    // arrived as tagged ops with no registry row of its own has to show.
    { name = fn "scmBranchName" 0
      typeParams = []
      parameters = [ Param.make "branchId" TString "\"\" is main" ]
      returnType = TString
      description = "The display name of <param branchId>. \"\" is main."
      fn =
        (function
        | _, _, _, [ DString branchId ] ->
          uply {
            if branchId = "" then
              return DString "main"
            else
              let! name = LibDB.Branches.nameForId branchId
              return DString name
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // The id a person means when they type <name> at a branch verb: the most recent branch still listed
    // under it, merged or not. Never creates -- unlike `scmResolveBranch`, this backs the paths that
    // should refuse rather than quietly start something.
    //
    // Merged branches are INCLUDED on purpose. `dark branches` lists them, so `dark diff <that name>` has
    // to find them; refusing a name you just read off the listing is the worst of both answers.
    { name = fn "scmBranchIdForName" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TypeReference.option TString
      description =
        "The id of the most recent listed branch named <param name>, if any. Merged branches count;
        archived ones don't, since archiving discards the ops there'd be anything to say about."
      fn =
        (function
        | _, _, _, [ DString name ] ->
          uply {
            let! idOpt = LibDB.Branches.idForName name
            return
              match idOpt with
              | Some id -> Dval.optionSome KTString (DString id)
              | None -> Dval.optionNone KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Whether a branch's work is already in its parent. `merge` asks before doing anything, because
    // merging an already-merged branch flips nothing and reports "Merged 0 op(s)", which reads like a
    // failure of the merge rather than an answer to a question you already had.
    { name = fn "scmBranchIsMerged" 0
      typeParams = []
      parameters = [ Param.make "branchId" TString "" ]
      returnType = TBool
      description =
        "True when <param branchId> has already been merged into its parent."
      fn =
        (function
        | _, _, _, [ DString branchId ] ->
          uply {
            let! merged = LibDB.Branches.isMerged branchId
            return DBool merged
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Change which branch THIS process is on, without restarting it.
    //
    // Boot (`--branch`, or `current_branch`) covers the one-shot case, but it can't be the only way in:
    // the interactive REPL is a single long-lived process, so `ops switch` there has to move the overlay
    // that name resolution and authoring actually read. Writing the config key alone would leave the
    // display saying one thing and the behaviour doing another.
    //
    // "" = main. Returns the branch it ended up on, so a caller reports what happened rather than what it
    // asked for.
    { name = fn "scmSelectBranch" 0
      typeParams = []
      parameters =
        [ Param.make
            "branchId"
            TString
            "the branch to move this process to (\"\" = main)" ]
      returnType = TString
      description =
        "Moves this process onto <param branchId>, loading that branch's delta ops as the overlay used
        for name resolution and execution. \"\" returns to main. Returns the branch now active."
      fn =
        (function
        | _, _, _, [ DString branchId ] ->
          uply {
            let selected = if branchId = "" then None else Some branchId
            LibDB.PackageManager.selectBranch selected
            return DString(selected |> Option.defaultValue "")
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Decode ONE op_blob (as stored in package_ops) into a PackageOp. The single F#
    // primitive Dark needs to read structured ops -- the binary format isn't Dark-
    // decodable. The QUERY that selects blobs lives in Dark (Stdlib.Sqlite), so this
    // replaces the bespoke scmGetRecentOps: it's reusable for any op read (recent,
    // pending, a change's ops). `id` is used only for error context, not the decode.
    { name = fn "packageOpFromBlob" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid ""; Param.make "blob" TBlob "" ]
      returnType = TCustomType(NR.ok (packageOpTypeName ()), [])
      description = "Deserialize a package_ops op_blob into a PackageOp."
      fn =
        function
        | exeState, _, _, [ DUuid id; DBlob blobRef ] ->
          uply {
            let! bytes = LibExecution.Blob.readBytes exeState blobRef
            let op = LibDB.Queries.deserializeOp id bytes
            return PT2DT.PackageOp.toDT op
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // Bulk-import synced ops (id, op_blob-hex, origin_ts) in ONE transaction, then FOLD them
    // so they take effect. The perf path for transport: Dark's per-op insert crawls on a real
    // log, so hex-decode + bulk INSERT + fold live in F#. (Sync moves ops and they apply --
    // no approval gate; that's a later effort.) Returns count newly inserted.
    { name = fn "scmImportOps" 0
      typeParams = []
      parameters =
        [ Param.make
            "commitHash"
            TString
            "commit the arriving ops into this commit (\"\" = leave uncommitted)"
          Param.make
            "records"
            (TList(TTuple(TString, TString, [ TString ])))
            "(id, blobHex, originTs) triples" ]
      returnType = TypeReference.result TInt TString
      description =
        "Bulk-import synced ops in one transaction, then fold them in. Returns count inserted."
      fn =
        let resultOk = Dval.resultOk KTInt KTString
        let resultError = Dval.resultError KTInt KTString
        (function
        | _, _, _, [ DString commitHash; DList(_, records) ] ->
          uply {
            try
              let rows =
                records
                |> List.choose (fun d ->
                  match d with
                  | DTuple(DString id, DString hex, [ DString ts ]) ->
                    Some(id, hex, ts)
                  | _ -> None)
              let! n = LibDB.Inserts.importOpsBulk commitHash rows
              let! _ = LibDB.Seed.applyUnappliedOps () // fold the just-inserted (effective=1) ops
              return resultOk (Dval.int (bigint n))
            with ex ->
              return resultError (Dval.string ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // RELAY store: bulk-insert ops + record ownership (owner) in one transaction, NO fold
    // (a relay serves blobs, not projections). The perf path for a relay recording pushes.
    { name = fn "scmStoreOps" 0
      typeParams = []
      parameters =
        [ Param.make
            "owner"
            TString
            "the pusher's identity (\"\" = don't record ownership)"
          Param.make
            "records"
            (TList(TTuple(TString, TString, [ TString ])))
            "(id, blobHex, originTs) triples" ]
      returnType = TypeReference.result TInt TString
      description =
        "Relay store: bulk-insert ops + record ownership, no fold. Returns count stored."
      fn =
        let resultOk = Dval.resultOk KTInt KTString
        let resultError = Dval.resultError KTInt KTString
        (function
        | _, _, _, [ DString owner; DList(_, records) ] ->
          uply {
            try
              let rows =
                records
                |> List.choose (fun d ->
                  match d with
                  | DTuple(DString id, DString hex, [ DString ts ]) ->
                    Some(id, hex, ts)
                  | _ -> None)
              let! n = LibDB.Inserts.storeOpsWithOwner owner rows
              return resultOk (Dval.int (bigint n))
            with ex ->
              return resultError (Dval.string ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // DISCARD main's draft: drop every uncommitted op and re-fold the store from the committed ones.
    //
    // In F# because it is a REWRITE of main, not a query: the projections record the result of the whole
    // op sequence, so removing a folded op means rebuilding from the ops that survive. The re-fold is the
    // same delete-and-reinsert the authoring refresh uses, so there is one such path, not two.
    { name = fn "scmDiscardDraft" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TypeReference.result TInt TString
      description =
        "Drop main's uncommitted (draft) ops and re-fold from the committed ones. Returns how many were dropped."
      fn =
        let resultOk = Dval.resultOk KTInt KTString
        let resultError = Dval.resultError KTInt KTString
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            match! LibDB.Draft.discard () with
            | Ok n -> return resultOk (Dval.int (bigint (int n)))
            | Error e -> return resultError (Dval.string e)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // COLLAPSE the draft's superseded namings, at commit. Five edits to one function leave five namings of
    // it, four of which describe a version that stopped being what the name meant before anyone else saw
    // it. Returns how many ops went.
    { name = fn "scmCollapseDraft" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TypeReference.result TInt TString
      description =
        "Collapse the draft's superseded namings, keeping the last binding per name. Returns how many ops
        were dropped."
      fn =
        let resultOk = Dval.resultOk KTInt KTString
        let resultError = Dval.resultError KTInt KTString
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            match! LibDB.Draft.collapse () with
            | Ok n -> return resultOk (Dval.int (bigint (int n)))
            | Error e -> return resultError (Dval.string e)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // UN-STAGE the repoint the draft holds for one name: what a PIN does before commit.
    //
    // A pin is retroactive -- by the time you decide something shouldn't have followed, it already has.
    // Before commit, saying so by dropping the staged repoint is better than saying so by authoring a
    // rebinding op: the second records a decision you're still in the middle of making, permanently.
    //
    // 0 means there was nothing staged (the binding is committed, or you authored it yourself), and the
    // caller then takes the post-commit path.
    { name = fn "scmUnstageRepoint" 0
      typeParams = []
      parameters =
        [ Param.make "owner" TString ""
          Param.make "modules" TString "dot-separated, \"\" for none"
          Param.make "name" TString "" ]
      returnType = TypeReference.result TInt TString
      description =
        "Drop the draft's propagated binding for one name. Returns how many ops were dropped; 0 when
        nothing was staged for it."
      fn =
        let resultOk = Dval.resultOk KTInt KTString
        let resultError = Dval.resultError KTInt KTString
        (function
        | _, _, _, [ DString owner; DString modules; DString name ] ->
          uply {
            let loc : PT.PackageLocation =
              { owner = owner
                modules = if modules = "" then [] else String.split "." modules
                name = name }

            match! LibDB.Draft.unstageRepoint loc with
            | Ok n -> return resultOk (Dval.int (bigint (int n)))
            | Error e -> return resultError (Dval.string e)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // ARCHIVING a branch travels, for the same reason merging does: on the other machine the branch is
    // still sitting there looking like live work. The archive itself is Dark's -- `SCM.Branches.archive`
    // owns that column and has already written it -- so all this does is author the op that says so.
    // Idempotent on arrival (the fold sets `archived_at` only while it is NULL), which is what makes it
    // safe for the authoring machine to fold its own event too.
    //
    // Separate from the merge path rather than one builtin taking an event, because these are the only
    // two events there are, and a `BranchEventKind` crossing the boundary as data would need the DU
    // marshalled for one caller each.
    { name = fn "scmRecordBranchArchived" 0
      typeParams = []
      parameters = [ Param.make "branchId" TString "the branch that was archived" ]
      returnType = TUnit
      description =
        "Author the op that says this branch was archived, so other machines learn it."
      fn =
        (function
        | _, _, _, [ DString branchId ] ->
          uply {
            do! recordBranchEvent branchId PT.Archived
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // MERGE a branch into its parent: the MECHANISM only. Whether a merge is allowed is decided in Dark,
    // by `SCM.Branches.canMerge`, which is where a decision belongs and where the tables it counts are
    // owned. Calling this directly skips that gate, and there is one caller.
    //
    // What happens here is transactional and stays: flip the frontier effective=1, fold into main's
    // projections, evaluate merged values (so a merged value's rt_dval is populated), clear the frontier,
    // mark merged. Only parent=main is exercised now. Deterministic replay + origin_ts LWW, not a CRDT.
    { name = fn "scmMergeBranch" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TString "the branch to merge into its parent" ]
      returnType = TypeReference.result TInt TString
      description =
        "Merge a branch into its parent. The gate is in Dark; this does the work. Returns count merged."
      fn =
        let resultOk = Dval.resultOk KTInt KTString
        let resultError = Dval.resultError KTInt KTString
        (function
        | exeState, _, _, [ DString branchId ] ->
          uply {
            try
              let! parentId = LibDB.Branches.parentOf branchId
              if parentId = "main" then
                // Into main: flip the frontier effective=1, fold into main's projections, evaluate
                // merged values (rt_dval), clear the frontier + mark merged.
                let! n = LibDB.Branches.markMergedEffective branchId
                let! _ = LibDB.Seed.applyUnappliedOps ()
                let builtins : Builtins =
                  { values = exeState.values.builtIn; fns = exeState.fns.builtIn }
                let! _ =
                  LibDB.Seed.evaluateAllValues builtins LibDB.PackageManager.rt
                // One transaction, because the gap between these two was the only interruption
                // point in a merge that could not be undone by running it again.
                LibDB.Branches.finishMerge branchId
                do! recordBranchEvent branchId PT.Merged
                return resultOk (Dval.int (bigint (int n)))
              else
                // Into a non-main parent (branches off branches): retag the frontier onto the parent
                // (its overlay folds it later). No effective-flip / fold -- that would leak into main.
                // retagFrontierToParent marks it merged in the same transaction as the retag.
                let! n = LibDB.Branches.retagFrontierToParent branchId parentId
                do! recordBranchEvent branchId PT.Merged
                return resultOk (Dval.int (bigint (int n)))
            with ex ->
              return resultError (Dval.string ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // REBASE a branch: accept main's current state as the branch's new base (reload-stable per-name
    // model). Returns the names main changed since the fork (so you see what moved); after this the
    // branch's own ops layer on top by origin_ts LWW and merge is unblocked. scm-spec.md 5.
    { name = fn "scmRebaseBranch" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TString "the branch to rebase onto its parent" ]
      returnType = TypeReference.result TString TString
      description =
        "Rebase a branch: accept main's current state; reports the names main changed since the fork."
      fn =
        let resultOk = Dval.resultOk KTString KTString
        let resultError = Dval.resultError KTString KTString
        (function
        | _, _, _, [ DString branchId ] ->
          uply {
            try
              let! parentId = LibDB.Branches.parentOf branchId
              let! changed = LibDB.Branches.rebase branchId
              match changed with
              | [] ->
                return
                  resultOk (
                    Dval.string
                      $"already current with \"{parentId}\" -- nothing to reconcile; merge is ready"
                  )
              | names ->
                let listed = names |> String.concat "\n  "
                return
                  resultOk (
                    Dval.string
                      $"rebased onto \"{parentId}\". it had changed {List.length names} name(s) you also touched:\n  {listed}\nyour branch's versions win by recency on merge -- re-author any you want to take the parent's version of."
                  )
            with ex ->
              return resultError (Dval.string ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // A branch's OWN frontier ops (oldest-first) as PackageOps, so `dark log <id>` can pretty-print
    // the branch's authoring history with the existing op pretty-printer (audit/review the sequence).
    { name = fn "scmBranchOps" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TString "the branch whose frontier ops to return" ]
      returnType = TList(TCustomType(NR.ok (packageOpTypeName ()), []))
      description = "The branch's own frontier ops (oldest-first), for `dark log`."
      fn =
        (function
        | _, _, _, [ DString branchId ] ->
          uply {
            let! ops = LibDB.Branches.frontierOps branchId
            return Dval.list (packageOpKT ()) (ops |> List.map PT2DT.PackageOp.toDT)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // RESOLVE one conflicted name (scm-spec 7): choice = "mine" (keep the branch's version, re-stamped
    // to win LWW) or "theirs" (take the parent's, dropping the branch's binding). Both clear the conflict.
    { name = fn "scmResolveConflict" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TString "the branch"
          Param.make "name" TString "the conflicted name (owner.Module.name)"
          Param.make "choice" TString "\"mine\" or \"theirs\"" ]
      returnType = TypeReference.result TString TString
      description =
        "Resolve a conflicted name on a branch: keep-mine or take-theirs."
      fn =
        let resultOk = Dval.resultOk KTString KTString
        let resultError = Dval.resultError KTString KTString
        (function
        | _, _, _, [ DString branchId; DString name; DString choice ] ->
          uply {
            try
              match choice with
              | "mine"
              | "theirs" ->
                let! result =
                  if choice = "mine" then
                    LibDB.Branches.resolveKeepMine branchId name
                  else
                    LibDB.Branches.resolveTakeTheirs branchId name
                match result with
                | Ok() ->
                  let kept =
                    if choice = "mine" then
                      $"kept the branch's {name} (it now wins on merge)"
                    else
                      $"took the parent's {name} (dropped the branch's binding)"
                  // The branch by NAME: this line is read by a person, and the id is a uuid.
                  let! label = LibDB.Branches.nameForId branchId
                  return
                    resultOk (Dval.string $"resolved {name} on \"{label}\": {kept}.")
                | Error e -> return resultError (Dval.string e)
              | other ->
                return
                  resultError (
                    Dval.string
                      $"unknown choice \"{other}\" -- use \"mine\" or \"theirs\""
                  )
            with ex ->
              return resultError (Dval.string ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated }


    // IMPORT a branch (from a portable bundle): register it, store its ops effective=0 + tag the
    // frontier (NOT folded into main), and re-derive the per-name bases against THIS instance's main
    // (recordNameBases -- the base is the destination's fork point). Cross-instance "branches follow
    // me" (scm-spec.md 8). Returns count stored.
    { name = fn "scmImportBranchOps" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TString ""
          Param.make "name" TString ""
          Param.make "parent" TString ""
          Param.make
            "records"
            (TList(TTuple(TString, TString, [ TString ])))
            "(id, blobHex, originTs) triples" ]
      returnType = TypeReference.result TInt TString
      description =
        "Import a branch bundle: register + store its ops effective=0 + tag + re-base. Returns count."
      fn =
        let resultOk = Dval.resultOk KTInt KTString
        let resultError = Dval.resultError KTInt KTString
        (function
        | _,
          _,
          _,
          [ DString branchId; DString name; DString parent; DList(_, records) ] ->
          uply {
            try
              // Decode EVERY record BEFORE storing any of them, and before the branch is even registered.
              //
              // A bundle is a unit. A branch that imports three ops short is a branch that resolves
              // differently here than on the machine that sent it, with nothing downstream able to tell:
              // the ops it does have store fine, the bases re-derive fine, and the count comes back
              // positive. Silently-partial is the one outcome with no recovery path, where a hard failure
              // is merely a retry. (The bulk sync path skips-and-logs instead, which is right there:
              // one op of thousands, and the op comes round again. A branch does not.)
              //
              // The wire record's `ts` is the op's ORIGIN stamp and has to survive, exactly as on the sync
              // path. Dropping it and re-stamping locally would make this machine look like the author, so
              // two instances holding the branch would resolve LWW by who imported last, not who edited
              // last.
              let decoded =
                records
                |> List.map (fun d ->
                  match d with
                  | DTuple(DString id, DString hex, [ DString ts ]) ->
                    try
                      Ok(
                        LibDB.Queries.deserializeOp
                          (System.Guid.Parse id)
                          (System.Convert.FromHexString hex),
                        ts
                      )
                    with _ ->
                      Error id
                  | _ -> Error "(record was not an (id, blobHex, originTs) triple)")

              let undecodable =
                decoded
                |> List.choose (fun r ->
                  match r with
                  | Error id -> Some id
                  | Ok _ -> None)

              match undecodable with
              | bad :: _ ->
                return
                  resultError (
                    Dval.string
                      $"could not decode {List.length undecodable} of {List.length records} ops (first: {bad}). Nothing was imported."
                  )
              | [] ->
                let stamped =
                  decoded
                  |> List.choose (fun r ->
                    match r with
                    | Ok x -> Some x
                    | Error _ -> None)

                do! LibDB.Branches.createBranch branchId name parent
                let ops = stamped |> List.map fst
                let! n = LibDB.Branches.storeDeltaOpsStamped branchId stamped
                // Re-derive bases against THIS instance's parent state (the bundle's bases don't travel).
                do! LibDB.Branches.recordNameBases branchId parent ops
                return resultOk (Dval.int (bigint (int n)))
            with ex ->
              return resultError (Dval.string ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      capabilities = LibExecution.Capabilities.noCaps
      deprecated = NotDeprecated } ]


let builtins (pm : PT.PackageManager) : Builtins =
  LibExecution.Builtin.make [] (fns pm)
