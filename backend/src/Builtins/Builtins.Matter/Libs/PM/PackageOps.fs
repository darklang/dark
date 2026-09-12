module Builtins.Matter.Libs.PM.PackageOps

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Effects

module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module Builtin = LibExecution.Builtin
module PackageRefs = LibExecution.PackageRefs
module Dval = LibExecution.Dval
module VT = LibExecution.ValueType
module NR = LibExecution.RuntimeTypes.NameResolution
module BS = LibSerialization.Binary.Serialization

open Builtin.Shortcuts


let packageOpTypeName () =
  FQTypeName.fqPackage (PackageRefs.Type.LanguageTools.ProgramTypes.packageOp ())

let packageOpKT () = KTCustomType(packageOpTypeName (), [])


/// Author a BranchEvent op so what happened to a branch travels the way everything else does.
///
/// The projections are already updated by the caller's own SQL; this is not how the local store learns
/// what happened. It is how the OTHER machine learns. The fold is idempotent for these events (each sets a
/// column only when it is still NULL), so the op landing here as well changes nothing locally.
/// Returns the event op's id, so the caller can commit it with what it describes.
let private recordBranchEvent
  (branchId : PT.BranchId)
  (event : PT.BranchEventKind)
  : Ply<System.Guid> =
  uply {
    let at = System.DateTime.UtcNow.ToString("yyyy-MM-ddTHH:mm:ss.fffZ")
    let op = PT.PackageOp.BranchEvent(branchId, event, at)
    let! _ = LibDB.Inserts.insertAndApplyOps [ op ]
    return LibDB.Inserts.computeOpHash op
  }


/// The `(id, blobHex, originTs)` triples the sync builtins take, as strings. Anything not shaped
/// like one is dropped; the caller decides whether that is an error.
let private opRecords (records : List<Dval>) : List<string * string * string> =
  records
  |> List.choose (fun d ->
    match d with
    | DTuple(DString id, DString hex, [ DString ts ]) -> Some(id, hex, ts)
    | _ -> None)


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
        | _, _, _, [| DList(_vt, ops) |] ->
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
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "pmDuplicateDeclarations" 0
      typeParams = []
      parameters =
        [ Param.make "ops" (TList(TCustomType(NR.ok (packageOpTypeName ()), []))) "" ]
      returnType = TList TString
      description =
        "The names that more than one declaration in this batch would bind, as "
        + "\"fn Owner.Module.name\" strings. Stabilizing such a batch would store one "
        + "body under the other's hash, so authoring surfaces refuse it."
      fn =
        (function
        | _, _, _, [| DList(_vt, ops) |] ->
          uply {
            let ptOps = ops |> List.choose PT2DT.PackageOp.fromDT
            return
              LibDB.OpValidation.duplicateDeclarations ptOps
              |> List.map Dval.string
              |> Dval.list KTString
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
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
        | _, _, _, [| DList(_vt, ops) |] ->
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
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    { name = fn "scmAddOps" 0
      typeParams = []
      parameters =
        [ Param.make
            "branchId"
            TUuid
            "the branch these ops land on (main is `SCM.Branch.mainBranchId`). Passed rather than ambient so a caller can author onto a branch it isn't sitting on -- which is what sync does"
          Param.make "ops" (TList(TCustomType(NR.ok (packageOpTypeName ()), []))) "" ]
      returnType = TypeReference.result TInt TString
      description =
        "Add package ops to <param branchId>, uncommitted. Returns the "
        + "number inserted; duplicates are skipped, since an op's id is its content."
      fn =
        let resultOk = Dval.resultOk KTInt KTString
        let resultError = Dval.resultError KTInt KTString
        (function
        | exeState, vm, _, [| DUuid branchId; DList(_vtTODO, ops) |] ->
          uply {
            try
              let ops = ops |> List.choose PT2DT.PackageOp.fromDT

              let branchId = PT.BranchId.Id branchId

              // Branch: the edit lands on the BRANCH, stored effective=0 and tagged, never folded into
              // main. Hashes stabilize exactly as the main path does, or a merged value's
              // `package_values` (keyed by AddValue) and `locations` (keyed by SetName) disagree and the
              // value cannot be found.
              if not branchId.IsMain then
                // Refuse, rather than write: a merged or archived branch must not be REVIVED by an
                // edit landing on it. A workbench still holding the id after a merge in another shell
                // would otherwise put its next edit on a branch nothing will ever merge again.
                match! LibDB.Branches.isFinished branchId with
                | true ->
                  return
                    resultError (
                      Dval.string
                        $"branch {branchId} has been merged or archived; `dark switch <name>` starts a new one"
                    )
                | false ->

                  do! LibDB.Branches.registerIfNew branchId "" PT.BranchId.Main

                  let stabilized = LibDB.HashStabilization.computeRealHashes ops
                  let! stabilized = LibDB.Branches.restateReverts branchId stabilized
                  let! n = LibDB.Branches.storeDeltaOps branchId stabilized
                  // The parent's current hash per name touched, so a later merge can tell whether the
                  // parent moved the same name.
                  let! parentId = LibDB.Branches.parentOf branchId
                  do! LibDB.Branches.recordNameBases branchId parentId stabilized
                  // Content (Add*, never SetName) folds into the shared content tables; the NAME layer is
                  // what a branch keeps to itself. Needed so an expression-valued branch value has an
                  // rt_dval to eval, and so propagation can see the branch item's dependency edges.
                  let contentOps =
                    stabilized
                    |> List.filter (fun op ->
                      match op with
                      | PT.PackageOp.AddValue _
                      | PT.PackageOp.AddFn _
                      | PT.PackageOp.AddType _ -> true
                      | _ -> false)
                  if not (List.isEmpty contentOps) then
                    do! LibDB.PackageOpPlayback.applyBranchContentOps contentOps
                    let builtins : Builtins =
                      { values = exeState.values.builtIn
                        fns = exeState.fns.builtIn }
                    // A branch's own bodies, arriving from guest code: same bound as the
                    // main-branch path below.
                    let! _ =
                      LibDB.Seed.evaluateAllValues
                        (LibDB.Seed.EvaluationAuthority.underInstancePolicyAnd
                          exeState.accountID
                          vm.activeAccess)
                        builtins
                        LibDB.PackageManager.rt
                    ()
                  // Move the overlay only for the branch this process is on; writing to another branch
                  // must not change what this caller resolves against. Other branches are memoized, so
                  // forget them rather than leave a stale answer.
                  if LibDB.PackageManager.currentBranchId () = branchId then
                    let! all = LibDB.Branches.loadDeltaOps branchId
                    LibDB.PackageManager.setBranchOverlay all
                  else
                    LibDB.PackageManager.forgetBranch branchId
                  return resultOk (Dval.int (bigint (int n)))

              else
                // Stabilize before inserting. Raw ops carry provisional hashes, so their SetName
                // targets would too, and the only thing that repairs those is `WipRefresh.refresh`
                // rewriting the ENTIRE log.
                let stabilizedOps = LibDB.HashStabilization.computeRealHashes ops

                // These package changes came from running Dark code, so they land uncommitted AND
                // through the guarded path: reserved bundled names and placeholder hashes are
                // rejected here. `commit` is a separate step.
                match! LibDB.Inserts.insertUntrustedOps stabilizedOps with
                | Error reason -> return resultError (Dval.string reason)
                | Ok insertedCount ->

                  // Refresh the EXISTING draft: re-resolve names and recompute SCC-aware hashes now
                  // that new items exist. This is the forward-ref case: an earlier draft item that
                  // references THIS newly-authored one.
                  let! _refreshed = LibDB.WipRefresh.refresh pm

                  // Evaluate values whose runtime form is still missing.
                  // New values start with NULL `rt_dval`; doing this now lets
                  // later operations use them without restarting the CLI.
                  // These bodies just arrived from guest code, so evaluating
                  // them is bounded by the instance policy and by this caller's
                  // access — otherwise `val x = <denied effect>` performs the
                  // effect that `eval <denied effect>` refuses.
                  let! evaluated =
                    LibDB.Seed.evaluateAllValues
                      (LibDB.Seed.EvaluationAuthority.underInstancePolicyAnd
                        exeState.accountID
                        vm.activeAccess)
                      exeState.builtins
                      LibDB.PackageManager.rt

                  // Report only failures from this call; evaluation also sweeps
                  // unrelated pending values. Match locations as well as hashes,
                  // because refresh may resolve a name and recompute its hash
                  // while its location remains stable.
                  let addedValueHashes =
                    ops
                    |> List.choose (fun op ->
                      match op with
                      | PT.PackageOp.AddValue value -> Some value.hash
                      | _ -> None)
                    |> Set.ofList

                  let addedValueLocations =
                    ops
                    |> List.choose (fun op ->
                      match op with
                      | PT.PackageOp.SetName(location, PT.PackageValue _, _) ->
                        Some(LibDB.PackageLocation.toFQN location)
                      | _ -> None)
                    |> Set.ofList

                  let ownFailures =
                    match evaluated with
                    | Ok() -> []
                    | Error errors ->
                      errors
                      |> List.filter (fun e ->
                        let byHash =
                          match e.hash with
                          | Some hash -> Set.contains hash addedValueHashes
                          | None -> false
                        byHash || Set.contains e.location addedValueLocations)

                  match ownFailures with
                  | [] -> return resultOk (Dval.int (bigint insertedCount))
                  | failures ->
                    return
                      resultError (
                        Dval.string (
                          failures
                          |> List.map LibDB.Seed.ValueEvaluationError.toString
                          |> String.concat "\n"
                        )
                      )
            with ex ->
              return resultError (Dval.string ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageWrite ]
      deprecated = NotDeprecated }


    // Which branch is THIS process on. Set by `--branch <id>` or the persistent
    // `current_branch`, both resolved in the CLI entry point before any Dark runs. Dark can't read it any
    // other way: it's process state, not a row, and `configGet "current_branch"` misses the flag form.
    { name = fn "scmCurrentBranch" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TUuid
      description = "The branch this process is on, as an id."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          uply { return DUuid (LibDB.PackageManager.currentBranchId ()).Guid }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
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
            TUuid
            "the branch id to parent a NEW branch to; `SCM.Branch.mainBranchId` at top level" ]
      returnType = TTuple(TUuid, TBool, [])
      description =
        "Resolves a branch name to its id, creating the branch if the name has no "
        + "live one. Returns (id, wasCreated)."
      fn =
        (function
        | _, _, _, [| DString name; DUuid parentIdGuid |] ->
          uply {
            let parentId = PT.BranchId.Id parentIdGuid
            let! (id, created) = LibDB.Branches.resolveOrCreate name parentId
            return DTuple(DUuid id.Guid, DBool created, [])
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    // Change which branch THIS process is on, without restarting it.
    //
    // Boot (`--branch`, or `current_branch`) covers the one-shot case, but it can't be the only way in:
    // the interactive REPL is a single long-lived process, so `ops switch` there has to move the overlay
    // that name resolution and authoring actually read. Writing the config key alone would leave the
    // display saying one thing and the behaviour doing another.
    //
    // Returns the branch it ended up on, so a caller reports what happened rather than what it asked for.
    { name = fn "scmSelectBranch" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "the branch to move this process to" ]
      returnType = TUuid
      description =
        "Moves this process onto <param branchId>, loading that branch's delta ops "
        + "as the overlay used for name resolution and execution. Returns the branch "
        + "now active."
      fn =
        (function
        | _, _, _, [| DUuid branchId |] ->
          uply {
            let branchId = PT.BranchId.Id branchId
            LibDB.PackageManager.selectBranch branchId
            return DUuid branchId.Guid
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    // Does this op bind a name, without decoding it?
    //
    // A sync import plans conflicts by decoding every incoming op and asking which name it moved. Only
    // `SetName` and `Decision` move one; `AddFn` and friends carry content and answer nothing, and they
    // are also the big ones, so nearly all of that decoding was spent producing "no". This reads the tag
    // and returns, which measured a fifth of the pull off on its own.
    { name = fn "packageOpBindsAName" 0
      typeParams = []
      parameters = [ Param.make "blob" TBlob "a package_ops op_blob" ]
      returnType = TBool
      description =
        "Whether <param blob> is an op that binds a name (SetName or Decision), read from "
        + "its tag without decoding it. False for content ops and for anything unreadable."
      fn =
        function
        | exeState, _, _, [| DBlob blobRef |] ->
          uply {
            let! bytes = LibExecution.Blob.readBytes exeState blobRef
            return DBool(LibDB.Queries.opBindsAName bytes)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // Decode one op_blob into a PackageOp, or None when this build cannot read it. Option rather
    // than raise, and deliberately with no raising variant: a synced store holds ops from other
    // builds in its own log on purpose, so every local reader meets them.
    { name = fn "packageOpFromBlobOption" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid ""; Param.make "blob" TBlob "" ]
      returnType =
        TypeReference.option (TCustomType(NR.ok (packageOpTypeName ()), []))
      description =
        "Deserialize an op_blob, or None when this build cannot read it. For blobs "
        + "received from a peer, where an unreadable one must be skipped rather than "
        + "fatal."
      fn =
        function
        | exeState, _, _, [| DUuid id; DBlob blobRef |] ->
          uply {
            let! bytes = LibExecution.Blob.readBytes exeState blobRef

            let decoded =
              try
                Some(LibDB.Queries.deserializeOp id bytes)
              with _ ->
                None

            match decoded with
            | Some op ->
              return
                Dval.optionSome
                  (KTCustomType(packageOpTypeName (), []))
                  (PT2DT.PackageOp.toDT op)
            | None -> return Dval.optionNone (KTCustomType(packageOpTypeName (), []))
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
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
        | _, _, _, [| DString commitHash; DList(_, records) |] ->
          uply {
            try
              let! n = LibDB.Inserts.importOpsBulk commitHash (opRecords records)
              let! _ = LibDB.Seed.applyUnappliedOps () // fold the just-inserted (effective=1) ops
              return resultOk (Dval.int (bigint n))
            with ex ->
              return resultError (Dval.string ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageWrite ]
      deprecated = NotDeprecated }


    // SERVER store: bulk-insert ops + record ownership in one transaction, THEN fold.
    //
    // It folded nothing until 2026-09-12, on the grounds that a server serves blobs rather than
    // projections. That also meant it could not see what it hosted: `/m` showed "Nothing here"
    // for packages every client had, a seed could not be cut from the hosted set, and pushing new
    // code to a server could never change what it ran. All three are wanted, so it folds.
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
        | _, _, _, [| DString owner; DList(_, records) |] ->
          uply {
            try
              let! n = LibDB.Inserts.storeOpsWithOwner owner (opRecords records)
              // Fold what just arrived, so the projection a seed and `/m` read is current. Cheap:
              // ~116us an op, and a push is tens of ops.
              let! _ = LibDB.Seed.applyUnappliedOps ()
              return resultOk (Dval.int (bigint n))
            with ex ->
              return resultError (Dval.string ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageWrite ]
      deprecated = NotDeprecated }
    // One page of the sync wire format, rendered straight from the database.
    { name = fn "scmExportPageJson" 0
      typeParams = []
      parameters =
        [ Param.make "sinceSeq" TInt64 "Return ops after this rowid"
          Param.make "limit" TInt64 "How many ops at most"
          Param.make "formatVersion" TInt64 "Wire format version to declare"
          Param.make "darkBuild" TString "Build that wrote this bundle"
          Param.make "kernelHash" TString "ABI fingerprint of that build"
          Param.make "owner" TString "This instance's identity" ]
      returnType = TTuple(TString, TInt64, [])
      description =
        "One page of the sync wire format as JSON, plus the cursor to hand back. "
        + "Reads and encodes the ops without turning any of them into Dark values."
      fn =
        function
        | _,
          _,
          _,
          [| DInt64 sinceSeq
             DInt64 limit
             DInt64 formatVersion
             DString darkBuild
             DString kernelHash
             DString owner |] ->
          uply {
            let! (json, cursor) =
              LibDB.Queries.exportPageJson
                sinceSeq
                limit
                formatVersion
                darkBuild
                kernelHash
                owner

            return DTuple(DString json, DInt64 cursor, [])
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    { name = fn "scmGetCommitNamedOps" 0
      typeParams = []
      parameters =
        [ Param.make "commitHash" TString "Commit hash"
          Param.make "limit" TInt "How many ops to return" ]
      returnType =
        TTuple(TList(TCustomType(NR.ok (packageOpTypeName ()), [])), TInt, [])
      description =
        "The ops in a commit that name or deprecate something, capped at "
        + "<param limit>, plus how many there are in total. A commit can hold "
        + "tens of thousands of ops and a caller showing a summary wants a "
        + "dozen, so the cap applies before they become Dark values."
      fn =
        function
        | _, vm, _, [| DString commitHash; DInt limit |] ->
          uply {
            let! ops = LibDB.Queries.getCommitOps (PT.Hash commitHash)
            let named =
              ops
              |> List.filter (fun op ->
                match op with
                | PT.PackageOp.SetName _
                | PT.PackageOp.Unbind _
                | PT.PackageOp.Deprecate _ -> true
                | _ -> false)
            let shown =
              named
              |> List.truncate (max 0 (intToInt32 vm limit))
              |> List.map PT2DT.PackageOp.toDT
            return
              DTuple(
                Dval.list (packageOpKT ()) shown,
                Dval.int (bigint (List.length named)),
                []
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }
    // The REBUILD half of a draft rewrite, and the reason it is not in Dark: it re-mints every surviving
    // op's id (hashing) and re-inserts with the original stamps, then re-folds. The delete it performs
    // spares ops this build cannot decode, BY ID, which is the invariant that stops authoring eating a
    // peer's synced work. Dark decides what survives; this executes it.
    { name = fn "scmRebuildDraftKeeping" 0
      typeParams = []
      parameters =
        [ Param.make "keptIds" (TList TString) "op ids that survive the rewrite" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Delete main's uncommitted ops and re-insert the ones named by <param "
        + "keptIds>, preserving their stamps, then re-fold. Ops this build cannot "
        + "decode are never deleted. Ok on success; Error with the message otherwise."
      fn =
        (function
        | _, _, _, [| DList(_, ids) |] ->
          uply {
            try
              // Every id must parse. This list is what SURVIVES a delete of main's whole draft, so
              // dropping an unreadable one silently WIDENS the delete: one malformed id would be one
              // op deleted for good. Refuse the call instead.
              let parsed =
                ids
                |> List.map (fun d ->
                  match d with
                  | DString s ->
                    match System.Guid.TryParse s with
                    | true, g -> Ok g
                    | _ -> Error s
                  | other -> Error(string other))

              match
                parsed
                |> List.tryPick (function
                  | Error s -> Some s
                  | Ok _ -> None)
              with
              | Some bad ->
                return
                  Dval.resultError
                    KTUnit
                    KTString
                    (DString $"not an op id: {bad}; nothing was changed")
              | None ->

                let kept =
                  parsed
                  |> List.choose (function
                    | Ok g -> Some g
                    | Error _ -> None)
                  |> Set.ofList

                do! LibDB.Draft.rebuild kept
                return Dval.resultOk KTUnit KTString DUnit
            with e ->
              return Dval.resultError KTUnit KTString (DString e.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated }

    // Dark edits `locations` directly on the surgical discard path, which is the one place outside the
    // fold that does. The in-memory caches key on what that table says, so whoever changes it has to say
    // so; F# does this inline (`Caching.invalidateAll`), and Dark needs the same reach.
    { name = fn "scmInvalidateCaches" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TUnit
      description =
        "Drop the in-memory package caches, after a write that changed `locations` "
        + "without going through the fold."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          uply {
            LibDB.Caching.invalidateAll ()
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }

    // ARCHIVING a branch travels, for the same reason merging does: on the other machine the branch is
    // still sitting there looking like live work. The archive itself is Dark's -- `SCM.Branches.archive`
    // owns that column and has already written it -- so all this does is author the op that says so.
    // Idempotent on arrival (the fold sets `archived_at` only while it is NULL), which is what makes it
    // safe for the authoring machine to fold its own event too.
    //
    // Separate from the merge path: only two events exist, and a `BranchEventKind`
    // crossing the boundary would need DU marshalling for one caller each.
    { name = fn "scmRecordBranchArchived" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "the branch that was archived" ]
      returnType = TUuid
      description =
        "Author the op that says this branch was archived, so other machines learn it."
      fn =
        (function
        | _, _, _, [| DUuid branchId |] ->
          uply {
            let branchId = PT.BranchId.Id branchId
            // The event's id, so the Dark caller can COMMIT it. Left uncommitted it sits in main's
            // draft, where `status` (which counts bindings) reads clean and the next unrelated commit
            // sweeps it up under a message about something else.
            let! eventId = recordBranchEvent branchId PT.Archived
            return DUuid eventId
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated }


    // Fold the ops a merge just made effective, and evaluate any values among them.
    //
    // The only part of merge in F#, and the part that has to be: replaying the op log into main's
    // projections, and evaluating a merged value so it has an `rt_dval` to run. Everything
    // around it -- whether a merge is allowed, which arm it takes, flipping the frontier effective,
    // marking it merged -- is decided and done in Dark (`SCM.PackageOps.mergeBranch`).
    { name = fn "scmApplyMergedOps" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TypeReference.result TUnit TString
      description =
        "Fold the newly-effective ops into main's projections and evaluate merged values."
      fn =
        (function
        | exeState, vm, _, [| DUnit |] ->
          uply {
            try
              let! _ = LibDB.Seed.applyUnappliedOps ()
              let builtins : Builtins =
                { values = exeState.values.builtIn; fns = exeState.fns.builtIn }
              // Ops that just folded in from a sync or an import: their bodies are code
              // from elsewhere, so evaluating them is bounded by the instance policy and
              // this caller's access.
              let! _ =
                LibDB.Seed.evaluateAllValues
                  (LibDB.Seed.EvaluationAuthority.underInstancePolicyAnd
                    exeState.accountID
                    vm.activeAccess)
                  builtins
                  LibDB.PackageManager.rt
              return Dval.resultOk KTUnit KTString DUnit
            with ex ->
              return Dval.resultError KTUnit KTString (DString ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated }


    { name = fn "scmContentOpId" 0
      typeParams = []
      parameters =
        [ Param.make "kind" TString "'fn', 'type' or 'value'"
          Param.make "hash" TString "the content hash" ]
      returnType = TUuid
      description =
        "The id of the Add op that adds this content. An Add op is identified by what it adds, so the id follows from the kind and the hash alone; a branch bundle uses this to carry the content its names point at."
      fn =
        (function
        | _, _, _, [| DString kind; DString hash |] ->
          let tag =
            match kind with
            | "fn" -> 0uy
            | "type" -> 1uy
            | "value" -> 2uy
            | other ->
              Exception.raiseInternal
                "scmContentOpId: unknown kind"
                [ "kind", other ]
          let (PT.Hash h) =
            LibSerialization.Hashing.Hashing.contentOpHash tag (PT.Hash hash)
          Ply(DUuid(System.Guid(System.Convert.FromHexString(h)[0..15])))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }


    { name = fn "scmRecordBranchMerged" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "the branch that was merged"
          Param.make "ops" (TList TUuid) "the ids of the ops the merge moved" ]
      returnType = TUuid
      description =
        "Author the op that says this branch was merged, naming what it moved, so other machines learn it. Returns the event op's id."
      fn =
        (function
        | _, _, _, [| DUuid branchId; DList(_, ops) |] ->
          uply {
            let ids =
              ops
              |> List.choose (fun d ->
                match d with
                | DUuid g -> Some g
                | _ -> None)
            let! eventId =
              recordBranchEvent (PT.BranchId.Id branchId) (PT.Merged ids)
            return DUuid eventId
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated }


    // Store ONE op on a branch under a stamp the caller chose, effective=0 and tagged like any other
    // branch op. Serializing and hashing is the whole of what F# is here for; WHICH op, and what stamp
    // it deserves, are decided in Dark (`SCM.Branches.resolveKeepMine`).
    //
    // The stamp is a parameter rather than "now" because these ops lose or win by it. `storeDeltaOps`
    // stamps for you and is right for authoring; this is for an op whose stamp is the point.
    { name = fn "scmStoreBranchOpStamped" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "the branch the op lands on"
          Param.make
            "op"
            (TCustomType(NR.ok (packageOpTypeName ()), []))
            "the op to store"
          Param.make "stamp" TString "its origin_ts, which is what LWW compares" ]
      returnType = TypeReference.result TInt TString
      description =
        "Store one op on a branch under the given stamp. Returns the number stored."
      fn =
        let resultOk = Dval.resultOk KTInt KTString
        let resultError = Dval.resultError KTInt KTString
        (function
        | _, _, _, [| DUuid branchId; opDval; DString stamp |] ->
          uply {
            try
              match PT2DT.PackageOp.fromDT opDval with
              | None -> return resultError (Dval.string "not a package op")
              | Some op ->
                let! n =
                  LibDB.Branches.storeDeltaOpsStamped
                    (PT.BranchId.Id branchId)
                    [ (op, stamp) ]
                return resultOk (Dval.int (bigint (int n)))
            with ex ->
              return resultError (Dval.string ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated }


    // IMPORT a branch (from a portable bundle): register it, store its ops effective=0 + tag the
    // frontier (NOT folded into main), and re-derive the per-name bases against THIS instance's main
    // (recordNameBases -- the base is the destination's fork point). Cross-instance "branches follow
    // me". Returns count stored.
    { name = fn "scmImportBranchOps" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid ""
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
        | exeState,
          vm,
          _,
          [| DUuid branchId; DString name; DString parentText; DList(_, records) |] ->
          uply {
            let branchId = PT.BranchId.Id branchId
            // The parent arrives inside a peer's bundle, so it is text this process did not write.
            // A value that is not an id means main, the same as a branch with no parent recorded;
            // raising here would fail an import over a field that is only used for the parent link.
            let parent =
              PT.BranchId.Parse parentText |> Option.defaultValue PT.BranchId.Main
            try
              // An op this build cannot decode is stored RAW and inert rather than refusing the bundle,
              // the way main sync stores such ops: present, a later build reads it. A branch three ops
              // short does resolve differently than on the sender, but that holds for main sync too,
              // and refusing would leave the branch absent altogether.
              //
              // The record's `ts` is the op's ORIGIN stamp and must survive; re-stamping locally would make
              // this machine look like the author and resolve LWW by who imported last.
              let parsed =
                opRecords records
                |> List.map (fun (id, hex, ts) ->
                  (System.Guid.Parse id, System.Convert.FromHexString hex, ts))

              // Shape first, then decode: the stored-inert note below claims ops were kept, and a
              // malformed bundle is about to be refused whole.
              if List.length parsed <> List.length records then
                return
                  resultError (
                    Dval.string
                      "a record was not an (id, blobHex, originTs) triple; nothing was imported"
                  )
              else

                let decoded =
                  parsed
                  |> List.map (fun (id, blob, ts) ->
                    ((id, blob, ts), BS.PT.PackageOp.tryDeserialize id blob))
                let stamped =
                  decoded
                  |> List.choose (fun ((_, _, ts), op) ->
                    op |> Option.map (fun op -> (op, ts)))
                let rawRecords =
                  decoded
                  |> List.choose (fun (record, op) ->
                    if Option.isNone op then Some record else None)

                if not (List.isEmpty rawRecords) then
                  System.Console.Error.WriteLine(
                    $"note: {List.length rawRecords} op(s) in this bundle were written in a format this build cannot "
                    + "read, and are stored inert. They are kept, not dropped, so a later build can apply them."
                  )

                do! LibDB.Branches.createBranch branchId name parent
                let ops = stamped |> List.map fst
                // Tagged 'import', not 'op': these are somebody else's ops arriving, and the
                // merge gate has to tell them from unreviewed work of your own.
                let! nDecoded =
                  LibDB.Branches.storeDeltaOpsStampedFrom "import" branchId stamped
                let! nRaw = LibDB.Branches.storeDeltaBlobsStamped branchId rawRecords
                let n = nDecoded + nRaw
                // Re-derive bases against THIS instance's parent state (the bundle's bases don't travel).
                do! LibDB.Branches.recordNameBases branchId parent ops

                // Fold the content ops (Add*, never SetName) exactly as authoring
                // onto a branch does -- see scmAddOps: the overlay binds names, not
                // bodies, so without this the imported branch is unusable.
                let contentOps =
                  ops
                  |> List.filter (fun op ->
                    match op with
                    | PT.PackageOp.AddValue _
                    | PT.PackageOp.AddFn _
                    | PT.PackageOp.AddType _ -> true
                    | _ -> false)
                if not (List.isEmpty contentOps) then
                  do! LibDB.PackageOpPlayback.applyBranchContentOps contentOps
                  let builtins : Builtins =
                    { values = exeState.values.builtIn; fns = exeState.fns.builtIn }
                  let! _ =
                    LibDB.Seed.evaluateAllValues
                      (LibDB.Seed.EvaluationAuthority.underInstancePolicyAnd
                        exeState.accountID
                        vm.activeAccess)
                      builtins
                      LibDB.PackageManager.rt
                  ()

                // A merge event for this branch may already be in the log, folded against a store that
                // had none of these ops (pull main, then pull the branch: the natural order). It
                // DEFERRED itself for exactly this moment, so re-arm it and fold now rather than at
                // the next startup, or the branch reads as live work that is already merged.
                do! LibDB.Branches.undeferBranchEvents branchId
                let! _ = LibDB.Seed.applyUnappliedOps ()

                // An overlay this process is already holding predates the import, so drop it rather than
                // let a memoized read answer for the branch as it was before its ops arrived.
                if LibDB.PackageManager.currentBranchId () = branchId then
                  let! all = LibDB.Branches.loadDeltaOps branchId
                  LibDB.PackageManager.setBranchOverlay all
                else
                  LibDB.PackageManager.forgetBranch branchId
                return resultOk (Dval.int (bigint (int n)))
            with ex ->
              return resultError (Dval.string ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      // Importing a branch REGISTERS it, folds its content and records its name bases. Behind
      // `dark branch pull` and `dark branch import`, both of which write.
      callEffects = set [ Effect.PackageRead; Effect.PackageWrite ]
      deprecated = NotDeprecated } ]


/// One constant, two languages. Dark compares against this rather than spelling main's id, for the same
/// reason F# compares against `BranchId.Main`: a spelling written twice drifts, and both times it did.
let values () : List<BuiltInValue> =
  [ { name = value "scmMainBranchId" 0
      typ = TUuid
      description =
        "Main's branch id: well-known, because main exists before anything creates it."
      deprecated = NotDeprecated
      body = DUuid PT.BranchId.Main.Guid } ]


let builtins (pm : PT.PackageManager) : Builtins =
  LibExecution.Builtin.make (values ()) (fns pm)
