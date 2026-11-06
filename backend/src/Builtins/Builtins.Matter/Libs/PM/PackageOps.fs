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


// TODO: review/reconsider the accessibility of these fns

/// The evaluation failures that belong to THIS call's values. Evaluation also sweeps unrelated
/// pending values, so match on location as well as hash: a refresh may move a value's hash while
/// its location stays put.
let private ownEvaluationFailures
  (ops : List<PT.PackageOp>)
  (evaluated : Result<unit, List<LibDB.Seed.ValueEvaluationError>>)
  : List<LibDB.Seed.ValueEvaluationError> =
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

let private failureText (failures : List<LibDB.Seed.ValueEvaluationError>) : string =
  failures
  |> List.map LibDB.Seed.ValueEvaluationError.toString
  |> String.concat "\n"


/// Author <param ops> onto <param branchId>, uncommitted: THE authoring path, behind `scmAddOps` and
/// behind every SCM action that authors an op of its own (settling a conflict). Ok is how many ops were
/// newly stored (an op the log already holds counts 0); Error is why nothing, or not everything, landed.
///
/// On main: stabilize, insert through the guarded path, refresh the draft, evaluate new values. On a
/// branch: stabilize, restate reverts, store inert and tagged, record name bases, fold the content
/// only, evaluate, and move the overlay if this process stands on the branch.
let authorOps
  (exeState : ExecutionState)
  (vm : VMState)
  (pm : PT.PackageManager)
  (branchId : PT.BranchId)
  (ops : List<PT.PackageOp>)
  : Ply<Result<int64, string>> =
  uply {
    // New edits sort after everything already stored, commits included, whatever this process's
    // clock last saw.
    do! LibDB.Queries.ensureAuthoringClockIsCurrent ()

    // Branch: the edit lands on the BRANCH, stored effective=0 and tagged, never folded into main.
    // Hashes stabilize exactly as the main path does, or a merged value's `package_values` (keyed by
    // AddValue) and `locations` (keyed by SetName) disagree and the value cannot be found.
    if not branchId.IsMain then
      // Refuse, rather than write: a merged or archived branch must not be REVIVED by an edit landing
      // on it. A workbench still holding the id after a merge in another shell would otherwise put its
      // next edit on a branch nothing will ever merge again.
      match! LibDB.Branches.isFinished branchId with
      | true ->
        return
          Error
            $"branch {branchId} has been merged or archived; `dark switch <name>` starts a new one"
      | false ->
        do! LibDB.Branches.registerIfNew branchId "" PT.BranchId.Main

        let stabilized = LibDB.HashStabilization.computeRealHashes ops
        let! referenceNames =
          LibDB.ReferenceNames.captureForStorage branchId stabilized
        // Capture while each declaration still has its SetName. A repeated revert becomes an
        // Override, which no longer forms that pair.
        let! stabilized = LibDB.Branches.restateReverts branchId stabilized
        let stabilized = stabilized @ referenceNames
        let! n = LibDB.Branches.storeDeltaOps branchId stabilized
        // The parent's current hash per name touched, so a later merge can tell whether the parent
        // moved the same name.
        let! parentId = LibDB.Branches.parentOf branchId
        do! LibDB.Branches.recordNameBases branchId parentId stabilized
        // Content (Add*, never SetName) folds into the shared content tables; the NAME layer is what a
        // branch keeps to itself. Needed so an expression-valued branch value has an rt_dval to eval,
        // and so propagation can see the branch item's dependency edges.
        let contentOps =
          stabilized
          |> List.filter (fun op ->
            match op with
            | PT.PackageOp.AddValue _
            | PT.PackageOp.AddFn _
            | PT.PackageOp.AddType _
            | PT.PackageOp.AddTrait _
            | PT.PackageOp.AddTraitImpl _
            | PT.PackageOp.AddTest _ -> true
            | _ -> false)
        let! branchFailures =
          uply {
            if List.isEmpty contentOps then
              return []
            else
              do! LibDB.PackageOpPlayback.applyBranchContentOps contentOps
              let builtins : Builtins =
                { values = exeState.values.builtIn; fns = exeState.fns.builtIn }
              // A branch's own bodies, arriving from guest code: same bound as the main-branch path.
              let! evaluated =
                LibDB.Seed.evaluateAllValues
                  (LibDB.Seed.EvaluationAuthority.underInstancePolicyAnd
                    exeState.accountID
                    vm.activeAccess)
                  builtins
                  LibDB.PackageManager.rt
              return ownEvaluationFailures ops evaluated
          }
        // Move the overlay only for the branch this process is on; writing to another branch must not
        // change what this caller resolves against. Other branches are memoized, so forget them rather
        // than leave a stale answer.
        if LibDB.PackageManager.currentBranchId () = branchId then
          let! all = LibDB.Branches.loadDeltaOps branchId
          LibDB.PackageManager.setBranchOverlay all
        else
          LibDB.PackageManager.forgetBranch branchId
        match branchFailures with
        | [] -> return Ok n
        | failures -> return Error(failureText failures)
    else
      // Stabilize before inserting. Raw ops carry provisional hashes, so their SetName targets would
      // too, and the only thing that repairs those is `WipRefresh.refresh` rewriting the ENTIRE log.
      let stabilizedOps = LibDB.HashStabilization.computeRealHashes ops
      // The store keeps one body per content hash. Two callers with that hash can still use
      // different aliases, such as `double` and `twice`. Save each caller's reference names before
      // storing the shared body.
      let! referenceNames =
        LibDB.ReferenceNames.captureForStorage branchId stabilizedOps
      let stabilizedOps = stabilizedOps @ referenceNames

      // These package changes came from running Dark code, so they land uncommitted AND through the
      // guarded path: reserved bundled names and placeholder hashes are rejected here.
      match! LibDB.Inserts.insertUntrustedOps stabilizedOps with
      | Error reason -> return Error reason
      | Ok insertedCount ->
        // Refresh the EXISTING draft: re-resolve names and recompute SCC-aware hashes now that new
        // items exist. This is the forward-ref case: an earlier draft item that references THIS
        // newly-authored one.
        let! _refreshed = LibDB.WipRefresh.refresh pm

        // Evaluate values whose runtime form is still missing, bounded by the instance policy and by
        // this caller's access -- otherwise `val x = <denied effect>` performs the effect that
        // `eval <denied effect>` refuses.
        let! evaluated =
          LibDB.Seed.evaluateAllValues
            (LibDB.Seed.EvaluationAuthority.underInstancePolicyAnd
              exeState.accountID
              vm.activeAccess)
            exeState.builtins
            LibDB.PackageManager.rt

        match ownEvaluationFailures ops evaluated with
        | [] -> return Ok insertedCount
        | failures -> return Error(failureText failures)
  }


/// Evaluate the values a merge just folded into main, bounded by the instance policy and this
/// caller's access: their bodies are code from elsewhere. What fails stays unevaluated, as it would
/// after any fold.
let evaluateLandedValues (exeState : ExecutionState) (vm : VMState) : Ply<unit> =
  uply {
    let builtins : Builtins =
      { values = exeState.values.builtIn; fns = exeState.fns.builtIn }
    let! _ =
      LibDB.Seed.evaluateAllValues
        (LibDB.Seed.EvaluationAuthority.underInstancePolicyAnd
          exeState.accountID
          vm.activeAccess)
        builtins
        LibDB.PackageManager.rt
    return ()
  }

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
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "pmResolveTraitCalls" 0
      typeParams = []
      parameters =
        [ Param.make "ops" (TList(TCustomType(NR.ok (packageOpTypeName ()), []))) "" ]
      returnType = TList(TCustomType(NR.ok (packageOpTypeName ()), []))
      description =
        "Writes onto each trait-method call in <param ops> the implementation it resolves "
        + "to, so a saved item goes on running what it was written against. Runs BEFORE "
        + "<fn pmStabilizeHashes>, because the choice is part of what the item is and has to "
        + "be hashed with it. A call whose self type is only known at run time comes back "
        + "unchanged."
      fn =
        (function
        | exeState, _, _, [| DList(_vt, ops) |] ->
          uply {
            let ptOps = ops |> List.choose PT2DT.PackageOp.fromDT
            let! resolved =
              TraitCalls.resolveTraitCalls exeState exeState.branchId pm false ptOps
            return
              Dval.list (packageOpKT ()) (resolved |> List.map PT2DT.PackageOp.toDT)
          }
        | _ -> incorrectArgs ())
      // Reads the store's implementations and stamps, so two calls on either side of someone
      // else's `impl` do not agree.
      previewable = Impure
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
      returnType = TypeReference.result TInt64 TString
      description =
        "Add package ops to <param branchId>, uncommitted. Returns the "
        + "number inserted; duplicates are skipped, since an op's id is its content."
      fn =
        let resultOk = Dval.resultOk KTInt64 KTString
        let resultError = Dval.resultError KTInt64 KTString
        (function
        | exeState, vm, _, [| DUuid branchId; DList(_vtTODO, ops) |] ->
          uply {
            try
              let ops = ops |> List.choose PT2DT.PackageOp.fromDT
              match! authorOps exeState vm pm (PT.BranchId.Id branchId) ops with
              | Ok n -> return resultOk (Dval.dint64 (int64 n))
              | Error message -> return resultError (Dval.string message)
            with ex ->
              return resultError (Dval.string ex.Message)
          }
        | _ -> incorrectArgs ())
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
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
      deprecated = NotDeprecated }



    // What `SCM.StoreHealth` reports: counts only. Which of them a reload fixes is decided in Dark.
    { name = fn "scmHealth" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TCustomType(
          NR.ok (FQTypeName.fqPackage (PackageRefs.Type.SCM.storeHealth ())),
          []
        )
      description =
        "Counts what is wrong with the store itself: ops stamped in the future, op tags "
        + "naming a missing op or branch, ops that never folded, and live names bound to "
        + "missing content. A count that cannot be taken is 0."
      fn =
        (function
        | _, _, _, [| DUnit |] ->
          uply {
            let! c = LibDB.StoreHealth.counts ()
            let typeName = FQTypeName.fqPackage (PackageRefs.Type.SCM.storeHealth ())
            return
              DRecord(
                typeName,
                typeName,
                [],
                Map
                  [ "futureStamped", DInt64 c.futureStamped
                    "orphanTags", DInt64 c.orphanTags
                    "taggedToNothing", DInt64 c.taggedToNothing
                    "unfolded", DInt64 c.unfolded
                    "danglingBindings", DInt64 c.danglingBindings ]
              )
          }
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = set [ Effect.PackageRead ]
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
      previewable = Impure
      callEffects = set [ Effect.Runtime ]
      deprecated = NotDeprecated }



    ]


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
