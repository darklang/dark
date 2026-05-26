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
      deprecated = NotDeprecated }


    { name = fn "scmAddOps" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch to add ops to"
          Param.make "ops" (TList(TCustomType(NR.ok (packageOpTypeName ()), []))) "" ]
      returnType = TypeReference.result TInt64 TString
      description =
        "Add package ops to the database as WIP (uncommitted) on the given branch.
        Returns the number of inserted ops on success (duplicates are skipped), or an error message on failure.
        Use scmCommitWipOpsByIds to commit WIP ops."
      fn =
        let resultOk = Dval.resultOk KTInt64 KTString
        let resultError = Dval.resultError KTInt64 KTString
        (function
        | exeState, _, _, [ DUuid branchId; DList(_vtTODO, ops) ] ->
          uply {
            try
              let ops = ops |> List.choose PT2DT.PackageOp.fromDT

              // All ops are added as WIP - use scmCommitWipOpsByIds to commit them
              let! insertedCount = LibDB.Inserts.insertAndApplyOpsAsWip branchId ops

              // Auto-refresh existing WIP items: re-resolve names and
              // recompute SCC-aware hashes now that new items exist
              let! _refreshed = LibDB.WipRefresh.refresh pm branchId

              // Populate `rt_dval` for any package_values rows still
              // NULL after this insert+refresh. `applyAddValue` always
              // inserts NULL and Phase-3 `evaluateAllValues` only runs
              // at startup when there are unapplied ops. Without this
              // step, a CLI-added value that references another value
              // (qualified or bare) would fail at eval with a NULL
              // rt_dval until the next cold restart.
              let builtins : Builtins =
                { values = exeState.values.builtIn; fns = exeState.fns.builtIn }
              let! _ = LibDB.Seed.evaluateAllValues builtins LibDB.PackageManager.rt

              return resultOk (Dval.int64 insertedCount)
            with ex ->
              return resultError (Dval.string ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetRecentOps" 0
      typeParams = []
      parameters = [ Param.make "limit" TInt64 "" ]
      returnType = TList(TCustomType(NR.ok (packageOpTypeName ()), []))
      description = "Get recent package ops from the database."
      fn =
        function
        | _, _, _, [ DInt64 limit ] ->
          uply {
            let! ops = LibDB.Queries.getRecentOps limit
            return Dval.list (packageOpKT ()) (ops |> List.map PT2DT.PackageOp.toDT)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetWipSummary" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch ID" ]
      returnType = TDict TInt64
      description = "Get summary of WIP ops on a branch (counts by type)."
      fn =
        function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            let! summary = LibDB.Queries.getWipSummary branchId
            return
              Dval.dict
                KTInt64
                [ "types", Dval.int64 summary.types
                  "values", Dval.int64 summary.values
                  "fns", Dval.int64 summary.fns
                  "renames", Dval.int64 summary.renames
                  "deprecations", Dval.int64 summary.deprecations
                  "total", Dval.int64 summary.total ]
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // CLEANUP: these three builtins are performance workarounds; see Queries.fs.
    { name = fn "scmGetWipItems" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch ID" ]
      returnType = TList(TDict TString)
      description =
        "Get WIP items on a branch (excludes auto-propagated ops). Returns list of dicts with name, kind, modulePath, propagatedCount."
      fn =
        function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            let! items = LibDB.Queries.getWipItems branchId
            return
              items
              |> List.map (fun item ->
                Dval.dict
                  KTString
                  [ "name", DString item.name
                    "kind", DString item.kind
                    "modulePath", DString item.modulePath
                    "propagatedCount", DString(string item.propagatedCount) ])
              |> Dval.list (KTDict(ValueType.Known KTString))
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetWipOpCount" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch ID" ]
      returnType = TInt64
      description = "Get count of WIP ops on a branch (fast, no deserialization)."
      fn =
        function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            let! count = LibDB.Queries.getWipOpCount branchId
            return Dval.int64 count
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetCommitCount" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch ID" ]
      returnType = TInt64
      description = "Get count of commits on a branch (fast, no deserialization)."
      fn =
        function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            let! count = LibDB.Queries.getCommitCount branchId
            return Dval.int64 count
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetWipOpsWithIds" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch ID" ]
      returnType =
        TList(
          TTuple(
            TUuid,
            TCustomType(NR.ok (packageOpTypeName ()), []),
            [ TypeReference.option TUuid ]
          )
        )
      description =
        "Get all WIP ops on a branch with their DB row id and propagation_id
        (None unless the op is part of a propagation batch). Use this when you
        need to operate on individual ops (e.g. partial commit / discard)."
      fn =
        function
        | _, vm, _, [ DUuid branchId ] ->
          uply {
            let! entries = LibDB.Queries.getWipOpsWithIds branchId
            let optionUuidDval =
              LibExecution.TypeChecker.DvalCreator.option vm.threadID VT.uuid
            let optionUuidVT =
              VT.known (KTCustomType(Dval.optionType (), [ VT.uuid ]))
            return
              entries
              |> List.map (fun (id, op, propId) ->
                let propDval = propId |> Option.map DUuid |> optionUuidDval
                DTuple(DUuid id, PT2DT.PackageOp.toDT op, [ propDval ]))
              |> Dval.list (
                KTTuple(VT.uuid, VT.known (packageOpKT ()), [ optionUuidVT ])
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmCommitWipOpsByIds" 0
      typeParams = []
      parameters =
        [ Param.make "accountId" TUuid "Author of the commit"
          Param.make "branchId" TUuid "Branch ID"
          Param.make "message" TString "Commit message"
          Param.make
            "opIds"
            (TList TUuid)
            "WIP op IDs from scmGetWipOpsWithIds. Every id must belong to this
            branch and still be WIP, or nothing is committed." ]
      returnType = TypeReference.result TString TString
      description =
        "Commit the named WIP ops and their derived projection rows. The caller
        owns selection policy and dependency closure. Projection rows are matched
        by content key until they can be tied directly to source op IDs. Returns
        the commit hash, or an error message on failure."
      fn =
        let resultOk = Dval.resultOk KTString KTString
        let resultError = Dval.resultError KTString KTString
        (function
        | _,
          _,
          _,
          [ DUuid accountId; DUuid branchId; DString message; DList(_, opIds) ] ->
          uply {
            try
              let ids =
                opIds
                |> List.map (function
                  | DUuid u -> u
                  | _ -> Exception.raiseInternal "opIds must be uuids" [])
              let! result =
                LibDB.Inserts.commitWipOpsByIds accountId branchId message ids
              match result with
              | Ok commitHash ->
                let (PT.Hash h) = commitHash
                return resultOk (Dval.string h)
              | Error msg -> return resultError (Dval.string msg)
            with ex ->
              return resultError (Dval.string ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmDiscard" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch ID" ]
      returnType = TypeReference.result TInt64 TString
      description =
        "Discard all WIP ops on a branch.
        Returns the count of discarded ops on success, or an error message on failure."
      fn =
        let resultOk = Dval.resultOk KTInt64 KTString
        let resultError = Dval.resultError KTInt64 KTString
        (function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            let! result = LibDB.Inserts.discardWipOps branchId
            match result with
            | Ok count -> return resultOk (Dval.int64 count)
            | Error msg -> return resultError (Dval.string msg)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetCommits" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch ID"
          Param.make "limit" TInt64 "Maximum commits to return" ]
      returnType = TList(TCustomType(NR.ok (PT2DT.Commit.typeName ()), []))
      description = "Get commit log for a branch ordered by date descending."
      fn =
        function
        | _, _, _, [ DUuid branchId; DInt64 limit ] ->
          uply {
            let! commits = LibDB.Queries.getCommits branchId limit
            return
              Dval.list
                (PT2DT.Commit.knownType ())
                (commits |> List.map PT2DT.Commit.toDT)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetCommitsForBranchChain" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch ID"
          Param.make "limit" TInt64 "Maximum commits to return" ]
      returnType = TList(TCustomType(NR.ok (PT2DT.Commit.typeName ()), []))
      description =
        "Get commit log across the entire branch chain (current + ancestors), ordered by date descending."
      fn =
        function
        | _, _, _, [ DUuid branchId; DInt64 limit ] ->
          uply {
            let! commits = LibDB.Queries.getCommitsForBranchChain branchId limit
            return
              Dval.list
                (PT2DT.Commit.knownType ())
                (commits |> List.map PT2DT.Commit.toDT)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetCommitOps" 0
      typeParams = []
      parameters = [ Param.make "commitHash" TString "Commit hash" ]
      returnType = TList(TCustomType(NR.ok (packageOpTypeName ()), []))
      description = "Get ops for a specific commit."
      fn =
        function
        | _, _, _, [ DString commitHash ] ->
          uply {
            let! ops = LibDB.Queries.getCommitOps (PT.Hash commitHash)
            return Dval.list (packageOpKT ()) (ops |> List.map PT2DT.PackageOp.toDT)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetDependencies" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch whose chain to resolve against"
          Param.make
            "itemHash"
            (TCustomType(NR.ok (PT2DT.Hash.typeName ()), []))
            "Content hash of the item whose forward dependencies to fetch" ]
      returnType =
        TList(
          TTuple(
            TCustomType(NR.ok (PT2DT.Hash.typeName ()), []),
            TCustomType(NR.ok (PT2DT.ItemKind.typeName ()), []),
            []
          )
        )
      description =
        "Get the items (content hash + kind) that the given item directly depends
        on, resolved over the branch chain. Used by partial commit to warn when a
        selected item references uncommitted items not in the selection."
      fn =
        (function
        | _, _, _, [ DUuid branchId; hashDval ] ->
          uply {
            let itemHash = PT2DT.Hash.fromDT hashDval
            let! chain = LibDB.Branches.getBranchChain branchId
            let! deps = LibDB.Queries.getDependencies chain itemHash
            return
              deps
              |> List.map (fun d ->
                DTuple(
                  PT2DT.Hash.toDT d.itemHash,
                  PT2DT.ItemKind.toDT d.itemKind,
                  []
                ))
              |> Dval.list (
                KTTuple(
                  VT.known (PT2DT.Hash.knownType ()),
                  VT.known (PT2DT.ItemKind.knownType ()),
                  []
                )
              )
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins (pm : PT.PackageManager) : Builtins =
  LibExecution.Builtin.make [] (fns pm)
