module BuiltinPM.Libs.PackageOps

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
            let stabilized =
              LibPackageManager.HashStabilization.computeRealHashes ptOps
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
        Use scmCommit to commit WIP ops."
      fn =
        let resultOk = Dval.resultOk KTInt64 KTString
        let resultError = Dval.resultError KTInt64 KTString
        (function
        | _, _, _, [ DUuid branchId; DList(_vtTODO, ops) ] ->
          uply {
            try
              let ops = ops |> List.choose PT2DT.PackageOp.fromDT

              // All ops are added as WIP - use scmCommit to commit them
              let! insertedCount =
                LibPackageManager.Inserts.insertAndApplyOpsAsWip branchId ops

              // Auto-refresh existing WIP items: re-resolve names and
              // recompute SCC-aware hashes now that new items exist
              let! _refreshed = LibPackageManager.WipRefresh.refresh pm branchId

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
            let! ops = LibPackageManager.Queries.getRecentOps limit
            return Dval.list (packageOpKT ()) (ops |> List.map PT2DT.PackageOp.toDT)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetWipOps" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch ID" ]
      returnType = TList(TCustomType(NR.ok (packageOpTypeName ()), []))
      description = "Get all WIP (uncommitted) package ops on a branch."
      fn =
        function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            let! ops = LibPackageManager.Queries.getWipOps branchId
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
            let! summary = LibPackageManager.Queries.getWipSummary branchId
            return
              Dval.dict
                KTInt64
                [ "types", Dval.int64 summary.types
                  "values", Dval.int64 summary.values
                  "fns", Dval.int64 summary.fns
                  "renames", Dval.int64 summary.renames
                  "total", Dval.int64 summary.total ]
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    // CLEANUP: these three builtins are performance workarounds — see Queries.fs
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
            let! items = LibPackageManager.Queries.getWipItems branchId
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
            let! count = LibPackageManager.Queries.getWipOpCount branchId
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
            let! count = LibPackageManager.Queries.getCommitCount branchId
            return Dval.int64 count
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmCommit" 0
      typeParams = []
      parameters =
        [ Param.make "accountId" TUuid "Committer account ID"
          Param.make "branchId" TUuid "Branch ID"
          Param.make "message" TString "Commit message" ]
      returnType = TypeReference.result TString TString
      description =
        "Commit all WIP ops on a branch with the given message.
        Returns the commit hash on success, or an error message on failure."
      fn =
        let resultOk = Dval.resultOk KTString KTString
        let resultError = Dval.resultError KTString KTString
        (function
        | _, _, _, [ DUuid accountId; DUuid branchId; DString message ] ->
          uply {
            let! result =
              LibPackageManager.Inserts.commitWipOps accountId branchId message
            match result with
            | Ok commitHash ->
              let (PT.Hash h) = commitHash
              return resultOk (Dval.string h)
            | Error msg -> return resultError (Dval.string msg)
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
            let! result = LibPackageManager.Inserts.discardWipOps branchId
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
            let! commits = LibPackageManager.Queries.getCommits branchId limit
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
            let! commits =
              LibPackageManager.Queries.getCommitsForBranchChain branchId limit
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
            let! ops = LibPackageManager.Queries.getCommitOps (PT.Hash commitHash)
            return Dval.list (packageOpKT ()) (ops |> List.map PT2DT.PackageOp.toDT)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins (pm : PT.PackageManager) : Builtins =
  LibExecution.Builtin.make [] (fns pm)
