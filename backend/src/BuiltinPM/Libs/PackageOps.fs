module BuiltinPM.Libs.PackageOps

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module PT2DT = LibExecution.ProgramTypesToDarkTypes
module Builtin = LibExecution.Builtin
module C2DT = LibExecution.CommonToDarkTypes
module D = LibExecution.DvalDecoder
module VT = LibExecution.ValueType
module PackageIDs = LibExecution.PackageIDs
module Dval = LibExecution.Dval

open Builtin.Shortcuts


let packageOpTypeName =
  FQTypeName.fqPackage PackageIDs.Type.LanguageTools.ProgramTypes.packageOp


// TODO: review/reconsider the accessibility of these fns
let fns : List<BuiltInFn> =
  [ { name = fn "scmAddOps" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch to add ops to"
          Param.make "ops" (TList(TCustomType(Ok packageOpTypeName, []))) "" ]
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

              return resultOk (DInt64 insertedCount)
            with ex ->
              return resultError (DString ex.Message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetRecentOps" 0
      typeParams = []
      parameters = [ Param.make "limit" TInt64 "" ]
      returnType = TList(TCustomType(Ok packageOpTypeName, []))
      description = "Get recent package ops from the database."
      fn =
        function
        | _, _, _, [ DInt64 limit ] ->
          uply {
            let! ops = LibPackageManager.Queries.getRecentOps limit

            return
              DList(
                VT.customType PT2DT.PackageOp.typeName [],
                ops |> List.map PT2DT.PackageOp.toDT
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetWipOps" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "Branch ID" ]
      returnType = TList(TCustomType(Ok packageOpTypeName, []))
      description = "Get all WIP (uncommitted) package ops on a branch."
      fn =
        function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            let! ops = LibPackageManager.Queries.getWipOps branchId

            return
              DList(
                VT.customType PT2DT.PackageOp.typeName [],
                ops |> List.map PT2DT.PackageOp.toDT
              )
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
              DDict(
                VT.int64,
                Map.ofList
                  [ "types", DInt64 summary.types
                    "values", DInt64 summary.values
                    "fns", DInt64 summary.fns
                    "renames", DInt64 summary.renames
                    "total", DInt64 summary.total ]
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmCommit" 0
      typeParams = []
      parameters =
        [ Param.make "branchId" TUuid "Branch ID"
          Param.make "message" TString "Commit message" ]
      returnType = TypeReference.result TUuid TString
      description =
        "Commit all WIP ops on a branch with the given message.
        Returns the commit ID on success, or an error message on failure."
      fn =
        let resultOk = Dval.resultOk KTUuid KTString
        let resultError = Dval.resultError KTUuid KTString
        (function
        | _, _, _, [ DUuid branchId; DString message ] ->
          uply {
            let! result = LibPackageManager.Inserts.commitWipOps branchId message

            match result with
            | Ok commitId -> return resultOk (DUuid commitId)
            | Error msg -> return resultError (DString msg)
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
            | Ok count -> return resultOk (DInt64 count)
            | Error msg -> return resultError (DString msg)
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
      returnType = TList(TDict TString)
      description = "Get commit log for a branch ordered by date descending."
      fn =
        function
        | _, _, _, [ DUuid branchId; DInt64 limit ] ->
          uply {
            let! commits = LibPackageManager.Queries.getCommits branchId limit

            let commitDvals =
              commits
              |> List.map (fun c ->
                DDict(
                  VT.string,
                  Map.ofList
                    [ "id", DString(string c.id)
                      "message", DString c.message
                      "createdAt", DString(c.createdAt.ToString())
                      "opCount", DString(string c.opCount) ]
                ))

            return DList(VT.dict VT.string, commitDvals)
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
      returnType = TList(TDict TString)
      description =
        "Get commit log across the entire branch chain (current + ancestors), ordered by date descending. Each commit includes branchId and branchName."
      fn =
        function
        | _, _, _, [ DUuid branchId; DInt64 limit ] ->
          uply {
            let! commits =
              LibPackageManager.Queries.getCommitsForBranchChain branchId limit

            let commitDvals =
              commits
              |> List.map (fun c ->
                DDict(
                  VT.string,
                  Map.ofList
                    [ "id", DString(string c.id)
                      "message", DString c.message
                      "createdAt", DString(c.createdAt.ToString())
                      "opCount", DString(string c.opCount)
                      "branchId", DString(string c.branchId)
                      "branchName", DString c.branchName ]
                ))

            return DList(VT.dict VT.string, commitDvals)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetCommitOps" 0
      typeParams = []
      parameters = [ Param.make "commitId" TUuid "Commit ID" ]
      returnType = TList(TCustomType(Ok packageOpTypeName, []))
      description = "Get ops for a specific commit."
      fn =
        function
        | _, _, _, [ DUuid commitId ] ->
          uply {
            let! ops = LibPackageManager.Queries.getCommitOps commitId

            return
              DList(
                VT.customType PT2DT.PackageOp.typeName [],
                ops |> List.map PT2DT.PackageOp.toDT
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
