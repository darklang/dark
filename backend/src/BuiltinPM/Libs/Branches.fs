module BuiltinPM.Libs.Branches

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module Builtin = LibExecution.Builtin
module Dval = LibExecution.Dval
module VT = LibExecution.ValueType

open Builtin.Shortcuts


let fns : List<BuiltInFn> =
  [ { name = fn "scmBranchCreate" 0
      typeParams = []
      parameters =
        [ Param.make "name" TString "Branch name"
          Param.make "parentBranchId" TUuid "Parent branch ID" ]
      returnType = TDict TString
      description = "Create a new branch from the given parent branch."
      fn =
        function
        | _, _, _, [ DString name; DUuid parentBranchId ] ->
          uply {
            let! branch = LibPackageManager.Branches.create name parentBranchId
            return
              DDict(
                VT.string,
                Map.ofList
                  [ "id", DString(string branch.id)
                    "name", DString branch.name
                    "parentBranchId",
                    DString(
                      branch.parentBranchId
                      |> Option.map string
                      |> Option.defaultValue ""
                    ) ]
              )
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmBranchList" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList(TDict TString)
      description = "List all active (non-merged) branches."
      fn =
        function
        | _, _, _, [ DUnit ] ->
          uply {
            let! branches = LibPackageManager.Branches.list ()
            let branchDvals =
              branches
              |> List.map (fun b ->
                DDict(
                  VT.string,
                  Map.ofList
                    [ "id", DString(string b.id)
                      "name", DString b.name
                      "parentBranchId",
                      DString(
                        b.parentBranchId
                        |> Option.map string
                        |> Option.defaultValue ""
                      ) ]
                ))
            return DList(VT.dict VT.string, branchDvals)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmBranchGet" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid "Branch ID" ]
      returnType = TypeReference.option (TDict TString)
      description = "Get a branch by ID."
      fn =
        function
        | _, _, _, [ DUuid id ] ->
          uply {
            let! branchOpt = LibPackageManager.Branches.get id
            match branchOpt with
            | None -> return Dval.optionNone KTString
            | Some b ->
              return
                Dval.optionSome
                  (KTDict VT.string)
                  (DDict(
                    VT.string,
                    Map.ofList
                      [ "id", DString(string b.id)
                        "name", DString b.name
                        "parentBranchId",
                        DString(
                          b.parentBranchId
                          |> Option.map string
                          |> Option.defaultValue ""
                        )
                        "mergedAt",
                        DString(
                          b.mergedAt |> Option.map string |> Option.defaultValue ""
                        ) ]
                  ))
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmBranchGetByName" 0
      typeParams = []
      parameters = [ Param.make "name" TString "Branch name" ]
      returnType = TypeReference.option (TDict TString)
      description = "Get a branch by name."
      fn =
        function
        | _, _, _, [ DString name ] ->
          uply {
            let! branchOpt = LibPackageManager.Branches.getByName name
            match branchOpt with
            | None -> return Dval.optionNone KTString
            | Some b ->
              return
                Dval.optionSome
                  (KTDict VT.string)
                  (DDict(
                    VT.string,
                    Map.ofList
                      [ "id", DString(string b.id)
                        "name", DString b.name
                        "parentBranchId",
                        DString(
                          b.parentBranchId
                          |> Option.map string
                          |> Option.defaultValue ""
                        ) ]
                  ))
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmBranchRename" 0
      typeParams = []
      parameters =
        [ Param.make "id" TUuid "Branch ID"
          Param.make "newName" TString "New name" ]
      returnType = TypeReference.result TUnit TString
      description = "Rename a branch."
      fn =
        let resultOk = Dval.resultOk KTUnit KTString
        let resultError = Dval.resultError KTUnit KTString
        (function
        | _, _, _, [ DUuid id; DString newName ] ->
          uply {
            let! result = LibPackageManager.Branches.rename id newName
            match result with
            | Ok() -> return resultOk DUnit
            | Error msg -> return resultError (DString msg)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmBranchDelete" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid "Branch ID" ]
      returnType = TypeReference.result TUnit TString
      description = "Delete a branch (must have no active children)."
      fn =
        let resultOk = Dval.resultOk KTUnit KTString
        let resultError = Dval.resultError KTUnit KTString
        (function
        | _, _, _, [ DUuid id ] ->
          uply {
            let! result = LibPackageManager.Branches.delete id
            match result with
            | Ok() -> return resultOk DUnit
            | Error msg -> return resultError (DString msg)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmBranchGetMainId" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TUuid
      description = "Get the well-known main branch UUID."
      fn =
        function
        | _, _, _, [ DUnit ] -> Ply(DUuid PT.mainBranchId)
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
