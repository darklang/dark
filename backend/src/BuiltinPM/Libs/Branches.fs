module BuiltinPM.Libs.Branches

open Prelude
open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module Builtin = LibExecution.Builtin
module D = LibExecution.Dval
module PT2DT = LibExecution.ProgramTypesToDarkTypes

open Builtin.Shortcuts


let private branchType = TCustomType(Ok PT2DT.Branch.typeName, [])

let fns : List<BuiltInFn> =
  [ { name = fn "scmBranchCreate" 0
      typeParams = []
      parameters =
        [ Param.make "name" TString "Branch name"
          Param.make "parentBranchId" TUuid "Parent branch ID" ]
      returnType = branchType
      description = "Create a new branch from the given parent branch."
      fn =
        function
        | _, _, _, [ DString name; DUuid parentBranchId ] ->
          uply {
            let! branch = LibPackageManager.Branches.create name parentBranchId
            return PT2DT.Branch.toDT branch
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmBranchList" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList branchType
      description = "List all active (non-merged) branches."
      fn =
        function
        | _, _, _, [ DUnit ] ->
          uply {
            let! branches = LibPackageManager.Branches.list ()
            return
              branches |> List.map PT2DT.Branch.toDT |> D.list PT2DT.Branch.knownType
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmBranchGet" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid "Branch ID" ]
      returnType = TypeReference.option branchType
      description = "Get a branch by ID."
      fn =
        function
        | _, _, _, [ DUuid id ] ->
          uply {
            let! branchOpt = LibPackageManager.Branches.get id
            return
              branchOpt
              |> Option.map PT2DT.Branch.toDT
              |> D.option PT2DT.Branch.knownType
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmBranchGetByName" 0
      typeParams = []
      parameters = [ Param.make "name" TString "Branch name" ]
      returnType = TypeReference.option branchType
      description = "Get a branch by name."
      fn =
        function
        | _, _, _, [ DString name ] ->
          uply {
            let! branchOpt = LibPackageManager.Branches.getByName name
            return
              branchOpt
              |> Option.map PT2DT.Branch.toDT
              |> D.option PT2DT.Branch.knownType
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
        function
        | _, _, _, [ DUuid id; DString newName ] ->
          uply {
            let! result = LibPackageManager.Branches.rename id newName
            return
              result
              |> Result.map (fun () -> DUnit)
              |> Result.mapError DString
              |> D.result KTUnit KTString
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmBranchDelete" 0
      typeParams = []
      parameters = [ Param.make "id" TUuid "Branch ID" ]
      returnType = TypeReference.result TUnit TString
      description = "Delete a branch (must have no active children)."
      fn =
        function
        | _, _, _, [ DUuid id ] ->
          uply {
            let! result = LibPackageManager.Branches.delete id
            return
              result
              |> Result.map (fun () -> DUnit)
              |> Result.mapError DString
              |> D.result KTUnit KTString
          }
        | _ -> incorrectArgs ()
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
