module BuiltinCli.Libs.Branches

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module Branches = LibPackageManager.Branches
module DarkDateTime = LibExecution.DarkDateTime
module PackageIDs = LibExecution.PackageIDs

open Builtin.Shortcuts


let branchStateTypeName =
  FQTypeName.fqPackage PackageIDs.Type.SCM.Branch.branchState

let branchTypeName =
  FQTypeName.fqPackage PackageIDs.Type.SCM.Branch.branch


let branchStateToDT (state : Branches.BranchState) : Dval =
  let typeName = branchStateTypeName
  let caseName =
    match state with
    | Branches.Active -> "Active"
    | Branches.Merged -> "Merged"
    | Branches.Abandoned -> "Abandoned"
  DEnum(typeName, typeName, [], caseName, [])


let branchToDT (branch : Branches.Branch) : Dval =
  let fields =
    [ "id", DUuid branch.id
      "createdBy",
      (match branch.createdBy with
       | Some id -> Dval.optionSome KTUuid (DUuid id)
       | None -> Dval.optionNone KTUuid)
      "title", DString branch.title
      "state", branchStateToDT branch.state
      "createdAt", DDateTime(DarkDateTime.fromInstant branch.createdAt)
      "lastActiveAt", DDateTime(DarkDateTime.fromInstant branch.lastActiveAt)
      "mergedAt",
      (match branch.mergedAt with
       | Some dt -> Dval.optionSome KTDateTime (DDateTime(DarkDateTime.fromInstant dt))
       | None -> Dval.optionNone KTDateTime) ]
    |> Map

  DRecord(branchTypeName, branchTypeName, [], fields)


let fns : List<BuiltInFn> =
  [ { name = fn "cliBranchList" 0
      typeParams = []
      parameters = [ Param.make "" TUnit "" ]
      returnType = TList(TCustomType(Ok branchTypeName, []))
      description = "List all active branches"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! branches = Branches.list (Some Branches.Active)
            let branchVT = VT.customType branchTypeName []
            return DList(branchVT, branches |> List.map branchToDT)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "cliBranchGet" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "" ]
      returnType = TypeReference.option (TCustomType(Ok branchTypeName, []))
      description = "Get a specific branch by ID"
      fn =
        (function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            let! branch = Branches.get branchId
            return
              match branch with
              | Some b -> Dval.optionSome (KTCustomType(branchTypeName, [])) (branchToDT b)
              | None -> Dval.optionNone (KTCustomType(branchTypeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "cliBranchFindByTitle" 0
      typeParams = []
      parameters = [ Param.make "title" TString "" ]
      returnType = TList(TCustomType(Ok branchTypeName, []))
      description = "Find branches by title (may return multiple if titles collide)"
      fn =
        (function
        | _, _, _, [ DString title ] ->
          uply {
            let! branches = Branches.findByTitle title
            let branchVT = VT.customType branchTypeName []
            return DList(branchVT, branches |> List.map branchToDT)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "cliBranchCreate" 0
      typeParams = []
      parameters =
        [ Param.make "createdBy" (TypeReference.option TUuid) ""
          Param.make "title" TString "" ]
      returnType = TCustomType(Ok branchTypeName, [])
      description = "Create a new branch"
      fn =
        (function
        | _, _, _, [ createdBy; DString title ] ->
          uply {
            let createdByOpt =
              match createdBy with
              | DEnum(_, _, _, "Some", [ DUuid id ]) -> Some id
              | _ -> None

            let! branch = Branches.create createdByOpt title
            return branchToDT branch
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "cliBranchUpdateLastActive" 0
      typeParams = []
      parameters = [ Param.make "branchId" TUuid "" ]
      returnType = TUnit
      description = "Update the last active timestamp for a branch"
      fn =
        (function
        | _, _, _, [ DUuid branchId ] ->
          uply {
            do! Branches.updateLastActive branchId
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "cliBranchEnsureMainBranch" 0
      typeParams = []
      parameters = [ Param.make "createdBy" (TypeReference.option TUuid) "" ]
      returnType = TCustomType(Ok branchTypeName, [])
      description =
        "Ensure a 'main' branch exists, creating it if needed. Returns the main branch."
      fn =
        (function
        | _, _, _, [ createdBy ] ->
          uply {
            let createdByOpt =
              match createdBy with
              | DEnum(_, _, _, "Some", [ DUuid id ]) -> Some id
              | _ -> None

            let! branch = Branches.ensureMainBranch createdByOpt
            return branchToDT branch
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
