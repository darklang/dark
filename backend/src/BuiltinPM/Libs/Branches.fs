module BuiltinPM.Libs.Branches

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


let branchTypeName =
  FQTypeName.fqPackage PackageIDs.Type.SCM.Branch.branch


let branchToDT (branch : Branches.Branch) : Dval =
  let fields =
    [ "id", DUuid branch.id
      "name", DString branch.name
      "createdAt", DDateTime(DarkDateTime.fromInstant branch.createdAt)
      "mergedAt",
      branch.mergedAt
      |> Option.map (DarkDateTime.fromInstant >> DDateTime)
      |> Dval.option KTDateTime ]
    |> Map

  DRecord(branchTypeName, branchTypeName, [], fields)


// TODO: Reconsider which of these functions should be public vs admin-only:
// - scmBranchList: Read-only, probably OK as public
// - scmBranchGet: Read-only, probably OK as public
// - scmBranchFindByName: Read-only, probably OK as public
// - scmBranchCreate: Creates branches - probably OK as public for collaborative development
let fns : List<BuiltInFn> =
  [ { name = fn "scmBranchList" 0
      typeParams = []
      parameters = [ Param.make "" TUnit "" ]
      returnType = TList(TCustomType(Ok branchTypeName, []))
      description = "List all branches"
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! branches = Branches.list ()
            let branchVT = VT.customType branchTypeName []
            return DList(branchVT, branches |> List.map branchToDT)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "scmBranchGet" 0
      typeParams = []
      parameters = [ Param.make "branchID" TUuid "" ]
      returnType = TypeReference.option (TCustomType(Ok branchTypeName, []))
      description = "Get a specific branch by ID"
      fn =
        (function
        | _, _, _, [ DUuid branchID ] ->
          uply {
            let! branch = Branches.get branchID
            return
              branch
              |> Option.map branchToDT
              |> Dval.option (KTCustomType(branchTypeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "scmBranchFindByName" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TList(TCustomType(Ok branchTypeName, []))
      description = "Find branches by name (may return multiple if names collide)"
      fn =
        (function
        | _, _, _, [ DString name ] ->
          uply {
            let! branches = Branches.findByName name
            let branchVT = VT.customType branchTypeName []
            return DList(branchVT, branches |> List.map branchToDT)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    { name = fn "scmBranchCreate" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TCustomType(Ok branchTypeName, [])
      description = "Create a new branch"
      fn =
        (function
        | _, _, _, [ DString name ] ->
          uply {
            let! branch = Branches.create name
            return branchToDT branch
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }

    ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
