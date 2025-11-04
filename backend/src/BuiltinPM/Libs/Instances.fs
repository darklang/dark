module BuiltinPM.Libs.Instances

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open LibExecution.RuntimeTypes

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Builtin = LibExecution.Builtin
module Instances = LibPackageManager.Instances
module PackageIDs = LibExecution.PackageIDs

open Builtin.Shortcuts


let instanceTypeName = FQTypeName.fqPackage PackageIDs.Type.SCM.Instances.instance


let instanceToDT (instance : Instances.Instance) : Dval =
  let fields =
    Map
      [ "id", DUuid instance.id
        "name", DString instance.name
        "url", DString instance.url ]

  DRecord(instanceTypeName, instanceTypeName, [], fields)


// TODO: Reconsider which of these functions should be public vs admin-only:
// - scmGetInstance: Read-only, probably OK as public
// - scmGetInstanceByName: Read-only, probably OK as public
// - scmListInstances: Read-only, probably OK as public
// - scmAddInstance: Writes to DB - should probably be admin-only
// - scmRemoveInstance: Deletes from DB - should probably be admin-only
let fns : List<BuiltInFn> =
  [ { name = fn "scmGetInstance" 0
      typeParams = []
      parameters = [ Param.make "instanceID" TUuid "" ]
      returnType = TypeReference.option (TCustomType(Ok instanceTypeName, []))
      description = "Get an instance by ID. Returns None if not found."
      fn =
        function
        | _, _, _, [ DUuid instanceID ] ->
          uply {
            let! instanceOpt = Instances.getByID instanceID

            return
              instanceOpt
              |> Option.map instanceToDT
              |> Dval.option (KTCustomType(instanceTypeName, []))
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmGetInstanceByName" 0
      typeParams = []
      parameters = [ Param.make "name" TString "" ]
      returnType = TypeReference.option (TCustomType(Ok instanceTypeName, []))
      description = "Get an instance by name. Returns None if not found."
      fn =
        function
        | _, _, _, [ DString name ] ->
          uply {
            let! instanceOpt = Instances.getByName name

            return
              instanceOpt
              |> Option.map instanceToDT
              |> Dval.option (KTCustomType(instanceTypeName, []))
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmListInstances" 0
      typeParams = []
      parameters = [ Param.make "" TUnit "" ]
      returnType = TList(TCustomType(Ok instanceTypeName, []))
      description = "List all instances."
      fn =
        function
        | _, _, _, [ DUnit ] ->
          uply {
            let! instances = Instances.list ()

            let instanceDvals = instances |> List.map instanceToDT

            let vt = VT.customType instanceTypeName []

            return DList(vt, instanceDvals)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmAddInstance" 0
      typeParams = []
      parameters = [ Param.make "name" TString ""; Param.make "url" TString "" ]
      returnType = TCustomType(Ok instanceTypeName, [])
      description = "Add a new instance."
      fn =
        function
        | _, _, _, [ DString name; DString url ] ->
          uply {
            let! instance = Instances.add name url

            return instanceToDT instance
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "scmRemoveInstance" 0
      typeParams = []
      parameters = [ Param.make "instanceID" TUuid "" ]
      returnType = TUnit
      description = "Remove an instance by ID."
      fn =
        function
        | _, _, _, [ DUuid instanceID ] ->
          uply {
            do! Instances.remove instanceID

            return DUnit
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins : Builtins = LibExecution.Builtin.make [] fns
