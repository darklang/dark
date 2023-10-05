module LocalExec.Libs.Packages

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open LibCloud.Db

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType

let packagePackagesType
  (addlModules : List<string>)
  (name : string)
  (version : int)
  : TypeName.TypeName =
  TypeName.fqPackage
    "Darklang"
    ("LocalExec" :: "Packages" :: addlModules)
    name
    version


let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn [ "LocalExec"; "Packages" ] "listFunctions" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TList(TCustomType(Ok(packagePackagesType [] "FunctionName" 0), []))
      description = "List all package functions"
      fn =
        function
        | _, _, [ DUnit ] ->
          uply {
            let! fns =
              Sql.query
                "SELECT owner, modules, fnname, version FROM package_functions_v0"
              |> Sql.executeAsync (fun read ->
                (read.string "owner",
                 read.string "fnname",
                 read.string "modules",
                 read.int "version"))

            let typeName = packagePackagesType [] "FunctionName" 0

            let fns =
              fns
              |> List.map (fun (owner, fnname, modules, version) ->
                let fields =
                  [ ("owner", DString owner)
                    ("modules",
                     DList(
                       VT.string,
                       modules |> String.split "." |> List.map DString
                     ))
                    ("name", DString fnname)
                    ("version", DInt version) ]

                DRecord(typeName, typeName, [], Map fields))

            return DList(VT.customType typeName [], fns)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn [ "LocalExec"; "Packages" ] "listTypes" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList(TCustomType(Ok(packagePackagesType [] "TypeName" 0), []))
      description = "List all package types"
      fn =
        function
        | _, _, [ DUnit ] ->
          uply {
            let! types =
              Sql.query
                "SELECT owner, modules, typename, version FROM package_types_v0"
              |> Sql.executeAsync (fun read ->
                (read.string "owner",
                 read.string "typename",
                 read.string "modules",
                 read.int "version"))

            let typeName = packagePackagesType [] "TypeName" 0

            let types =
              types
              |> List.map (fun (owner, typename, modules, version) ->
                let fields =
                  [ ("owner", DString owner)
                    ("modules",
                     DList(
                       VT.string,
                       modules |> String.split "." |> List.map DString
                     ))
                    ("name", DString typename)
                    ("version", DInt version) ]
                DRecord(typeName, typeName, [], Map fields))

            return DList(VT.customType typeName [], types)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn [ "LocalExec"; "Packages" ] "listConstants" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TList(TCustomType(Ok(packagePackagesType [] "ConstantName" 0), []))
      description = "List all package constants"
      fn =
        function
        | _, _, [ DUnit ] ->
          uply {
            let! consts =
              Sql.query
                "SELECT owner, modules, name, version FROM package_constants_v0"
              |> Sql.executeAsync (fun read ->
                (read.string "owner",
                 read.string "name",
                 read.string "modules",
                 read.int "version"))

            let typeName = packagePackagesType [] "ConstantName" 0

            let consts =
              consts
              |> List.map (fun (owner, fnname, modules, version) ->
                let fields =
                  [ ("owner", DString owner)
                    ("modules",
                     DList(
                       VT.string,
                       modules |> String.split "." |> List.map DString
                     ))
                    ("name", DString fnname)
                    ("version", DInt version) ]
                DRecord(typeName, typeName, [], Map fields))

            return DList(VT.customType typeName [], consts)
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let constants : List<BuiltInConstant> = []
let contents : LibExecution.Builtin.Contents = (fns, types, constants)
