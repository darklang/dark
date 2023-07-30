module LocalExec.Libs.Packages

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open LibCloud.Db

open Prelude
open Tablecloth
open LibExecution.RuntimeTypes

open LibExecution.StdLib.Shortcuts

module PT2DT = StdLibDarkInternal.Helpers.ProgramTypesToDarkTypes

let types : List<BuiltInType> =
  [ { name = typ [ "LocalExec"; "Packages" ] "Function" 0
      description = "The name of a package function"
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              { name = "owner"
                typ = TString
                description = "The username of the owner of the function" },
              [ { name = "modules"
                  typ = TList TString
                  description = "The modules the function is in" }
                { name = "name"
                  typ = TString
                  description = "The name of the function" }
                { name = "version"
                  typ = TInt
                  description = "The version of the function" } ]
            ) }
      deprecated = NotDeprecated }


    { name = typ [ "LocalExec"; "Packages" ] "Type" 0
      description = "The name of a package type"
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              { name = "owner"
                typ = TString
                description = "The username of the owner of the function" },
              [ { name = "modules"
                  typ = TList TString
                  description = "The module the type is in" }
                { name = "name"
                  typ = TString
                  description = "The name of the type" }
                { name = "version"
                  typ = TInt
                  description = "The version of the type" } ]
            ) }
      deprecated = NotDeprecated }

    { name = typ [ "LocalExec"; "Packages" ] "Constant" 0
      description = "The name of a package constant"
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              { name = "owner"
                typ = TString
                description = "The username of the owner of the function" },
              [ { name = "modules"
                  typ = TList TString
                  description = "The module the constant is in" }
                { name = "name"
                  typ = TString
                  description = "The name of the constant" }
                { name = "version"
                  typ = TInt
                  description = "The version of the constant" } ]
            ) }
      deprecated = NotDeprecated } ]


let fns : List<BuiltInFn> =
  [ { name = fn [ "LocalExec"; "Packages" ] "clear" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TUnit
      description = "Delete all packages"
      fn =
        function
        | _, _, [ DUnit ] ->
          uply {
            do!
              Sql.query "DELETE FROM package_functions_v0"
              |> Sql.executeStatementAsync
            do! Sql.query "DELETE FROM package_types_v0" |> Sql.executeStatementAsync
            do!
              Sql.query "DELETE FROM package_constants_v0"
              |> Sql.executeStatementAsync
            return DUnit
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn [ "LocalExec"; "Packages" ] "listFunctions" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TList(
          TCustomType(
            FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Function" 0),
            []
          )
        )
      description = "List all package functions"
      fn =
        function
        | _, _, [ DUnit ] ->
          uply {
            let! packages =
              Sql.query
                "SELECT owner, modules, fnname, version FROM package_functions_v0"
              |> Sql.executeAsync (fun read ->
                (read.string "owner",
                 read.string "fnname",
                 read.string "modules",
                 read.int "version"))
            return
              (DList(
                packages
                |> List.map (fun (owner, fnname, modules, version) ->
                  DRecord(
                    FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Function" 0),
                    Map(
                      [ ("owner", DString owner)
                        ("modules",
                         modules |> String.split "." |> List.map DString |> DList)
                        ("name", DString fnname)
                        ("version", DInt version) ]
                    )
                  ))
              ))
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn [ "LocalExec"; "Packages" ] "listTypes" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TList(
          TCustomType(FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Type" 0), [])
        )
      description = "List all package types"
      fn =
        function
        | _, _, [ DUnit ] ->
          uply {
            let! packages =
              Sql.query
                "SELECT owner, modules, typename, version FROM package_types_v0"
              |> Sql.executeAsync (fun read ->
                (read.string "owner",
                 read.string "typename",
                 read.string "modules",
                 read.int "version"))
            return
              (DList(
                packages
                |> List.map (fun (owner, typename, modules, version) ->
                  DRecord(
                    FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Type" 0),
                    Map(
                      [ ("owner", DString owner)
                        ("modules",
                         modules |> String.split "." |> List.map DString |> DList)
                        ("name", DString typename)
                        ("version", DInt version) ]
                    )
                  ))
              ))
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn [ "LocalExec"; "Packages" ] "listConstants" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TList(
          TCustomType(
            FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Constant" 0),
            []
          )
        )
      description = "List all package constants"
      fn =
        function
        | _, _, [ DUnit ] ->
          uply {
            let! packages =
              Sql.query
                "SELECT owner, modules, name, version FROM package_constants_v0"
              |> Sql.executeAsync (fun read ->
                (read.string "owner",
                 read.string "name",
                 read.string "modules",
                 read.int "version"))
            return
              (DList(
                packages
                |> List.map (fun (owner, fnname, modules, version) ->
                  DRecord(
                    FQName.BuiltIn(typ [ "LocalExec"; "Packages" ] "Constant" 0),
                    Map(
                      [ ("owner", DString owner)
                        ("modules",
                         modules |> String.split "." |> List.map DString |> DList)
                        ("name", DString fnname)
                        ("version", DInt version) ]
                    )
                  ))
              ))
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let constants : List<BuiltInConstant> = []
let contents : LibExecution.StdLib.Contents = (fns, types, constants)
