module InternalCLI.Libs.Packages

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Npgsql
open LibBackend.Db

open Prelude
open Tablecloth
open LibExecution.RuntimeTypes

open LibExecution.StdLib.Shortcuts

let types : List<BuiltInType> =
  [ { name = typ [ "InternalCLI"; "Packages" ] "Function" 0
      description = "The name of a package function"
      typeParams = []
      definition =
        CustomType.Record(
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
        )
      deprecated = NotDeprecated }


    { name = typ [ "InternalCLI"; "Packages" ] "Type" 0
      description = "The name of a package type"
      typeParams = []
      definition =
        CustomType.Record(
          { name = "owner"
            typ = TString
            description = "The username of the owner of the function" },
          [ { name = "modules"
              typ = TList TString
              description = "The module the type is in" }
            { name = "name"; typ = TString; description = "The name of the type" }
            { name = "version"; typ = TInt; description = "The version of the type" } ]
        )
      deprecated = NotDeprecated } ]


let fns : List<BuiltInFn> =
  [ { name = fn [ "InternalCLI"; "Packages" ] "clear" 0
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
            return DUnit
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn [ "InternalCLI"; "Packages" ] "parseAndSave" 0
      typeParams = []
      parameters =
        [ Param.make "package source" TString "The source code of the package"
          Param.make "filename" TString "Used for error message" ]
      returnType = TResult(TUnit, TString)
      description = "Parse a package and save it to the database"
      fn =
        function
        | _, _, [ DString contents; DString path ] ->
          uply {
            let packages = Parser.Package.parse path contents
            do! LibBackend.PackageManager.savePackageFunctions packages.fns
            do! LibBackend.PackageManager.savePackageTypes packages.types
            return DResult(Ok(DUnit))
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn [ "InternalCLI"; "Packages" ] "listFunctions" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TList(
          TCustomType(
            FQName.BuiltIn(typ [ "InternalCLI"; "Packages" ] "Function" 0),
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
                    FQName.BuiltIn(typ [ "InternalCLI"; "Packages" ] "Function" 0),
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


    { name = fn [ "InternalCLI"; "Packages" ] "listTypes" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TList(
          TCustomType(FQName.BuiltIn(typ [ "InternalCLI"; "Packages" ] "Type" 0), [])
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
                    FQName.BuiltIn(typ [ "InternalCLI"; "Packages" ] "Type" 0),
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
      deprecated = NotDeprecated } ]

let contents : LibExecution.StdLib.Contents = (fns, types)
