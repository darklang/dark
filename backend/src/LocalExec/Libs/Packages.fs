module LocalExec.Libs.Packages

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
  [ { name = typ' [ "LocalExec"; "Packages" ] "Package" 0
      description = "The name of a package"
      typeParams = []
      definition =
        CustomType.Record(
          { name = "modules"
            typ = TList TString
            description = "The modules in the package" },
          [ { name = "name"; typ = TString; description = "The name of the package" }
            { name = "version"
              typ = TString
              description = "The version of the package" } ]
        )
      deprecated = NotDeprecated } ]



let fns : List<BuiltInFn> =
  [ { name = fn' [ "LocalExec"; "Packages" ] "clear" 0
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
            return DUnit
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn' [ "LocalExec"; "Packages" ] "parseAndSave" 0
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
            do! LibBackend.PackageManager.savePackages packages.fns
            return DResult(Ok(DUnit))
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn' [ "LocalExec"; "Packages" ] "list" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TList(TString)
      description = "List all packages"
      fn =
        function
        | _, _, [ DUnit ] ->
          uply {
            let! packages =
              Sql.query "SELECT fnname, modules, version FROM package_functions_v0"
              |> Sql.executeAsync (fun read ->
                (read.string "fnname", read.string "modules", read.int "version"))
            debuG "packages" packages
            return
              (DList(
                packages
                |> List.map (fun (fnname, modules, version) ->
                  DRecord(
                    FQTypeName.Stdlib(typ' [ "LocalExec"; "Packages" ] "Package" 0),
                    Map(
                      [ ("name", DString fnname)
                        ("modules",
                         modules |> String.split "." |> List.map DString |> DList)
                        ("version", DInt version) ]
                    )
                  ))
              ))
          }
        | _ -> incorrectArgs ()
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "Debug" "inspect" 0
      typeParams = []
      parameters =
        [ Param.make "var" (TVariable "value") ""; Param.make "msg" TString "" ]
      returnType = TVariable "value"
      description =
        "Prints the value into stdout, and returns the value. The output format is not stable and should not be relied upon"
      fn =
        (function
        | _, _, [ v; DString msg ] ->
          print $"{msg}: {LibExecution.DvalReprDeveloper.toRepr v}"
          Ply v
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }

    { name = fn "Test" "unwrap" 0
      typeParams = []
      parameters = [ Param.make "value" (TOption(TVariable "a")) "" ]
      returnType = TVariable "a"
      description =
        "Unwrap an Option or Result, returning the value or a DError if Nothing"
      fn =
        (function
        | _, _, [ DOption opt ] ->
          uply {
            match opt with
            | Some value -> return value
            | None -> return (DError(SourceNone, "Nothing"))
          }
        | _, _, [ DResult res ] ->
          uply {
            match res with
            | Ok value -> return value
            | Error e ->
              return
                (DError(
                  SourceNone,
                  ("Error: " + LibExecution.DvalReprDeveloper.toRepr e)
                ))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }



    ]

let contents : LibExecution.StdLib.Contents = (fns, types)
