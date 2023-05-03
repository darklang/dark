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

let types : List<BuiltInType> = []

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


    { name = fn' [ "LocalExec"; "Packages" ] "parse" 0
      typeParams = []
      parameters = [ Param.make "path" TString "The path of the package" ]
      returnType = TUnit
      description = "Parse a package file"
      fn =
        function
        | _, _, [ DString path ] -> uply { return DUnit }
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



    ]

let contents : LibExecution.StdLib.Contents = (fns, types)
