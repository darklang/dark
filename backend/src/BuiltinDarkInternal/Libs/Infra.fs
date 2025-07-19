/// Builtin functions for looking at Dark infra
module BuiltinDarkInternal.Libs.Infra

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module PackageIDs = LibExecution.PackageIDs
module Telemetry = LibService.Telemetry


let fns : List<BuiltInFn> =
  [ { name = fn "darkInternalInfraLog" 0
      typeParams = []
      parameters =
        [ Param.make "level" TString ""
          Param.make "name" TString ""
          Param.make "log" (TDict TString) "" ]
      returnType = TDict TString
      description =
        "Write the log object to a honeycomb log, along with whatever enrichment the backend provides. Returns its input"
      fn =
        (function
        | _, _, _, [ DString level; DString name; DDict(_, log) as result ] ->
          let args =
            log
            |> Map.toList
            // We could just leave the dval vals as strings and use params, but
            // then we can't do numeric things (MAX, AVG, >, etc) with these
            // logs
            |> List.map (fun (k, v) -> (k, DvalReprDeveloper.toRepr v :> obj))
          Telemetry.addEvent name (("level", level) :: args)
          Ply result
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalInfraGetAndLogTableSizes" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType =
        TDict(
          TCustomType(
            Ok(FQTypeName.Package PackageIDs.Type.Internal.Infra.tableSize),
            []
          )
        )
      description =
        "Query the SQLite database for the current row count of all tables. Disk size is approximated via PRAGMA page_count * page_size. Data is logged to Honeycomb per table."
      fn =
        (function
        | _, _, _, [ DUnit ] ->
          uply {
            let! tableStats = LibDB.Db.tableStats ()

            let typeName =
              FQTypeName.Package PackageIDs.Type.Internal.Infra.tableSize

            return
              tableStats
              |> List.map (fun ts ->
                let fields =
                  [ ("disk", DInt64(ts.diskBytes))
                    ("rows", DInt64(ts.rows))
                    ("diskHuman", DString ts.diskHuman)
                    ("rowsHuman", DString ts.rowsHuman) ]

                (ts.relation, DRecord(typeName, typeName, [], Map fields)))
              |> Dval.dict (KTCustomType(typeName, []))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalInfraRaiseInternalException" 0
      typeParams = []
      parameters = [ Param.make "argument" (TVariable "a") "Added as a tag" ]
      returnType = TUnit
      description =
        "Raise an internal exception inside Dark. This is intended to test exceptions
        and exception tracking, not for any real use."
      fn =
        (function
        | _, _, _, [ arg ] ->
          Exception.raiseInternal
            "DarkInternal.raiseInternalException"
            [ "arg", arg ]
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "darkInternalInfraServerBuildHash" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Returns the git hash of the server's current deploy"
      fn =
        (function
        | _, _, _, [ DUnit ] -> LibService.Config.buildHash |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
