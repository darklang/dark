/// StdLib functions for looking at Dark infra
module StdLibDarkInternal.Libs.Infra

open System.Threading.Tasks

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module DvalReprDeveloper = LibExecution.DvalReprDeveloper
module Telemetry = LibService.Telemetry

let modules = [ "DarkInternal"; "Infra" ]
let typ = typ modules
let fn = fn modules


let types : List<BuiltInType> =
  [ { name = typ "TableSize" 0
      declaration =
        { typeParams = []
          definition =
            TypeDeclaration.Record(
              { name = "disk"; typ = TInt; description = "Number of bytes on disk" },
              [ { name = "rows"; typ = TInt; description = "Number of rows" }
                { name = "diskHuman"
                  typ = TString
                  description = "Disk space in human readable form" }
                { name = "rowsHuman"
                  typ = TString
                  description = "Number of rows in human readable form" } ]
            ) }
      deprecated = NotDeprecated
      description = "Size info for Postgres tables" } ]


let fns : List<BuiltInFn> =
  [ { name = fn "log" 0
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
        | _, _, [ DString level; DString name; DDict log as result ] ->
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


    { name = fn "getAndLogTableSizes" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TDict(stdlibTypeRef [ "DarkInternal"; "Infra" ] "TableSize" 0)
      description =
        "Query the postgres database for the current size (disk + rowcount) of all
tables. This uses pg_stat, so it is fast but imprecise. This function is logged
via the backend; its primary purpose is to send data to Honeycomb, but also gives
human-readable data."
      fn =
        (function
        | _, _, [ DUnit ] ->
          uply {
            let! tableStats = LibBackend.Db.tableStats ()
            // Send events to honeycomb. We could save some events by sending
            // these all as a single event - tablename.disk = 1, etc - but
            // by having an event per table, it's easier to query and graph:
            // `VISUALIZE MAX(disk), MAX(rows);  GROUP BY relation`.
            // (Also, if/when we add more tables, the graph-query doesn't need
            // to be updated)
            //
            // There are ~40k minutes/month, and 20 tables, so a 1/min cron
            // would consume 80k of our 1.5B monthly events. That seems
            // reasonable.
            tableStats
            |> List.iter (fun ts ->
              Telemetry.addEvent
                "postgres_table_sizes"
                [ ("relation", ts.relation)
                  ("disk_bytes", ts.diskBytes)
                  ("rows", ts.rows)
                  ("disk_human", ts.diskHuman)
                  ("rows_human", ts.rowsHuman) ])
            let typeName = FQName.BuiltIn(typ "TableSize" 0)
            return
              tableStats
              |> List.map (fun ts ->
                (ts.relation,
                 [ ("disk", DInt(ts.diskBytes))
                   ("rows", DInt(ts.rows))
                   ("diskHuman", DString ts.diskHuman)
                   ("rowsHuman", DString ts.rowsHuman) ]
                 |> Dval.record typeName))
              |> Map
              |> DDict
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "raiseInternalException" 0
      typeParams = []
      parameters = [ Param.make "argument" (TVariable "a") "Added as a tag" ]
      returnType = TUnit
      description =
        "Raise an internal exception inside Dark. This is intended to test exceptions
        and exception tracking, not for any real use."
      fn =
        (function
        | _, _, [ arg ] ->
          Exception.raiseInternal
            "DarkInternal.raiseInternalException"
            [ "arg", arg ]
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "serverBuildHash" 0
      typeParams = []
      parameters = [ Param.make "unit" TUnit "" ]
      returnType = TString
      description = "Returns the git hash of the server's current deploy"
      fn =
        (function
        | _, _, [ DUnit ] -> uply { return DString LibService.Config.buildHash }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]

let constants : List<BuiltInConstant> = []
let contents = (fns, types, constants)
