/// StdLib functions for looking at Dark infra
module StdLibDarkInternal.Libs.Infra

open System.Threading.Tasks

open Npgsql.FSharp
open LibBackend.Db

open Prelude

open LibExecution.RuntimeTypes

module PT = LibExecution.ProgramTypes
module DvalReprDeveloper = LibExecution.DvalReprDeveloper
module Errors = LibExecution.Errors
module Telemetry = LibService.Telemetry

open LibBackend
module SchedulingRules = LibBackend.QueueSchedulingRules

let fn = FQFnName.stdlibFnName
let typ = FQTypeName.stdlibTypeName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"


let types : List<BuiltInType> = []


let fns : List<BuiltInFn> =
  [ { name = fn "DarkInternal" "log" 0
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


    { name = fn "DarkInternal" "getAndLogTableSizes" 0
      typeParams = []
      parameters = []
      returnType = TDict varA
      // returnType = varA CLEANUPTYPES
      description =
        "Query the postgres database for the current size (disk + rowcount) of all
tables. This uses pg_stat, so it is fast but imprecise. This function is logged
in OCaml; its primary purpose is to send data to honeycomb, but also gives
human-readable data."
      fn =
        (function
        | _, _, [] ->
          uply {
            let! tableStats = Db.tableStats ()
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
            // Reformat a bit for human-usable dval output.
            // - Example from my local dev: {
            //     Total: {
            //       disk: 835584,
            //       diskHuman: "816 kB",
            //       rows: 139,
            //       rowsHuman: 139
            //     },
            //     access: {...},
            //     ...
            // }
            return
              tableStats
              |> List.map (fun ts ->
                (ts.relation,
                 [ ("disk_bytes", DInt(ts.diskBytes))
                   ("rows", DInt(ts.rows))
                   ("disk_human", DString ts.diskHuman)
                   ("rows_human", DString ts.rowsHuman) ]
                 |> Map
                 |> DRecord))
              |> Map
              |> DDict
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "raiseInternalException" 0
      typeParams = []
      parameters = [ Param.make "argument" varA "Added as a tag" ]
      returnType = TUnit
      description =
        "Raise an internal exception inside Dark. This is intended to test exceptions
        and exception tracking, not for any real use."
      fn =
        (function
        | _, _, [ arg ] ->
          Exception.raiseInternal
            "DarkInternal::raiseInternalException"
            [ "arg", arg ]
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "DarkInternal" "serverBuildHash" 0
      typeParams = []
      parameters = []
      returnType = TString
      description = "Returns the git hash of the server's current deploy"
      fn =
        (function
        | _, _, [] -> uply { return DString LibService.Config.buildHash }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]