module LibBackend.Init

// LibBackend holds the whole framework

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Db

open Prelude
open Microsoft.Extensions.Diagnostics.HealthChecks

module Telemetry = LibService.Telemetry


let _waitForDB () : Task<unit> =
  task {
    use (_span : Telemetry.Span.T) = Telemetry.createRoot "wait for db"
    let mutable success = false
    let mutable count = 0
    Telemetry.addEvent "starting to loop to wait for DB" []
    while not success do
      use (_span : Telemetry.Span.T) = Telemetry.child "iteration" [ "count", count ]
      try
        count <- count + 1
        do!
          Sql.query "select current_date"
          |> Sql.parameters []
          |> Sql.executeStatementAsync
        success <- true
      with
      | e ->
        Telemetry.addException [] e
        do! Task.Delay 1000
    return ()
  }

type WaitForDB =
  | WaitForDB
  | DontWaitForDB

/// <summary>Initialize LibBackend.</summary>
///
/// <remarks> This function does not do any behaviour which accesses DB tables and
/// data, as the DB might not be migrated to it's correct form at this point (eg in
/// the test of dev environment). This is called by ProdExec, which does the
/// migration. You cannot expect the DB to be ready when LibBackend is initialized -
/// call `waitForDB` if you need that.</remarks>
let init (shouldWaitForDB : WaitForDB) (serviceName : string) : Task<unit> =
  task {
    print $"Initing LibBackend in {serviceName}"
    Db.init ()

    match shouldWaitForDB with
    | WaitForDB -> do! _waitForDB ()
    | DontWaitForDB -> ()

    do! Queue.init ()
    do! TraceCloudStorage.init ()

    print $" Inited LibBackend in {serviceName}"
  }
