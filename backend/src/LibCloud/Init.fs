/// LibCloud holds the whole framework
module LibCloud.Init

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Db

open Prelude

module Telemetry = LibService.Telemetry


let waitForDB () : Task<unit> =
  task {
    use (_span : Telemetry.Span.T) = Telemetry.createRoot "wait for db"
    let mutable success = false
    let mutable count = 0
    Telemetry.addEvent "starting to loop to wait for DB" []
    while not success do
      use (_span : Telemetry.Span.T) = Telemetry.child "iteration" [ "count", count ]
      try
        let cs = LibService.DBConnection.connectionString ()
        let cs = FsRegEx.replace "password=.*;" "password=***;" cs
        let cs = FsRegEx.replace "Password=.*;" "Password=***;" cs
        printTime $"Trying to connect to DB ({count} - {cs})"
        count <- count + 1
        do!
          Sql.query "select current_date"
          |> Sql.parameters []
          |> Sql.executeStatementAsync
        success <- true
      with e ->
        Telemetry.addException [] e
        do! Task.Delay 10
    return ()
  }

type WaitForDB =
  | WaitForDB
  | DontWaitForDB

/// <summary>Initialize LibCloud.</summary>
///
/// <remarks> This function does not do any behaviour which accesses DB tables and
/// data, as the DB might not be migrated to it's correct form at this point (eg in
/// the test of dev environment). This is called by ProdExec, which does the
/// migration. You cannot expect the DB to be ready when LibCloud is initialized -
/// call `waitForDB` if you need that.</remarks>
let init (shouldWaitForDB : WaitForDB) (serviceName : string) : Task<unit> =
  task {
    printTime $"Initing LibCloud in {serviceName}"
    let dbTask =
      match shouldWaitForDB with
      | WaitForDB ->
        task {
          printTime "Initing DB connection"
          let! result = waitForDB ()
          printTime " Inited DB connection"
          return result
        }
      | DontWaitForDB -> Task.FromResult()

    let queueTask = Queue.init ()
    let traceStorageTask = TraceCloudStorage.init ()
    let! (_ : List<unit>) = Task.flatten [ queueTask; traceStorageTask; dbTask ]

    printTime $" Inited LibCloud in {serviceName}"
  }
