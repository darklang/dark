/// LibCloud holds the whole framework
module LibCloud.Init

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open Db

open Prelude

type WaitForDB =
  | WaitForDB
  | DontWaitForDB

let waitForDB (shouldWaitForDB : WaitForDB) : Task<unit> =
  match shouldWaitForDB with
  | WaitForDB ->
    task {
      printTime "Initing DB connection"
      let! result = Db.waitUntilConnected ()
      printTime " Inited DB connection"
      return result
    }
  | DontWaitForDB -> Task.FromResult()

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
    let dbTask = waitForDB shouldWaitForDB

    let queueTask = Queue.init ()
    let traceStorageTask = TraceCloudStorage.init ()
    let! (_ : List<unit>) = Task.flatten [ queueTask; traceStorageTask; dbTask ]

    printTime $" Inited LibCloud in {serviceName}"
  }
