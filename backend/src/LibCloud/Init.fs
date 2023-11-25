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
    // get the id of the host to check it resolves
    LibService.Config.pgHost
    |> System.Net.Dns.GetHostAddresses
    |> Seq.iter (fun ip -> printTime $"IP is ip {ip}")
    // check it's reachable - using a method which works across vpcs on google cloud
    let url = $"{LibService.Config.pgHost}:{LibService.Config.pgPort}"
    try
      let client = new System.Net.Sockets.TcpClient()
      let connection =
        client.BeginConnect(
          LibService.Config.pgHost,
          LibService.Config.pgPort,
          null,
          null
        )
      client.EndConnect(connection)
      let success = connection.AsyncWaitHandle.WaitOne(5000)
      if not success then
        printTime $"Failed to connect to {url}"
      else
        printTime $"Got connected to {url}"
    with e ->
      Telemetry.addException [] e
      printTime $"Failed to get response from {url}: {e.Message} {e.StackTrace}"

    Telemetry.addEvent "starting to loop to wait for DB" []
    while not success do
      use (_span : Telemetry.Span.T) = Telemetry.child "iteration" [ "count", count ]
      try
        let cs = LibService.DBConnection.connectionString ()
        let cs = FsRegEx.replace "password=.*?;" "password=***;" cs
        let cs = FsRegEx.replace "Password=.*?;" "Password=***;" cs
        printTime $"Trying to connect to DB ({count} - {cs})"
        count <- count + 1
        do!
          Sql.query "select current_date"
          |> Sql.parameters []
          |> Sql.executeStatementAsync
        success <- true
      with e ->
        Telemetry.addException [] e
        printTime $"Failed to connect to DB: {e.Message} {e.StackTrace}"
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
