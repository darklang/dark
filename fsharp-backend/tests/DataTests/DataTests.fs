module FuzzTests.Tests

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth

module Account = LibBackend.Account

type CheckpointData =
  { mutable complete : Set<string>
    mutable erroring : Set<string>
    broken : Set<string> }


let loadCheckpointData (filename : string) : CheckpointData =
  try
    LibBackend.File.readfile LibBackend.Config.NoCheck filename
    |> Json.Vanilla.deserialize<CheckpointData>
  with
  | e ->
    print "No test file found or test file error"
    print e.Message
    print (Exception.toMetadata e |> string)
    print e.StackTrace
    System.Environment.Exit(-1)
    Unchecked.defaultof<CheckpointData>

let saveCheckpointData (filename : string) (tested : CheckpointData) : unit =
  print "saving to test file"
  tested
  |> Json.Vanilla.prettySerialize
  |> LibBackend.File.writefile LibBackend.Config.NoCheck filename

/// Skip if it's something we verify is allowed
let shouldRun (cd : CheckpointData) (canvasName : CanvasName.T) : bool =
  let cn = string canvasName
  not (Set.contains cn cd.complete)
  && not (Set.contains cn cd.erroring)
  && not (Set.contains cn cd.broken)

let catchException
  (filename : string)
  (failOnError : bool)
  (cd : CheckpointData)
  (e : exn)
  =
  try
    if failOnError then print "exiting" else print "error found"
    saveCheckpointData filename cd
    print e.Message
    print (Exception.toMetadata e |> string)
    if failOnError then
      e.StackTrace
      |> FsRegEx.replace
           "at Prelude.Task.foldSequentially@1475-20.Invoke(Unit unitVar0) in /home/dark/app/fsharp-backend/src/Prelude/Prelude.fs:line 1475"
           ""
      |> FsRegEx.replace
           "at Ply.TplPrimitives.ContinuationStateMachine`1.System-Runtime-CompilerServices-IAsyncStateMachine-MoveNext()"
           ""
      |> print
      System.Environment.Exit(-1)
  with
  | e ->
    print $"exception in catch exception: {e.Message}"
    System.Environment.Exit(-1)



let forEachCanvas
  (canvasConcurrency : int)
  (filename : string)
  (failOnError : bool)
  (cd : CheckpointData)
  (fn : Tests.ApiServer.C -> CanvasName.T -> Task<unit>)
  : Task<unit> =
  task {
    let semaphore = new System.Threading.SemaphoreSlim(canvasConcurrency)
    let! canvases = LibBackend.Serialize.getAllCanvases ()
    let! (result : List<unit>) =
      canvases
      |> List.filter (shouldRun cd)
      |> Task.mapInParallel (fun canvasName ->
        task {
          do! semaphore.WaitAsync()
          let username = (Account.ownerNameFromCanvasName canvasName).toUserName ()
          let! user = Account.getUser username
          let client = Tests.ApiServer.forceLogin username
          let user = Exception.unwrapOptionInternal "" [] user
          print $"start c: {canvasName}"
          try
            let! result = fn (lazy client) canvasName
            print $"done  c: {canvasName}"
            cd.complete <- Set.add cd.complete (string canvasName)
            saveCheckpointData filename cd
            semaphore.Release() |> ignore<int>
            return result
          with
          | e ->
            print $"                failed at {canvasName}"
            catchException filename failOnError cd e
            // if catchException exits, so that before saving
            cd.erroring <- Set.add cd.erroring (string canvasName)
            semaphore.Release() |> ignore<int>
        })
    return ()
  }


[<EntryPoint>]
let main args =
  LibService.Init.init "Tests"
  LibExecution.Init.init "Tests"
  LibExecutionStdLib.Init.init "Tests"
  (LibBackend.Init.init "Tests" true).Result
  LibRealExecution.Init.init "Tests"
  HttpMiddleware.Init.init "Tests"
  TestUtils.Init.init "Tests"

  LibService.Telemetry.Console.loadTelemetry
    "DataTests"
    LibService.Telemetry.DontTraceDBQueries

  let concurrency = int args[1]
  let filename = args[2]
  let failOnError = args[3] = "--fail"
  print $"Fail on error: {failOnError}"

  let checkpointData = loadCheckpointData filename
  let handler _ _ = saveCheckpointData filename checkpointData
  System.Console.CancelKeyPress.AddHandler(
    new System.ConsoleCancelEventHandler(handler)
  )

  try
    (forEachCanvas
      concurrency
      filename
      failOnError
      checkpointData
      Tests.ApiServer.testGetTraceData)
      .Result
  with
  | e -> catchException filename true checkpointData e
  0
