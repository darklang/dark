module FuzzTests.Tests

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils

module Account = LibBackend.Account
module Canvas = LibBackend.Canvas
module Execution = LibExecution.Execution
module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes

type CDict() =
  inherit System.Collections.Concurrent.ConcurrentDictionary<string, bool>()
  member this.has(str : string) : bool = this.ContainsKey(str)
  member this.add(str : string) : unit = this.TryAdd(str, true) |> ignore<bool> // false if already exists, which is fine
  static member create(strs : Set<string>) : CDict =
    let result = CDict()
    Set.iter (fun str -> result.TryAdd(str, true) |> ignore<bool>) strs
    result

type SerializedCheckpointData =
  { complete : Set<string>
    erroring : Set<string>
    broken : Set<string> }

type CheckpointData = { complete : CDict; erroring : CDict; broken : Set<string> }


let loadCheckpointData (filename : string) : CheckpointData =
  try
    let scd =
      LibBackend.File.readfile LibBackend.Config.NoCheck filename
      |> Json.Vanilla.deserialize<SerializedCheckpointData>
    { complete = CDict.create scd.complete
      erroring = CDict.create scd.erroring
      broken = scd.broken }
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
  let cd : SerializedCheckpointData =
    { complete = tested.complete.Keys |> Set
      broken = tested.broken
      erroring = tested.erroring.Keys |> Set }
  cd
  |> Json.Vanilla.prettySerialize
  |> LibBackend.File.writefile LibBackend.Config.NoCheck filename

/// Skip if it's something we verify is allowed
let shouldRun (cd : CheckpointData) (canvasName : CanvasName.T) : bool =
  let cn = string canvasName
  not (cd.complete.has (cn))
  && not (cd.erroring.has cn)
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





/// Iterate through all canvases, running `fn` and storing thhe results
let forEachCanvas
  (concurrency : int)
  (filename : string)
  (failOnError : bool)
  (cd : CheckpointData)
  (fn : CanvasName.T -> Task<unit>)
  : Task<unit> =
  task {
    let semaphore = new System.Threading.SemaphoreSlim(concurrency)
    let! canvases = LibBackend.Serialize.getAllCanvases ()
    let! (result : List<unit>) =
      canvases
      |> List.filter (shouldRun cd)
      |> Task.mapInParallel (fun canvasName ->
        task {
          do! semaphore.WaitAsync()
          print $"start c: {canvasName}"
          try
            let! result = fn canvasName
            print $"done  c: {canvasName}"
            cd.complete.add (string canvasName)
            saveCheckpointData filename cd
            semaphore.Release() |> ignore<int>
            return result
          with
          | e ->
            print $"                failed at {canvasName}"
            catchException filename failOnError cd e
            // if catchException exits, so that before saving
            cd.erroring.add (string canvasName)
            semaphore.Release() |> ignore<int>
        })
    return ()
  }


/// Iterate through all canvases passing in an appropriate HTTP client
let forEachCanvasWithClient
  (concurrency : int)
  (filename : string)
  (failOnError : bool)
  (cd : CheckpointData)
  (fn : Tests.ApiServer.C -> CanvasName.T -> Task<unit>)
  =
  forEachCanvas concurrency filename failOnError cd (fun canvasName ->
    let username = (Account.ownerNameFromCanvasName canvasName).toUserName ()
    let client = lazy (Tests.ApiServer.forceLogin username)
    fn client canvasName)

/// Iterate through all canvases and all DBs in those canvases
let loadAllUserData
  (concurrency : int)
  (filename : string)
  (failOnError : bool)
  (cd : CheckpointData)
  =
  forEachCanvas concurrency filename failOnError cd (fun canvasName ->
    task {
      let! meta = Canvas.getMeta canvasName
      let! c = Canvas.loadAllDBs meta
      let dbs =
        c.dbs
        |> Map.values
        |> List.map (fun db -> db.name, PT.DB.toRuntimeType db)
        |> Map
      let! state = TestUtils.TestUtils.executionStateFor meta dbs Map.empty
      let! (result : List<unit>) =
        dbs
        |> Map.values
        |> List.map (fun (db : RT.DB.T) ->
          uply {
            let code = $"DB.getAllWithKeys_v2 {db.name}"
            let ast = FSharpToExpr.parsePTExpr code
            let! actual =
              LibExecution.Execution.executeExpr
                state
                (Map.map (fun (db : RT.DB.T) -> RT.DDB db.name) dbs)
                (ast.toRuntimeType ())

            // For this to work, we need to make PasswordBytes.to_yojson return
            // [`String (Bytes.to_string bytes)]
            let! expected =
              LibBackend.OCamlInterop.execute
                state.program.accountID
                state.program.canvasID
                ast
                Map.empty
                (Map.values c.dbs)
                []
            let expected =
              match expected with
              | RT.DError (source, str) ->
                RT.DError(source, TestUtils.TestUtils.parseOCamlError str)
              | other -> other

            // The values should be the same
            Expect.equal actual expected "getAll should be the same equal"
            return ()
          }
          |> Ply.toTask)
        |> Task.flatten

      let fn cn = task { return () }

      return! fn canvasName
    })


/// Iterate through all canvases and load all data from the queue
let loadAllQueueData
  (concurrency : int)
  (filename : string)
  (failOnError : bool)
  (cd : CheckpointData)
  =
  forEachCanvas concurrency filename failOnError cd (fun canvasName ->
    task {
      let! dvalStrs = LibBackend.EventQueue.fetchAllQueueItems canvasName
      let! (_ : List<unit>) =
        dvalStrs
        |> List.map (fun dvalStr ->
          task {
            let fsharpDval =
              LibExecution.DvalReprInternal.ofInternalRoundtrippableV0 dvalStr
            let! ocamlDval =
              LibBackend.OCamlInterop.ofInternalRoundtrippableV0 dvalStr
            return Expect.equalDval fsharpDval ocamlDval ""
          })
        |> Task.flatten
      return ()
    })


let loadAllTraceData
  (concurrency : int)
  (filename : string)
  (failOnError : bool)
  (cd : CheckpointData)
  =
  forEachCanvasWithClient
    concurrency
    filename
    failOnError
    cd
    (fun client canvasName -> Tests.ApiServer.testGetTraceData client canvasName)


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
    (loadAllQueueData concurrency filename failOnError checkpointData).Result
  with
  | e -> catchException filename true checkpointData e
  0
