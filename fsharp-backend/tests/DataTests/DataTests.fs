module FuzzTests.Tests

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth
open TestUtils.TestUtils

module Account = LibBackend.Account
module Serialize = LibBackend.Serialize
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

module CD =
  let mutable needsSaving = false
  let mutable filename = ""
  let mutable checkpointData : CheckpointData = Unchecked.defaultof<CheckpointData>

  let init (file : string) : unit =
    filename <- file
    try
      let scd =
        LibBackend.File.readfile LibBackend.Config.NoCheck filename
        |> Json.Vanilla.deserialize<SerializedCheckpointData>
      checkpointData <-
        { complete = CDict.create scd.complete
          erroring = CDict.create scd.erroring
          broken = scd.broken }
    with
    | e ->
      print $"No test file found or test file error: {filename}"
      print e.Message
      print (Exception.toMetadata e |> string)
      print e.StackTrace
      System.Environment.Exit(-1)

  let saveCheckpointData () : unit =
    print "saving to test file"
    let scd : SerializedCheckpointData =
      { complete = checkpointData.complete.Keys |> Set
        broken = checkpointData.broken
        erroring = checkpointData.erroring.Keys |> Set }
    scd
    |> Json.Vanilla.prettySerialize
    |> LibBackend.File.writefile LibBackend.Config.NoCheck filename


  let markComplete (cn : CanvasName.T) =
    checkpointData.complete.add (string cn)
    needsSaving <- true

  let markErroring (cn : CanvasName.T) =
    checkpointData.erroring.add (string cn)
    needsSaving <- true

  /// Skip if it's something we verify is allowed
  let shouldRun (canvasName : CanvasName.T) : bool =
    let cn = string canvasName
    not (checkpointData.complete.has (cn))
    && not (checkpointData.erroring.has cn)
    && not (Set.contains cn checkpointData.broken)

  let saveInBackground () =
    let f () =
      while true do
        if needsSaving then
          saveCheckpointData ()
          needsSaving <- false
          System.Threading.Thread.Sleep 1000
    let thread = System.Threading.Thread(f)
    do
      thread.IsBackground <- true
      thread.Name <- "Background saving checkpoint"
      thread.Start()

  do saveInBackground ()







let catchException (failOnError : bool) (e : exn) =
  try
    if failOnError then print "exiting" else print "error found"
    print e.Message
    print (Exception.toMetadata e |> string)
    if failOnError then
      CD.saveCheckpointData ()
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
  (failOnError : bool)
  (fn : CanvasName.T -> Task<unit>)
  : Task<unit> =
  task {
    let semaphore = new System.Threading.SemaphoreSlim(concurrency)
    let! canvases = LibBackend.Serialize.getAllCanvases ()
    let! (result : List<unit>) =
      canvases
      |> List.filter CD.shouldRun
      |> Task.mapInParallel (fun canvasName ->
        task {
          do! semaphore.WaitAsync()
          print $"start c: {canvasName}"
          try
            let! result = fn canvasName
            print $"done  c: {canvasName}"
            CD.markComplete canvasName
            semaphore.Release() |> ignore<int>
            return result
          with
          | e ->
            print $"                failed at {canvasName}"
            catchException failOnError e
            // if catchException exits, do that before saving
            CD.markErroring canvasName
            semaphore.Release() |> ignore<int>
        })
    return ()
  }


/// Iterate through all canvases passing in an appropriate HTTP client
let forEachCanvasWithClient
  (concurrency : int)
  (failOnError : bool)
  (fn : Tests.ApiServer.C -> CanvasName.T -> Task<unit>)
  =
  forEachCanvas concurrency failOnError (fun canvasName ->
    let username = (Account.ownerNameFromCanvasName canvasName).toUserName ()
    let client = lazy (Tests.ApiServer.forceLogin username)
    fn client canvasName)

/// Iterate through all canvases and all DBs in those canvases
let loadAllUserData (concurrency : int) (failOnError : bool) =
  forEachCanvas concurrency failOnError (fun canvasName ->
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
let loadAllQueueData (concurrency : int) (failOnError : bool) =
  forEachCanvas concurrency failOnError (fun canvasName ->
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


let loadAllTraceData (concurrency : int) (failOnError : bool) =
  forEachCanvasWithClient concurrency failOnError (fun client canvasName ->
    Tests.ApiServer.testGetTraceData client canvasName)

open MessagePack
open MessagePack.Resolvers
open MessagePack.FSharp

let init () : unit =
  let resolver =
    Resolvers.CompositeResolver.Create(
      FSharpResolver.Instance,
      StandardResolver.Instance,
      NativeGuidResolver.Instance,
      ContractlessStandardResolver.Instance
    )
  let options = MessagePackSerializerOptions.Standard.WithResolver(resolver)
  MessagePackSerializer.DefaultOptions <- options


let serialize (data : 'a) : byte [] = MessagePackSerializer.Serialize<'a> data



let deserialize<'a> (bytes : byte []) : 'a =
  MessagePackSerializer.Deserialize<'a> bytes

let validate (expected : 'a when 'a : equality) =
  // serialize
  let serializeWatch = System.Diagnostics.Stopwatch()
  serializeWatch.Start()
  let bytes = serialize expected
  serializeWatch.Stop()
  debuG "serialize time  " serializeWatch.ElapsedMilliseconds
  debuG "serialized size " (Array.length bytes)

  // deserialize
  let deserializeWatch = System.Diagnostics.Stopwatch()
  deserializeWatch.Start()
  let (actual : 'a) = deserialize<'a> (bytes)
  deserializeWatch.Stop()
  debuG "deserialize time" deserializeWatch.ElapsedMilliseconds

  // test it
  Expect.equal actual expected "same"




let checkRendered (meta : Canvas.Meta) (tlids : List<tlid>) =
  task {
    let loadWatch = System.Diagnostics.Stopwatch()
    loadWatch.Start()
    let! expected = Serialize.loadOnlyRenderedTLIDs meta.id tlids
    loadWatch.Stop()
    debuG "load time" loadWatch.ElapsedMilliseconds
    debuG "rendered items" (List.length expected)
    return validate expected
  }

let checkOplists (meta : Canvas.Meta) (tlids : List<tlid>) =
  task {
    let loadWatch = System.Diagnostics.Stopwatch()
    loadWatch.Start()
    let! expected = Canvas.loadOplists Canvas.IncludeDeletedToplevels meta.id tlids
    loadWatch.Stop()
    debuG "load time" loadWatch.ElapsedMilliseconds
    debuG "oplist load items" (List.length expected)
    return validate expected
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

  init ()

  let fn (canvasName : CanvasName.T) : Task<unit> =
    task {
      let! meta = Canvas.getMeta canvasName
      let! tlids = Serialize.fetchAllTLIDs meta.id

      do! checkRendered meta tlids
      do! checkOplists meta tlids
      return ()
    }

  let concurrency = int args[1]
  let filename = args[2]
  let failOnError = args[3] = "--fail"
  print $"Fail on error: {failOnError}"

  CD.init filename
  let handler _ _ = CD.saveCheckpointData ()
  System.Console.CancelKeyPress.AddHandler(
    new System.ConsoleCancelEventHandler(handler)
  )

  try
    (forEachCanvas concurrency failOnError fn).Result
    CD.saveCheckpointData ()
  with
  | e -> catchException true e
  0
