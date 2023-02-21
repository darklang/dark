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
module BinarySerialization = LibBinarySerialization.BinarySerialization
module Canvas = LibBackend.Canvas
module Execution = LibExecution.Execution
module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes

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
      printException "" [] e
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
    print (string (Exception.toMetadata e))
    if failOnError then
      CD.saveCheckpointData ()
      e.StackTrace
      |> FsRegEx.replace
           "at Prelude.Task.foldSequentially@1475-20.Invoke(Unit unitVar0) in /home/dark/app/backend/src/Prelude/Prelude.fs:line 1475"
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
    return!
      canvases
      |> List.filter CD.shouldRun
      |> Task.iterInParallel (fun canvasName ->
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
  }


let validate
  (name : string)
  (expected : 'a)
  (serialize : 'a -> byte [])
  (deserialize : byte [] -> 'a)
  =
  try
    // serialize
    let serializeWatch = System.Diagnostics.Stopwatch()
    serializeWatch.Start()
    let bytes = serialize expected
    serializeWatch.Stop()
    // debuG $"{name} serialize time  " serializeWatch.ElapsedMilliseconds
    // debuG $"{name} serialized size " (Array.length bytes)
    let _token = System.Threading.CancellationToken()

    // print (
    //   MessagePack.MessagePackSerializer.SerializeToJson(expected, options, token)
    // )

    // deserialize
    let deserializeWatch = System.Diagnostics.Stopwatch()
    deserializeWatch.Start()
    // debuG $"{name} serialized" (UTF8.ofBytesWithReplacement bytes)
    let actual = deserialize bytes
    deserializeWatch.Stop()
    // debuG $"{name} deserialize time" deserializeWatch.ElapsedMilliseconds

    // test it
    Expect.equal actual expected $"same: {name}"
  with
  | e ->
    print e.Message
    print e.StackTrace
    reraise ()

let checkCached (meta : Canvas.Meta) (tlids : List<tlid>) =
  task {
    let loadWatch = System.Diagnostics.Stopwatch()
    loadWatch.Start()
    let! expected = Serialize.loadOnlyCachedTLIDs meta.id tlids
    loadWatch.Stop()
    // debuG "legacyserver load time" loadWatch.ElapsedMilliseconds
    // debuG "cached items" (List.length expected)
    expected
    |> List.iter (fun v ->
      let tlid = PT.Toplevel.toTLID v
      validate
        "cached"
        v
        BinarySerialization.serializeToplevel
        (BinarySerialization.deserializeToplevel tlid))
    return ()
  }

let checkOplists (meta : Canvas.Meta) (tlids : List<tlid>) =
  task {
    let loadWatch = System.Diagnostics.Stopwatch()
    loadWatch.Start()
    let! expected =
      Serialize.loadOplists Serialize.IncludeDeletedToplevels meta.id tlids
    loadWatch.Stop()
    // debuG "load time" loadWatch.ElapsedMilliseconds
    // debuG "oplist load items" (List.length expected)
    expected
    |> List.iter (fun (tlid, v) ->
      validate
        "oplists"
        v
        (BinarySerialization.serializeOplist tlid)
        (BinarySerialization.deserializeOplist tlid))
    return ()
  }

let initSerializers () =
  // TODO: this probably needs more serializable types registered
  // Resolve this next time we run DataTests
  Json.Vanilla.allow<SerializedCheckpointData> "datatests"

[<EntryPoint>]
let main args =
  let name = "DataTests"
  LibService.Init.init name
  LibService.Telemetry.Console.loadTelemetry
    "DataTests"
    LibService.Telemetry.DontTraceDBQueries
  (LibBackend.Init.init LibBackend.Init.WaitForDB name).Result
  (LibRealExecution.Init.init name).Result
  initSerializers ()

  let fn (canvasName : CanvasName.T) : Task<unit> =
    task {
      let! meta = Canvas.getMetaExn canvasName
      let! tlids = Serialize.fetchAllTLIDs meta.id

      do! checkCached meta tlids
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
