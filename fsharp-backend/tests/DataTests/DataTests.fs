module FuzzTests.Tests

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth

open LibService.Exception

type CheckpointData = { mutable complete : Set<string>; ignored : Set<string> }
let testedFilename = "datatests.json"

let loadCheckpointData () =
  try
    LibBackend.File.readfile LibBackend.Config.NoCheck testedFilename
    |> Json.Vanilla.deserialize<CheckpointData>
  with
  | e ->
    print "No test file found"
    { complete = Set []; ignored = Set [] }

let saveCheckpointData (tested : CheckpointData) : unit =
  print "saving to test file"
  tested
  |> Json.Vanilla.prettySerialize
  |> LibBackend.File.writefile LibBackend.Config.NoCheck testedFilename

/// Skip if it's something we verify is allowed
let shouldRun (canvasName : CanvasName.T) : bool =
  let cn = string canvasName
  not (String.endsWith "-" (string cn))
  && not (String.endsWith "_" (string cn))
  && not (cn.ToString().Contains("--"))
  && not (cn.ToString().Contains("__"))

let catchException (cd : CheckpointData) (e : exn) =
  try
    print "exiting"
    saveCheckpointData cd
    print e.Message
    print (Exception.toMetadata e |> string)
    e.StackTrace
    |> FsRegEx.replace
         "at Prelude.Task.foldSequentially@1475-20.Invoke(Unit unitVar0) in /home/dark/app/fsharp-backend/src/Prelude/Prelude.fs:line 1475"
         ""
    |> FsRegEx.replace
         "at Ply.TplPrimitives.ContinuationStateMachine`1.System-Runtime-CompilerServices-IAsyncStateMachine-MoveNext()"
         ""
    |> print
  finally
    System.Environment.Exit(-1)



let forEachCanvas
  (users : int)
  (canvases : int)
  (cd : CheckpointData)
  (fn : Tests.ApiServer.C -> CanvasName.T -> Task<unit>)
  : Task<unit> =
  task {
    let userSemaphor = new System.Threading.SemaphoreSlim(users)
    let canvasSemaphore = new System.Threading.SemaphoreSlim(canvases)
    let! users = LibBackend.Account.getUsers ()
    let! (results : List<unit>) =
      users
      |> Task.mapInParallel (fun username ->
        task {
          if Set.contains (string username) cd.complete then
            return [ () ]
          elif Set.contains (string username) cd.ignored then
            print $"ignoring {username}"
            return [ () ]
          else
            try
              do! userSemaphor.WaitAsync()
              print $"start u: {username}"
              let! user = LibBackend.Account.getUser username
              let client = Tests.ApiServer.forceLogin username
              let user = Exception.unwrapOptionInternal "" [] user
              let! canvases = LibBackend.Account.ownedCanvases user.id
              let! result =
                canvases
                |> List.filter shouldRun
                |> Task.mapInParallel (fun canvasName ->
                  task {
                    do! canvasSemaphore.WaitAsync()
                    print
                      $"                                            start c: {canvasName}"
                    try
                      let! result = fn (lazy client) canvasName
                      print
                        $"                                            done  c: {canvasName}"
                      canvasSemaphore.Release() |> ignore<int>
                      return result
                    with
                    | e -> catchException cd e
                  })
              print $"DONE u:  {username}"
              cd.complete <- Set.add cd.complete (string username)
              saveCheckpointData cd
              userSemaphor.Release() |> ignore<int>
              return result
            with
            | e ->
              catchException cd e
              return [ () ]
        })
      |> Task.map List.flatten
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

  let checkpointData = loadCheckpointData ()
  let handler _ _ = saveCheckpointData checkpointData
  System.Console.CancelKeyPress.AddHandler(
    new System.ConsoleCancelEventHandler(handler)
  )
  let userCount =
    try
      (int args[1])
    with
    | _ -> 1
  let canvasCount =
    try
      (int args[2])
    with
    | _ -> 20
  try
    (forEachCanvas
      userCount
      canvasCount
      checkpointData
      Tests.ApiServer.testInitialLoadReturnsTheSame)
      .Result
  with
  | e -> catchException checkpointData e
  0
