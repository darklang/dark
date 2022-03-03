module FuzzTests.Tests

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth

open LibService.Exception

type CheckpointData = { mutable uiUsernames : Set<string> }
let testedFilename = "dataloadtest.json"

let loadCheckpointData () =
  try
    LibBackend.File.readfile LibBackend.Config.NoCheck testedFilename
    |> Json.Vanilla.deserialize<CheckpointData>
  with
  | e ->
    print "No test file found"
    { uiUsernames = Set [] }

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

let dataValidatorTests (cd : CheckpointData) : Task<unit> =
  task {
    try
      let userSemaphor = new System.Threading.SemaphoreSlim(20)
      let canvasSemaphore = new System.Threading.SemaphoreSlim(20)
      let! users = LibBackend.Account.getUsers ()
      let! (results : List<unit>) =
        users
        |> Task.mapInParallel (fun username ->
          task {
            if Set.contains (string username) cd.uiUsernames then
              print $"already completed: {username}"
              return [ () ]
            else
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
                    print $"start c: {canvasName}"
                    let! result =
                      Tests.ApiServer.testUiReturnsTheSame (lazy client) canvasName
                    print $"done  c: {canvasName}"
                    canvasSemaphore.Release() |> ignore<int>
                    return result
                  })
              print $"done u:  {username}"
              cd.uiUsernames <- Set.add cd.uiUsernames (string username)
              saveCheckpointData cd
              userSemaphor.Release() |> ignore<int>
              return result
          })
        |> Task.map List.flatten
      return ()
    with
    | e ->
      saveCheckpointData cd
      e.Reraise()
  }


open Microsoft.Extensions.Hosting

[<EntryPoint>]
let main args =
  let builder =
    Host
      .CreateDefaultBuilder(args)
      .ConfigureServices(fun _ -> ())
      .UseConsoleLifetime(fun options -> ())
      .Build()
  let checkpointData = loadCheckpointData ()
  let handler obj args = saveCheckpointData checkpointData
  System.Console.CancelKeyPress.AddHandler(
    new System.ConsoleCancelEventHandler(handler)
  )
  (dataValidatorTests checkpointData).Result
  0
