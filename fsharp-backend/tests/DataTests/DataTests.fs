module FuzzTests.Tests

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Prelude.Tablecloth
open Tablecloth

open LibService.Exception

type Tested = { mutable uiTestedUsernames : Set<string> }
let testedFilename = "dataloadtest.json"

let loadTested () =
  try
    LibBackend.File.readfile LibBackend.Config.NoCheck testedFilename
    |> Json.Vanilla.deserialize<Tested>
  with
  | e ->
    print "No test file found"
    { uiTestedUsernames = Set [] }

let saveTested (tested : Tested) : unit =
  tested
  |> Json.Vanilla.prettySerialize
  |> LibBackend.File.writefile LibBackend.Config.NoCheck testedFilename

let dataValidatorTests () : Task<unit> =
  task {
    let tested = loadTested ()
    try

      let! users = LibBackend.Account.getUsers ()
      let! (results : List<unit>) =
        users
        |> Task.mapSequentially (fun username ->
          task {
            print $"start u: {username}"
            if Set.contains (string username) tested.uiTestedUsernames then
              print $"already completed: {username}"
              return [ () ]
            else
              let! user = LibBackend.Account.getUser username
              let client = Tests.ApiServer.forceLogin username
              let user = Exception.unwrapOptionInternal "" [] user
              let! canvases = LibBackend.Account.ownedCanvases user.id
              let! result =
                canvases
                |> List.filter (fun cn ->
                  not (String.endsWith "-" (string cn))
                  && not (cn.ToString().Contains("--")))
                |> Task.mapInParallel (fun canvasName ->
                  task {
                    print $"start c: {canvasName}"
                    let! result =
                      Tests.ApiServer.testUiReturnsTheSame (lazy client) canvasName
                    print $"done  c: {canvasName}"
                    return result
                  })
              print $"done u:  {username}"
              tested.uiTestedUsernames <-
                Set.add tested.uiTestedUsernames (string username)
              return result
          })
        |> Task.map List.flatten
      return ()
    with
    | e ->
      saveTested tested
      e.Reraise()
  }

let tests =
  testList
    "tests"
    [ testTask "data-validator" { do! dataValidatorTests () }
      // testPostApi "all_traces" "" (deserialize<Traces.AllTraces.T>) ident
      // testDelete404s
      // testExecuteFunction
      // testPostApi "get_404s" "" (deserialize<F404s.List.T>) ident
      // testDBStats
      // testGetTraceData
      // testPostApi "get_unlocked_dbs" "" (deserialize<DBs.Unlocked.T>) ident
      // testWorkerStats
      // testInitialLoadReturnsTheSame
      // testInsertDeleteSecrets
      // testPostApi "packages" "" (deserialize<Packages.List.T>) canonicalizePackages
      // testTriggerHandler
      // FSTODO worker_schedule
      ]



[<EntryPoint>]
let main args = runTestsWithCLIArgs [] args tests
