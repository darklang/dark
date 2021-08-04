module Tests.Prelude

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude
open TestUtils

let canvasName =
  testMany
    "canvasName.create"
    (fun c ->
      try
        CanvasName.create c |> ignore
        true
      with
      | e -> false)
    [ ("a", true)
      ("demo-hello", true)
      ("demo-", false)
      ("demo--", false)
      ("demo", true)
      ("demo-hello-world", true)
      ("demo-hello_world", true)
      ("demo-hello world", false)
      ("-demo", false)
      ("demo-(^@^)", false)
      ("demo-a_a", true)
      ("demo-9", true) ]

let asyncTests =

  // slow it down so later items might be run first
  let delay (f : unit -> 'a) (i : int) : TaskOrValue<'a> =
    taskv {
      do! Task.Delay(100 - (i * 10))
      return (f ())
    }

  testList
    "TaskOrValue"
    [ testTask "map_s" {
        let fn (i : int) = delay (fun () -> i + 1) i
        let! result = List.map_s fn [ 1; 2; 3; 4 ] |> TaskOrValue.toTask
        Expect.equal result [ 2; 3; 4; 5 ] ""
      }
      testTask "filter_s" {
        let fn (i : int) = taskv { return (i % 2) = 0 }
        let! result = List.filter_s fn [ 1; 2; 3; 4 ] |> TaskOrValue.toTask
        Expect.equal result [ 2; 4 ] ""
      }
      testTask "find_s" {
        let fn (i : int) = delay (fun () -> i = 3) i
        let! result = List.find_s fn [ 1; 2; 3; 4 ] |> TaskOrValue.toTask
        Expect.equal result (Some 3) ""
      }
      testTask "iter_s" {
        let state = ref []
        let fn (i : int) = delay (fun () -> state := i + 1 :: !state) i
        do! List.iter_s fn [ 1; 2; 3; 4 ] |> TaskOrValue.toTask
        Expect.equal !state [ 5; 4; 3; 2 ] ""
      } ]


let tests = testList "prelude" [ canvasName; asyncTests ]
