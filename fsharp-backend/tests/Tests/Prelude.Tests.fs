module Tests.Prelude

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto
open Prelude
open TestUtils.TestUtils

let canvasName =
  testMany
    "canvasName.create"
    (fun c ->
      try
        CanvasName.create c |> ignore<CanvasName.T>
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
  let delay (f : unit -> 'a) (i : int) : Ply<'a> =
    uply {
      do! Task.Delay(100 - (i * 10))
      return (f ())
    }

  testList
    "sequential"
    [ testTask "mapSequentially" {
        let fn (i : int) = delay (fun () -> i + 1) i
        let! result = Ply.List.mapSequentially fn [ 1; 2; 3; 4 ] |> Ply.toTask
        Expect.equal result [ 2; 3; 4; 5 ] ""
      }
      testTask "filterSequentially" {
        let fn (i : int) = uply { return (i % 2) = 0 }
        let! result = Ply.List.filterSequentially fn [ 1; 2; 3; 4 ] |> Ply.toTask
        Expect.equal result [ 2; 4 ] ""
      }
      testTask "findSequentially" {
        let fn (i : int) = delay (fun () -> i = 3) i
        let! result = Ply.List.findSequentially fn [ 1; 2; 3; 4 ] |> Ply.toTask
        Expect.equal result (Some 3) ""
      }
      testTask "iterSequentially" {
        let state = ref []
        let fn (i : int) = delay (fun () -> state.Value <- i + 1 :: state.Value) i
        do! Ply.List.iterSequentially fn [ 1; 2; 3; 4 ] |> Ply.toTask
        Expect.equal state.Value [ 5; 4; 3; 2 ] ""
      } ]

let mapTests =
  testList
    "map"
    [ testMany2
        "Map.mergeFavoringRight"
        Map.mergeFavoringRight
        [ Map.empty, Map.empty, Map.empty
          (Map.ofList [ (1, 1); (2, 2); (3, 3) ],
           Map.ofList [ (1, -1); (2, -2); (3, -3) ],
           Map.ofList [ (1, -1); (2, -2); (3, -3) ]) ]
      testMany2
        "Map.mergeFavoringLeft"
        Map.mergeFavoringLeft
        [ Map.empty, Map.empty, Map.empty
          (Map.ofList [ (1, 1); (2, 2); (3, 3) ],
           Map.ofList [ (1, -1); (2, -2); (3, -3) ],
           Map.ofList [ (1, 1); (2, 2); (3, 3) ]) ] ]

let listTests =
  testList
    "List"
    [ testMany2
        "List.chunksOf"
        Tablecloth.List.chunksOf
        [ (2, [ 1; 2; 3 ], [ [ 1; 2 ]; [ 3 ] ]) ] ]

let floatTests =
  testList
    "Float"
    [ testMany
        "readFloat"
        readFloat
        [ -0.0, (Negative, "0", "0")
          0.0, (Positive, "0", "0")
          82.10, (Positive, "82", "099999999999994315658113919198513031005859375")
          -180.0, (Negative, "180", "0") ] ]

let dateTests =
  testList
    "Date"
    [ testMany
        "toIsoString"
        (fun (d : System.DateTime) -> d.toIsoString ())
        [ System.DateTime(2000, 10, 1, 16, 1, 1), "2000-10-01T16:01:01Z" ]
      testMany
        "ofIsoString"
        System.DateTime.ofIsoString
        [ "2000-10-01T16:01:01Z", System.DateTime(2000, 10, 1, 16, 1, 1) ] ]


let tests =
  testList
    "prelude"
    [ canvasName; asyncTests; mapTests; listTests; floatTests; dateTests ]
