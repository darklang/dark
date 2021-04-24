module Tests.Prelude

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
      with e -> false)
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


let tests = testList "prelude" [ canvasName ]
