/// `dark generate`: a generator is a fn that writes Dark, and its output is staged the way
/// typed code is. These follow the loop a person runs: generate from a sample, look at
/// what landed, run it again (nothing to do), change the sample, run it again (only what
/// moved moves).
module Tests.CliGenerate

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Tests.CliTestHarness
open Tests.CliDsl


let private sampleFile (name : string) (json : string) : string =
  let path = System.IO.Path.Combine(System.IO.Path.GetTempPath(), name)
  System.IO.File.WriteAllText(path, json)
  path


let private orderSample =
  """{ "id": 7, "customer": "ada", "lines": [ { "sku": "A1", "qty": 2, "price": 9.5 } ] }"""

let private orderSampleWithDiscount =
  """{ "id": 7, "customer": "ada", "discount": 2.5, "lines": [ { "sku": "A1", "qty": 2, "price": 9.5 } ] }"""


let generateFromAJsonSample =
  instanceTest
    "generate writes types from a JSON sample and a saved generator beside them"
    (fun state ->
      task {
        let file = sampleFile "dark-generate-order.json" orderSample

        let! out =
          runCliPlain
            state
            [ "generate"
              "Darklang.Generate.Json.fromSample"
              "Order"
              file
              "--into"
              "Tests.Gen.Orders" ]

        for expected in
          [ "stored"
            "wrote Tests.Gen.Orders.orderGenerator"
            "+ Order"
            "+ OrderLine"
            "+ parse" ] do
          Expect.stringContains out expected $"generate reports each step: {out}"

        do!
          shows
            state
            [ "view"; "Tests.Gen.Orders.Order" ]
            "lines: List<OrderLine>"
            "the root record refers to the nested one"
        do!
          shows
            state
            [ "view"; "Tests.Gen.Orders.OrderLine" ]
            "price: Float"
            "a non-whole number is a Float"
        do!
          shows
            state
            [ "view"; "Tests.Gen.Orders.orderGenerator" ]
            "Generate.Json.fromSample"
            "the saved generator is ordinary code that names its inputs"
        do!
          evals
            state
            "(Tests.Gen.Orders.parse \"{\\\"id\\\": 1, \\\"customer\\\": \\\"bo\\\", \\\"lines\\\": []}\") |> Stdlib.Result.isOk"
            "true"
            "the generated parse fn parses a document of the sample's shape"
      })


let runningAgainIsANoOp =
  instanceTest "generate with an unchanged sample stages nothing" (fun state ->
    task {
      let file = sampleFile "dark-generate-noop.json" orderSample
      let args =
        [ "generate"
          "Darklang.Generate.Json.fromSample"
          "Order"
          file
          "--into"
          "Tests.Gen.Noop" ]

      do! run state args
      let! again = runCliPlain state args

      Expect.stringContains
        again
        "kept Tests.Gen.Noop.orderGenerator"
        $"the saved generator is unchanged: {again}"
      Expect.stringContains again "nothing to do" $"and so is every output: {again}"
      Expect.isFalse
        (again.Contains "+ Order")
        $"nothing is reported as new: {again}"
    })


let aChangedSampleMovesOnlyWhatChanged =
  instanceTest
    "generate after the sample changed updates only the types it touched"
    (fun state ->
      task {
        let file = sampleFile "dark-generate-changed.json" orderSample
        let args =
          [ "generate"
            "Darklang.Generate.Json.fromSample"
            "Order"
            file
            "--into"
            "Tests.Gen.Changed" ]

        do! run state args

        System.IO.File.WriteAllText(file, orderSampleWithDiscount)
        let! again = runCliPlain state args

        Expect.stringContains
          again
          "updated Tests.Gen.Changed.orderGenerator"
          $"the saved generator now names the new blob: {again}"
        Expect.stringContains again "~ Order" $"the root type changed: {again}"
        Expect.stringContains
          again
          "= OrderLine"
          $"the nested type did not: {again}"
        do!
          shows
            state
            [ "view"; "Tests.Gen.Changed.Order" ]
            "discount: Float"
            "the new field is on the type"
      })


let dryRunPrintsAndStagesNothing =
  instanceTest
    "generate --dry-run prints the saved generator and stages nothing"
    (fun state ->
      task {
        let file = sampleFile "dark-generate-dry.json" orderSample

        let! out =
          runCliPlain
            state
            [ "generate"
              "Darklang.Generate.Json.fromSample"
              "Order"
              file
              "--into"
              "Tests.Gen.Dry"
              "--dry-run" ]

        Expect.stringContains
          out
          "let orderGenerator () : Darklang.Generate.Run"
          $"the saved fn is printed: {out}"
        do!
          shows
            state
            [ "view"; "Tests.Gen.Dry.Order" ]
            "Not found"
            "and nothing was staged"
      })


let aSavedGeneratorRerunsByName =
  instanceTest "generate <savedGenerator> re-runs it" (fun state ->
    task {
      let file = sampleFile "dark-generate-rerun.json" orderSample
      do!
        run
          state
          [ "generate"
            "Darklang.Generate.Json.fromSample"
            "Order"
            file
            "--into"
            "Tests.Gen.Rerun" ]

      let! again =
        runCliPlain state [ "generate"; "Tests.Gen.Rerun.orderGenerator" ]
      Expect.stringContains
        again
        "ran Tests.Gen.Rerun.orderGenerator"
        $"the saved form runs by name: {again}"
      Expect.stringContains
        again
        "nothing to do"
        $"and finds nothing moved: {again}"

      let! printed =
        runCliPlain
          state
          [ "generate"; "Tests.Gen.Rerun.orderGenerator"; "--print" ]
      Expect.stringContains
        printed
        "type Order"
        $"--print shows the source it would stage: {printed}"
    })


let generateRefusesWhatIsNotAGenerator =
  instanceTest "generate refuses a name that is not a function" (fun state ->
    task {
      do!
        refuses
          state
          [ "generate"; "Darklang.Stdlib.Option.Option" ]
          "is not a function"
          "+ "
          "a type is not something to run"
      do!
        refuses
          state
          [ "generate" ]
          "which generator"
          "+ "
          "no name is a usage error"
    })


let generateRefusesAMissingFileBeforeWritingAnything =
  instanceTest
    "generate refuses a missing file before it writes anything"
    (fun state ->
      task {
        do!
          refuses
            state
            [ "generate"
              "Json.fromSample"
              "Order"
              "/nowhere/order.json"
              "--into"
              "Tests.Gen.Missing" ]
            "does not exist"
            "wrote"
            "a Blob parameter wants a file that is there, and the short name resolves"
        do!
          refuses
            state
            [ "generate"
              "Json.fromSample"
              "Order"
              "--into"
              "Tests.Gen.Missing" ]
            "argument(s)"
            "wrote"
            "the arity is checked against the fn's signature"
        do!
          refuses
            state
            [ "generate"; "Json.fromSample"; "Order"; "x"; "--into"; "Orders" ]
            "needs an owner and a module"
            "stored"
            "--into is checked before any file is stored"
      })


let generateRefusesAnEffectfulGenerator =
  instanceTest "generate refuses a generator that is not effect-free" (fun state ->
    task {
      do!
        fn
          state
          "Tests.Gen.Impure.peek"
          "(path: String) : Darklang.Generate.Run =\n  let _ = Stdlib.Cli.File.exists path\n  Darklang.Generate.ofSource \"\""

      do!
        refuses
          state
          [ "generate"; "Tests.Gen.Impure.peek"; "x"; "--into"; "Tests.Gen.Impure" ]
          "nothing else"
          "wrote"
          "a generator that reads the host cannot run at authoring time"
    })


// ─── package inputs: the mirror, and regeneration on propagation ──────────────

let mirrorSharesTheSourcesHash =
  instanceTest
    "mirror copies a type under another name with the same hash"
    (fun state ->
      task {
        do!
          run
            state
            [ "type"; "Tests.Gen.Src.Money"; "= { amount: Int; currency: String }" ]
        let! out =
          runCliPlain
            state
            [ "generate"
              "Dark.mirror"
              "Tests.Gen.Src.Money"
              "--into"
              "Tests.Gen.Mirror" ]
        Expect.stringContains
          out
          "wrote Tests.Gen.Mirror.moneyGenerator"
          $"the saved fn is named after the type: {out}"
        Expect.stringContains out "+ Money" $"the mirror lands: {out}"

        let! src = runCliPlain state [ "hash"; "Tests.Gen.Src.Money" ]
        let! copy = runCliPlain state [ "hash"; "Tests.Gen.Mirror.Money" ]
        Expect.equal
          (copy.Trim())
          (src.Trim())
          "identical content, so the same hash: a mirror is a second name"
        do!
          shows
            state
            [ "view"; "Tests.Gen.Mirror.moneyGenerator"; "--raw" ]
            "mirror<"
            "the saved fn names the source type as a type argument"
      })


let editingTheSourceRegeneratesTheMirror =
  instanceTest
    "editing a generator's package input regenerates its output in the draft"
    (fun state ->
      task {
        do! run state [ "type"; "Tests.Gen.Live.Money"; "= { amount: Int }" ]
        do!
          run
            state
            [ "generate"
              "Dark.mirror"
              "Tests.Gen.Live.Money"
              "--into"
              "Tests.Gen.LiveMirror" ]
        do! commit state "fixture"

        let! edited =
          runCliPlain
            state
            [ "type"; "Tests.Gen.Live.Money"; "= { amount: Int; note: String }" ]
        Expect.stringContains
          edited
          "repointed: Tests.Gen.LiveMirror.moneyGenerator"
          $"the generator follows its input: {edited}"
        Expect.stringContains
          edited
          "regenerated by Tests.Gen.LiveMirror.moneyGenerator: Money"
          $"and re-runs: {edited}"
        do!
          shows
            state
            [ "view"; "Tests.Gen.LiveMirror.Money"; "--raw" ]
            "note: String"
            "the mirror has the new field without anyone running generate"

        // A pinned generator stays put; follow catches it up and regenerates.
        do! run state [ "propagate"; "pin"; "Tests.Gen.LiveMirror.moneyGenerator" ]
        do!
          run
            state
            [ "type"
              "Tests.Gen.Live.Money"
              "= { amount: Int; note: String; tag: String }" ]
        do!
          lacks
            state
            [ "view"; "Tests.Gen.LiveMirror.Money"; "--raw" ]
            "tag: String"
            "pinned, so the mirror did not move"
        let! followed =
          runCliPlain
            state
            [ "propagate"; "follow"; "Tests.Gen.LiveMirror.moneyGenerator" ]
        Expect.stringContains
          followed
          "regenerated by"
          $"follow catches up and re-runs: {followed}"
        do!
          shows
            state
            [ "view"; "Tests.Gen.LiveMirror.Money"; "--raw" ]
            "tag: String"
            "and the mirror caught up"

        // Undo brings the source back, and the mirror with it.
        let! undone = runCliPlain state [ "undo"; "Tests.Gen.Live.Money" ]
        Expect.stringContains
          undone
          "regenerated by"
          $"undo re-runs the generator against the older input: {undone}"
        do!
          lacks
            state
            [ "view"; "Tests.Gen.LiveMirror.Money"; "--raw" ]
            "tag: String"
            "the mirror followed the source back down"
      })


let tests : List<Test> =
  [ generateFromAJsonSample
    runningAgainIsANoOp
    aChangedSampleMovesOnlyWhatChanged
    dryRunPrintsAndStagesNothing
    aSavedGeneratorRerunsByName
    generateRefusesWhatIsNotAGenerator
    generateRefusesAMissingFileBeforeWritingAnything
    generateRefusesAnEffectfulGenerator
    mirrorSharesTheSourcesHash
    editingTheSourceRegeneratesTheMirror ]
