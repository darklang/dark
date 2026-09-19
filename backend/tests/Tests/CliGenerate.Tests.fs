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
        Expect.stringContains
          out
          "type Order"
          $"and so is what it would stage: {out}"
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
          "no function named"
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
              "Dark.mirror"
              "Tests.Gen.NotAType"
              "--into"
              "Tests.Gen.Missing" ]
            "is not a type"
            "wrote"
            "a type argument has to name a type, checked before anything is written"
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
            [ "status" ]
            "2 followed"
            "the generator and its regenerated output both count as followed, not typed"
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


// ─── OpenAPI ─────────────────────────────────────────────────────────────────

let private petstore = "testfiles/generate/petstore.json"

let openApiClientFromAFile =
  instanceTest
    "generate OpenApi.client makes types and one fn per operation"
    (fun state ->
      task {
        let! out =
          runCliPlain
            state
            [ "generate"
              "OpenApi.client"
              petstore
              "--into"
              "Tests.Gen.Petstore" ]
        for expected in
          [ "+ Client"
            "+ ApiError"
            "+ Pet"
            "+ PetOwner"
            "+ listPets"
            "+ createPet"
            "+ showPetById"
            "+ deletePet" ] do
          Expect.stringContains
            out
            expected
            $"every schema and operation lands: {out}"
        Expect.isFalse
          (out.Contains "type check failed")
          $"the generated client passes the at-rest check: {out}"

        do!
          shows
            state
            [ "view"; "Tests.Gen.Petstore.Pet"; "--raw" ]
            "tag: Option<String>"
            "a property not in `required` is optional"
        do!
          shows
            state
            [ "view"; "Tests.Gen.Petstore.showPetById"; "--raw" ]
            "\"/pets/\" ++ petId"
            "a path parameter is spliced into the URL"
        do!
          shows
            state
            [ "view"; "Tests.Gen.Petstore.createPet"; "--raw" ]
            "encodeNewPet body"
            "a request body is encoded with its generated encoder"
        do!
          shows
            state
            [ "view"; "Tests.Gen.Petstore.deletePet"; "--raw" ]
            "Result<Unit, ApiError>"
            "an operation with no response schema returns Unit"
      })


/// A static file server for the fetch tests, on a port of its own. The CLI reaches
/// it through the sync transport, which is what `--fetch` uses.
let private withFileServer
  (dir : string)
  (port : int)
  (body : string -> Task<unit>)
  : Task<unit> =
  task {
    let psi = System.Diagnostics.ProcessStartInfo("python3")
    psi.ArgumentList.Add "-m"
    psi.ArgumentList.Add "http.server"
    psi.ArgumentList.Add(string port)
    psi.ArgumentList.Add "--directory"
    psi.ArgumentList.Add dir
    psi.ArgumentList.Add "--bind"
    psi.ArgumentList.Add "127.0.0.1"
    psi.RedirectStandardOutput <- true
    psi.RedirectStandardError <- true
    use server = System.Diagnostics.Process.Start psi
    try
      do! Task.Delay 800
      do! body $"http://127.0.0.1:{port}"
    finally
      try
        server.Kill true
      with _ ->
        ()
  }


let fetchAndRefresh =
  instanceTest
    "generate --fetch stores the document, and --refresh re-runs only if it moved"
    (fun state ->
      task {
        let dir =
          System.IO.Path.Combine(
            System.IO.Path.GetTempPath(),
            "dark-generate-fetch"
          )
        System.IO.Directory.CreateDirectory dir |> ignore
        let doc = System.IO.Path.Combine(dir, "openapi.json")
        System.IO.File.Copy(petstore, doc, true)

        do!
          withFileServer dir 9377 (fun baseUrl ->
            task {
              let url = $"{baseUrl}/openapi.json"
              let! out =
                runCliPlain
                  state
                  [ "generate"
                    "OpenApi.client"
                    "--fetch"
                    url
                    "--into"
                    "Tests.Gen.Fetched" ]
              Expect.stringContains
                out
                "fetched"
                $"the document is fetched and stored: {out}"
              Expect.stringContains
                out
                "+ Pet"
                $"and the client is generated from it: {out}"
              do!
                shows
                  state
                  [ "view"; "Tests.Gen.Fetched.fetchedGenerator"; "--raw" ]
                  "fetched from"
                  "the saved fn records the URL in its doc comment"

              let! same =
                runCliPlain
                  state
                  [ "generate"; "Tests.Gen.Fetched.fetchedGenerator"; "--refresh" ]
              Expect.stringContains
                same
                "unchanged since it was last fetched"
                $"same document, nothing to do: {same}"

              let changed =
                System.IO.File
                  .ReadAllText(doc)
                  .Replace(
                    "\"tag\": { \"type\": \"string\" },",
                    "\"tag\": { \"type\": \"string\" }, \"age\": { \"type\": \"integer\" },"
                  )
              Expect.isTrue
                (changed <> System.IO.File.ReadAllText doc)
                "the fixture edit took"
              System.IO.File.WriteAllText(doc, changed)

              let! moved =
                runCliPlain
                  state
                  [ "generate"; "Tests.Gen.Fetched.fetchedGenerator"; "--refresh" ]
              Expect.stringContains
                moved
                "the document moved"
                $"a changed document rewrites the saved fn: {moved}"
              Expect.stringContains
                moved
                "~ Pet"
                $"and only what it touched moves: {moved}"
              Expect.stringContains
                moved
                "= Client  unchanged"
                $"the fixed part stays: {moved}"
            })
      })


// ─── the sample-driven family: CSV, JSON Schema, several JSON samples ──────────

let private peopleCsv =
  "id,name,age,active,note\n1,ada,36,true,\n2,\"bo\",41,false,late\n"

let private ticketSchema =
  """{ "type": "object", "required": ["id"],
  "properties": { "id": { "type": "integer" }, "owner": { "$ref": "#/$defs/Person" } },
  "$defs": { "Person": { "type": "object", "required": ["name"], "properties": { "name": { "type": "string" }, "age": { "type": "integer" } } } } }"""


let csvFromSample =
  instanceTest "Csv.fromSample makes a row type, fromFields and parse" (fun state ->
    task {
      let file = sampleFile "dark-generate-people.csv" peopleCsv
      let! out =
        runCliPlain
          state
          [ "generate"
            "Csv.fromSample"
            "Person"
            file
            "--into"
            "Tests.Gen.Csv" ]
      for expected in [ "+ Person"; "+ fromFields"; "+ parse" ] do
        Expect.stringContains
          out
          expected
          $"the row type and its readers land: {out}"
      do!
        shows
          state
          [ "view"; "Tests.Gen.Csv.Person"; "--raw" ]
          "note: Option<String>"
          "a column with an empty cell is optional"
      do!
        shows
          state
          [ "view"; "Tests.Gen.Csv.Person"; "--raw" ]
          "active: Bool"
          "true/false columns are Bool"
      do!
        evals
          state
          "(Tests.Gen.Csv.parse \"id,name,age,active,note\\n7,cy,50,true,\") |> Stdlib.List.length"
          "1"
          "the generated parse reads a document"
      do!
        evals
          state
          "Tests.Gen.Csv.fromFields [ \"x\", \"cy\", \"50\", \"true\", \"\" ]"
          "not a whole number"
          "a cell that does not convert names its column"
    })


let jsonSchemaTypes =
  instanceTest
    "JsonSchema.types makes records with decoders that read real JSON"
    (fun state ->
      task {
        let file = sampleFile "dark-generate-ticket.schema.json" ticketSchema
        let! out =
          runCliPlain
            state
            [ "generate"
              "JsonSchema.types"
              "Ticket"
              file
              "--into"
              "Tests.Gen.Schema" ]
        for expected in
          [ "+ Person"; "+ Ticket"; "+ decodeTicket"; "+ encodeTicket"; "+ parse" ] do
          Expect.stringContains
            out
            expected
            $"definitions, root, codecs and parse land: {out}"
        do!
          evals
            state
            "Tests.Gen.Schema.parse \"{\\\"id\\\": 1, \\\"owner\\\": {\\\"name\\\": \\\"ada\\\"}}\""
            "age: None"
            "a missing optional field decodes as None, the way other systems write JSON"
        do!
          evals
            state
            "Tests.Gen.Schema.parse \"{\\\"owner\\\": {}}\""
            "missing field"
            "a missing required field is an error that names it"
      })


let jsonFromSamplesUnifies =
  instanceTest "Json.fromSamples unifies a directory of documents" (fun state ->
    task {
      let dir =
        System.IO.Path.Combine(
          System.IO.Path.GetTempPath(),
          "dark-generate-samples"
        )
      System.IO.Directory.CreateDirectory dir |> ignore
      System.IO.File.WriteAllText(
        System.IO.Path.Combine(dir, "a.json"),
        """{ "id": 1, "name": "ada" }"""
      )
      System.IO.File.WriteAllText(
        System.IO.Path.Combine(dir, "b.json"),
        """{ "id": 2.5, "name": "bo", "email": "b@x" }"""
      )
      let! out =
        runCliPlain
          state
          [ "generate"
            "Json.fromSamples"
            "Person"
            dir
            "--into"
            "Tests.Gen.Samples" ]
      Expect.stringContains
        out
        "stored 2 files"
        $"every file in the directory is a sample: {out}"
      do!
        shows
          state
          [ "view"; "Tests.Gen.Samples.Person"; "--raw" ]
          "id: Float"
          "an Int in one sample and a Float in another is a Float"
      do!
        shows
          state
          [ "view"; "Tests.Gen.Samples.Person"; "--raw" ]
          "email: Option<String>"
          "a field present in one sample and absent in another is optional"
    })


// ─── SQL, derive, fixtures, whole-module mirror ─────────────────────────────────

let private schemaSql =
  "CREATE TABLE users (id INTEGER PRIMARY KEY, email TEXT NOT NULL, \"display name\" VARCHAR(80), score REAL, active BOOLEAN NOT NULL, UNIQUE (email));\nCREATE INDEX i ON users(email);\nCREATE TABLE IF NOT EXISTS categories (id INTEGER PRIMARY KEY, title TEXT NOT NULL, parent_id INTEGER, FOREIGN KEY (parent_id) REFERENCES categories(id));\n"


let sqlSchemaTypes =
  instanceTest
    "Sql.schemaTypes makes a record, fromRow and selectAll per table"
    (fun state ->
      task {
        let file = sampleFile "dark-generate-schema.sql" schemaSql
        let! out =
          runCliPlain
            state
            [ "generate"; "Sql.schemaTypes"; file; "--into"; "Tests.Gen.Db" ]
        for expected in
          [ "+ User"
            "+ Category"
            "+ userFromRow"
            "+ allUsers"
            "+ categoryFromRow" ] do
          Expect.stringContains
            out
            expected
            $"one record and two readers per table: {out}"
        do!
          shows
            state
            [ "view"; "Tests.Gen.Db.User"; "--raw" ]
            "displayname: Option<String>"
            "a quoted column name with a space becomes a legal field, optional without NOT NULL"
        do!
          shows
            state
            [ "view"; "Tests.Gen.Db.User"; "--raw" ]
            "active: Bool"
            "BOOLEAN is Bool"
        do!
          evals
            state
            "Tests.Gen.Db.userFromRow (Stdlib.Dict.fromListOverwritingDuplicates [ (\"id\", Stdlib.Sqlite.Value.Int 7L), (\"email\", Stdlib.Sqlite.Value.Text \"a@x\"), (\"active\", Stdlib.Sqlite.Value.Int 1L) ])"
            "active: true"
            "fromRow reads a row; missing optional columns are None"
        do!
          evals
            state
            "Tests.Gen.Db.userFromRow (Stdlib.Dict.fromListOverwritingDuplicates [ (\"id\", Stdlib.Sqlite.Value.Int 7L) ])"
            "email: null"
            "a missing required column names itself"
      })


let deriveShowEqualsSetters =
  instanceTest
    "Derive.forType writes show, equals and a setter per field"
    (fun state ->
      task {
        do!
          run
            state
            [ "type"
              "Tests.Gen.Dv.Money"
              "= { amount: Int; currency: String; note: Option<String> }" ]
        do! run state [ "type"; "Tests.Gen.Dv.Shape"; "= | Circle of Float | Dot" ]
        let! out =
          runCliPlain
            state
            [ "generate"
              "Derive.forType"
              "Tests.Gen.Dv.Money"
              "--into"
              "Tests.Gen.Dv" ]
        for expected in
          [ "+ showMoney"; "+ equalsMoney"; "+ withAmount"; "+ withNote" ] do
          Expect.stringContains out expected $"show, equals and setters land: {out}"
        do!
          evals
            state
            "Tests.Gen.Dv.showMoney (Tests.Gen.Dv.Money { amount = 3; currency = \"EUR\"; note = Stdlib.Option.Option.None })"
            "Money { amount = 3; currency = \"EUR\"; note = None }"
            "show renders every field"
        do!
          evals
            state
            "(Tests.Gen.Dv.withAmount (Tests.Gen.Dv.Money { amount = 3; currency = \"EUR\"; note = Stdlib.Option.Option.None }) 9).amount"
            "9"
            "a setter replaces one field"
        do!
          run
            state
            [ "generate"
              "Derive.forType"
              "Tests.Gen.Dv.Shape"
              "--into"
              "Tests.Gen.Dv" ]
        do!
          evals
            state
            "Tests.Gen.Dv.showShape (Tests.Gen.Dv.Shape.Circle 2.5)"
            "Circle(2.5)"
            "an enum shows its case and fields"
      })


let fixturesForType =
  instanceTest
    "Fixtures.forType writes an example for the type and every type it reaches"
    (fun state ->
      task {
        do!
          run
            state
            [ "type"; "Tests.Gen.Fx.Money"; "= { amount: Int; currency: String }" ]
        do! run state [ "type"; "Tests.Gen.Fx.Line"; "= { sku: String; qty: Int }" ]
        do!
          run
            state
            [ "type"
              "Tests.Gen.Fx.Order"
              "= { id: Int64; lines: List<Tests.Gen.Fx.Line>; money: Tests.Gen.Fx.Money; paid: Option<Bool> }" ]
        let! out =
          runCliPlain
            state
            [ "generate"
              "Fixtures.forType"
              "Tests.Gen.Fx.Order"
              "--into"
              "Tests.Gen.FxOut" ]
        for expected in [ "+ exampleOrder"; "+ exampleLine"; "+ exampleMoney" ] do
          Expect.stringContains out expected $"the root and what it reaches: {out}"
        do!
          evals
            state
            "Tests.Gen.FxOut.exampleOrder ()"
            "paid: Some(true)"
            "the example constructs and runs"
      })


let mirrorModuleCopiesAModule =
  instanceTest
    "Dark.mirrorModule copies a module's items with their references rewritten"
    (fun state ->
      task {
        do! run state [ "type"; "Tests.Gen.Mm.Line"; "= { sku: String; qty: Int }" ]
        do!
          run
            state
            [ "type"; "Tests.Gen.Mm.Order"; "= { lines: List<Tests.Gen.Mm.Line> }" ]
        do!
          fn
            state
            "Tests.Gen.Mm.count"
            "(o: Tests.Gen.Mm.Order) : Int = Stdlib.List.length o.lines"
        let! out =
          runCliPlain
            state
            [ "generate"
              "Dark.mirrorModule"
              "Tests.Gen.Mm"
              "Tests.Gen.MmCopy"
              "--into"
              "Tests.Gen.MmCopy" ]
        for expected in [ "+ Line"; "+ Order"; "+ count" ] do
          Expect.stringContains out expected $"types and fns copy: {out}"
        do!
          evals
            state
            "Tests.Gen.MmCopy.count (Tests.Gen.MmCopy.Order { lines = [ Tests.Gen.MmCopy.Line { sku = \"a\"; qty = 1 } ] })"
            "1"
            "the copy's fn takes the copy's types"
      })


// ─── files out, server stubs, GraphQL ────────────────────────────────────────────

let typeScriptDeclarations =
  instanceTest
    "TypeScript.toTypeScript writes a .d.ts for a module with --out"
    (fun state ->
      task {
        do!
          run
            state
            [ "type"
              "Tests.Gen.Ts.Money"
              "= { amount: Int; note: Option<String> }" ]
        do! run state [ "type"; "Tests.Gen.Ts.Shape"; "= | Circle of Float | Dot" ]
        do!
          fn
            state
            "Tests.Gen.Ts.double"
            "(m: Tests.Gen.Ts.Money) : Tests.Gen.Ts.Money = { m with amount = m.amount * 2 }"
        let dir =
          System.IO.Path.Combine(System.IO.Path.GetTempPath(), "dark-generate-ts")
        System.IO.Directory.CreateDirectory dir |> ignore
        do!
          refuses
            state
            [ "generate"
              "TypeScript.toTypeScript"
              "Tests.Gen.Ts"
              "--into"
              "Tests.Gen.TsOut" ]
            "--out"
            ".d.ts"
            "a run that produces files needs somewhere to put them"
        let! out =
          runCliPlain
            state
            [ "generate"
              "TypeScript.toTypeScript"
              "Tests.Gen.Ts"
              "--into"
              "Tests.Gen.TsOut"
              "--out"
              dir ]
        Expect.stringContains out "wrote" $"the file is written: {out}"
        let text =
          System.IO.File.ReadAllText(
            System.IO.Path.Combine(dir, "Tests.Gen.Ts.d.ts")
          )
        Expect.stringContains
          text
          "export interface Money"
          "a record is an interface"
        Expect.stringContains
          text
          "note?: string | null;"
          "an Option field is optional and nullable"
        Expect.stringContains
          text
          "| { Circle: [number] }"
          "an enum is a union in Json.serialize's shape"
        Expect.stringContains
          text
          "export declare function double(m: Money): Money;"
          "a fn is a declaration"
      })


let openApiServerStubs =
  instanceTest
    "OpenApi.server makes Handlers, notImplemented and a router"
    (fun state ->
      task {
        let! out =
          runCliPlain
            state
            [ "generate"; "OpenApi.server"; petstore; "--into"; "Tests.Gen.Api" ]
        for expected in
          [ "+ Handlers"; "+ notImplemented"; "+ router"; "+ decodeNewPet" ] do
          Expect.stringContains out expected $"the server side lands: {out}"
        // Two arguments, so `fn` reads a definition rather than probing for a file
        // named after the whole body.
        do!
          run
            state
            [ "fn"
              "Tests.Gen.Api.handlers"
              "()"
              ": Tests.Gen.Api.Handlers = { Tests.Gen.Api.notImplemented () with showPetById = fun id -> Stdlib.Result.Result.Ok(Tests.Gen.Api.Pet { id = 1; name = id; tag = Stdlib.Option.Option.None; status = Stdlib.Option.Option.None; owner = Stdlib.Option.Option.None }) }" ]
        do!
          evals
            state
            "((Tests.Gen.Api.router (Tests.Gen.Api.handlers ())) (Stdlib.Http.Request { url = \"http://x/pets/rex\"; headers = []; body = Stdlib.Blob.empty })).statusCode"
            "200"
            "an implemented operation answers 200 through the router"
        do!
          evals
            state
            "((Tests.Gen.Api.router (Tests.Gen.Api.handlers ())) (Stdlib.Http.Request { url = \"http://x/pets?limit=2\"; headers = []; body = Stdlib.Blob.empty })).statusCode"
            "501"
            "an unimplemented one answers 501"
      })


let graphQlClient =
  instanceTest "GraphQL.client makes records and one fn per root field" (fun state ->
    task {
      let! out =
        runCliPlain
          state
          [ "generate"
            "GraphQL.client"
            "testfiles/generate/starwars.graphql"
            "--into"
            "Tests.Gen.Gql" ]
      for expected in
        [ "+ Character"
          "+ ReviewInput"
          "+ hero"
          "+ characters"
          "+ createReview" ] do
        Expect.stringContains out expected $"types and operations land: {out}"
      do!
        shows
          state
          [ "view"; "Tests.Gen.Gql.Character"; "--raw" ]
          "height: Option<Float>"
          "a nullable field is optional"
      do!
        shows
          state
          [ "view"; "Tests.Gen.Gql.hero"; "--raw" ]
          "query($episode: Episode) { hero(episode: $episode) {"
          "the operation carries its query text with a selection set"
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
    editingTheSourceRegeneratesTheMirror
    openApiClientFromAFile
    fetchAndRefresh
    csvFromSample
    jsonSchemaTypes
    jsonFromSamplesUnifies
    sqlSchemaTypes
    deriveShowEqualsSetters
    fixturesForType
    mirrorModuleCopiesAModule
    typeScriptDeclarations
    openApiServerStubs
    graphQlClient ]
