module Tests.LibExecution

// Create test cases from .tests files in the tests/testfiles dir
// A readme in that same directory exists to explain usage.

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open LibBackend.Db

open Prelude
open Prelude.Tablecloth
open Tablecloth

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module Canvas = LibBackend.Canvas

open TestUtils.TestUtils

let setupWorkers (meta : Canvas.Meta) (workers : List<string>) : Task<unit> =
  task {
    let workersWithIDs = workers |> List.map (fun w -> w, (gid ()))

    let ops =
      workersWithIDs
      |> List.map (fun (worker, tlid) ->
        PT.SetHandler(
          tlid,
          testPos,
          { tlid = tlid
            pos = testPos
            ast = PT.Expr.EBlank(gid ())
            spec =
              PT.Handler.Worker(
                worker,
                { moduleID = gid (); nameID = gid (); modifierID = gid () }
              ) }
        ))

    let c = Canvas.empty meta |> Canvas.addOps ops []

    let oplists =
      workersWithIDs
      |> List.map (fun (_w, tlid) ->
        tlid, ops, PT.Toplevel.TLHandler c.handlers[tlid], Canvas.NotDeleted)

    do! Canvas.saveTLIDs meta oplists
  }

let setupStaticAssets
  (meta : Canvas.Meta)
  (deployHashes : List<string>)
  : Task<unit> =
  task {
    return!
      Task.iterSequentially
        (fun hash ->
          Sql.query
            "INSERT INTO static_asset_deploys
              (canvas_id, branch, deploy_hash, uploaded_by_account_id, created_at, live_at)
            VALUES
              (@canvasID, @branch, @deployHash, @uploadedBy, NOW(), NOW())"
          |> Sql.parameters [ "canvasID", Sql.uuid meta.id
                              "branch", Sql.string "main"
                              "deployHash", Sql.string hash
                              "uploadedBy", Sql.uuid meta.owner ]
          |> Sql.executeStatementAsync)
        deployHashes
  }

let t
  (owner : Task<LibBackend.Account.UserInfo>)
  (initializeCanvas : bool)
  (canvasName : TestCanvasName)
  (comment : string)
  (code : string)
  (dbs : List<PT.DB.T>)
  (packageFns : Map<PT.FQFnName.PackageFnName, PT.Package.Fn>)
  (functions : Map<string, PT.UserFunction.T>)
  (workers : List<string>)
  (staticAssetsDeployHashes : List<string>)
  : Test =
  let name = $"{comment} ({code})"

  if matches @"^\s*$" code || matches @"^\s*//" code then
    ptestTask name { return (Expect.equal "skipped" "skipped" "") }
  else
    testTask name {
      try
        let! owner = owner
        let! meta =
          let initializeCanvas =
            initializeCanvas
            || dbs <> []
            || workers <> []
            || staticAssetsDeployHashes <> []
          // Little optimization to skip the DB sometimes
          if initializeCanvas then
            initializeCanvasForOwner owner canvasName
          else
            createCanvasForOwner owner canvasName

        let rtDBs =
          (dbs |> List.map (fun db -> db.name, PT2RT.DB.toRT db) |> Map.ofList)

        let rtFunctions = functions |> Map.map PT2RT.UserFunction.toRT

        let rtPackageFns =
          packageFns
          |> Map.toList
          |> List.map (fun (k, v) ->
            let fn = PT2RT.Package.toRT v
            ((RT.FQFnName.Package fn.name), fn))
          |> Map

        let! (state : RT.ExecutionState) = executionStateFor meta rtDBs rtFunctions
        let state =
          { state with libraries = { state.libraries with packageFns = rtPackageFns } }

        let source = FSharpToExpr.parse code

        let shouldEqual, actualProg, expectedResult =
          FSharpToExpr.convertToTest source

        let msg = $"\n\n{actualProg}\n=\n{expectedResult} ->"

        let! expected =
          Exe.executeExpr state Map.empty (PT2RT.Expr.toRT expectedResult)

        let resetCanvas () : Task<unit> =
          task {
            do! clearCanvasData meta.owner meta.name
            if staticAssetsDeployHashes <> [] then
              do! setupStaticAssets meta staticAssetsDeployHashes
            if workers <> [] then do! setupWorkers meta workers
          }
        do! resetCanvas ()

        // Only do this now so that the error doesn't fire while evaluating the expectedExpr
        let state =
          let expectedExceptionCount =
            match comment with
            | Regex ".*EXPECTED_EXCEPTION_COUNT: (\d+)" [ count ] -> int count
            | _ -> 0
          { state with
              test =
                { state.test with expectedExceptionCount = expectedExceptionCount } }

        let testOCaml, testFSharp =
          if String.includes "FSHARPONLY" comment then (false, true)
          else if String.includes "OCAMLONLY" comment then (true, false)
          else (true, true)

        if testOCaml then
          let! ocamlActual =
            LibBackend.OCamlInterop.execute
              state.program.accountID
              state.program.canvasID
              actualProg
              Map.empty
              dbs
              (Map.values functions)
              (Map.values packageFns)

          Expect.isTrue (Expect.isCanonical ocamlActual) "actual is normalized"

          if shouldEqual then
            Expect.equalDval
              (normalizeDvalResult ocamlActual)
              expected
              $"OCaml: {msg}"
          else
            Expect.notEqual
              (normalizeDvalResult ocamlActual)
              expected
              $"OCaml: {msg}"

          // Clear the canvas before we run the OCaml tests
          do! resetCanvas ()

        if testFSharp then
          let! fsharpActual =
            Exe.executeExpr state Map.empty (PT2RT.Expr.toRT actualProg)

          let fsharpActual = normalizeDvalResult fsharpActual

          let canonical = Expect.isCanonical fsharpActual
          if not canonical then
            debugDval fsharpActual |> debuG "not canonicalized"
            Expect.isTrue canonical "expected is canonicalized"

          if shouldEqual then
            Expect.equalDval fsharpActual expected $"FSharp: {msg}"
          else
            Expect.notEqual fsharpActual expected $"FSharp: {msg}"

        return ()
      with
      | e ->
        let metadata = Exception.toMetadata e
        return
          Expect.equal
            (e.Message, metadata, e.StackTrace)
            ("Exception thrown in test", [], "")
            ""
    }

type TestExtras =
  { dbs : List<PT.DB.T>
    workers : List<string>
    staticAssetsDeployHashes : List<string>
    exactCanvasName : Option<string> }

type TestInfo =
  { name : string
    recording : bool
    code : string
    extras : TestExtras }

type TestGroup = { name : string; tests : List<Test>; extras : TestExtras }

type FnInfo =
  { name : string
    recording : bool
    isPackage : bool
    code : string
    tlid : tlid
    parameters : List<PT.UserFunction.Parameter> }

let testAdmin =
  lazy
    (task {
      let username = UserName.create "libexe_admin"
      let account : LibBackend.Account.Account =
        { username = username
          password = LibBackend.Password.invalid
          email = "admin-test@darklang.com"
          name = "test name" }
      do!
        LibBackend.Account.upsertAdmin account
        |> Task.map (Exception.unwrapResultInternal [])
      return!
        LibBackend.Account.getUser username
        |> Task.map (Exception.unwrapOptionInternal "can't get testAdmin" [])
    })

let emptyExtras =
  { dbs = []; workers = []; staticAssetsDeployHashes = []; exactCanvasName = None }

let parseExtras (annotation : string) (dbs : Map<string, PT.DB.T>) : TestExtras =
  annotation
  |> String.split ","
  |> List.fold emptyExtras (fun extras s ->
    match String.split " " (String.trim s) |> debug "split" with
    | [ "DB"; dbName ] ->
      let dbName = String.trim dbName
      match Map.get dbName dbs with
      | Some db -> { extras with dbs = db :: extras.dbs }
      | None -> Exception.raiseInternal $"No DB named {dbName} found" []
    | [ "Worker"; workerName ] ->
      let workerName = String.trim workerName
      { extras with workers = workerName :: extras.workers }
    | [ "ExactCanvasName"; canvasName ] ->
      { extras with exactCanvasName = Some(String.trim canvasName) }
    | [ "StaticAssetsDeployHash"; hash ] ->
      let hash = String.trim hash
      { extras with
          staticAssetsDeployHashes = hash :: extras.staticAssetsDeployHashes }
    | other -> Exception.raiseInternal "invalid option" [ "annotation", other ])




// Read all test files. The test file format is described in README.md
let fileTests () : Test =
  let dir = "tests/testfiles/"

  System.IO.Directory.GetFiles(dir, "*.tests")
  |> Array.filter ((<>) "README.md")
  |> Array.filter ((<>) ".gitattributes")
  |> Array.map (fun file ->
    let filename = System.IO.Path.GetFileName file
    let emptyTest = { recording = false; name = ""; code = ""; extras = emptyExtras }

    let emptyFn =
      { recording = false
        name = ""
        parameters = []
        code = ""
        tlid = id 7
        isPackage = false }

    let emptyGroup = { name = ""; tests = []; extras = emptyExtras }

    // for recording a multiline test
    let mutable currentTest = emptyTest
    // for recording a multiline function
    let mutable currentFn = emptyFn
    // for recording a bunch if single-line tests grouped together
    let mutable currentGroup = emptyGroup
    let mutable allTests = []
    let mutable functions : Map<string, PT.UserFunction.T> = Map.empty
    let mutable packageFunctions : Map<PT.FQFnName.PackageFnName, PT.Package.Fn> =
      Map.empty
    let mutable dbs : Map<string, PT.DB.T> = Map.empty
    let owner =
      if filename = "internal.tests" then testAdmin.Force() else testOwner.Force()
    let initializeCanvas =
      filename = "internal.tests"
      // staticassets.tests only needs initialization for OCaml (where the
      // canvas_id is fetched from the DB during test function call, rather than
      // up-front). This is also true of the other places in this file this
      // condition is repeated.
      || filename = "staticassets.tests"

    let finish () =
      if currentTest.recording then
        let canvasName =
          match currentTest.extras.exactCanvasName with
          | None -> Randomized currentTest.name
          | Some exactName -> Exact exactName
        let newTestCase =
          t
            owner
            initializeCanvas
            canvasName
            currentTest.name
            currentTest.code
            currentTest.extras.dbs
            packageFunctions
            functions
            currentTest.extras.workers
            currentTest.extras.staticAssetsDeployHashes

        allTests <- allTests @ [ newTestCase ]

      if List.length currentGroup.tests > 0 then
        let newTestCase = testList currentGroup.name currentGroup.tests
        allTests <- allTests @ [ newTestCase ]

      if currentFn.recording then

        if currentFn.isPackage then
          let parameters =
            currentFn.parameters
            |> List.map (fun p ->
              let typ =
                p.typ |> Exception.unwrapOptionInternal "type must not be option" []
              { description = p.description; name = p.name; typ = typ } : PT.Package.Parameter)
          let (fn : PT.Package.Fn) =
            { tlid = currentFn.tlid
              name =
                { owner = "test"
                  package = "test"
                  module_ = "Test"
                  function_ = currentFn.name
                  version = 0 }
              body = FSharpToExpr.parsePTExpr currentFn.code
              parameters = parameters
              returnType = PT.TVariable "a"
              author = "test"
              deprecated = false
              description = "test package function" }
          packageFunctions <- Map.add fn.name fn packageFunctions
        else
          let (fn : PT.UserFunction.T) =
            { tlid = currentFn.tlid
              name = currentFn.name
              nameID = gid ()
              returnType = PT.TVariable "a"
              returnTypeID = gid ()
              description = "test function"
              infix = false
              body = FSharpToExpr.parsePTExpr currentFn.code
              parameters = currentFn.parameters }
          functions <- Map.add currentFn.name fn functions

      // Clear settings
      currentTest <- emptyTest
      currentFn <- emptyFn
      currentGroup <- emptyGroup

    (dir + filename)
    |> System.IO.File.ReadLines
    |> Seq.iteri (fun i line ->
      let i = i + 1

      // This format is described in testfiles/README.md. If you make
      // any changes, update that file.
      match line with
      // [tests] indicator
      | Regex @"^\[tests\.(.*)\] with (.*)$" [ name; extras ] ->
        finish ()
        let extras = parseExtras extras dbs

        currentGroup <- { currentGroup with name = name; extras = extras }
      | Regex @"^\[tests\.(.*)\]$" [ name ] ->
        finish ()
        currentGroup <- { currentGroup with name = name }

      // [db] declaration
      | Regex @"^\[db.(.*) (\{.*\})\]\s*$" [ name; definition ] ->
        finish ()

        let (db : PT.DB.T) =
          { tlid = id i
            pos = { x = 0; y = 0 }
            name = name
            nameID = gid ()
            version = 0
            cols =
              definition
              |> Json.Vanilla.deserialize<Map<string, string>>
              |> Map.mapWithIndex (fun k v ->
                let typ =
                  PTParser.DType.parse v
                  |> Exception.unwrapOptionInternal "Cannot parse type" []
                ({ name = if k = "" then None else Some k
                   nameID = gid ()
                   typ = if v = "" then None else Some typ
                   typeID = gid () } : PT.DB.Col))
              |> Map.values }

        dbs <- Map.add name db dbs


      // [function] declaration
      | Regex @"^\[(package)?fn\.(\S+) (.*)\]$" [ package; name; definition ] ->
        finish ()

        let parameters : List<PT.UserFunction.Parameter> =
          definition
          |> String.split " "
          |> List.map (fun def ->
            let (name, typ) =
              match String.split ":" def with
              | [ name; typ ] ->
                (name,
                 (typ
                  |> String.trim
                  |> PTParser.DType.parse
                  |> Exception.unwrapOptionInternal
                       "Invalid type name"
                       [ "definition", def
                         "type", typ
                         "name", name
                         "lineNumber", i
                         "filename", filename ]))
              | _ ->
                Exception.raiseInternal
                  "Invalid test function type declaration"
                  [ "definition", def
                    "name", name
                    "lineNumber", i
                    "filename", filename ]
            { name = String.trim name
              nameID = gid ()
              description = ""
              typ = Some typ
              typeID = gid () })

        currentFn <-
          { tlid = id i
            recording = true
            name = name
            parameters = parameters
            isPackage = package = "package"
            code = "" }

      // [test] with DB indicator
      | Regex @"^\[test\.(.*)\] with (.*)$" [ name; extras ] ->
        finish ()
        let extras = parseExtras extras dbs

        currentTest <-
          { currentTest with
              name = $"{name} (line {i})"
              recording = true
              extras = extras }

      // [test] indicator (no DB)
      | Regex @"^\[test\.(.*)\]$" [ name ] ->
        finish ()

        currentTest <-
          { currentTest with name = $"{name} (line {i})"; recording = true }
      // Skip whitespace lines
      | Regex @"^\s*$" [] -> ()
      // Skip whole-line comments
      | Regex @"^\s*//.*$" [] when currentTest.recording || currentFn.recording -> ()
      // Append to the current test string
      | _ when currentTest.recording ->
        currentTest <- { currentTest with code = currentTest.code + "\n" + line }
      | _ when currentFn.recording ->
        currentFn <- { currentFn with code = currentFn.code + "\n" + line }
      // 1-line test
      | Regex @"^(.*)\s+//\s+(.*)$" [ code; comment ] ->
        let test =
          t
            owner
            initializeCanvas
            (Randomized $"{comment} (line {i})")
            $"{comment} (line {i})"
            code
            currentGroup.extras.dbs
            packageFunctions
            functions
            currentTest.extras.workers
            currentTest.extras.staticAssetsDeployHashes

        currentGroup <- { currentGroup with tests = currentGroup.tests @ [ test ] }
      | Regex @"^(.*)\s*$" [ code ] ->
        let test =
          t
            owner
            initializeCanvas
            (Randomized $"line {i}")
            $"line {i}"
            code
            currentGroup.extras.dbs
            packageFunctions
            functions
            currentTest.extras.workers
            currentTest.extras.staticAssetsDeployHashes

        currentGroup <- { currentGroup with tests = currentGroup.tests @ [ test ] }

      | _ -> raise (System.Exception $"can't parse line {i}: {line}"))

    finish ()
    testList $"Tests from {filename}" allTests)
  |> Array.toList
  |> testList "All files"

let tests = lazy (testList "LibExecution" [ fileTests () ])
