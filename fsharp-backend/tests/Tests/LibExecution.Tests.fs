module Tests.LibExecution

// Create test cases from .tests files in the tests/stdlib dir

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude
open Prelude.Tablecloth
open Tablecloth

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module Exe = LibExecution.Execution

open TestUtils.TestUtils

let t
  (owner : Task<LibBackend.Account.UserInfo>)
  (initializeDB : bool)
  (comment : string)
  (code : string)
  (dbs : List<PT.DB.T>)
  (functions : Map<string, PT.UserFunction.T>)
  : Test =
  let name = $"{comment} ({code})"

  if matches @"^\s*$" code || matches @"^\s*//" code then
    ptestTask name { return (Expect.equal "skipped" "skipped" "") }
  else
    testTask name {
      try
        let! owner = owner
        let! meta =
          // Little optimization to skip the DB sometimes
          if initializeDB then
            initializeCanvasForOwner owner name
          else
            createCanvasForOwner owner name

        let rtDBs =
          (dbs |> List.map (fun db -> db.name, PT.DB.toRuntimeType db) |> Map.ofList)

        let rtFunctions = functions |> Map.map PT.UserFunction.toRuntimeType

        let! state = executionStateFor meta rtDBs rtFunctions

        let source = FSharpToExpr.parse code

        let shouldEqual, actualProg, expectedResult =
          FSharpToExpr.convertToTest source

        let msg = $"\n\n{actualProg}\n=\n{expectedResult} ->"

        let! expected =
          Exe.executeExpr state Map.empty (expectedResult.toRuntimeType ())

        do! clearCanvasData meta.owner meta.name

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
          do! clearCanvasData meta.owner meta.name

        if testFSharp then
          let! fsharpActual =
            Exe.executeExpr state Map.empty (actualProg.toRuntimeType ())

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
      | :? DarkException as e ->
        let metadata = Exception.toMetadata e
        print $"Exception thrown in test: {string e}\n{metadata}"
        return Expect.equal ("Exception thrown in test", []) (string e, metadata) ""
      | e ->
        print $"Exception thrown in test: {e}"
        return Expect.equal "Exception thrown in test" (string e) ""
    }


type TestInfo =
  { name : string
    recording : bool
    code : string
    dbs : List<PT.DB.T> }

type TestGroup = { name : string; tests : List<Test>; dbs : List<PT.DB.T> }

type FnInfo =
  { name : string
    recording : bool
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
      do! LibBackend.Account.upsertAdmin account |> Task.map Result.unwrapUnsafe
      return! LibBackend.Account.getUser username |> Task.map Option.unwrapUnsafe
    })

// Read all test files. The test file format is described in README.md
let fileTests () : Test =
  let dir = "tests/testfiles/"

  System.IO.Directory.GetFiles(dir, "*.tests")
  |> Array.filter ((<>) "README.md")
  |> Array.filter ((<>) ".gitattributes")
  |> Array.map (fun file ->
    let filename = System.IO.Path.GetFileName file
    let emptyTest = { recording = false; name = ""; dbs = []; code = "" }

    let emptyFn =
      { recording = false; name = ""; parameters = []; code = ""; tlid = id 7 }

    let emptyGroup = { name = ""; tests = []; dbs = [] }

    // for recording a multiline test
    let mutable currentTest = emptyTest
    // for recording a multiline function
    let mutable currentFn = emptyFn
    // for recording a bunch if single-line tests grouped together
    let mutable currentGroup = emptyGroup
    let mutable allTests = []
    let mutable functions : Map<string, PT.UserFunction.T> = Map.empty
    let mutable dbs : Map<string, PT.DB.T> = Map.empty
    let owner =
      if filename = "internal.tests" then testAdmin.Force() else testOwner.Force()

    let finish () =
      let initializeDB = filename = "internal.tests" || currentTest.dbs <> []
      if currentTest.recording then
        let newTestCase =
          t
            owner
            initializeDB
            currentTest.name
            currentTest.code
            currentTest.dbs
            functions

        allTests <- allTests @ [ newTestCase ]

      if List.length currentGroup.tests > 0 then
        let newTestCase = testList currentGroup.name currentGroup.tests
        allTests <- allTests @ [ newTestCase ]

      if currentFn.recording then
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
      | Regex @"^\[tests\.(.*)\] with DB (.*)$" [ name; dbName ] ->
        finish ()

        match Map.get dbName dbs with
        | Some db -> currentGroup <- { currentGroup with dbs = [ db ] }
        | None -> Exception.raiseInternal $"No DB named {dbName} found" []

        currentGroup <- { currentGroup with name = name }
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
                  PT.DType.parse v
                  |> Exception.unwrapOptionInternal "Cannot parse type" []
                ({ name = k
                   nameID = gid ()
                   typ = if v = "" then None else Some typ
                   typeID = gid () } : PT.DB.Col))
              |> Map.values }

        dbs <- Map.add name db dbs
      // [function] declaration
      | Regex @"^\[fn\.(\S+) (.*)\]$" [ name; definition ] ->
        finish ()

        let parameters : List<PT.UserFunction.Parameter> =
          definition
          |> String.split " "
          |> List.map (fun name ->
            { name = name
              nameID = gid ()
              description = ""
              typ = Some(PT.TVariable "a")
              typeID = gid () })

        currentFn <-
          { tlid = id i
            recording = true
            name = name
            parameters = parameters
            code = "" }
      // [test] with DB indicator
      | Regex @"^\[test\.(.*)\] with DB (.*)$" [ name; dbName ] ->
        finish ()

        match Map.get dbName dbs with
        | Some db -> currentTest <- { currentTest with dbs = [ db ] }
        | None -> Exception.raiseInternal $"No DB named {dbName} found" []

        currentTest <-
          { currentTest with name = $"{name} (line {i})"; recording = true }
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
        let initializeDB = filename = "internal.tests" || currentGroup.dbs <> []
        let test =
          t
            owner
            initializeDB
            $"{comment} (line {i})"
            code
            currentGroup.dbs
            functions

        currentGroup <- { currentGroup with tests = currentGroup.tests @ [ test ] }
      | Regex @"^(.*)\s*$" [ code ] ->
        let initializeDB = filename = "internal.tests" || currentGroup.dbs <> []
        let test = t owner initializeDB $"line {i}" code currentGroup.dbs functions

        currentGroup <- { currentGroup with tests = currentGroup.tests @ [ test ] }

      | _ -> raise (System.Exception $"can't parse line {i}: {line}"))

    finish ()
    testList $"Tests from {filename}" allTests)
  |> Array.toList
  |> testList "All files"

let tests = lazy (testList "LibExecution" [ fileTests () ])
