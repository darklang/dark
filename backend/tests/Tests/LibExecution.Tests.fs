/// Reads, parses, and runs the Dark test files in `testfiles/execution`.
///
/// `testfiles/execution/README.md` provides an outline for how the test files,
/// along with `testfiles/README.md` providing additional context related to
/// syntax.
module Tests.LibExecution

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Npgsql.FSharp
open LibCloud.Db

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PTParser = LibExecution.ProgramTypesParser
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module Canvas = LibCloud.Canvas
module Serialize = LibCloud.Serialize

open TestUtils.TestUtils

let setupWorkers (canvasID : CanvasID) (workers : List<string>) : Task<unit> =
  task {
    let tls =
      workers
      |> List.map (fun worker ->
        let w : PT.Handler.T =
          { tlid = gid ()
            ast = PT.Expr.EUnit(gid ())
            spec = PT.Handler.Worker(worker) }
        PT.Toplevel.TLHandler w, Serialize.NotDeleted)

    do! Canvas.saveTLIDs canvasID tls
  }

let setupDBs (canvasID : CanvasID) (dbs : List<PT.DB.T>) : Task<unit> =
  task {
    let tls = dbs |> List.map (fun db -> PT.Toplevel.TLDB db, Serialize.NotDeleted)
    do! Canvas.saveTLIDs canvasID tls
  }


let t
  (internalFnsAllowed : bool)
  (canvasName : string)
  (actualExpr : PT.Expr)
  (expectedExpr : PT.Expr)
  (filename : string)
  (lineNumber : int)
  (dbs : List<PT.DB.T>)
  (types : List<PT.UserType.T>)
  (functions : List<PT.UserFunction.T>)
  (constants : List<PT.UserConstant.T>)
  (workers : List<string>)
  : Test =
  testTask $"line{lineNumber}" {
    try
      let! canvasID =
        // Little optimization to skip the DB sometimes
        let initializeCanvas = internalFnsAllowed || dbs <> [] || workers <> []
        if initializeCanvas then
          initializeTestCanvas canvasName
        else
          System.Guid.NewGuid() |> Task.FromResult

      let rtTypes =
        types
        |> List.map (fun typ ->
          PT2RT.TypeName.UserProgram.toRT typ.name, PT2RT.UserType.toRT typ)
        |> Map.ofList

      let rtDBs =
        (dbs |> List.map (fun db -> db.name, PT2RT.DB.toRT db) |> Map.ofList)

      let rtFunctions =
        (functions
         |> List.map (fun fn ->
           let fn = PT2RT.UserFunction.toRT fn
           (fn.name, fn))
         |> Map.ofList)

      let rtConstants =
        (constants
         |> List.map (fun c ->
           let c = PT2RT.UserConstant.toRT c
           (c.name, c))
         |> Map.ofList)

      let! (state : RT.ExecutionState) =
        executionStateFor
          canvasID
          internalFnsAllowed
          false
          rtDBs
          rtTypes
          rtFunctions
          rtConstants
      let red = "\u001b[31m"
      let green = "\u001b[32m"
      let bold = "\u001b[1;37m"
      let underline = "\u001b[4m"
      let reset = "\u001b[0m"

      let rhsMsg =
        $"{underline}Right-hand-side test code{reset} (aka {bold}\"expected\"{reset}):\n{green}\n{expectedExpr}\n{reset}"

      let lhsMsg =
        $"{underline}Left-hand-side test code{reset} (aka {bold}\"actual\"{reset}):\n{red}\n{actualExpr}\n{reset}"

      let msg =
        $"\n\n{rhsMsg}\n\n{lhsMsg}\n\nTest location: {bold}{underline}{filename}:{lineNumber}{reset}"

      let! expected = Exe.executeExpr state Map.empty (PT2RT.Expr.toRT expectedExpr)

      // Initialize
      if workers <> [] then do! setupWorkers canvasID workers
      if dbs <> [] then do! setupDBs canvasID dbs

      let results, traceDvalFn = Exe.traceDvals ()
      let state =
        if System.Environment.GetEnvironmentVariable "DEBUG" <> null then
          { state with tracing = { state.tracing with traceDval = traceDvalFn } }
        else
          state

      // Run the actual program (left-hand-side of the =)
      let! actual = Exe.executeExpr state Map.empty (PT2RT.Expr.toRT actualExpr)

      if System.Environment.GetEnvironmentVariable "DEBUG" <> null then
        debuGList "results" (Dictionary.toList results |> List.sortBy fst)

      let actual = normalizeDvalResult actual
      let expected = normalizeDvalResult expected

      let canonical = Expect.isCanonical actual
      if not canonical then
        debugDval actual |> debuG "not canonicalized"
        Expect.isTrue canonical "expected is canonicalized"

      // CLEANUP consider not doing the toErrorMessage call
      // just test the actual RuntimeError Dval,
      // and have separate tests around pretty-printing the error
      let! actual =
        task {
          match actual with
          | RT.DError(_, actual) ->
            let actual = RT.RuntimeError.toDT actual
            let! typeChecked =
              let expected =
                RT.TCustomType(
                  Ok(
                    RT.TypeName.fqPackage
                      "Darklang"
                      [ "LanguageTools"; "RuntimeErrors" ]
                      "Error"
                      0
                  ),
                  []
                )
              let context =
                LibExecution.TypeChecker.Context.FunctionCallParameter(
                  (RT.FnName.fqPackage
                    "Darklang"
                    [ "LanguageTools"; "RuntimeErrors"; "Error" ]
                    "toErrorMessage"
                    0),
                  { name = ""; typ = expected },
                  0,
                  None
                )
              let types = RT.ExecutionState.availableTypes state
              let typeArgSymbolTable = Map.empty
              LibExecution.TypeChecker.unify
                context
                types
                typeArgSymbolTable
                expected
                actual

            let errorMessageFn =
              (RT.FnName.fqPackage
                "Darklang"
                [ "LanguageTools"; "RuntimeErrors"; "Error" ]
                "toErrorMessage"
                0)

            match typeChecked with
            | Ok _ ->
              return!
                LibExecution.Interpreter.callFn
                  state
                  Map.empty
                  0UL
                  errorMessageFn
                  []
                  (NEList.ofList actual [])

            | Error e ->
              return!
                LibExecution.Interpreter.callFn
                  state
                  Map.empty
                  0UL
                  errorMessageFn
                  []
                  (NEList.ofList (RT.RuntimeError.toDT e) [])

          | _ -> return actual
        }

      return Expect.equalDval actual expected msg
    with
    | :? Expecto.AssertException as e -> Exception.reraise e
    | e ->
      let metadata = Exception.toMetadata e
      printMetadata "" metadata
      return
        Expect.equal
          (e.Message, metadata, e.StackTrace)
          ("Exception thrown in test", [], "")
          ""
  }

let baseDir = "testfiles/execution/"



// Read all test files. The test file format is described in README.md
let fileTests () : Test =
  System.IO.Directory.GetDirectories(baseDir, "*")
  |> Array.map (fun dir ->
    System.IO.Directory.GetFiles(dir, "*.dark")
    |> Array.toList
    |> List.map (fun file ->
      let filename = System.IO.Path.GetFileName file
      let testName = System.IO.Path.GetFileNameWithoutExtension file
      let initializeCanvas = testName = "internal"
      let shouldSkip = String.startsWith "_" filename

      if shouldSkip then
        testList $"skipped - {testName}" []
      else
        try
          let modules =
            $"{dir}/{filename}"
            |> LibParser.TestModule.parseTestFile
              resolverWithBuiltinsAndPackageManager
            |> fun ply -> ply.Result

          // Within a module, tests have access to
          let fns = modules |> List.map (fun m -> m.fns) |> List.concat
          let types = modules |> List.map (fun m -> m.types) |> List.concat
          let constants = modules |> List.map (fun m -> m.constants) |> List.concat
          let tests =
            modules
            |> List.map (fun m ->
              m.tests
              |> List.map (fun test ->
                t
                  initializeCanvas
                  test.name
                  test.actual
                  test.expected
                  filename
                  test.lineNumber
                  m.dbs
                  types
                  fns
                  constants
                  []))
            |> List.concat


          testList testName tests
        with e ->
          print $"Exception in {file}: {e.Message}"
          reraise ())
    |> testList dir)
  |> Array.toList
  |> testList "All"

let tests = lazy (testList "LibExecution" [ fileTests () ])
