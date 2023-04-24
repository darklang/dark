/// Reads, parses, and runs the Dark test files in `testfiles/execution`.
///
/// `testfiles/execution/README.md` provides an outline for how the test files,
/// along with `testfiles/README.md` providing additional context related to
/// syntax.
module Tests.LibExecution

let baseDir = "testfiles/execution/"

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

let setupWorkers (canvasID : CanvasID) (workers : List<string>) : Task<unit> =
  task {
    let workersWithIDs = workers |> List.map (fun w -> w, (gid ()))

    let ops =
      workersWithIDs
      |> List.map (fun (worker, tlid) ->
        PT.SetHandler(
          { tlid = tlid
            ast = PT.Expr.EUnit(gid ())
            spec = PT.Handler.Worker(worker) }
        ))

    let c = Canvas.empty canvasID |> Canvas.addOps ops []

    let oplists =
      workersWithIDs
      |> List.map (fun (_w, tlid) ->
        tlid, ops, PT.Toplevel.TLHandler c.handlers[tlid], Canvas.NotDeleted)

    do! Canvas.saveTLIDs canvasID oplists
  }

let setupDBs (canvasID : CanvasID) (dbs : List<PT.DB.T>) : Task<unit> =
  task {
    let ops =
      // Convert the DBs back into ops so that DB operations will run
      dbs
      |> List.map (fun (db : PT.DB.T) ->
        let initial = PT.CreateDB(db.tlid, db.name, db.typ)
        (db, [ initial ]))

    let oplists =
      ops
      |> List.map (fun (db, ops) ->
        db.tlid, ops, PT.Toplevel.TLDB db, Canvas.NotDeleted)
    do! Canvas.saveTLIDs canvasID oplists
  }


let t
  (internalFnsAllowed : bool)
  (canvasName : string)
  (actualExpr : PT.Expr)
  (expectedExpr : PT.Expr)
  (lineNumber : int)
  (dbs : List<PT.DB.T>)
  (packageFns : List<PT.Package.Fn>)
  (types : List<PT.UserType.T>)
  (functions : List<PT.UserFunction.T>)
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
          PT2RT.FQTypeName.UserTypeName.toRT typ.name, PT2RT.UserType.toRT typ)
        |> Map.ofList

      let rtDBs =
        (dbs |> List.map (fun db -> db.name, PT2RT.DB.toRT db) |> Map.ofList)

      let rtFunctions =
        (functions
         |> List.map (fun fn ->
           let fn = PT2RT.UserFunction.toRT fn
           (fn.name, fn))
         |> Map.ofList)

      let rtPackageFns =
        packageFns
        |> List.map (fun v ->
          let fn = PT2RT.Package.toRT v
          ((RT.FQFnName.Package fn.name), fn))
        |> Map

      let! (state : RT.ExecutionState) =
        executionStateFor canvasID internalFnsAllowed rtDBs rtTypes rtFunctions
      let state =
        { state with libraries = { state.libraries with packageFns = rtPackageFns } }

      let msg = $"\n\n{actualExpr}\n=\n{expectedExpr} ->"

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

      // Run the actual program
      let! actual = Exe.executeExpr state Map.empty (PT2RT.Expr.toRT actualExpr)

      if System.Environment.GetEnvironmentVariable "DEBUG" <> null then
        debuGList "results" (Dictionary.toList results |> List.sortBy fst)

      let actual = normalizeDvalResult actual

      let canonical = Expect.isCanonical actual
      if not canonical then
        debugDval actual |> debuG "not canonicalized"
        Expect.isTrue canonical "expected is canonicalized"

      return Expect.equalDval actual expected msg
    with
    | e ->
      let metadata = Exception.toMetadata e
      printMetadata "" metadata
      return
        Expect.equal
          (e.Message, metadata, e.StackTrace)
          ("Exception thrown in test", [], "")
          ""
  }

// Read all test files. The test file format is described in README.md
let fileTests () : Test =
  System.IO.Directory.GetFiles(baseDir, "*.tests")
  |> Array.filter ((<>) "README.md")
  |> Array.filter ((<>) ".gitattributes")
  |> Array.map (fun file ->

    let filename = System.IO.Path.GetFileName file
    let testName = System.IO.Path.GetFileNameWithoutExtension file
    let initializeCanvas = testName = "internal"
    let shouldSkip = String.startsWith "_" filename

    let rec moduleToTests (moduleName : string) (module' : Parser.TestModule.T) =

      let nestedModules =
        List.map (fun (name, m) -> moduleToTests name m) module'.modules

      let tests =
        module'.tests
        |> List.map (fun test ->
          t
            initializeCanvas
            test.name
            test.actual
            test.expected
            test.lineNumber
            module'.dbs
            module'.packageFns
            module'.types
            module'.fns
            [])

      if List.isEmpty tests && List.isEmpty nestedModules then
        Exception.raiseInternal "No tests or modules found" [ "module", moduleName ]
      else
        testList moduleName (nestedModules @ tests)

    if shouldSkip then
      testList $"skipped - {testName}" []
    else
      try
        (baseDir + filename)
        |> Parser.TestModule.parseTestFile
        |> moduleToTests testName
      with
      | e ->
        print $"Exception in {file}: {e.Message}"
        reraise ())
  |> Array.toList
  |> testList "All"

let tests = lazy (testList "LibExecution" [ fileTests () ])
