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
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module Exe = LibExecution.Execution
module PackageIDs = LibExecution.PackageIDs
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
  (pmPT : PT.PackageManager)
  (actualExpr : PT.Expr)
  (expectedExpr : PT.Expr)
  (filename : string)
  (lineNumber : int)
  (dbs : List<PT.DB.T>)
  (workers : List<string>)
  : Test =
  testTask $"line{lineNumber}" {
    try
      // Little optimization to skip the DB sometimes
      let! canvasID =
        let initializeCanvas = internalFnsAllowed || dbs <> [] || workers <> []
        if initializeCanvas then
          initializeTestCanvas canvasName
        else
          System.Guid.NewGuid() |> Task.FromResult

      let rtDBs =
        dbs |> List.map (fun db -> (db.name, PT2RT.DB.toRT db)) |> Map.ofList

      let! (state : RT.ExecutionState) =
        executionStateFor pmPT canvasID internalFnsAllowed false rtDBs

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

      let expectedExpr = PT2RT.Expr.toRT expectedExpr
      let! expected = Exe.executeExpr state Map.empty expectedExpr

      // Initialize
      if workers <> [] then do! setupWorkers canvasID workers
      if dbs <> [] then do! setupDBs canvasID dbs

      let results, traceDvalFn = Exe.traceDvals ()
      let state =
        if System.Environment.GetEnvironmentVariable "DEBUG" <> null then
          { state with tracing.traceDval = traceDvalFn }
        else
          state

      // Run the actual program (left-hand-side of the =)
      let actualExpr = PT2RT.Expr.toRT actualExpr
      let! actual = Exe.executeExpr state Map.empty actualExpr

      if System.Environment.GetEnvironmentVariable "DEBUG" <> null then
        debuGList "results" (Dictionary.toList results |> List.sortBy fst)

      let actual = Result.map normalizeDvalResult actual
      let expected = Result.map normalizeDvalResult expected

      match actual with
      | Error _ -> ()
      | Ok actual ->
        let canonical = Expect.isCanonical actual
        if not canonical then
          debugDval actual |> debuG "not canonicalized"
          Expect.isTrue canonical "expected is canonicalized"

      // CLEANUP consider not doing the toErrorMessage call
      // just test the actual RuntimeError Dval,
      // and have separate tests around pretty-printing the error
      let! actual =
        uply {
          match actual with
          | Ok _ -> return actual
          // "alleged" because sometimes we incorrectly construct an RTE... (should be rare, and only during big refactors)
          | Error(_, allegedRTE) ->
            let actual = RT.RuntimeError.toDT allegedRTE
            let errorMessageFn =
              RT.FQFnName.fqPackage
                PackageIDs.Fn.LanguageTools.RuntimeErrors.Error.toErrorMessage

            let! typeChecked =
              let expected =
                RT.TCustomType(
                  Ok(
                    RT.FQTypeName.fqPackage
                      PackageIDs.Type.LanguageTools.RuntimeError.error
                  ),
                  []
                )

              let context =
                LibExecution.TypeChecker.Context.FunctionCallParameter(
                  errorMessageFn,
                  { name = ""; typ = expected },
                  0
                )
              let types = RT.ExecutionState.availableTypes state
              LibExecution.TypeChecker.unify context types Map.empty expected actual

            match typeChecked with
            | Ok _ ->
              // The result was correctly a RuntimeError, try to stringify it
              let! result =
                LibExecution.Execution.executeFunction
                  state
                  errorMessageFn
                  []
                  (NEList.ofList actual [])

              match result with
              | Error(_, result) ->
                let result = RT.RuntimeError.toDT result
                print $"{state.test.exceptionReports}"
                return
                  Exception.raiseInternal
                    ("We received an RTE, and when trying to stringify it, there was another RTE error.
                    There is probably a bug in Darklang.LanguageTools.RuntimeErrors.Error.toString")
                    [ "originalError", LibExecution.DvalReprDeveloper.toRepr actual
                      "stringified", LibExecution.DvalReprDeveloper.toRepr result ]
              | Ok(RT.DEnum(_, _, [], "ErrorString", [ RT.DString _ ])) ->
                return result
              | Ok _ ->
                return
                  Exception.raiseInternal
                    "We received an RTE, and when trying to stringify it, got a non-ErrorString response. Instead we got"
                    [ "result", result ]

            | Error e ->
              debuG "Alleged RTE was not an RTE" e
              // The result was not a RuntimeError, try to stringify the typechecker error
              return!
                LibExecution.Execution.executeFunction
                  state
                  errorMessageFn
                  []
                  (NEList.ofList (RT.RuntimeError.toDT e) [])
        }
        |> Ply.toTask

      match actual, expected with
      | Ok actual, Ok expected -> return Expect.equalDval actual expected msg
      | _ -> return Expect.equal actual expected msg
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
  // Note: we use this at parse-time - but later we need to use an enhanced one,
  // with the 'extra' things defined in the test modules.
  let pmPT = LibCloud.PackageManager.pt

  let parseTestFile fileName =
    uply {
      let! (state : RT.ExecutionState) =
        let canvasID = System.Guid.NewGuid()
        executionStateFor pmPT canvasID false false Map.empty

      let name =
        RT.FQFnName.FQFnName.Package PackageIDs.Fn.Internal.Test.parseTestFile

      let onMissingType =
        RT.FQTypeName.FQTypeName.Package
          PackageIDs.Type.LanguageTools.NameResolver.nameResolverOnMissing

      let onMissingAllow =
        RT.Dval.DEnum(onMissingType, onMissingType, [], "Allow", [])

      let getPmFnName =
        RT.FQFnName.FQFnName.Package PackageIDs.Fn.LanguageTools.PackageManager.pm

      let! execGetPmResult =
        Exe.executeFunction state getPmFnName [] (NEList.singleton RT.Dval.DUnit)

      let! pm =
        uply {
          match execGetPmResult with
          | Ok dval -> return dval
          | Error(_callStack, rte) ->
            let! rteString = (Exe.rteToString state rte)
            return
              Exception.raiseInternal
                "Error executing pm function"
                [ "rte", rteString ]
        }

      let parseTestFileArgs =
        NEList.ofList
          (RT.DString "Tests")
          [ pm; onMissingAllow; RT.DString fileName ]

      let! execResult = LibExecution.Execution.executeFunction state name [] parseTestFileArgs

      match execResult with
      | Ok dval -> return Internal.TestModule.fromDT dval
      | Error(_, rte) ->
        let! rteString = Exe.rteToString state rte
        return
          Exception.raiseInternal
            "Error executing parseTestFile function"
            [ "error", rteString ]
    }

  System.IO.Directory.GetDirectories(baseDir, "*")
  |> Array.map (fun dir ->
    System.IO.Directory.GetFiles(dir, "*.dark")
    |> Array.toList
    |> List.filter (String.endsWith "alt-json.dark")
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
            $"{dir}/{filename}" |> parseTestFile |> (fun ply -> ply.Result)

          let pm =
            PT.PackageManager.withExtras
              pmPT
              (modules |> List.collect _.types)
              (modules |> List.collect _.constants)
              (modules |> List.collect _.fns)

          let allTypes = modules |> List.collect _.types

          let typesWithNoModule =
            modules
            |> List.collect _.types
            |> List.filter (fun t -> t.name.modules = [])

          let tests =
            modules
            |> List.map (fun m ->
              let dbTypes =
                List.fold
                  (fun (accumulatedTypes, currentModule) moduleName ->
                    let relevantModules = currentModule @ [ moduleName ]
                    let typesInCurrentModule =
                      allTypes
                      |> List.filter (fun t -> t.name.modules = relevantModules)
                    (typesInCurrentModule @ accumulatedTypes, relevantModules))
                  ([], [])
                  m.name
                |> fst
                |> List.append typesWithNoModule

              let dbs =
                dbTypes
                |> List.collect (fun t ->
                  m.dbs
                  |> List.filter (fun db ->
                    db.name = t.name.name
                    && match t.declaration.definition with
                       | PT.TypeDeclaration.Alias typ -> db.typ = typ
                       | _ -> false))

              m.tests
              |> List.map (fun test ->
                t
                  initializeCanvas
                  test.name
                  pm
                  test.actual
                  test.expected
                  filename
                  test.lineNumber
                  dbs
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
