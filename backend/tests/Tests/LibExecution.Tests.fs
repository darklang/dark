/// Reads, parses, and runs the Dark test files in `testfiles/execution`.
///
/// `testfiles/execution/README.md` provides an outline for how the test files,
/// along with `testfiles/README.md` providing additional context related to
/// syntax.
module Tests.LibExecution

open Expecto

open System.Threading.Tasks
open FSharp.Control.Tasks

open Fumble
open LibDB.Db

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module Exe = LibExecution.Execution
module PackageIDs = LibExecution.PackageIDs
module NR = LibParser.NameResolver
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
  (actual : PT.Expr)
  (expected : PT.Expr)
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

      let msg (expectedDv : Option<RT.Dval>) (actualDv : Option<RT.Dval>) =
        let lhsPreamble =
          $"{underline}Left-hand side{reset} ({bold}\"actual\"{reset}):"
        let rhsPreamble =
          $"{underline}Right-hand side{reset} ({bold}\"expected\"{reset}):"

        let dvMsg (dv : Option<RT.Dval>) : string =
          match dv with
          | Some dv -> $"{dv}"
          | None -> "(no Dval -- error we couldn't handle)"

        let lhs =
          $"{lhsPreamble}\nsource:\n{red}{actual}\n{reset}value:\n{red}{dvMsg actualDv}\n{reset}"

        let rhs =
          $"{rhsPreamble}\nsource:\n{green}{expected}\n{reset}value:\n{green}{dvMsg expectedDv}\n{reset}"

        $"\n\n{lhs}\n\n{rhs}\n\nTest location: {bold}{underline}{filename}:{lineNumber}{reset}"

      let expected = expected |> PT2RT.Expr.toRT Map.empty 0 None
      let! expected = Exe.executeExpr state expected

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
      let actual = actual |> PT2RT.Expr.toRT Map.empty 0 None
      let! (actual : RT.ExecutionResult) = Exe.executeExpr state actual

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
          | Error(allegedRTE, callStack) ->
            let actual = RT2DT.RuntimeError.toDT allegedRTE
            let errorMessageFn =
              RT.FQFnName.fqPackage
                PackageIDs.Fn.PrettyPrinter.RuntimeTypes.RuntimeError.toErrorMessage

            let! _csString = Exe.callStackString state callStack

            let optionTypeName = RT.FQTypeName.fqPackage PackageIDs.Type.Stdlib.option
            let branchID =
              RT.Dval.DEnum(
                optionTypeName,
                optionTypeName,
                [ RT.ValueType.Known RT.KTUuid ],
                "None",
                []
              )

            let! typeChecked =
              let expected =
                RT.TCustomType(
                  Ok(
                    RT.FQTypeName.fqPackage
                      PackageIDs.Type.LanguageTools.RuntimeTypes.RuntimeError.error
                  ),
                  []
                )
              LibExecution.TypeChecker.unify state.types Map.empty expected actual

            match typeChecked with
            | Ok _ ->
              // The result was correctly a RuntimeError, try to stringify it
              let! result =
                LibExecution.Execution.executeFunction
                  state
                  errorMessageFn
                  []
                  (NEList.ofList branchID [ actual ])

              match result with
              | Ok(RT.DEnum(_, _, [], "ErrorString", [ RT.DString _ ])) ->
                //debuG "callStack" csString
                return result
              | Ok _ ->
                return
                  Exception.raiseInternal
                    "We received an RTE, and when trying to stringify it, got a non-ErrorString response. Instead we got"
                    [ "result", result ]
              | Error(rte, _cs) ->
                let rte = RT2DT.RuntimeError.toDT rte
                print $"{state.test.exceptionReports}"
                return
                  Exception.raiseInternal
                    ("We received an RTE, and when trying to stringify it, there was another RTE error.
                    There is probably a bug in Darklang.LanguageTools.RuntimeErrors.Error.toString")
                    [ "originalRTE", DvalReprDeveloper.toRepr actual
                      "subsequentRTE", DvalReprDeveloper.toRepr rte ]

            | Error reverseTypeCheckPath ->
              return
                Exception.raiseInternal
                  "Alleged RTE was not an RTE  (failed type-check)"
                  [ "reverseTypeCheckPath", reverseTypeCheckPath
                    "allegedRTE", allegedRTE ]
        }
        |> Ply.toTask

      match actual, expected with
      | Ok actual, Ok expected ->
        return
          Expect.RT.equalDval actual expected (msg (Some expected) (Some actual))
      | _ -> return Expect.equal actual expected (msg None None)
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
  let pmPT = LibPackageManager.PackageManager.pt

  let parseTestFile fileName =
    LibParser.TestModule.parseTestFile
      "Tests"
      (localBuiltIns pmPT)
      pmPT
      NR.OnMissing.Allow
      fileName

  System.IO.Directory.GetDirectories(
    baseDir,
    "*",
    System.IO.SearchOption.AllDirectories
  )
  |> Array.map (fun dir ->
    System.IO.Directory.GetFiles(dir, "*.dark")
    |> Array.toList
    |> List.map (fun file ->
      let filename = System.IO.Path.GetFileName file
      let testName = System.IO.Path.GetFileNameWithoutExtension file
      let initializeCanvas = testName = "internal"
      let shouldSkip = filename |> String.contains "_"

      if shouldSkip then
        testList $"skipped - {testName}" []
      else
        try
          let filePath = $"{dir}/{filename}"
          let modules = filePath |> parseTestFile |> (fun ply -> ply.Result)

          let allOps = modules |> List.collect _.ops

          let pm = LibPackageManager.PackageManager.withExtraOps pmPT allOps

          let tests =
            modules
            |> List.map (fun m ->
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
                  m.dbs
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
