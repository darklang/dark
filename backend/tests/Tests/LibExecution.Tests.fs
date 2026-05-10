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
open LibDB.Sqlite

open Prelude

module RT = LibExecution.RuntimeTypes
module PT = LibExecution.ProgramTypes
module PT2RT = LibExecution.ProgramTypesToRuntimeTypes
module RT2DT = LibExecution.RuntimeTypesToDarkTypes
module Exe = LibExecution.Execution
module PackageRefs = LibExecution.PackageRefs
module Dval = LibExecution.Dval
module NR = LibParser.NameResolver
module RTNR = LibExecution.RuntimeTypes.NameResolution
module Toplevels = LibCloud.Toplevels
module Serialize = LibCloud.Serialize

open TestUtils.TestUtils

let setupDBs (dbs : List<PT.DB.T>) : Task<unit> =
  task {
    let tls = dbs |> List.map (fun db -> db, Serialize.NotDeleted)
    do! Toplevels.saveTLIDs tls
  }

// FUTURE: drop `program.dbs` entirely
// ----------------------------------------------------------------
// `RT.Program.dbs : Map<string, DB.T>` is a canvas-era relic. When
// each test had its own `canvasID`, the `dbs` map was the
// per-canvas whitelist of accessible DBs. Single-instance Dark
// removed canvases, so the whitelist no longer scopes anything —
// `toplevels_v0` is global, so every name resolves the same way
// for every executor.
//
// What it still does:
//   - DB builtins (DB.fs) match `DDB dbname` and look up
//     `exeState.program.dbs[dbname]` to get the `DB.T`
//     (tlid + type).
//   - Interpreter's `VarNotFound` case checks `program.dbs` to
//     decide whether `XDB` becomes `DDB "XDB"` vs. raises an
//     unbound-variable error.
//   - Tests pre-populate it via `setupDBs` so the test code can
//     reference `[<DB>]`-declared names. The `t` function above
//     wires `rtDBs` into `executionStateFor`.
//
// What we'd do instead:
//   - DB builtins query `toplevels_v0 WHERE name = @name`,
//     fronted by a process-global cache (`name -> DB.T`) so we
//     don't do a SELECT per `DB.set` call.
//   - `VarNotFound` does the same cached lookup.
//   - `Builtin.dbCreate` just writes the row; subsequent
//     references find it via the cache. No state mutation
//     required.
//   - Drop `RT.Program.dbs` (and the field it adds to
//     `RT.Program`). `executionStateFor` stops taking `dbs`.
//     Tests stop calling `setupDBs` for the rtDBs map.
//   - `[<DB>]` parser sugar can either go away (tests do
//     `Builtin.dbCreate` inline) or become "shorthand that
//     emits a `dbCreate` call at parse-time".
//
// Scope: ~16 builtin call sites in `Builtins.Matter/Libs/DB.fs`,
// `Interpreter.VarNotFound`, `RT.Program`, every
// `executionStateFor` call site, plus the cache and (optionally)
// test conversion. Hours, not minutes.
//
// Why deferred: the perf pain that motivated this thread is
// already gone — batched-INSERT in `LibCloud.Toplevels.saveTLIDs`
// took the cloud/db.dark bucket from 3:22 to 3.8s. The
// `program.dbs` removal is now a clean-up, not a perf necessity,
// and it's big enough to deserve its own PR.

let runtimeErrorMessage
  (state : RT.ExecutionState)
  (allegedRTE : RT.RuntimeError.Error)
  (callStack : RT.CallStack)
  : Ply<string> =
  uply {
    let actual = RT2DT.RuntimeError.toDT allegedRTE
    let errorMessageFn =
      RT.FQFnName.fqPackage (
        PackageRefs.Fn.PrettyPrinter.RuntimeTypes.RuntimeError.toErrorMessage ()
      )

    let! _csString = Exe.callStackString state callStack

    let! typeChecked =
      let expected =
        RT.TCustomType(
          RT.NameResolution.ok (
            RT.FQTypeName.fqPackage (
              PackageRefs.Type.LanguageTools.RuntimeTypes.RuntimeError.error ()
            )
          ),
          []
        )
      LibExecution.TypeChecker.unify state.types Map.empty expected actual

    match typeChecked with
    | Ok _ ->
      let! result =
        LibExecution.Execution.executeFunction
          state
          errorMessageFn
          []
          (NEList.ofList (RT.DUuid PT.mainBranchId) [ actual ])

      match result with
      | Ok(RT.DEnum(_, _, [], "ErrorString", [ RT.DString msg ])) -> return msg
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
            [ "originalRTE", actual; "subsequentRTE", rte ]

    | Error reverseTypeCheckPath ->
      return
        Exception.raiseInternal
          "Alleged RTE was not an RTE  (failed type-check)"
          [ "reverseTypeCheckPath", reverseTypeCheckPath; "allegedRTE", allegedRTE ]
  }


let t
  (_canvasName : string)
  (pmPT : PT.PackageManager)
  (actual : PT.Expr)
  (expected : LibParser.TestModule.PTExpected)
  (filename : string)
  (lineNumber : int)
  (dbs : List<PT.DB.T>)
  : Test =
  testTask $"line{lineNumber}" {
    try
      // Wipe per-test state when this test uses UserDB.
      // Single-instance Dark — toplevels_v0 / user_data_v0 are global,
      // so isolation between .dark testfile cases is "wipe +
      // repopulate" (the surrounding testList is testSequenced so
      // wipes don't race parallel writes).
      if dbs <> [] then
        do! Sql.query "DELETE FROM toplevels_v0" |> Sql.executeStatementAsync
        do! Sql.query "DELETE FROM user_data_v0" |> Sql.executeStatementAsync

      let rtDBs =
        dbs |> List.map (fun db -> (db.name, PT2RT.DB.toRT db)) |> Map.ofList

      let! (state : RT.ExecutionState) = executionStateFor pmPT false rtDBs

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

      // Initialize
      if dbs <> [] then do! setupDBs dbs

      // Run the actual program (left-hand-side of the =)
      let actual = actual |> PT2RT.Expr.toRT Map.empty 0 None
      let! (actual : RT.ExecutionResult) = Exe.executeExpr state actual

      let! expectedValueResult =
        match expected with
        | LibParser.TestModule.PTExpected.PTExpectedExpr expected ->
          task {
            let expected = expected |> PT2RT.Expr.toRT Map.empty 0 None
            let! expected = Exe.executeExpr state expected
            return Some expected
          }
        | LibParser.TestModule.PTExpected.PTExpectedError _ -> Task.FromResult None
        | LibParser.TestModule.PTExpected.PTExpectedSqlError _ ->
          Task.FromResult None

      // DEBUG dump of per-expr trace values used to live here, but
      // the traceDval hook + Execution.traceDvals collector were
      // removed with the trace rewrite (per-AST-node values are
      // a follow-up PR). Drop the dump until the new tracer lands.
      ignore<unit> ()

      // Promote any ephemeral blobs to content-addressed Persistent
      // refs so two expressions that construct the same bytes via
      // different `newEphemeralBlob` calls compare equal under the
      // test framework's structural `dvalEquality`. The no-op insert
      // means we don't persist to `package_blobs` — we only need the
      // hash to dedupe UUID identity.
      let noopInsert _ _ = uply { return () }
      let promoteIfOk (r : RT.ExecutionResult) : Task<RT.ExecutionResult> =
        task {
          match r with
          | Ok dv ->
            let! promoted =
              LibExecution.Blob.promote state noopInsert dv |> Ply.toTask
            return Ok promoted
          | Error _ -> return r
        }
      let! actual = promoteIfOk actual
      let! expectedValueResult =
        task {
          match expectedValueResult with
          | Some r ->
            let! p = promoteIfOk r
            return Some p
          | None -> return None
        }
      let actual = Result.map normalizeDvalResult actual
      let expectedValueResult =
        expectedValueResult |> Option.map (Result.map normalizeDvalResult)

      match expected with
      | LibParser.TestModule.PTExpected.PTExpectedExpr _ ->
        match actual with
        | Error _ -> ()
        | Ok actual ->
          let canonical = Expect.isCanonical actual
          if not canonical then
            debugDval actual |> debuG "not canonicalized"
            Expect.isTrue canonical "expected is canonicalized"

        match expectedValueResult with
        | None ->
          return Expect.isTrue false "expected value result should be present"
        | Some expected ->
          match actual, expected with
          | Ok actual, Ok expected ->
            return
              Expect.RT.equalDval actual expected (msg (Some expected) (Some actual))
          | _ -> return Expect.equal actual expected (msg None None)

      | LibParser.TestModule.PTExpected.PTExpectedError expectedError ->
        match actual with
        | Ok actual ->
          let actual = normalizeDvalResult actual
          return
            Expect.equal
              (Some $"value: {actual}")
              None
              $"Expected runtime error `{expectedError}` but expression returned a value.\n\nTest location: {filename}:{lineNumber}"
        | Error(allegedRTE, callStack) ->
          let! actualError =
            runtimeErrorMessage state allegedRTE callStack |> Ply.toTask
          return Expect.equal actualError expectedError ""

      | LibParser.TestModule.PTExpected.PTExpectedSqlError expectedSqlError ->
        match actual with
        | Ok actual ->
          let actual = normalizeDvalResult actual
          return
            Expect.equal
              (Some $"value: {actual}")
              None
              $"Expected SQL runtime error `{expectedSqlError}` but expression returned a value.\n\nTest location: {filename}:{lineNumber}"
        | Error(allegedRTE, callStack) ->
          let! actualError =
            runtimeErrorMessage state allegedRTE callStack |> Ply.toTask
          let expected =
            LibExecution.RTQueryCompiler.errorTemplate + expectedSqlError
          return Expect.equal actualError expected ""
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
  let pmPT = LibDB.PackageManager.pt

  let parseTestFile fileName =
    LibParser.TestModule.parseTestFile "Tests" (localBuiltIns pmPT) pmPT fileName

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
      let shouldSkip = filename |> String.contains "_"

      if shouldSkip then
        testList $"skipped - {testName}" []
      else
        try
          let filePath = $"{dir}/{filename}"
          let modules = filePath |> parseTestFile |> (fun ply -> ply.Result)

          let allOps = modules |> List.collect _.ops

          let pm = LibDB.PackageManager.withExtraOps pmPT allOps

          let tests =
            modules
            |> List.map (fun m ->
              m.tests
              |> List.map (fun test ->
                t
                  test.name
                  pm
                  test.actual
                  test.expected
                  filename
                  test.lineNumber
                  m.dbs))
            |> List.concat

          // .dark files under `cloud/` exercise UserDB / handler state
          // through shared toplevels_v0 / user_data_v0 rows. Run those
          // file's cases sequentially so the per-test wipe + setupDBs
          // in `t` doesn't race parallel writes from sibling tests.
          let usesSharedDBState = dir.Contains "/cloud"
          if usesSharedDBState then
            testSequenced (testList testName tests)
          else
            testList testName tests
        with e ->
          print $"Exception in {file}: {e.Message}"
          reraise ())
    |> testList dir)
  |> Array.toList
  |> testList "All"

let tests = lazy (testList "LibExecution" [ fileTests () ])
