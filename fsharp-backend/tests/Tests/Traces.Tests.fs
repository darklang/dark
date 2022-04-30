module Tests.Traces

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Tablecloth
open TestUtils.TestUtils

open LibExecution.RuntimeTypes

module Shortcuts = TestUtils.RTShortcuts

module Traces = LibBackend.Traces
module Canvas = LibBackend.Canvas
module AT = LibExecution.AnalysisTypes
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module TI = LibBackend.TraceInputs
module TFR = LibBackend.TraceFunctionResults
module RealExecution = LibRealExecution.RealExecution

let testTraceIDsOfTlidsMatch =
  test "traceIDs from tlids are as expected" {
    Expect.equal
      "e170d0d5-14de-530e-8dd0-a445aee7ca81"
      (Traces.traceIDofTLID 325970458UL |> string)
      "traceisasexpected"

    Expect.equal
      "1d10dd39-9638-53c8-86ca-643c267efe44"
      (Traces.traceIDofTLID 1539654774UL |> string)
      "traceisasexpected"
  }


let testFilterSlash =
  testTask "test that a request which doesnt match doesnt end up in the traces" {
    // set up handler with route param
    let! meta = initializeTestCanvas (Randomized "test-filter_slash")
    let route = "/:rest"
    let handler = testHttpRouteHandler route "GET" (PT.EBlank 0UL)
    let! (c : Canvas.T) = canvasForTLs meta [ PT.Toplevel.TLHandler handler ]

    // make irrelevant request
    let t1 = System.Guid.NewGuid()
    let desc = ("HTTP", "/", "GET")
    let! (_d : NodaTime.Instant) = TI.storeEvent meta.id t1 desc (DStr "1")

    // load+check irrelevant trace
    let! loaded = Traces.traceIDsForHandler c handler
    Expect.equal loaded [ Traces.traceIDofTLID handler.tlid ] "ids is the default"

    return ()
  }


let testRouteVariablesWorkWithStoredEvents =
  testTask "route variables work with stored events" {
    let! meta = initializeTestCanvas (Randomized "route_variables_works")

    // set up handler
    let httpRoute = "/some/:vars/:and/such"
    let handler = testHttpRouteHandler httpRoute "GET" (PT.EBlank 0UL)
    let! (c : Canvas.T) = canvasForTLs meta [ PT.Toplevel.TLHandler handler ]

    // store an event that matches the handler
    let t1 = System.Guid.NewGuid()
    let httpRequestPath = "/some/vars/and/such"
    let desc = ("HTTP", httpRequestPath, "GET")
    let! (_ : NodaTime.Instant) = TI.storeEvent c.meta.id t1 desc (DStr "1")

    // check we get back the data we put into it
    let! events = TI.loadEvents c.meta.id ("HTTP", httpRoute, "GET")

    let (loadedPaths, loadedDvals) =
      events
      |> List.map (fun (loadedPath, _, _, loadedDval) -> (loadedPath, loadedDval))
      |> List.unzip

    Expect.equal loadedPaths [ httpRequestPath ] "path is the same"
    Expect.equal loadedDvals [ (DStr "1") ] "data is the same"

    // check that the event is not in the 404s
    let! f404s = TI.getRecent404s c.meta.id
    Expect.equal [] f404s "no 404s"
  }


let testRouteVariablesWorkWithTraceInputsAndWildcards =
  testTask "route variables work with trace inputs and wildcards" {
    let! meta =
      initializeTestCanvas (Randomized "route_variables_works_with_withcards")

    // '_' is the "wildcard" here, and the '-' matches the wildcard.
    // '-' could equally well be '!' or 'Z' or '🇨🇭' or "-matcheswildcard-"
    let route = "/api/create_token"
    let requestPath = "/api/create-token"

    // set up handler
    let handler = testHttpRouteHandler route "GET" (PT.EBlank 0UL)
    let! (c : Canvas.T) = canvasForTLs meta [ PT.Toplevel.TLHandler handler ]

    // store an event
    let t1 = System.Guid.NewGuid()
    let desc = ("HTTP", requestPath, "GET")
    let! (_ : NodaTime.Instant) = TI.storeEvent c.meta.id t1 desc (DStr "1")

    // check we get back the path for a route with a variable in it
    let! events = TI.loadEvents c.meta.id ("HTTP", route, "GET")

    Expect.equal [] events ""
  }

let testStoredEventRoundtrip =
  testTask "test stored events can be roundtripped" {
    let! (meta1 : Canvas.Meta) =
      initializeTestCanvas (Randomized "stored_events_can_be_roundtripped1")
    let! (meta2 : Canvas.Meta) =
      initializeTestCanvas (Randomized "stored_events_can_be_roundtripped2")
    let id1 = meta1.id
    let id2 = meta2.id

    let t1 = System.Guid.NewGuid()
    let t2 = System.Guid.NewGuid()
    let t3 = System.Guid.NewGuid()
    let t4 = System.Guid.NewGuid()
    let t5 = System.Guid.NewGuid()
    let t6 = System.Guid.NewGuid()
    do! TI.clearAllEvents id1
    do! TI.clearAllEvents id2

    let desc1 = ("HTTP", "/path", "GET")
    let desc2 = ("HTTP", "/path2", "GET")
    let desc3 = ("HTTP", "/path", "POST")
    let desc4 = ("BG", "lol", "_")
    do! TI.storeEvent id1 t1 desc1 (DStr "1")
    do! TI.storeEvent id1 t2 desc1 (DStr "2")
    do! TI.storeEvent id1 t3 desc3 (DStr "3")
    do! TI.storeEvent id1 t4 desc2 (DStr "3")
    do! TI.storeEvent id2 t5 desc2 (DStr "3")
    do! TI.storeEvent id2 t6 desc4 (DStr "3")
    let t4_get4th (_, _, _, x) = x
    let t5_get5th (_, _, _, _, x) = x

    // This is a bit racy
    let! listed = TI.listEvents TI.All id1
    let actual = (List.sort (List.map t5_get5th listed))
    let result =
      actual = (List.sort [ t1; t3; t4 ]) || actual = (List.sort [ t2; t3; t4 ])
    Expect.equal result true "list host events"

    let! loaded = TI.loadEventIDs id2 desc4 |> Task.map (List.map Tuple2.first)
    Expect.equal (List.sort loaded) (List.sort [ t6 ]) "list desc events"

    let! loaded1 = TI.loadEvents id1 desc1 |> Task.map (List.map t4_get4th)
    Expect.equal loaded1 [ DStr "2"; DStr "1" ] "load GET events"

    let! loaded2 = TI.loadEvents id1 desc3 |> Task.map (List.map t4_get4th)
    Expect.equal loaded2 [ DStr "3" ] "load POST events"

    let! loaded3 = TI.loadEvents id2 desc3 |> Task.map (List.map t4_get4th)
    Expect.equal loaded3 [] "load no host2 events"

    let! loaded4 = TI.loadEvents id2 desc2 |> Task.map (List.map t4_get4th)
    Expect.equal loaded4 [ DStr "3" ] "load host2 events"
  }


let testTraceDataJsonFormatRedactsPasswords =
  testTask "trace data json format redacts passwords" {
    // set up
    let id = gid ()

    let traceData : AT.TraceData =
      { input = [ ("event", DPassword(Password(UTF8.toBytes "redactme1"))) ]
        timestamp = NodaTime.Instant.UnixEpoch
        function_results =
          [ ("Password::hash",
             id,
             "foobar",
             0,
             DPassword(Password(UTF8.toBytes "redactme2"))) ] }

    let expected : AT.TraceData =
      { input = [ ("event", DPassword(Password(UTF8.toBytes "Redacted"))) ]
        timestamp = NodaTime.Instant.UnixEpoch
        function_results =
          [ ("Password::hash",
             id,
             "foobar",
             0,
             DPassword(Password(UTF8.toBytes "Redacted"))) ] }

    // roundtrip serialization
    let actual =
      traceData
      |> Json.OCamlCompatible.serialize
      |> Json.OCamlCompatible.deserialize<AT.TraceData>

    // check
    Expect.equal actual expected "traceData round trip"
  }


let testFunctionTracesAreStored =
  testTask "function traces are stored" {
    // set up canvas, user fn
    let! (meta : Canvas.Meta) =
      initializeTestCanvas (Randomized "test-function-traces-are-stored")
    let fnid = 12312345234UL

    let (userFn : RT.UserFunction.T) =
      { tlid = fnid
        name = "test_fn"
        parameters = []
        returnType = RT.TInt
        description = ""
        infix = false
        body = FSharpToExpr.parseRTExpr "DB.generateKey" }

    let program =
      { canvasID = meta.id
        canvasName = meta.name
        accountID = meta.owner
        dbs = Map.empty
        userFns = Map.singleton userFn.name userFn
        userTypes = Map.empty
        secrets = [] }

    // call the user fn, which should result in a trace being stored
    let executionID = LibService.Telemetry.executionID ()
    let traceID = System.Guid.NewGuid()

    let! (state, _) = RealExecution.createState executionID traceID (gid ()) program

    let (ast : Expr) = (Shortcuts.eApply (Shortcuts.eUserFnVal "test_fn") [])

    let! (_ : Dval) = LibExecution.Execution.executeExpr state Map.empty ast

    // check for traces - they're saved in the background so wait for them
    let waitForResult id : Task<List<AT.FunctionResult>> =
      task {
        let mutable testFnResult = None
        for i in 1..10 do
          do!
            task {
              if testFnResult = None then
                let! result = TFR.load meta.id traceID id
                testFnResult <- Some result
                return ()
              else
                do! Task.Delay 300
                return ()
            }
        return testFnResult |> Option.unwrap []
      }


    let! testFnResult = waitForResult state.tlid
    Expect.equal
      (List.length testFnResult)
      (1)
      "handler should only have fn result for test_fn"

    let! dbGenerateResult = waitForResult fnid
    Expect.equal
      (List.length dbGenerateResult)
      1
      "functions should only have fn result for DB::generateKey"
  }

let testErrorTracesAreStored =
  testTask "error traces are stored" {
    // set up canvas, user fn
    let! (meta : Canvas.Meta) =
      initializeTestCanvas (Randomized "test-error-traces-are-stored")

    let (db : DB.T) = { tlid = gid (); name = "MyDB"; cols = []; version = 0 }

    let program =
      { canvasID = meta.id
        canvasName = meta.name
        accountID = meta.owner
        dbs = Map [ "MyDB", db ]
        userFns = Map.empty
        userTypes = Map.empty
        secrets = [] }

    // call the user fn, which should result in a trace being stored
    let executionID = LibService.Telemetry.executionID ()
    let traceID = System.Guid.NewGuid()

    let! (state, _) = RealExecution.createState executionID traceID (gid ()) program

    // the DB has no columns, but the code expects one, causing it to fail
    let code = "DB.set_v1 { a = \"y\" } \"key\" MyDB"

    let (ast : Expr) = FSharpToExpr.parseRTExpr code

    let! (_ : Dval) = LibExecution.Execution.executeExpr state Map.empty ast

    // check for traces
    let! testFnResult = TFR.load meta.id traceID state.tlid
    Expect.equal
      (List.length testFnResult)
      1
      "handler should have a result for test_fn"
  }


let tests =
  testList
    "Analysis"
    [ testTraceIDsOfTlidsMatch
      testFilterSlash
      testRouteVariablesWorkWithStoredEvents
      testRouteVariablesWorkWithTraceInputsAndWildcards
      testStoredEventRoundtrip
      testTraceDataJsonFormatRedactsPasswords
      testFunctionTracesAreStored
      testErrorTracesAreStored ]
