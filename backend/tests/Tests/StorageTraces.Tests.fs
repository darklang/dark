/// Tests for the cloud storage version of traces
module Tests.StorageTraces

// We duplicate existing tests and translate them for the cloud storage version. We
// can safely delete Traces.Tests when this is done.

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Tablecloth
open TestUtils.TestUtils

open LibExecution.RuntimeTypes

module Shortcuts = TestUtils.RTShortcuts

module Canvas = LibBackend.Canvas
module AT = LibExecution.AnalysisTypes
module PT = LibExecution.ProgramTypes
module RT = LibExecution.RuntimeTypes
module RealExecution = LibRealExecution.RealExecution
module Tracing = LibBackend.Tracing
module TSR = Tracing.TraceSamplingRule
module TCS = LibBackend.TraceCloudStorage


// let testFilterSlash =
//   testTask "test that a request which doesnt match doesnt end up in the traces" {
//     // set up handler with route param
//     let! meta = initializeTestCanvas "test-filter_slash"
//     let route = "/:rest"
//     let handler = testHttpRouteHandler route "GET" (PT.EBlank 0UL)
//     let! (c : Canvas.T) = canvasForTLs meta [ PT.Toplevel.TLHandler handler ]

//     // make irrelevant request
//     let t1 = AT.TraceID.create()
//     let desc = ("HTTP", "/", "GET")
//     let! (_d : NodaTime.Instant) = TI.storeEvent canvasID t1 desc (DString "1")

//     // load+check irrelevant trace
//     let! loaded = Traces.traceIDsForHttpHandler c handler
//     Expect.equal loaded [ Traces.traceIDofTLID handler.tlid ] "ids is the default"

//     return ()
//   }


// let testRouteVariablesWorkWithStoredEvents =
//   testTask "route variables work with stored events" {
//     let! meta = initializeTestCanvas "route_variables_works"

//     // set up handler
//     let httpRoute = "/some/:vars/:and/such"
//     let handler = testHttpRouteHandler httpRoute "GET" (PT.EBlank 0UL)
//     let! (c : Canvas.T) = canvasForTLs meta [ PT.Toplevel.TLHandler handler ]

//     // store an event that matches the handler
//     let t1 = AT.TraceID.create()
//     let httpRequestPath = "/some/vars/and/such"
//     let desc = ("HTTP", httpRequestPath, "GET")
//     let! (_ : NodaTime.Instant) = TI.storeEvent c.id t1 desc (DString "1")

//     // check we get back the data we put into it
//     let! events = TI.loadEvents c.id ("HTTP", httpRoute, "GET")

//     let (loadedPaths, loadedDvals) =
//       events
//       |> List.map (fun (loadedPath, _, _, loadedDval) -> (loadedPath, loadedDval))
//       |> List.unzip

//     Expect.equal loadedPaths [ httpRequestPath ] "path is the same"
//     Expect.equal loadedDvals [ (DString "1") ] "data is the same"

//     // check that the event is not in the 404s
//     let! f404s = TI.getRecent404s c.id
//     Expect.equal [] f404s "no 404s"
//   }


// let testRouteVariablesWorkWithTraceInputsAndWildcards =
//   testTask "route variables work with trace inputs and wildcards" {
//     let! meta =
//       initializeTestCanvas "route_variables_works_with_withcards"

//     // '_' is the "wildcard" here, and the '-' matches the wildcard.
//     // '-' could equally well be '!' or 'Z' or '🇨🇭' or "-matcheswildcard-"
//     let route = "/api/create_token"
//     let requestPath = "/api/create-token"

//     // set up handler
//     let handler = testHttpRouteHandler route "GET" (PT.EBlank 0UL)
//     let! (c : Canvas.T) = canvasForTLs meta [ PT.Toplevel.TLHandler handler ]

//     // store an event
//     let t1 = AT.TraceID.create()
//     let desc = ("HTTP", requestPath, "GET")
//     let! (_ : NodaTime.Instant) = TI.storeEvent c.id t1 desc (DString "1")

//     // check we get back the path for a route with a variable in it
//     let! events = TI.loadEvents c.id ("HTTP", route, "GET")

//     Expect.equal [] events ""
//   }

let testTraceRoundtrip =
  testTask "test stored events can be roundtripped" {
    let! (c1 : CanvasID) = initializeTestCanvas "stored_events_can_be_roundtripped1"
    let! (c2 : CanvasID) = initializeTestCanvas "stored_events_can_be_roundtripped2"

    let t1 = AT.TraceID.create ()
    do! Task.Delay(2) // make sure of ordering with t1 and t2
    let t2 = AT.TraceID.create ()
    let t3 = AT.TraceID.create ()
    let t4 = AT.TraceID.create ()
    let t5 = AT.TraceID.create ()
    let t6 = AT.TraceID.create ()

    let tlid1 = 6UL
    let tlid2 = 7UL
    let tlid3 = 8UL
    let tlid4 = 9UL
    let functionResults = Dictionary.empty ()
    do!
      TCS.storeToCloudStorage
        c1
        tlid1
        t1
        [ tlid1 ]
        [ "request", DString "1" ]
        functionResults
    do!
      TCS.storeToCloudStorage
        c1
        tlid1
        t2
        [ tlid1 ]
        [ "request", DString "2" ]
        functionResults
    do!
      TCS.storeToCloudStorage
        c1
        tlid3
        t3
        [ tlid3 ]
        [ "request", DString "3" ]
        functionResults
    do!
      TCS.storeToCloudStorage
        c1
        tlid2
        t4
        [ tlid2 ]
        [ "request", DString "3" ]
        functionResults
    do!
      TCS.storeToCloudStorage
        c2
        tlid2
        t5
        [ tlid2 ]
        [ "request", DString "3" ]
        functionResults
    do!
      TCS.storeToCloudStorage
        c2
        tlid4
        t6
        [ tlid4 ]
        [ "request", DString "3" ]
        functionResults

    let! actual = TCS.Test.listAllTraceIDs c1 |> Task.map List.sort
    Expect.equal actual (List.sort [ t1; t2; t3; t4 ]) "list canvas events"

    let! loaded = TCS.listMostRecentTraceIDsForTLIDs c2 [ tlid4 ]
    Expect.equal (List.sort loaded) (List.sort [ tlid4, t6 ]) "list desc events"

    let fetchRequestsFor (cid : CanvasID) (tlid : tlid) : Task<List<RT.Dval>> =
      task {
        let! traces = TCS.listMostRecentTraceIDsForTLIDs cid [ tlid ]
        let! traceData =
          traces
          |> List.map Tuple2.second
          |> Task.mapInParallel (TCS.getTraceData cid tlid)
        return
          traceData
          |> List.map (fun ((_, traceData) : AT.Trace) -> traceData.input)
          |> List.flatten
          |> List.map Tuple2.second
      }

    let! loaded1 = fetchRequestsFor c1 tlid1
    Expect.equal loaded1 [ DString "2"; DString "1" ] "load GET events"

    let! loaded2 = fetchRequestsFor c1 tlid3
    Expect.equal loaded2 [ DString "3" ] "load POST events"

    let! loaded3 = fetchRequestsFor c2 tlid3
    Expect.equal loaded3 [] "load no host2 events"

    let! loaded4 = fetchRequestsFor c2 tlid2
    Expect.equal loaded4 [ DString "3" ] "load host2 events"
  }


// let testTraceDataJsonFormatRedactsPasswords =
//   testTask "trace data json format redacts passwords" {
//     // set up
//     let id = gid ()

//     let traceData : AT.TraceData =
//       { input = [ ("event", DPassword(Password(UTF8.toBytes "redactme1"))) ]
//         timestamp = NodaTime.Instant.UnixEpoch
//         function_results =
//           [ ("Password.hash",
//              id,
//              "foobar",
//              0,
//              DPassword(Password(UTF8.toBytes "redactme2"))) ] }

//     let expected : AT.TraceData =
//       { input = [ ("event", DPassword(Password(UTF8.toBytes "Redacted"))) ]
//         timestamp = NodaTime.Instant.UnixEpoch
//         function_results =
//           [ ("Password.hash",
//              id,
//              "foobar",
//              0,
//              DPassword(Password(UTF8.toBytes "Redacted"))) ] }

//     // roundtrip serialization
//     let actual =
//       traceData
//       |> Json.OCamlCompatible.serialize
//       |> Json.OCamlCompatible.deserialize<AT.TraceData>

//     // check
//     Expect.equal actual expected "traceData round trip"
//   }


// let testFunctionTracesAreStored =
//   testTask "function traces are stored" {
//     // set up canvas, user fn
//     let! (canvasID : CanvasID) =
//       initializeTestCanvas "test-function-traces-are-stored"

//     let (userFn : RT.UserFunction.T) =
//       { tlid = 12312345234UL
//         name = "test_fn"
//         parameters = []
//         returnType = RT.TInt
//         description = ""
//         infix = false
//         body = Parser.RuntimeTypes.parseExpr "DB.generateKey" }

//     let program =
//       { canvasID = canvasID
//         dbs = Map.empty
//         userFns = Map.singleton userFn.name userFn
//         userTypes = Map.empty
//         secrets = [] }

//     let callerTLID = 98765UL
//     let callerID = 1234UL
//     let fnName = RT.FQFnName.User "test_fn"
//     let args = []
//     let traceID = AT.TraceID.create()

//     // call the user fn, which should result in a trace being stored
//     let! (_, _) =
//       RealExecution.reexecuteFunction
//         meta
//         program
//         callerTLID
//         callerID
//         traceID
//         fnName
//         args

//     // check for traces - they're saved in the background so wait for them
//     let rec getValue (count : int) =
//       task {
//         let! result = TFR.load canvasID traceID callerTLID
//         if result = [] && count < 10 then
//           do! Task.Delay 1000
//           return! getValue (count + 1)
//         else
//           return result
//       }

//     let! testFnResult = getValue 0
//     Expect.equal
//       (List.length testFnResult)
//       1
//       "one function was called by the 'caller'"

//     let! dbGenerateResult = TFR.load canvasID traceID userFn.tlid
//     Expect.equal
//       (List.length dbGenerateResult)
//       1
//       "test_fn called one function (DB.generateKey)"
//   }

// let testErrorTracesAreStored =
//   testTask "error traces are stored" {
//     // set up canvas, user fn
//     let! (canvasID : CanvasID) =
//       initializeTestCanvas "test-error-traces-are-stored"

//     let (db : DB.T) = { tlid = gid (); name = "MyDB"; cols = []; version = 0 }

//     let program =
//       { canvasID = canvasID
//         dbs = Map [ "MyDB", db ]
//         userFns = Map.empty
//         userTypes = Map.empty
//         secrets = [] }

//     // call the user fn, which should result in a trace being stored
//     let traceID = AT.TraceID.create()

//     let tracer = Tracing.createStandardTracer canvasID traceID
//     let! state =
//       RealExecution.createState traceID (gid ()) program tracer.executionTracing

//     // the DB has no columns, but the code expects one, causing it to fail
//     let code = "DB.set_v1 { a = \"y\" } \"key\" MyDB"

//     let (ast : Expr) = Parser.RuntimeTypes.parseExpr code

//     let! (_ : Dval) = LibExecution.Execution.executeExpr state Map.empty ast

//     do! Tracing.Test.saveTraceResult canvasID traceID tracer.results

//     // check for traces
//     let! testFnResult = TFR.load canvasID traceID state.tlid
//     Expect.equal
//       (List.length testFnResult)
//       1
//       "handler should have a result for test_fn"
//   }

// let testLaunchdarklyParsingCode =
//   testMany
//     "test launchdarkly trace sampling rule parser"
//     TSR.parseRule
//     [ "sample-none", Ok(TSR.SampleNone)
//       "sample-all", Ok(TSR.SampleAll)
//       "sample-all-with-telemetry", Ok(TSR.SampleAllWithTelemetry)
//       "sample-one-in-10", Ok(TSR.SampleOneIn 10)
//       "sample-one-in-50", Ok(TSR.SampleOneIn 50)
//       "sample-one-in-1000000000", Ok(TSR.SampleOneIn 1000000000)
//       "sample-one-in-gibberish", Error "Exception thrown"
//       "gibberish", Error "Invalid sample" ]


let tests =
  testList
    "tracing-storage"
    [
      // testFilterSlash
      // testRouteVariablesWorkWithStoredEvents
      // testRouteVariablesWorkWithTraceInputsAndWildcards
      testTraceRoundtrip
      // testTraceDataJsonFormatRedactsPasswords
      // testFunctionTracesAreStored
      // testErrorTracesAreStored
      // testLaunchdarklyParsingCode
      ]
