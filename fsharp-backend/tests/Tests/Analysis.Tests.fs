module Tests.Analysis

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Tablecloth
open TestUtils

open LibExecution.RuntimeTypes

module Analysis = LibBackend.Analysis
module Canvas = LibBackend.Canvas
module AT = LibExecution.AnalysisTypes
module PT = LibBackend.ProgramTypes
module TI = LibBackend.TraceInputs
module S = PT.Shortcuts


let testFilterSlash : Test =
  testTask "test that a request which doesnt match doesnt end up in the traces" {
    do! clearCanvasData (CanvasName.create "test-filter_slash")
    let route = "/:rest"
    let handler = testHttpRouteHandler route "GET" (PT.EBlank 0UL)
    let! meta = testCanvasInfo "test-filter_slash"
    let! (c : Canvas.T) = canvasForTLs meta [ PT.TLHandler handler ]

    let t1 = System.Guid.NewGuid()
    let desc = ("HTTP", "/", "GET")
    let! (_d : System.DateTime) = TI.storeEvent meta.id t1 desc (DStr "1")
    let! loaded = Analysis.traceIDsForHandler c handler
    Expect.equal loaded [ Analysis.traceIDofTLID handler.tlid ] "ids is the default"

    return ()
  }


let testRouteVariablesWorkWithStoredEvents : Test =
  testTask "route variables work with stored events" {

    let canvasName = "test-route_variables_works"
    do! clearCanvasData (CanvasName.create canvasName)

    // set up test
    let httpRoute = "/some/:vars/:and/such"
    let handler = testHttpRouteHandler httpRoute "GET" (PT.EBlank 0UL)
    let! meta = testCanvasInfo canvasName
    let! (c : Canvas.T) = canvasForTLs meta [ PT.TLHandler handler ]

    // store an event and check it comes out
    let t1 = System.Guid.NewGuid()
    let httpRequestPath = "/some/vars/and/such"
    let desc = ("HTTP", httpRequestPath, "GET")
    let! (_ : System.DateTime) = TI.storeEvent c.meta.id t1 desc (DStr "1")

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


let testRouteVariablesWorkWithTraceInputsAndWildcards : Test =
  testTask "route variables work with trace inputs and wildcards" {
    let canvasName = "test-route_variables_works_with_withcards"
    do! clearCanvasData (CanvasName.create canvasName)

    // note hyphen vs undeerscore
    let route = "/api/create_token"
    let requestPath = "/api/create-token"

    // set up test
    let handler = testHttpRouteHandler route "GET" (PT.EBlank 0UL)
    let! meta = testCanvasInfo canvasName
    let! (c : Canvas.T) = canvasForTLs meta [ PT.TLHandler handler ]

    // store an event and check it comes out
    let t1 = System.Guid.NewGuid()
    let desc = ("HTTP", requestPath, "GET")
    let! (_ : System.DateTime) = TI.storeEvent c.meta.id t1 desc (DStr "1")

    // check we get back the path for a route with a variable in it
    let! events = TI.loadEvents c.meta.id ("HTTP", route, "GET")

    Expect.equal [] events ""
  }



let tests =
  testList
    "Analysis"
    [ testFilterSlash
      testRouteVariablesWorkWithStoredEvents
      testRouteVariablesWorkWithTraceInputsAndWildcards ]
