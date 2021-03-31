module Tests.Analysis

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Tablecloth
open TestUtils

open LibExecution.RuntimeTypes
open LibExecution.Shortcuts

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
    let route = ("HTTP", httpRoute, "GET")
    let handler = testHttpRouteHandler httpRoute "GET" (PT.EBlank 0UL)
    let! meta = testCanvasInfo canvasName
    let! (c : Canvas.T) = canvasForTLs meta [ PT.TLHandler handler ]

    // store an event and check it comes out
    let t1 = System.Guid.NewGuid()
    let httpRequestPath = "/some/vars/and/such"
    let data = (DStr "1")
    let desc = ("HTTP", httpRequestPath, "GET")
    let! (_ : System.DateTime) = TI.storeEvent c.meta.id t1 desc data

    // check we get back the data we put into it
    let! events = TI.loadEvents c.meta.id route

    let (loadedPaths, loadedDvals) =
      events
      |> List.map (fun (loadedPath, _, _, loadedDval) -> (loadedPath, loadedDval))
      |> List.unzip

    Expect.equal loadedPaths [ httpRequestPath ] "path is the same"
    Expect.equal loadedDvals [ data ] "data is the same"

    // check that the event is not in the 404s
    let! f404s = TI.getRecent404s c.meta.id
    Expect.equal [] f404s "no 404s"
  }


// let t_route_variables_work_with_stored_events_and_wildcards () =
//   (* set up test *)
//   clear_test_data () ;
//   let host = "test-route_variables_works_with_wildcards" in
//   let route = "/api/create_token" in
//   let request_path = "/api/create-token" in
//   (* note hyphen vs undeerscore *)
//   let oplist = [SetHandler (tlid, pos, http_route_handler ~route ())] in
//   let c = ops2c_exn host oplist in
//   Canvas.serialize_only [tlid] !c ;
//   let t1 = Util.create_uuid () in
//   let desc = ("HTTP", request_path, "GET") in
//   let route = ("HTTP", route, "GET") in
//   (* store an event and check it comes out *)
//   ignore
//     (SE.store_event
//        ~canvas_id:!c.id
//        ~trace_id:t1
//        desc
//        (Dval.dstr_of_string_exn "1")) ;
//   (* check we get back the path for a route with a variable in it *)
//   let loaded1 = SE.load_events ~canvas_id:!c.id route in
//   check_dval_list "load GET events" [] (loaded1 |> List.map ~f:t4_get4th) ;
//   ()
//


let tests =
  testList "Analysis" [ testFilterSlash; testRouteVariablesWorkWithStoredEvents ]
