module Tests.Traces

open System.Threading.Tasks
open FSharp.Control.Tasks

open Expecto

open Prelude
open Tablecloth
open TestUtils.TestUtils

open LibExecution.RuntimeTypes

module Traces = LibBackend.Traces
module Canvas = LibBackend.Canvas
module AT = LibExecution.AnalysisTypes
module PT = LibExecution.ProgramTypes
module TI = LibBackend.TraceInputs

let testTraceIDsOfTlidsMatch : Test =
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




let testFilterSlash : Test =
  testTask "test that a request which doesnt match doesnt end up in the traces" {
    let! owner = testOwner.Force()
    do! clearCanvasData owner (CanvasName.create "test-filter_slash")
    let route = "/:rest"
    let handler = testHttpRouteHandler route "GET" (PT.EBlank 0UL)
    let! meta = testCanvasInfo owner "test-filter_slash"
    let! (c : Canvas.T) = canvasForTLs meta [ PT.TLHandler handler ]

    let t1 = System.Guid.NewGuid()
    let desc = ("HTTP", "/", "GET")
    let! (_d : System.DateTime) = TI.storeEvent meta.id t1 desc (DStr "1")
    let! loaded = Traces.traceIDsForHandler c handler
    Expect.equal loaded [ Traces.traceIDofTLID handler.tlid ] "ids is the default"

    return ()
  }


let testRouteVariablesWorkWithStoredEvents : Test =
  testTask "route variables work with stored events" {

    let! owner = testOwner.Force()
    let canvasName = "test-route_variables_works"
    do! clearCanvasData owner (CanvasName.create canvasName)

    // set up test
    let httpRoute = "/some/:vars/:and/such"
    let handler = testHttpRouteHandler httpRoute "GET" (PT.EBlank 0UL)
    let! meta = testCanvasInfo owner canvasName
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
    let! owner = testOwner.Force()
    do! clearCanvasData owner (CanvasName.create canvasName)

    // note hyphen vs undeerscore
    let route = "/api/create_token"
    let requestPath = "/api/create-token"

    // set up test
    let handler = testHttpRouteHandler route "GET" (PT.EBlank 0UL)
    let! meta = testCanvasInfo owner canvasName
    let! (c : Canvas.T) = canvasForTLs meta [ PT.TLHandler handler ]

    // store an event and check it comes out
    let t1 = System.Guid.NewGuid()
    let desc = ("HTTP", requestPath, "GET")
    let! (_ : System.DateTime) = TI.storeEvent c.meta.id t1 desc (DStr "1")

    // check we get back the path for a route with a variable in it
    let! events = TI.loadEvents c.meta.id ("HTTP", route, "GET")

    Expect.equal [] events ""
  }

// FSTODO
// let t_stored_event_roundtrip () =
//   clear_test_data () ;
//   let owner : Uuidm.t =
//     Account.owner ~auth_domain:"test" |> fun x -> Option.value_exn x
//   in
//   let id1 = Serialize.fetch_canvas_id owner "host" in
//   let id2 = Serialize.fetch_canvas_id owner "host2" in
//   let t1 = Util.create_uuid () in
//   let t2 = Util.create_uuid () in
//   let t3 = Util.create_uuid () in
//   let t4 = Util.create_uuid () in
//   let t5 = Util.create_uuid () in
//   let t6 = Util.create_uuid () in
//   SE.clear_all_events ~canvas_id:id1 () ;
//   SE.clear_all_events ~canvas_id:id2 () ;
//   let desc1 = ("HTTP", "/path", "GET") in
//   let desc2 = ("HTTP", "/path2", "GET") in
//   let desc3 = ("HTTP", "/path", "POST") in
//   let desc4 = ("BG", "lol", "_") in
//   ignore
//     (SE.store_event
//        ~canvas_id:id1
//        ~trace_id:t1
//        desc1
//        (Dval.dstr_of_string_exn "1")) ;
//   ignore
//     (SE.store_event
//        ~canvas_id:id1
//        ~trace_id:t2
//        desc1
//        (Dval.dstr_of_string_exn "2")) ;
//   ignore
//     (SE.store_event
//        ~canvas_id:id1
//        ~trace_id:t3
//        desc3
//        (Dval.dstr_of_string_exn "3")) ;
//   ignore
//     (SE.store_event
//        ~canvas_id:id1
//        ~trace_id:t4
//        desc2
//        (Dval.dstr_of_string_exn "3")) ;
//   ignore
//     (SE.store_event
//        ~canvas_id:id2
//        ~trace_id:t5
//        desc2
//        (Dval.dstr_of_string_exn "3")) ;
//   ignore
//     (SE.store_event
//        ~canvas_id:id2
//        ~trace_id:t6
//        desc4
//        (Dval.dstr_of_string_exn "3")) ;
//   let at_trace_id = AT.of_pp Uuidm.pp_string in
//   let to_trace_id (t1, t2, t3, t4, t5) = t5 in
//   let listed = SE.list_events ~limit:`All ~canvas_id:id1 () in
//   AT.check
//     (AT.list at_trace_id)
//     "list host events"
//     (List.sort ~compare [t1; t3; t4])
//     (List.sort ~compare (List.map ~f:to_trace_id listed)) ;
//   let loaded =
//     SE.load_event_ids ~canvas_id:id2 desc4 |> List.map ~f:Tuple.T2.get1
//   in
//   AT.check
//     (AT.list at_trace_id)
//     "list desc events"
//     (List.sort ~compare [t6])
//     (List.sort ~compare loaded) ;
//   let loaded1 = SE.load_events ~canvas_id:id1 desc1 |> List.map ~f:t4_get4th in
//   check_dval_list
//     "load GET events"
//     [Dval.dstr_of_string_exn "2"; Dval.dstr_of_string_exn "1"]
//     loaded1 ;
//   let loaded2 = SE.load_events ~canvas_id:id1 desc3 |> List.map ~f:t4_get4th in
//   check_dval_list "load POST events" [Dval.dstr_of_string_exn "3"] loaded2 ;
//   let loaded3 = SE.load_events ~canvas_id:id2 desc3 |> List.map ~f:t4_get4th in
//   check_dval_list "load no host2 events" [] loaded3 ;
//   let loaded4 = SE.load_events ~canvas_id:id2 desc2 |> List.map ~f:t4_get4th in
//   check_dval_list "load host2 events" [Dval.dstr_of_string_exn "3"] loaded4 ;
//   ()
//
//
// let t_trace_data_json_format_redacts_passwords () =
//   let id = fid () in
//   let trace_data : Analysis_types.trace_data =
//     { input = [("event", DPassword (PasswordBytes.of_string "redactme1"))]
//     ; timestamp = Time.epoch
//     ; function_results =
//         [ ( "Password::hash"
//           , id
//           , "foobar"
//           , 0
//           , DPassword (PasswordBytes.of_string "redactme2") ) ] }
//   in
//   let expected : Analysis_types.trace_data =
//     { input = [("event", DPassword (PasswordBytes.of_string "Redacted"))]
//     ; timestamp = Time.epoch
//     ; function_results =
//         [ ( "Password::hash"
//           , id
//           , "foobar"
//           , 0
//           , DPassword (PasswordBytes.of_string "Redacted") ) ] }
//   in
//   trace_data
//   |> Analysis_types.trace_data_to_yojson
//   |> Analysis_types.trace_data_of_yojson
//   |> Result.ok_or_Exception.raiseInternal
//   |> AT.check
//        (AT.testable
//           Analysis_types.pp_trace_data
//           Analysis_types.equal_trace_data)
//        "trace_data round trip"
//        expected
//
//
// let t_function_traces_are_stored () =
//   clear_test_data () ;
//   let fntlid : tlid = id_of_int 12312345234 in
//   let f = user_fn "test_fn" [] (fn "DB::generateKey" []) in
//   let f = {f with tlid = fntlid} in
//   let h = handler (fn "test_fn" []) in
//   let host = "test" in
//   let owner = Account.for_host_exn host in
//   let canvas_id = Serialize.fetch_canvas_id owner host in
//   let trace_id = Util.create_uuid () in
//   let _ = execute_ops ~trace_id [fop f; hop h] in
//   (* get the trace for the execution *)
//   AT.check
//     AT.int
//     "handler should only have fn result for test_fn"
//     1
//     (Stored_function_result.load ~canvas_id ~trace_id h.tlid |> List.length) ;
//   AT.check
//     AT.int
//     "functions should only have fn result for DB::generateKey"
//     1
//     (Stored_function_result.load ~canvas_id ~trace_id fntlid |> List.length) ;
//   ()
//




let tests =
  testList
    "Analysis"
    [ testTraceIDsOfTlidsMatch
      testFilterSlash
      testRouteVariablesWorkWithStoredEvents
      testRouteVariablesWorkWithTraceInputsAndWildcards ]
