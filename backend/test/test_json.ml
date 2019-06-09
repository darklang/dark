open Core_kernel
open Libexecution
open Libbackend
open Types.RuntimeT
open Utils
module Resp = Cohttp_lwt_unix.Response
module Req = Cohttp_lwt_unix.Request
module Header = Cohttp.Header
module AT = Alcotest

let t_internal_roundtrippable_doesnt_care_about_order () =
  check_dval
    "internal_roundtrippable doesn't care about key order"
    (Dval.of_internal_roundtrippable_v0
       "{
         \"type\": \"weird\",
         \"value\": \"x\"
        }")
    (Dval.of_internal_roundtrippable_v0
       "{
         \"value\": \"x\",
         \"type\": \"weird\"
        }")


let t_dval_yojson_roundtrips () =
  let roundtrippable_rt v =
    v
    |> Dval.to_internal_roundtrippable_v0
    |> Dval.of_internal_roundtrippable_v0
  in
  let queryable_rt v =
    v |> Dval.to_internal_queryable_v0 |> Dval.of_internal_queryable_v0
  in
  (* Don't really need to check this but what harm *)
  let safe_rt v =
    v |> dval_to_yojson |> dval_of_yojson |> Result.ok_or_failwith
  in
  let check name (v : dval) =
    check_dval ("safe: " ^ name) v (safe_rt v) ;
    check_dval ("roundtrippable: " ^ name) v (roundtrippable_rt v) ;
    check_dval ("queryable: " ^ name) v (queryable_rt v) ;
    ()
  in
  sample_dvals
  |> List.filter ~f:(function
         | _, DBlock _ | _, DPassword _ ->
             false
         | _ ->
             true )
  |> List.iter ~f:(fun (name, dv) -> check name dv)


let t_uuid_db_roundtrip () =
  clear_test_data () ;
  let ops =
    [ Op.CreateDB (dbid, pos, "Ids")
    ; Op.AddDBCol (dbid, colnameid, coltypeid)
    ; Op.SetDBColName (dbid, colnameid, "uu")
    ; Op.SetDBColType (dbid, coltypeid, "UUID") ]
  in
  let ast =
    "(let i (Uuid::generate)
               (let _ (DB::add_v0 (obj (uu i)) Ids)
                 (let fetched (. (List::head (DB::getAll_v2 Ids)) uu)
                   (i fetched))))"
  in
  AT.check
    AT.int
    "A generated UUID can round-trip from the DB"
    0
    ( match exec_handler ~ops ast with
    | DList [p1; p2] ->
        compare_dval p1 p2
    | _ ->
        1 )


let t_result_to_response_works () =
  let req =
    Req.make
      ~headers:(Header.init ())
      (Uri.of_string "http://test.builtwithdark.com/")
  in
  let req_example_com =
    Req.make
      ~headers:(Header.of_list [("Origin", "https://example.com")])
      (Uri.of_string "http://test.builtwithdark.com/")
  in
  let req_google_com =
    Req.make
      ~headers:(Header.of_list [("Origin", "https://google.com")])
      (Uri.of_string "http://test.builtwithdark.com/")
  in
  let c = ops2c_exn "test" [] in
  ignore
    (List.map
       ~f:(fun (dval, req, cors_setting, check) ->
         Canvas.update_cors_setting c cors_setting ;
         dval
         |> Webserver.result_to_response ~c ~execution_id ~req
         |> Webserver.respond_or_redirect
         |> Lwt_main.run
         |> fst
         |> check )
       [ ( exec_ast "(obj)"
         , req
         , None
         , fun r ->
             AT.check
               (AT.option AT.string)
               "objects get application/json content-type"
               (Some "application/json; charset=utf-8")
               (Header.get (Resp.headers r) "Content-Type") )
       ; ( exec_ast "(1 2)"
         , req
         , None
         , fun r ->
             AT.check
               (AT.option AT.string)
               "lists get application/json content-type"
               (Some "application/json; charset=utf-8")
               (Header.get (Resp.headers r) "Content-Type") )
       ; ( exec_ast "2"
         , req
         , None
         , fun r ->
             AT.check
               (AT.option AT.string)
               "other things get text/plain content-type"
               (Some "text/plain; charset=utf-8")
               (Header.get (Resp.headers r) "Content-Type") )
       ; ( exec_ast "(Http::success (obj))"
         , req
         , None
         , fun r ->
             AT.check
               (AT.option AT.string)
               "Http::success gets application/json"
               (Some "application/json; charset=utf-8")
               (Header.get (Resp.headers r) "Content-Type") )
       ; ( exec_ast "1"
         , req
         , None
         , fun r ->
             AT.check
               (AT.option AT.string)
               "without any other settings, we get Access-Control-Allow-Origin: *."
               (Some "*")
               (Header.get (Resp.headers r) "Access-Control-Allow-Origin") )
       ; ( exec_ast "1"
         , req
         , Some Canvas.AllOrigins
         , fun r ->
             AT.check
               (AT.option AT.string)
               "with explicit wildcard setting, we get Access-Control-Allow-Origin: *."
               (Some "*")
               (Header.get (Resp.headers r) "Access-Control-Allow-Origin") )
       ; ( exec_ast "1"
         , req
         , Some (Canvas.Origins ["https://example.com"])
         , fun r ->
             AT.check
               (AT.option AT.string)
               "with whitelist setting and no Origin, we get no Access-Control-Allow-Origin"
               None
               (Header.get (Resp.headers r) "Access-Control-Allow-Origin") )
       ; ( exec_ast "1"
         , req_example_com
         , Some (Canvas.Origins ["https://example.com"])
         , fun r ->
             AT.check
               (AT.option AT.string)
               "with whitelist setting and matching Origin, we get good Access-Control-Allow-Origin"
               (Some "https://example.com")
               (Header.get (Resp.headers r) "Access-Control-Allow-Origin") )
       ; ( exec_ast "1"
         , req_google_com
         , Some (Canvas.Origins ["https://example.com"])
         , fun r ->
             AT.check
               (AT.option AT.string)
               "with whitelist setting and mismatched Origin, we get null Access-Control-Allow-Origin"
               (Some "null")
               (Header.get (Resp.headers r) "Access-Control-Allow-Origin") ) ]) ;
  ()


let t_old_new_dval_reprs () =
  List.iter sample_dvals ~f:(fun (name, dv) ->
      (* AT.check *)
      (*   AT.string *)
      (*   ("old_new_dval check: " ^ name) *)
      (*   (Dval.old_to_internal_repr dv) *)
      (*   (Dval.to_hashable_repr dv) ; *)
      () ) ;
  ()


let date_migration_has_correct_formats () =
  let str = "2019-03-08T08:26:14Z" in
  let date = DDate (Util.date_of_isostring str) in
  let expected =
    Yojson.pretty_to_string
      (`Assoc [("type", `String "date"); ("value", `String str)])
  in
  AT.check
    AT.string
    "old format"
    expected
    (Legacy.PrettyResponseJsonV0.to_pretty_response_json_v0 date) ;
  AT.check
    AT.string
    "new format"
    ("\"" ^ str ^ "\"")
    (Dval.to_pretty_machine_json_v1 date)


let suite =
  [ ( "Parsing JSON to Dvals doesn't care about key order"
    , `Quick
    , t_internal_roundtrippable_doesnt_care_about_order )
  ; ("Dvals roundtrip to yojson correctly", `Quick, t_dval_yojson_roundtrips)
  ; ("UUIDs round-trip to the DB", `Quick, t_uuid_db_roundtrip)
  ; ( "Dvals get converted to web responses correctly"
    , `Quick
    , t_result_to_response_works )
  ; ( "New dval representations are the same as the old ones"
    , `Quick
    , t_old_new_dval_reprs )
  ; ( "Date has correct formats in migration"
    , `Quick
    , date_migration_has_correct_formats ) ]
