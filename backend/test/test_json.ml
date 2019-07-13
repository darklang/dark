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
         \"type\": \"option\",
         \"value\": 5
        }")
    (Dval.of_internal_roundtrippable_v0
       "{
         \"value\": 5,
         \"type\": \"option\"
        }")


let t_dval_yojson_roundtrips () =
  let checks =
    [ ( "roundtrippable"
      , Dval.to_internal_roundtrippable_v0
      , Dval.of_internal_roundtrippable_v0 )
    ; ( "safe"
      , (fun v -> v |> dval_to_yojson |> Yojson.Safe.to_string)
      , fun v ->
          v
          |> Yojson.Safe.from_string
          |> dval_of_yojson
          |> Result.ok_or_failwith ) ]
  in
  let check name (v : dval) =
    List.iter
      checks
      ~f:(fun (test_name, (encode : dval -> string), (decode : string -> dval))
         ->
        check_dval (test_name ^ ": " ^ name) v (v |> encode |> decode) ;
        AT.check
          AT.string
          (test_name ^ " as string: " ^ name)
          (v |> encode)
          (v |> encode |> decode |> encode) ;
        () )
  in
  sample_dvals
  |> List.filter ~f:(function
         | _, DBlock _ | _, DPassword _ ->
             false
         | _ ->
             true )
  |> List.iter ~f:(fun (name, dv) -> check name dv)


let t_dval_user_db_json_roundtrips () =
  let queryable_rt v =
    v |> Dval.to_internal_queryable_v1 |> Dval.of_internal_queryable_v1
  in
  let check name (v : dval) =
    check_dval ("queryable: " ^ name) v (queryable_rt v) ;
    ()
  in
  let dvals =
    [ ( "looks like an option but isn't"
      , Dval.to_dobj_exn
          [("type", Dval.dstr_of_string_exn "option"); ("value", Dval.dint 5)]
      ) ]
  in
  List.iter dvals ~f:(fun (name, dv) -> check name dv)


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


let t_password_json_round_trip_forwards () =
  let password = DPassword (Bytes.of_string "x") in
  check_dval
    "Passwords serialize and deserialize if there's no redaction."
    password
    ( password
    |> Dval.to_internal_roundtrippable_v0
    |> Dval.of_internal_roundtrippable_v0
    |> Dval.to_internal_roundtrippable_v0
    |> Dval.of_internal_roundtrippable_v0 )


let t_password_serialization () =
  let does_serialize name expected f =
    let bytes = Bytes.of_string "encryptedbytes" in
    let password = DPassword bytes in
    AT.check
      AT.bool
      ("Passwords serialize in non-redaction function: " ^ name)
      expected
      (String.is_substring
         ~substring:(B64.encode "encryptedbytes")
         (f password))
  in
  let roundtrips name serialize deserialize =
    let bytes = Bytes.of_string "encryptedbytes" in
    let password = DPassword bytes in
    AT.check
      at_dval
      ("Passwords serialize in non-redaction function: " ^ name)
      password
      (password |> serialize |> deserialize |> serialize |> deserialize)
  in
  (* doesn't redact *)
  does_serialize
    "to_internal_roundtrippable_v0"
    true
    Dval.to_internal_roundtrippable_v0 ;
  does_serialize "to_internal_queryable_v0" true Dval.to_internal_queryable_v0 ;
  (* roundtrips *)
  roundtrips
    "to_internal_roundtrippable_v0"
    Dval.to_internal_roundtrippable_v0
    Dval.of_internal_roundtrippable_v0 ;
  roundtrips
    "to_internal_queryable_v0"
    Dval.to_internal_queryable_v0
    Dval.of_internal_queryable_v0 ;
  (* redacting *)
  does_serialize
    "to_enduser_readable_text_v0"
    false
    Dval.to_enduser_readable_text_v0 ;
  does_serialize
    "to_enduser_readable_html_v0"
    false
    Dval.to_enduser_readable_html_v0 ;
  does_serialize "to_developer_repr_v0" false Dval.to_developer_repr_v0 ;
  does_serialize
    "to_pretty_machine_json_v1"
    false
    Dval.to_pretty_machine_json_v1 ;
  does_serialize
    "to_pretty_request_json_v0"
    false
    Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0 ;
  does_serialize
    "to_pretty_response_json_v1"
    false
    Legacy.PrettyResponseJsonV0.to_pretty_response_json_v0 ;
  ()


let suite =
  [ ( "Parsing JSON to Dvals doesn't care about key order"
    , `Quick
    , t_internal_roundtrippable_doesnt_care_about_order )
  ; ("Dvals roundtrip to yojson correctly", `Quick, t_dval_yojson_roundtrips)
  ; ( "UserDB values roundtrip to yojson correctly"
    , `Quick
    , t_dval_user_db_json_roundtrips )
  ; ( "Dvals get converted to web responses correctly"
    , `Quick
    , t_result_to_response_works )
  ; ( "Date has correct formats in migration"
    , `Quick
    , date_migration_has_correct_formats )
  ; ( "Passwords serialize correctly and redact (or not) correctly"
    , `Quick
    , t_password_serialization )
  ; ( "Passwords serialize when not redacting"
    , `Quick
    , t_password_json_round_trip_forwards ) ]
