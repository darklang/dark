open Core_kernel
open Libcommon
open Libexecution
open Libbackend
open Types
open Types.RuntimeT
open Ast
open Lwt
open Utils
module Resp = Cohttp_lwt_unix.Response
module Req = Cohttp_lwt_unix.Request
module Header = Cohttp.Header
module Code = Cohttp.Code
module C = Canvas
module RT = Runtime
module TL = Toplevel
module AT = Alcotest

let t_should_use_https () =
  AT.check
    (AT.list AT.bool)
    "should_use_https works"
    (List.map
       ~f:(fun x -> Webserver.should_use_https (Uri.of_string x))
       [ "http://builtwithdark.com"
       ; "http://test.builtwithdark.com"
       ; "http://localhost"
       ; "http://test.localhost" ])
    [true; true; false; false]


let t_redirect_to () =
  AT.check
    (AT.list (AT.option AT.string))
    "redirect_to works"
    (List.map
       ~f:(fun x ->
         x
         |> Uri.of_string
         |> Webserver.redirect_to
         |> Option.map ~f:Uri.to_string )
       [ "http://example.com"
       ; "http://builtwithdark.com"
       ; "https://builtwithdark.com"
       ; "http://test.builtwithdark.com"
       ; "https://test.builtwithdark.com"
       ; "http://test.builtwithdark.com/x/y?z=a" ])
    [ None
    ; Some "https://builtwithdark.com"
    ; None
    ; Some "https://test.builtwithdark.com"
    ; None
    ; Some "https://test.builtwithdark.com/x/y?z=a" ]


let t_bad_ssl_cert _ =
  check_error_contains
    "should get bad_ssl"
    (exec_ast "(HttpClient::get 'https://self-signed.badssl.com' {} {} {})")
    "Bad HTTP request: Peer certificate cannot be authenticated with given CA certificates"


let t_sanitize_uri_path_with_repeated_slashes () =
  AT.check
    AT.string
    "/foo//bar->/foo/bar"
    (Webserver.sanitize_uri_path "/foo//bar")
    "/foo/bar"


let t_sanitize_uri_path_with_trailing_slash () =
  AT.check AT.string "/foo/->/foo" (Webserver.sanitize_uri_path "/foo/") "/foo"


let t_sanitize_uri_path_with_root_noops () =
  AT.check AT.string "/->/" (Webserver.sanitize_uri_path "/") "/"


let t_sanitize_uri_path_with_repeated_root () =
  AT.check AT.string "//->/" (Webserver.sanitize_uri_path "//") "/"


let t_route_variables_work () =
  AT.check
    (AT.list AT.string)
    "Variables are as expected"
    ["userid"; "cardid"]
    (Http.route_variables "/user/:userid/card/:cardid") ;
  AT.check
    (AT.list (AT.pair AT.string at_dval))
    "Variables are bound as expected"
    [ ("cardid", Dval.dstr_of_string_exn "0")
    ; ("userid", Dval.dstr_of_string_exn "myid") ]
    (Http.bind_route_variables_exn
       "/user/myid/card/0"
       ~route:"/user/:userid/card/:cardid") ;
  AT.check
    AT.bool
    "Path matches the route"
    true
    (Http.request_path_matches_route
       "/user/myid/card/0"
       ~route:"/user/:userid/card/:cardid") ;
  AT.check
    AT.bool
    "Path doesnt match erroneously"
    false
    (Http.request_path_matches_route
       "/api/create-token"
       ~route:"/api/create_token")


let t_concrete_over_wild () =
  let wild = http_route_handler ~route:"/:foo" () in
  let concrete = http_route_handler ~tlid:tlid2 ~route:"/a" () in
  let ordered =
    Http.filter_matching_handlers_by_specificity [concrete; wild]
  in
  AT.check (AT.list testable_handler) "concrete over wild" [concrete] ordered


let t_wild_over_nothing () =
  let wild = http_route_handler ~route:"/a/:foo" () in
  let nothing = http_route_handler ~tlid:tlid2 ~route:"/a" () in
  let ordered = Http.filter_matching_handlers_by_specificity [wild; nothing] in
  AT.check (AT.list testable_handler) "wild over nothing" [wild] ordered


let t_differing_wildcards () =
  let single = http_route_handler ~route:"/:first" () in
  let double = http_route_handler ~tlid:tlid2 ~route:"/:first/:second" () in
  let ordered =
    Http.filter_matching_handlers_by_specificity [single; double]
  in
  AT.check (AT.list testable_handler) "differing wildcards" [double] ordered


let t_lengthy_abcdef_wildcard () =
  let more = http_route_handler ~route:"/:a/b/c/d/:e/:f" () in
  let earlier = http_route_handler ~tlid:tlid2 ~route:"/:a/b/c/:d/e/f" () in
  let ordered = Http.filter_matching_handlers_by_specificity [more; earlier] in
  AT.check (AT.list testable_handler) "lengthy abcdef wildcard" [more] ordered


let t_same_length_abc_diff_wildcards () =
  let a = http_route_handler ~route:"/a/:b/:c" () in
  let b = http_route_handler ~tlid:tlid2 ~route:"/:a/b/c" () in
  let ordered = Http.filter_matching_handlers_by_specificity [a; b] in
  AT.check
    (AT.list testable_handler)
    "same length abc route with diff # wildcards"
    [a]
    ordered


let t_same_length_abc_same_wildcards () =
  let a = http_route_handler ~route:"/:a/b/c" () in
  let b = http_route_handler ~tlid:tlid2 ~route:"/a/:b/c" () in
  let c = http_route_handler ~tlid:tlid3 ~route:"/a/b/:c" () in
  let ordered = Http.filter_matching_handlers_by_specificity [a; b; c] in
  AT.check
    (AT.list testable_handler)
    "same length abc routes with same # wildcards"
    [c]
    ordered


(* note this test depends on the current reverse ordering, even though there's
 * no reason to guarantee the reversal for routes of the same specificity. *)
let t_same_specificity_are_returned () =
  let single = http_route_handler ~route:"/:first" () in
  let double = http_route_handler ~tlid:tlid2 ~route:"/:first/:second" () in
  let double2 = http_route_handler ~tlid:tlid3 ~route:"/:foo/:bar" () in
  let ordered =
    Http.filter_matching_handlers_by_specificity [single; double; double2]
  in
  AT.check
    (AT.list testable_handler)
    "multiple specificity are returned"
    [double2; double]
    ordered


let t_mismatch_is_filtered () =
  let single = http_route_handler ~route:"/:first" () in
  let filtered = Http.filter_invalid_handler_matches ~path:"/" [single] in
  AT.check (AT.list testable_handler) "mismatch is filtered out" [] filtered


let t_mismatch_filtering_leaves_root () =
  let single = http_route_handler ~route:"/:first" () in
  let root = http_route_handler ~tlid:tlid2 ~route:"/" () in
  let filtered =
    Http.filter_invalid_handler_matches ~path:"/" [single; root]
  in
  AT.check
    (AT.list testable_handler)
    "mismatch is filtered out but root is left"
    [root]
    filtered


let t_route_equals_path () =
  let route = "/a/:b/c" in
  let path = "/a/pickmeup/c" in
  let bound = Http.bind_route_variables ~route path in
  AT.check
    (AT.option (AT.list testable_string_dval_pair))
    "route binds to path when they're same length"
    (Some [("b", Dval.dstr_of_string_exn "pickmeup")])
    bound


let t_route_lt_path_with_wildcard () =
  let route = "/a/:b" in
  let path = "/a/pickmeup/c/d" in
  let bound = Http.bind_route_variables ~route path in
  AT.check
    (AT.option (AT.list testable_string_dval_pair))
    "len(route) < len(path) with a trailing wildcard should succeed in binding all of the remaining path bits"
    (Some [("b", Dval.dstr_of_string_exn "pickmeup/c/d")])
    bound


let t_route_lt_path_without_wildcard () =
  let route = "/:a/b" in
  let path = "/a/b/c" in
  let bound = Http.bind_route_variables ~route path in
  AT.check
    (AT.option (AT.list testable_string_dval_pair))
    "len(route) < len(path) without trailing wildcards should fail binding"
    None
    bound


let t_route_gt_path () =
  let route = "/a/b/c/d" in
  let path = "/a/b/c" in
  let bound = Http.bind_route_variables ~route path in
  AT.check
    (AT.option (AT.list testable_string_dval_pair))
    "len(route) > len(path) should fail binding"
    None
    bound


let t_route_eq_path_mismatch_concrete () =
  let route = "/a/:b/c/d" in
  let path = "/a/b/c/e" in
  let bound = Http.bind_route_variables ~route path in
  AT.check
    (AT.option (AT.list testable_string_dval_pair))
    "binding fails due to mismatch in concrete elems"
    None
    bound


let t_head_and_get_requests_are_coalesced () =
  let test_name = "head-and-get-requests-are-coalsced" in
  let setup_canvas () =
    let n1 = hop (http_handler (ast_for "'test_body'")) in
    let canvas = ops2c_exn ("test-" ^ test_name) [n1] in
    Log.infO "canvas account" ~params:[("_", !canvas |> C.show_canvas)] ;
    C.save_all !canvas ;
    canvas
  in
  let respond_to_head_from_get (req : Req.t) : int * (int * string) =
    Lwt_main.run
      (let%lwt () = Nocrypto_entropy_lwt.initialize () in
       let test_id = Types.id_of_int 1234 in
       let canvas = setup_canvas () in
       let%lwt resp, body =
         Webserver.canvas_handler
           ~execution_id:test_id
           ~canvas:!canvas.host
           ~ip:""
           ~uri:(req |> Req.uri)
           ~body:""
           req
       in
       let code = resp |> Resp.status |> Code.code_of_status in
       let body_string = Cohttp_lwt__.Body.to_string body |> Lwt_main.run in
       resp
       |> Resp.headers
       |> (fun headers ->
            match Header.get headers "Content-Length" with
            | None ->
                0
            | Some h ->
                int_of_string h )
       |> fun content_length -> return (code, (content_length, body_string)))
  in
  let expected_body = "\"test_body\"" in
  let expected_content_length = String.length expected_body in
  AT.check
    (AT.list (AT.pair AT.int (AT.pair AT.int AT.string)))
    "canvas_handler returns same content-length for HEAD and GET requests"
    (List.map
       ~f:respond_to_head_from_get
       (* valid basic auth login on darklang.com *)
       [ Req.make
           ?meth:(Some `GET)
           (Uri.of_string
              ("http://" ^ test_name ^ ".builtwithdark.localhost:8000/test"))
         (* valid basic auth login on localhost *)
       ; Req.make
           ?meth:(Some `HEAD)
           (Uri.of_string
              ("http://" ^ test_name ^ ".builtwithdark.localhost:8000/test"))
       ])
    [ (200, (expected_content_length, expected_body))
    ; (200, (expected_content_length, "")) ]


let t_route_eq_path_match_concrete () =
  let route = "/a/b/c/d" in
  let path = "/a/b/c/d" in
  let bound = Http.bind_route_variables ~route path in
  AT.check
    (AT.option (AT.list testable_string_dval_pair))
    "empty binding succeeds"
    (Some [])
    bound


let t_route_non_prefix_colon_does_not_denote_variable () =
  (* as the colon does not denote a variable, this is actually a malformed
   * route as `:` is reserved in the URL alphabet and thus we could never
   * receive a path that matches it *)
  let route = "/letters:var" in
  let path = "/lettersextra" in
  let bound = Http.bind_route_variables ~route path in
  AT.check
    (AT.option (AT.list testable_string_dval_pair))
    "binding fails due to concrete mismatch"
    None
    bound


let t_query_params_with_duplicate_keys () =
  let parsed =
    Parsed_request.parsed_query_string [("a", ["b"]); ("a", ["c"])]
  in
  check_dval
    "parsed_query_string"
    (DObj
       (DvalMap.singleton
          "queryParams"
          (DObj (DvalMap.singleton "a" (Dval.dstr_of_string_exn "c")))))
    parsed ;
  check_dval
    "query_to_dval"
    (Dval.query_to_dval [("a", ["b"]); ("a", ["c"])])
    (DObj (DvalMap.singleton "a" (Dval.dstr_of_string_exn "c"))) ;
  ()


let t_path_gt_route_does_not_crash () =
  let route = "/" in
  let path = "/a/b/c/d" in
  let bound = Http.bind_route_variables ~route path in
  AT.check
    (AT.option (AT.list testable_string_dval_pair))
    "binding fails without crash"
    None
    bound


let suite =
  [ ("Webserver.should_use_https works", `Quick, t_should_use_https)
  ; ("Webserver.redirect_to works", `Quick, t_redirect_to) (* errorrail *)
  ; ("bad ssl cert", `Slow, t_bad_ssl_cert)
  ; ( "t_sanitize_uri_path_with_repeated_slashes"
    , `Quick
    , t_sanitize_uri_path_with_repeated_slashes )
  ; ( "t_sanitize_uri_path_with_trailing_slash"
    , `Quick
    , t_sanitize_uri_path_with_trailing_slash )
  ; ( "t_sanitize_uri_path_with_root_noops"
    , `Quick
    , t_sanitize_uri_path_with_root_noops )
  ; ( "t_sanitize_uri_path_with_repeated_root"
    , `Quick
    , t_sanitize_uri_path_with_repeated_root )
  ; ("Route variables work", `Quick, t_route_variables_work)
  ; ("concrete is more specific than wild", `Quick, t_concrete_over_wild)
  ; ("wild is more specific than nothing", `Quick, t_wild_over_nothing)
  ; ("differing size wildcard routes", `Quick, t_differing_wildcards)
  ; ("lengthy a/b/c/d/e/f wildcard", `Quick, t_lengthy_abcdef_wildcard)
  ; ( "same length a/b/c with different # wildcards "
    , `Quick
    , t_same_length_abc_diff_wildcards )
  ; ( "same length a/b/c with same # wildcards "
    , `Quick
    , t_same_length_abc_same_wildcards )
  ; ("same specificity are returned", `Quick, t_same_specificity_are_returned)
  ; ("route /:a results in 404 for /", `Quick, t_mismatch_is_filtered)
  ; ( "root handler is not filtered out"
    , `Quick
    , t_mismatch_filtering_leaves_root )
  ; ("route = path", `Quick, t_route_equals_path)
  ; ("route < path", `Quick, t_route_lt_path_with_wildcard)
  ; ("route < path not wildcard", `Quick, t_route_lt_path_without_wildcard)
  ; ("route > path", `Quick, t_route_gt_path)
  ; ( "route = path but concrete mismatch"
    , `Quick
    , t_route_eq_path_mismatch_concrete )
  ; ( "head and get requests are coalsced"
    , `Quick
    , t_head_and_get_requests_are_coalesced )
  ; ( "route = path solely concrete match"
    , `Quick
    , t_route_eq_path_match_concrete )
  ; ( "apparent route variable that's not a prefix does not match"
    , `Quick
    , t_route_non_prefix_colon_does_not_denote_variable )
  ; ( "path > route with root handler does not crash"
    , `Quick
    , t_path_gt_route_does_not_crash )
  ; ( "Query strings behave properly given multiple duplicate keys"
    , `Quick
    , t_query_params_with_duplicate_keys ) ]
