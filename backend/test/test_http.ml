open Core_kernel
open Libcommon
open Libexecution
open Libbackend
open Types
open Types.RuntimeT
open Lwt
open Utils
module Resp = Cohttp_lwt_unix.Response
module Req = Cohttp_lwt_unix.Request
module Header = Cohttp.Header
module Code = Cohttp.Code
module AT = Alcotest

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


let t_incomplete_handler_doesnt_throw () =
  let filled = http_route_handler ~route:"/:foo" () in
  let empty = handler (ast_for "5") in
  let ordered = Http.filter_matching_handlers "/a" [filled; empty] in
  AT.check
    (AT.list testable_handler)
    "incomplete handler is filtered out without throwing"
    [filled]
    ordered


let t_parsed_request_cookies () =
  let with_headers h =
    Parsed_request.from_request (Uri.of_string "test") h [] ""
    |> Parsed_request.to_dval
    |> fun v ->
    match v with
    | DObj o ->
        Base.Map.find_exn o "cookies"
    | _ ->
        failwith "didn't end up with 'cookies' in the DObj"
  in
  let with_cookies c = with_headers [("cookie", c)] in
  AT.check
    (AT.list at_dval)
    "Parsed_request.from_request parses cookies correctly."
    [ with_headers []
    ; with_cookies ""
    ; with_cookies "a"
    ; with_cookies "a="
    ; with_cookies "a=b"
    ; with_cookies "a=b;"
    ; with_cookies "a=b; c=d"
    ; with_cookies "a=b; c=d;" ]
    [ Dval.to_dobj_exn []
    ; Dval.to_dobj_exn []
    ; Dval.to_dobj_exn []
    ; Dval.to_dobj_exn [("a", Dval.dstr_of_string_exn "")]
    ; Dval.to_dobj_exn [("a", Dval.dstr_of_string_exn "b")]
    ; Dval.to_dobj_exn [("a", Dval.dstr_of_string_exn "b")]
    ; Dval.to_dobj_exn
        [("a", Dval.dstr_of_string_exn "b"); ("c", Dval.dstr_of_string_exn "d")]
    ; Dval.to_dobj_exn
        [("a", Dval.dstr_of_string_exn "b"); ("c", Dval.dstr_of_string_exn "d")]
    ]


let t_parsed_request_bodies () =
  let formHeader = ("content-type", "application/x-www-form-urlencoded") in
  let jsonHeader = ("content-type", "application/json") in
  let parse header body =
    let response =
      Parsed_request.from_request (Uri.of_string "test") [header] [] body
      |> Parsed_request.to_dval
    in
    match response with
    | DObj r ->
        ( DvalMap.get "jsonBody" r |> Option.value_exn
        , DvalMap.get "formBody" r |> Option.value_exn )
    | _ ->
        Exception.internal "wrong shape"
  in
  let expectedObj =
    Dval.to_dobj_exn [("field1", Dval.dstr_of_string_exn "value1")]
  in
  AT.check
    (AT.pair at_dval at_dval)
    "form no json"
    (DNull, expectedObj)
    (parse formHeader "field1=value1") ;
  AT.check
    (AT.pair at_dval at_dval)
    "json no form"
    (expectedObj, DNull)
    (parse jsonHeader "{ \"field1\": \"value1\" }") ;
  ()


let suite =
  [ ( "t_sanitize_uri_path_with_repeated_slashes"
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
    , t_query_params_with_duplicate_keys )
  ; ("Cookies are parsed correctly", `Quick, t_parsed_request_cookies)
  ; ("Bodies are parsed correctly", `Quick, t_parsed_request_bodies)
  ; ( "Incomplete handler is filtered out safely"
    , `Quick
    , t_incomplete_handler_doesnt_throw ) ]
