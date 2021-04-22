let t_mismatch_is_filtered () =
  let single = http_route_handler ~route:"/:first" () in
  let filtered = Http.filter_invalid_handler_matches ~path:"/" [single] in
  AT.check (AT.list testable_handler) "mismatch is filtered out" [] filtered


let t_mismatch_filtering_leaves_root () =
  let single = http_route_handler ~route:"/:first" () in
  let root = http_route_handler ~tlid:tlid2 ~route:"/" () in
  let filtered = Http.filter_invalid_handler_matches ~path:"/" [single; root] in
  AT.check
    (AT.list testable_handler)
    "mismatch is filtered out but root is left"
    [root]
    filtered


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


let t_incomplete_handler_doesnt_throw () =
  let filled = http_route_handler ~route:"/:foo" () in
  let empty = handler (int 5) in
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
