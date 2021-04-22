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
