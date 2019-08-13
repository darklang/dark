open Core_kernel
open Libexecution
open Runtime
open Lib
open Types.RuntimeT

module HttpclientV0 = struct
  let http_call
      (url : string)
      (query_params : (string * string list) list)
      (verb : Httpclient.verb)
      (headers : (string * string) list)
      (body : string) : string * (string * string) list =
    let resp_body, code, resp_headers, error =
      Httpclient.http_call_with_code url query_params verb headers body
    in
    if code < 200 || code > 299
    then
      let info =
        [ ("url", url)
        ; ("code", string_of_int code)
        ; ("error", error)
        ; ("response", resp_body) ]
      in
      Exception.code
        ~info
        ("Bad HTTP response (" ^ string_of_int code ^ ") in call to " ^ url)
    else (resp_body, resp_headers)


  let call
      (url : string)
      (verb : Httpclient.verb)
      (headers : (string * string) list)
      (body : string) : string =
    let results, _ = http_call url [] verb headers body in
    results
end

module LibhttpclientV0 = struct
  type headers = (string * string) list

  let has_form_header (headers : headers) : bool =
    List.exists headers ~f:(fun (k, v) ->
        String.lowercase k = "content-type"
        && String.lowercase v = "application/x-www-form-urlencoded" )


  let has_json_header (headers : headers) : bool =
    List.exists headers ~f:(fun (k, v) ->
        String.lowercase k = "content-type"
        && v
           |> String.lowercase
           |> String.is_substring ~substring:"application/json" )


  (* TODO: integrate with dark_request *)
  let send_request
      (uri : string)
      (verb : Httpclient.verb)
      (json_fn : dval -> string)
      (body : dval)
      (query : dval)
      (headers : dval) : dval =
    let query = Dval.dval_to_query query in
    let headers = Dval.to_string_pairs_exn headers in
    let body =
      match body with
      | DObj obj when has_form_header headers ->
          Dval.to_form_encoding body
      | _ ->
          json_fn body
    in
    let result, headers = HttpclientV0.http_call uri query verb headers body in
    let parsed_result =
      if has_form_header headers
      then Dval.of_form_encoding result
      else if has_json_header headers
      then Dval.of_unknown_json_v0 result
      else Dval.dstr_of_string_exn result
    in
    let parsed_headers =
      headers
      |> List.map ~f:(fun (k, v) ->
             (String.strip k, Dval.dstr_of_string_exn (String.strip v)) )
      |> List.filter ~f:(fun (k, _) -> String.length k > 0)
      |> DvalMap.from_list
      |> fun dm -> DObj dm
    in
    Dval.to_dobj_exn
      [ ("body", parsed_result)
      ; ("headers", parsed_headers)
      ; ("raw", Dval.dstr_of_string_exn result) ]


  let call verb json_fn =
    InProcess
      (function
      | _, [DStr uri; body; query; headers] ->
          send_request
            (Unicode_string.to_string uri)
            verb
            json_fn
            body
            query
            headers
      | args ->
          fail args)


  (* Some verbs dont have HTTP bodies *)
  let call_no_body verb json_fn =
    InProcess
      (function
      | _, [DStr uri; query; headers] ->
          send_request
            (Unicode_string.to_string uri)
            verb
            json_fn
            (Dval.dstr_of_string_exn "")
            query
            headers
      | args ->
          fail args)


  (* This isn't great, but we throw a lot of exceptions below this point
     * in the callstack and it'd be a lot of churn to rewrite that to propagate Results,
 * especially given it probably needs a rewrite anyway *)
  let wrapped_send_request
      (uri : string)
      (verb : Httpclient.verb)
      (json_fn : dval -> string)
      (body : dval)
      (query : dval)
      (headers : dval) : dval =
    Libcommon.Log.inspecT "uri" uri ;
    Libcommon.Log.inspecT "body" body ;
    Libcommon.Log.inspecT "query" query ;
    Libcommon.Log.inspecT "headers" headers ;
    try DResult (ResOk (send_request uri verb json_fn body query headers)) with
    | Exception.DarkException ed ->
        DResult (ResError (Dval.dstr_of_string_exn ed.short))
    | e ->
        raise e


  let wrapped_call verb json_fn =
    InProcess
      (function
      | _, [DStr uri; body; query; headers] ->
          wrapped_send_request
            (Unicode_string.to_string uri)
            verb
            json_fn
            body
            query
            headers
      | args ->
          fail args)


  (* Some verbs dont have HTTP bodies *)
  let wrapped_call_no_body verb json_fn =
    InProcess
      (function
      | _, [DStr uri; query; headers] ->
          wrapped_send_request
            (Unicode_string.to_string uri)
            verb
            json_fn
            (Dval.dstr_of_string_exn "")
            query
            headers
      | args ->
          fail args)
end
