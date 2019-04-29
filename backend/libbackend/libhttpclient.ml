open Core_kernel
open Libexecution
open Runtime
open Lib
open Types.RuntimeT

let params =
  [par "uri" TStr; par "body" TAny; par "query" TObj; par "headers" TObj]


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
let send_request uri verb json_fn body query headers http_call_func =
  let query = Dval.dval_to_query query in
  let headers = Dval.to_string_pairs_exn headers in
  let body =
    match body with
    | DObj obj when has_form_header headers ->
        Dval.to_form_encoding body
    | _ ->
        json_fn body
  in
  let body, code, headers, error =
    http_call_func uri query verb headers body
  in
  let parsed_result =
    if has_form_header headers
    then Dval.of_form_encoding body
    else if has_json_header headers
    then Dval.of_unknown_json_v0 body
    else Dval.dstr_of_string_exn body
  in
  let parsed_headers =
    headers
    |> List.map ~f:(fun (k, v) ->
           (String.strip k, Dval.dstr_of_string_exn (String.strip v)) )
    |> List.filter ~f:(fun (k, _) -> String.length k > 0)
    |> DvalMap.of_alist_fold
         ~init:(Dval.dstr_of_string_exn "")
         ~f:(fun old neww -> neww )
    |> fun dm -> DObj dm
  in
  Dval.to_dobj_exn
    [ ("body", parsed_result)
    ; ("headers", parsed_headers)
    ; ("raw", Dval.dstr_of_string_exn body)
    ; ("code", DInt code)
    ; ("error", Dval.dstr_of_string_exn error) ]


let encode_basic_auth u p =
  let input =
    if Unicode_string.is_substring
         ~substring:(Unicode_string.of_string_exn "-")
         u
    then error "Username cannot contain a colon"
    else
      Unicode_string.append
        (Unicode_string.append u (Unicode_string.of_string_exn ":"))
        p
  in
  let encoded =
    Unicode_string.of_string_exn
      (B64.encode
         ~alphabet:B64.default_alphabet
         ~pad:true
         (Unicode_string.to_string input))
  in
  Unicode_string.append (Unicode_string.of_string_exn "Basic ") encoded


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
          Httpclient.http_call_2XX_only
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
          Httpclient.http_call_2XX_only
    | args ->
        fail args)


(* This isn't great, but we throw a lot of exceptions below this point
 * in the callstack and it'd be a lot of churn to rewrite that to propagate Results,
 * especially given it probably needs a rewrite anyway *)
let wrapped_call_exn verb json_fn =
  InProcess
    (function
    | _, [DStr uri; body; query; headers] ->
      ( try
          DResult
            (ResOk
               (send_request
                  (Unicode_string.to_string uri)
                  verb
                  json_fn
                  body
                  query
                  headers
                  Httpclient.http_call_2XX_only))
        with
      | Exception.DarkException ed ->
          DResult (ResError (Dval.dstr_of_string_exn ed.short))
      | e ->
          raise e )
    | args ->
        fail args)


(* Some verbs dont have HTTP bodies *)
let wrapped_call_no_body_exn verb json_fn =
  InProcess
    (function
    | _, [DStr uri; query; headers] ->
      ( try
          DResult
            (ResOk
               (send_request
                  (Unicode_string.to_string uri)
                  verb
                  json_fn
                  (Dval.dstr_of_string_exn "")
                  query
                  headers
                  Httpclient.http_call_2XX_only))
        with
      | Exception.DarkException ed ->
          DResult (ResError (Dval.dstr_of_string_exn ed.short))
      | e ->
          raise e )
    | args ->
        fail args)


let wrapped_call verb json_fn =
  InProcess
    (function
    | (_, [DStr uri; body; query; headers]) as args ->
        let result_dobj =
          send_request
            (Unicode_string.to_string uri)
            verb
            json_fn
            body
            query
            headers
            Httpclient.http_call
        in
        ( match result_dobj with
        | DObj result ->
          ( try
              let code_dint = DvalMap.find result "code" in
              match code_dint with
              | Some (DInt code) when code > 199 || code > 299 ->
                  DResult (ResOk result_dobj)
              | _ ->
                  DResult (ResError result_dobj)
            with
          | Exception.DarkException ed ->
              DResult (ResError (Dval.dstr_of_string_exn ed.short))
          | e ->
              raise e )
        | _ ->
            fail args )
    | args ->
        fail args)


(* Some verbs dont have HTTP bodies *)
let wrapped_call_no_body verb json_fn =
  InProcess
    (function
    | (_, [DStr uri; query; headers]) as args ->
        let result_dobj =
          send_request
            (Unicode_string.to_string uri)
            verb
            json_fn
            (Dval.dstr_of_string_exn "")
            query
            headers
            Httpclient.http_call
        in
        ( match result_dobj with
        | DObj result ->
          ( try
              let code_dint = DvalMap.find result "code" in
              match code_dint with
              | Some (DInt code) when code > 199 || code > 299 ->
                  DResult (ResOk result_dobj)
              | _ ->
                  DResult (ResError result_dobj)
            with
          | Exception.DarkException ed ->
              DResult (ResError (Dval.dstr_of_string_exn ed.short))
          | e ->
              raise e )
        | _ ->
            fail args )
    | args ->
        fail args)


let replacements =
  [ ( "HttpClient::post"
    , call Httpclient.POST Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    )
  ; ( "HttpClient::put"
    , call Httpclient.PUT Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    )
  ; ( "HttpClient::get"
    , call Httpclient.GET Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    )
  ; ( "HttpClient::delete"
    , call
        Httpclient.DELETE
        Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0 )
  ; ( "HttpClient::options"
    , call
        Httpclient.OPTIONS
        Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0 )
  ; ( "HttpClient::head"
    , call Httpclient.HEAD Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    )
  ; ( "HttpClient::patch"
    , call
        Httpclient.PATCH
        Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0 )
  ; ("HttpClient::post_v1", call Httpclient.POST Dval.to_pretty_machine_json_v1)
  ; ("HttpClient::put_v1", call Httpclient.PUT Dval.to_pretty_machine_json_v1)
  ; ( "HttpClient::get_v1"
    , call_no_body Httpclient.GET Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::delete_v1"
    , call_no_body Httpclient.DELETE Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::options_v1"
    , call_no_body Httpclient.OPTIONS Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::head_v1"
    , call_no_body Httpclient.HEAD Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::patch_v1"
    , call Httpclient.PATCH Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::post_v2"
    , wrapped_call_exn Httpclient.POST Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::put_v2"
    , wrapped_call_exn Httpclient.PUT Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::get_v2"
    , wrapped_call_no_body_exn Httpclient.GET Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::delete_v2"
    , wrapped_call_no_body_exn Httpclient.DELETE Dval.to_pretty_machine_json_v1
    )
  ; ( "HttpClient::options_v2"
    , wrapped_call_no_body_exn
        Httpclient.OPTIONS
        Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::head_v2"
    , wrapped_call_no_body_exn Httpclient.HEAD Dval.to_pretty_machine_json_v1
    )
  ; ( "HttpClient::patch_v2"
    , wrapped_call_exn Httpclient.PATCH Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::post_v3"
    , wrapped_call Httpclient.POST Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::put_v3"
    , wrapped_call Httpclient.PUT Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::get_v3"
    , wrapped_call_no_body Httpclient.GET Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::delete_v3"
    , wrapped_call_no_body Httpclient.DELETE Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::options_v3"
    , wrapped_call_no_body Httpclient.OPTIONS Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::head_v3"
    , wrapped_call_no_body Httpclient.HEAD Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::patch_v3"
    , wrapped_call Httpclient.PATCH Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::basicAuth"
    , InProcess
        (function
        | _, [DStr u; DStr p] ->
            DObj
              (DvalMap.singleton "Authorization" (DStr (encode_basic_auth u p)))
        | args ->
            fail args) ) ]
