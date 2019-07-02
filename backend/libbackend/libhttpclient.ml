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
let send_request uri verb json_fn body query headers =
  let query = Dval.dval_to_query query in
  let headers = Dval.to_string_pairs_exn headers in
  let body =
    match body with
    | DObj obj when has_form_header headers ->
        Dval.to_form_encoding body
    | _ ->
        json_fn body
  in
  let result, headers = Httpclient.http_call uri query verb headers body in
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
let wrapped_call verb json_fn =
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
                  headers))
        with
      | Exception.DarkException ed ->
          DResult (ResError (Dval.dstr_of_string_exn ed.short))
      | e ->
          raise e )
    | args ->
        fail args)


(* Some verbs dont have HTTP bodies *)
let wrapped_call_no_body verb json_fn =
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
                  headers))
        with
      | Exception.DarkException ed ->
          DResult (ResError (Dval.dstr_of_string_exn ed.short))
      | e ->
          raise e )
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
    , wrapped_call Httpclient.POST Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::put_v2"
    , wrapped_call Httpclient.PUT Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::get_v2"
    , wrapped_call_no_body Httpclient.GET Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::delete_v2"
    , wrapped_call_no_body Httpclient.DELETE Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::options_v2"
    , wrapped_call_no_body Httpclient.OPTIONS Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::head_v2"
    , wrapped_call_no_body Httpclient.HEAD Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::patch_v2"
    , wrapped_call Httpclient.PATCH Dval.to_pretty_machine_json_v1 )
  ; ( "HttpClient::basicAuth"
    , InProcess
        (function
        | _, [DStr u; DStr p] ->
            DObj
              (DvalMap.singleton "Authorization" (DStr (encode_basic_auth u p)))
        | args ->
            fail args) ) ]
