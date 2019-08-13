open Core_kernel
open Libexecution.Runtime
open Libexecution.Lib
open Libexecution.Types.RuntimeT
module Dval = Libexecution.Dval
module Dint = Libexecution.Dint
module Unicode_string = Libexecution.Unicode_string

let params =
  [par "uri" TStr; par "body" TAny; par "query" TObj; par "headers" TObj]


let params_no_body = [par "uri" TStr; par "query" TObj; par "headers" TObj]

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
  let result, headers, code =
    Httpclient.http_call uri query verb headers body
  in
  let parsed_result =
    if has_form_header headers
    then
      try Dval.of_form_encoding result with _ ->
        Dval.dstr_of_string_exn "form decoding error"
    else if has_json_header headers
    then
      try Dval.of_unknown_json_v0 result with _ ->
        Dval.dstr_of_string_exn "json decoding error"
    else
      try Dval.dstr_of_string_exn result with _ ->
        Dval.dstr_of_string_exn "utf-8 decoding error"
  in
  let parsed_headers =
    headers
    |> List.map ~f:(fun (k, v) ->
           (String.strip k, Dval.dstr_of_string_exn (String.strip v)) )
    |> List.filter ~f:(fun (k, _) -> String.length k > 0)
    |> DvalMap.from_list
    |> fun dm -> DObj dm
  in
  let obj =
    Dval.to_dobj_exn
      [ ("body", parsed_result)
      ; ("headers", parsed_headers)
      ; ( "raw"
        , result
          |> Dval.dstr_of_string
          |> Option.value
               ~default:(Dval.dstr_of_string_exn "utf-8 decoding error") )
      ; ("code", DInt (Dint.of_int code)) ]
  in
  if code >= 200 && code <= 299
  then DResult (ResOk obj)
  else DResult (ResError obj)


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


let fns : shortfn list =
  [ { pns = ["HttpClient::post"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP POST call to `uri`. Uses broken JSON format"
    ; f =
        Legacy.LibhttpclientV0.call
          Httpclient.POST
          Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::put"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP PUT call to `uri`. Uses broken JSON format"
    ; f =
        Legacy.LibhttpclientV0.call
          Httpclient.PUT
          Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::get"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP GET call to `uri`. Uses broken JSON format"
    ; f =
        Legacy.LibhttpclientV0.call
          Httpclient.GET
          Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::delete"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP DELETE call to `uri`. Uses broken JSON format"
    ; f =
        Legacy.LibhttpclientV0.call
          Httpclient.DELETE
          Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::options"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP OPTIONS call to `uri`. Uses broken JSON format"
    ; f =
        Legacy.LibhttpclientV0.call
          Httpclient.OPTIONS
          Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::head"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP HEAD call to `uri`. Uses broken JSON format"
    ; f =
        Legacy.LibhttpclientV0.call
          Httpclient.HEAD
          Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::patch"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP PATCH call to `uri`. Uses broken JSON format"
    ; f =
        Legacy.LibhttpclientV0.call
          Httpclient.PATCH
          Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::post_v1"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP POST call to `uri`"
    ; f =
        Legacy.LibhttpclientV0.call
          Httpclient.POST
          Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::put_v1"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP PUT call to `uri`"
    ; f =
        Legacy.LibhttpclientV0.call
          Httpclient.PUT
          Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::get_v1"]
    ; ins = []
    ; p = params_no_body
    ; r = TObj
    ; d = "Make blocking HTTP GET call to `uri`"
    ; f =
        Legacy.LibhttpclientV0.call_no_body
          Httpclient.GET
          Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::delete_v1"]
    ; ins =
        []
        (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
         * the spec says it may have a body *)
    ; p = params_no_body
    ; r = TObj
    ; d = "Make blocking HTTP DELETE call to `uri`"
    ; f =
        Legacy.LibhttpclientV0.call_no_body
          Httpclient.DELETE
          Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::options_v1"]
    ; ins = []
    ; p = params_no_body
    ; r = TObj
    ; d = "Make blocking HTTP OPTIONS call to `uri`"
    ; f =
        Legacy.LibhttpclientV0.call_no_body
          Httpclient.OPTIONS
          Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::head_v1"]
    ; ins = []
    ; p = params_no_body
    ; r = TObj
    ; d = "Make blocking HTTP HEAD call to `uri`"
    ; f =
        Legacy.LibhttpclientV0.call_no_body
          Httpclient.HEAD
          Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::patch_v1"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP PATCH call to `uri`"
    ; f =
        Legacy.LibhttpclientV0.call
          Httpclient.PATCH
          Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::post_v2"]
    ; ins = []
    ; p = params
    ; r = TResult
    ; d =
        "Make blocking HTTP POST call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f =
        Legacy.LibhttpclientV0.wrapped_call
          Httpclient.POST
          Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::put_v2"]
    ; ins = []
    ; p = params
    ; r = TResult
    ; d =
        "Make blocking HTTP PUT call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f =
        Legacy.LibhttpclientV0.wrapped_call
          Httpclient.PUT
          Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::get_v2"]
    ; ins = []
    ; p = params_no_body
    ; r = TResult
    ; d =
        "Make blocking HTTP GET call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f =
        Legacy.LibhttpclientV0.wrapped_call_no_body
          Httpclient.GET
          Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::delete_v2"]
    ; ins =
        []
        (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
         * the spec says it may have a body *)
    ; p = params_no_body
    ; r = TResult
    ; d =
        "Make blocking HTTP DELETE call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f =
        Legacy.LibhttpclientV0.wrapped_call_no_body
          Httpclient.DELETE
          Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::options_v2"]
    ; ins = []
    ; p = params_no_body
    ; r = TResult
    ; d =
        "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f =
        Legacy.LibhttpclientV0.wrapped_call_no_body
          Httpclient.OPTIONS
          Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::head_v2"]
    ; ins = []
    ; p = params_no_body
    ; r = TResult
    ; d =
        "Make blocking HTTP HEAD call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f =
        Legacy.LibhttpclientV0.wrapped_call_no_body
          Httpclient.HEAD
          Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::patch_v2"]
    ; ins = []
    ; p = params
    ; r = TResult
    ; d =
        "Make blocking HTTP PATCH call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f =
        Legacy.LibhttpclientV0.wrapped_call
          Httpclient.PATCH
          Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::post_v3"]
    ; ins = []
    ; p = params
    ; r = TResult
    ; d =
        "Make blocking HTTP POST call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; f = call Httpclient.POST Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::put_v3"]
    ; ins = []
    ; p = params
    ; r = TResult
    ; d =
        "Make blocking HTTP PUT call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; f = call Httpclient.PUT Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::get_v3"]
    ; ins = []
    ; p = params_no_body
    ; r = TResult
    ; d =
        "Make blocking HTTP GET call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; f = call_no_body Httpclient.GET Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::delete_v3"]
    ; ins =
        []
        (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
         * the spec says it may have a body *)
    ; p = params_no_body
    ; r = TResult
    ; d =
        "Make blocking HTTP DELETE call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; f = call_no_body Httpclient.DELETE Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::options_v3"]
    ; ins = []
    ; p = params_no_body
    ; r = TResult
    ; d =
        "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; f = call_no_body Httpclient.OPTIONS Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::head_v3"]
    ; ins = []
    ; p = params_no_body
    ; r = TResult
    ; d =
        "Make blocking HTTP HEAD call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; f = call_no_body Httpclient.HEAD Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::patch_v3"]
    ; ins = []
    ; p = params
    ; r = TResult
    ; d =
        "Make blocking HTTP PATCH call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; f = call Httpclient.PATCH Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::basicAuth"]
    ; ins = []
    ; p = [par "username" TStr; par "password" TStr]
    ; r = TObj
    ; d =
        "Returns an object with 'Authorization' created using HTTP basic auth"
    ; f =
        InProcess
          (function
          | _, [DStr u; DStr p] ->
              DObj
                (DvalMap.singleton
                   "Authorization"
                   (DStr (encode_basic_auth u p)))
          | args ->
              fail args)
    ; ps = false
    ; dep = false } ]
