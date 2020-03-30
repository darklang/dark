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
      && String.lowercase v = "application/x-www-form-urlencoded")


let has_json_header (headers : headers) : bool =
  List.exists headers ~f:(fun (k, v) ->
      String.lowercase k = "content-type"
      && v
         |> String.lowercase
         |> String.is_substring ~substring:"application/json")


let has_plaintext_header (headers : headers) : bool =
  List.exists headers ~f:(fun (k, v) ->
      String.lowercase k = "content-type"
      && v |> String.lowercase |> String.is_substring ~substring:"text/plain")


let encode_request_body (headers : headers) (body : dval option) : string option
    =
  body
  |> Option.map ~f:(function
         | DObj _ as dv when has_form_header headers ->
             Dval.to_form_encoding dv
         | DStr s ->
             (* Do nothing to strings, ever. The reasoning here is that users do not expect any
              * magic to happen to their raw strings. It's also the only real way (barring Bytes) to support
              * users doing their _own_ encoding (say, jsonifying themselves and passing the Content-Type header
              * manually).
              *
              * See: https://www.notion.so/darklang/Httpclient-Empty-Body-2020-03-10-5fa468b5de6c4261b5dc81ff243f79d9 for
              * more information. *)
             Unicode_string.to_string s
         | dv when has_plaintext_header headers ->
             Dval.to_enduser_readable_text_v0 dv
         | dv ->
             (* Otherwise, jsonify (this is the 'easy' API afterall), regardless of headers passed. This makes a little more
              * sense than you might think on first glance, due to interaction with the `DStr` case. If a user actually
              * _wants_ to use a different Content-Type than the form/plain-text magic provided, they're responsible for
              * encoding the value to a String first and not just giving us a random dval. *)
             Dval.to_pretty_machine_json_v1 dv)
  |> Option.bind ~f:(fun body ->
         (* Explicitly convert the empty String to `None`, to ensure downstream we set the right bits on the outgoing
          * cURL request. *)
         if String.length body <> 0 then Some body else None)


let send_request
    (uri : string)
    (verb : Httpclient.verb)
    (request_body : dval option)
    (query : dval)
    (request_headers : dval) : dval =
  let raw_response_body, raw_response_headers, response_code =
    let encoded_query = Dval.dval_to_query query in
    let encoded_request_headers = Dval.to_string_pairs_exn request_headers in
    let encoded_request_body =
      (* We use the user-provided Content-Type headers to make ~magic~ decisions
       * about how to encode the the outgoing request *)
      encode_request_body encoded_request_headers request_body
    in
    Httpclient.http_call
      uri
      encoded_query
      verb
      encoded_request_headers
      encoded_request_body
  in
  let parsed_response_body =
    if has_form_header raw_response_headers
    then
      try Dval.of_form_encoding raw_response_body
      with _ -> Dval.dstr_of_string_exn "form decoding error"
    else if has_json_header raw_response_headers
    then
      try Dval.of_unknown_json_v0 raw_response_body
      with _ -> Dval.dstr_of_string_exn "json decoding error"
    else
      try Dval.dstr_of_string_exn raw_response_body
      with _ -> Dval.dstr_of_string_exn "utf-8 decoding error"
  in
  let parsed_response_headers =
    raw_response_headers
    |> List.map ~f:(fun (k, v) ->
           (String.strip k, Dval.dstr_of_string_exn (String.strip v)))
    |> List.filter ~f:(fun (k, _) -> String.length k > 0)
    |> DvalMap.from_list
    |> fun dm -> DObj dm
  in
  let obj =
    Dval.to_dobj_exn
      [ ("body", parsed_response_body)
      ; ("headers", parsed_response_headers)
      ; ( "raw"
        , raw_response_body
          |> Dval.dstr_of_string
          |> Option.value
               ~default:(Dval.dstr_of_string_exn "utf-8 decoding error") )
      ; ("code", DInt (Dint.of_int response_code)) ]
  in
  if response_code >= 200 && response_code <= 299
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


let call verb =
  InProcess
    (function
    | _, [DStr uri; body; query; headers] ->
        send_request
          (Unicode_string.to_string uri)
          verb
          (Some body)
          query
          headers
    | args ->
        fail args)


let call_no_body verb =
  InProcess
    (function
    | _, [DStr uri; query; headers] ->
        send_request (Unicode_string.to_string uri) verb None query headers
    | args ->
        fail args)


let fns : fn list =
  [ { prefix_names = ["HttpClient::post"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TObj
    ; description =
        "Make blocking HTTP POST call to `uri`. Uses broken JSON format"
    ; func =
        Legacy.LibhttpclientV0.call
          Httpclient.POST
          Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::put"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TObj
    ; description =
        "Make blocking HTTP PUT call to `uri`. Uses broken JSON format"
    ; func =
        Legacy.LibhttpclientV0.call
          Httpclient.PUT
          Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::get"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TObj
    ; description =
        "Make blocking HTTP GET call to `uri`. Uses broken JSON format"
    ; func =
        Legacy.LibhttpclientV0.call
          Httpclient.GET
          Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::delete"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TObj
    ; description =
        "Make blocking HTTP DELETE call to `uri`. Uses broken JSON format"
    ; func =
        Legacy.LibhttpclientV0.call
          Httpclient.DELETE
          Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::options"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TObj
    ; description =
        "Make blocking HTTP OPTIONS call to `uri`. Uses broken JSON format"
    ; func =
        Legacy.LibhttpclientV0.call
          Httpclient.OPTIONS
          Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::head"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TObj
    ; description =
        "Make blocking HTTP HEAD call to `uri`. Uses broken JSON format"
    ; func =
        Legacy.LibhttpclientV0.call
          Httpclient.HEAD
          Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::patch"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TObj
    ; description =
        "Make blocking HTTP PATCH call to `uri`. Uses broken JSON format"
    ; func =
        Legacy.LibhttpclientV0.call
          Httpclient.PATCH
          Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::post_v1"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TObj
    ; description = "Make blocking HTTP POST call to `uri`"
    ; func =
        Legacy.LibhttpclientV0.call
          Httpclient.POST
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::put_v1"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TObj
    ; description = "Make blocking HTTP PUT call to `uri`"
    ; func =
        Legacy.LibhttpclientV0.call
          Httpclient.PUT
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::get_v1"]
    ; infix_names = []
    ; parameters = params_no_body
    ; return_type = TObj
    ; description = "Make blocking HTTP GET call to `uri`"
    ; func =
        Legacy.LibhttpclientV0.call_no_body
          Httpclient.GET
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::delete_v1"]
    ; infix_names =
        []
        (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
         * the spec says it may have a body *)
    ; parameters = params_no_body
    ; return_type = TObj
    ; description = "Make blocking HTTP DELETE call to `uri`"
    ; func =
        Legacy.LibhttpclientV0.call_no_body
          Httpclient.DELETE
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::options_v1"]
    ; infix_names = []
    ; parameters = params_no_body
    ; return_type = TObj
    ; description = "Make blocking HTTP OPTIONS call to `uri`"
    ; func =
        Legacy.LibhttpclientV0.call_no_body
          Httpclient.OPTIONS
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::head_v1"]
    ; infix_names = []
    ; parameters = params_no_body
    ; return_type = TObj
    ; description = "Make blocking HTTP HEAD call to `uri`"
    ; func =
        Legacy.LibhttpclientV0.call_no_body
          Httpclient.HEAD
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::patch_v1"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TObj
    ; description = "Make blocking HTTP PATCH call to `uri`"
    ; func =
        Legacy.LibhttpclientV0.call
          Httpclient.PATCH
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::post_v2"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TResult
    ; description =
        "Make blocking HTTP POST call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; func =
        Legacy.LibhttpclientV0.wrapped_call
          Httpclient.POST
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::put_v2"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TResult
    ; description =
        "Make blocking HTTP PUT call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; func =
        Legacy.LibhttpclientV0.wrapped_call
          Httpclient.PUT
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::get_v2"]
    ; infix_names = []
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP GET call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; func =
        Legacy.LibhttpclientV0.wrapped_call_no_body
          Httpclient.GET
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::delete_v2"]
    ; infix_names =
        []
        (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
         * the spec says it may have a body *)
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP DELETE call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; func =
        Legacy.LibhttpclientV0.wrapped_call_no_body
          Httpclient.DELETE
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::options_v2"]
    ; infix_names = []
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; func =
        Legacy.LibhttpclientV0.wrapped_call_no_body
          Httpclient.OPTIONS
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::head_v2"]
    ; infix_names = []
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP HEAD call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; func =
        Legacy.LibhttpclientV0.wrapped_call_no_body
          Httpclient.HEAD
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::patch_v2"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TResult
    ; description =
        "Make blocking HTTP PATCH call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; func =
        Legacy.LibhttpclientV0.wrapped_call
          Httpclient.PATCH
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::post_v3"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TResult
    ; description =
        "Make blocking HTTP POST call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func =
        Legacy.LibhttpclientV1.call
          Httpclient.POST
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::put_v3"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TResult
    ; description =
        "Make blocking HTTP PUT call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func =
        Legacy.LibhttpclientV1.call
          Httpclient.PUT
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::get_v3"]
    ; infix_names = []
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP GET call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func =
        Legacy.LibhttpclientV1.call_no_body
          Httpclient.GET
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::delete_v3"]
    ; infix_names =
        []
        (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
         * the spec says it may have a body *)
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP DELETE call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func =
        Legacy.LibhttpclientV1.call_no_body
          Httpclient.DELETE
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::options_v3"]
    ; infix_names = []
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func =
        Legacy.LibhttpclientV1.call_no_body
          Httpclient.OPTIONS
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::head_v3"]
    ; infix_names = []
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP HEAD call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func =
        Legacy.LibhttpclientV1.call_no_body
          Httpclient.HEAD
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::patch_v3"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TResult
    ; description =
        "Make blocking HTTP PATCH call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func =
        Legacy.LibhttpclientV1.call
          Httpclient.PATCH
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::post_v4"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TResult
    ; description =
        "Make blocking HTTP POST call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func =
        Legacy.LibhttpclientV2.call
          Httpclient.POST
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::put_v4"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TResult
    ; description =
        "Make blocking HTTP PUT call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func =
        Legacy.LibhttpclientV2.call
          Httpclient.PUT
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::get_v4"]
    ; infix_names = []
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP GET call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func =
        Legacy.LibhttpclientV2.call_no_body
          Httpclient.GET
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::delete_v4"]
    ; infix_names =
        []
        (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
         * the spec says it may have a body *)
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP DELETE call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func =
        Legacy.LibhttpclientV2.call_no_body
          Httpclient.DELETE
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::options_v4"]
    ; infix_names = []
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func =
        Legacy.LibhttpclientV2.call_no_body
          Httpclient.OPTIONS
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::head_v4"]
    ; infix_names = []
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP HEAD call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func =
        Legacy.LibhttpclientV2.call_no_body
          Httpclient.HEAD
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::patch_v4"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TResult
    ; description =
        "Make blocking HTTP PATCH call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func =
        Legacy.LibhttpclientV2.call
          Httpclient.PATCH
          Dval.to_pretty_machine_json_v1
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["HttpClient::post_v5"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TResult
    ; description =
        "Make blocking HTTP POST call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func = call Httpclient.POST
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["HttpClient::put_v5"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TResult
    ; description =
        "Make blocking HTTP PUT call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func = call Httpclient.PUT
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["HttpClient::get_v5"]
    ; infix_names = []
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP GET call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func = call_no_body Httpclient.GET
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["HttpClient::delete_v5"]
    ; infix_names =
        []
        (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
         * the spec says it may have a body *)
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP DELETE call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func = call_no_body Httpclient.DELETE
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["HttpClient::options_v5"]
    ; infix_names = []
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func = call_no_body Httpclient.OPTIONS
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["HttpClient::head_v5"]
    ; infix_names = []
    ; parameters = params_no_body
    ; return_type = TResult
    ; description =
        "Make blocking HTTP HEAD call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func = call_no_body Httpclient.HEAD
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["HttpClient::patch_v5"]
    ; infix_names = []
    ; parameters = params
    ; return_type = TResult
    ; description =
        "Make blocking HTTP PATCH call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
    ; func = call Httpclient.PATCH
    ; preview_safety = Unsafe
    ; deprecated = false }
  ; { prefix_names = ["HttpClient::basicAuth"]
    ; infix_names = []
    ; parameters = [par "username" TStr; par "password" TStr]
    ; return_type = TObj
    ; description =
        "Returns an object with 'Authorization' created using HTTP basic auth"
    ; func =
        InProcess
          (function
          | _, [DStr u; DStr p] ->
              DObj
                (DvalMap.singleton
                   "Authorization"
                   (DStr (encode_basic_auth u p)))
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false } ]
