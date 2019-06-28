open Core_kernel
open Libexecution
open Runtime
open Lib
open Types.RuntimeT

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


let fns : Lib.shortfn list =
  [ { pns = ["HttpClient::post"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP POST call to `uri`. Uses broken JSON format"
    ; f =
        call
          Httpclient.POST
          Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::put"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP PUT call to `uri`. Uses broken JSON format"
    ; f =
        call
          Httpclient.PUT
          Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::get"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP GET call to `uri`. Uses broken JSON format"
    ; f =
        call
          Httpclient.GET
          Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::delete"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP DELETE call to `uri`. Uses broken JSON format"
    ; f =
        call
          Httpclient.DELETE
          Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::options"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP OPTIONS call to `uri`. Uses broken JSON format"
    ; f =
        call
          Httpclient.OPTIONS
          Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::head"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP HEAD call to `uri`. Uses broken JSON format"
    ; f =
        call
          Httpclient.HEAD
          Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::patch"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP PATCH call to `uri`. Uses broken JSON format"
    ; f =
        call
          Httpclient.PATCH
          Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::post_v1"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP POST call to `uri`"
    ; f = call Httpclient.POST Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::put_v1"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP PUT call to `uri`"
    ; f = call Httpclient.PUT Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::get_v1"]
    ; ins = []
    ; p = params_no_body
    ; r = TObj
    ; d = "Make blocking HTTP GET call to `uri`"
    ; f = call_no_body Httpclient.GET Dval.to_pretty_machine_json_v1
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
    ; f = call_no_body Httpclient.DELETE Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::options_v1"]
    ; ins = []
    ; p = params_no_body
    ; r = TObj
    ; d = "Make blocking HTTP OPTIONS call to `uri`"
    ; f = call_no_body Httpclient.OPTIONS Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::head_v1"]
    ; ins = []
    ; p = params_no_body
    ; r = TObj
    ; d = "Make blocking HTTP HEAD call to `uri`"
    ; f = call_no_body Httpclient.HEAD Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::patch_v1"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP PATCH call to `uri`"
    ; f = call Httpclient.PATCH Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::post_v2"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d =
        "Make blocking HTTP POST call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f = wrapped_call Httpclient.POST Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::put_v2"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d =
        "Make blocking HTTP PUT call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f = wrapped_call Httpclient.PUT Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::get_v2"]
    ; ins = []
    ; p = params_no_body
    ; r = TResult
    ; d =
        "Make blocking HTTP GET call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f = wrapped_call_no_body Httpclient.GET Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::delete_v2"]
    ; ins =
        []
        (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
         * the spec says it may have a body *)
    ; p = params_no_body
    ; r = TObj
    ; d =
        "Make blocking HTTP DELETE call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f = wrapped_call_no_body Httpclient.DELETE Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::options_v2"]
    ; ins = []
    ; p = params_no_body
    ; r = TObj
    ; d =
        "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f =
        wrapped_call_no_body Httpclient.OPTIONS Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::head_v2"]
    ; ins = []
    ; p = params_no_body
    ; r = TObj
    ; d =
        "Make blocking HTTP HEAD call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f = wrapped_call_no_body Httpclient.HEAD Dval.to_pretty_machine_json_v1
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::patch_v2"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d =
        "Make blocking HTTP PATCH call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f = wrapped_call Httpclient.PATCH Dval.to_pretty_machine_json_v1
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
