open Core_kernel
open Runtime
open Lib
open Types.RuntimeT

let params_no_body = [par "uri" TStr; par "query" TObj; par "headers" TObj]

let params =
  [par "uri" TStr; par "body" TAny; par "query" TObj; par "headers" TObj]


type headers = (string * string) list

let fns : Lib.shortfn list =
  [ { pns = ["HttpClient::post"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP POST call to `uri`. Uses broken JSON format"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::put"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP PUT call to `uri`. Uses broken JSON format"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::get"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP GET call to `uri`. Uses broken JSON format"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::delete"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP DELETE call to `uri`. Uses broken JSON format"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::options"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP OPTIONS call to `uri`. Uses broken JSON format"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::head"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP HEAD call to `uri`. Uses broken JSON format"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::patch"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP PATCH call to `uri`. Uses broken JSON format"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::post_v1"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP POST call to `uri`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::put_v1"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP PUT call to `uri`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::get_v1"]
    ; ins = []
    ; p = params_no_body
    ; r = TObj
    ; d = "Make blocking HTTP GET call to `uri`"
    ; f = NotClientAvailable
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
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::options_v1"]
    ; ins = []
    ; p = params_no_body
    ; r = TObj
    ; d = "Make blocking HTTP OPTIONS call to `uri`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::head_v1"]
    ; ins = []
    ; p = params_no_body
    ; r = TObj
    ; d = "Make blocking HTTP HEAD call to `uri`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::patch_v1"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP PATCH call to `uri`"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::post_v2"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d =
        "Make blocking HTTP POST call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::put_v2"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d =
        "Make blocking HTTP PUT call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::get_v2"]
    ; ins = []
    ; p = params_no_body
    ; r = TResult
    ; d =
        "Make blocking HTTP GET call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::delete_v2"]
    ; ins =
        []
        (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
         * the spec says it may have a body *)
    ; p = params_no_body
    ; r = TObj
    ; d =
        "Make blocking HTTP DELETE call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::options_v2"]
    ; ins = []
    ; p = params_no_body
    ; r = TObj
    ; d =
        "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::head_v2"]
    ; ins = []
    ; p = params_no_body
    ; r = TObj
    ; d =
        "Make blocking HTTP HEAD call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::patch_v2"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d =
        "Make blocking HTTP PATCH call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is also a response Obj if not successful"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = true }
  ; { pns = ["HttpClient::post_v3"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d =
        "Make blocking HTTP POST call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is also a response Obj if not successful"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::put_v3"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d =
        "Make blocking HTTP PUT call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is also a response Obj if not successful"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::get_v3"]
    ; ins = []
    ; p = params_no_body
    ; r = TResult
    ; d =
        "Make blocking HTTP GET call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is also a response Obj if not successful"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::delete_v3"]
    ; ins =
        []
        (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
         * the spec says it may have a body *)
    ; p = params_no_body
    ; r = TObj
    ; d =
        "Make blocking HTTP DELETE call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is also a response Obj if not successful"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::options_v3"]
    ; ins = []
    ; p = params_no_body
    ; r = TObj
    ; d =
        "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is also a response Obj if not successful"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::head_v3"]
    ; ins = []
    ; p = params_no_body
    ; r = TObj
    ; d =
        "Make blocking HTTP HEAD call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is also a response Obj if not successful"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::patch_v3"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d =
        "Make blocking HTTP PATCH call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is also a response Obj if not successful"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::formContentType"]
    ; ins = []
    ; p = []
    ; r = TObj
    ; d = "Returns an object with 'Content-Type' for url-encoded HTML forms"
    ; f =
        InProcess
          (function
          | _, [] ->
              DObj
                (DvalMap.of_alist_exn
                   [ ( "Content-Type"
                     , Dval.dstr_of_string_exn
                         "application/x-www-form-urlencoded" ) ])
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["HttpClient::jsonContentType"]
    ; ins = []
    ; p = []
    ; r = TObj
    ; d = "Returns an object with 'Content-Type' for JSON"
    ; f =
        InProcess
          (function
          | _, [] ->
              DObj
                (DvalMap.of_alist_exn
                   [ ( "Content-Type"
                     , Dval.dstr_of_string_exn
                         "application/json; charset=utf-8" ) ])
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["HttpClient::plainTextContentType"]
    ; ins = []
    ; p = []
    ; r = TObj
    ; d = "Returns an object with 'Content-Type' for plain text"
    ; f =
        InProcess
          (function
          | _, [] ->
              DObj
                (DvalMap.of_alist_exn
                   [ ( "Content-Type"
                     , Dval.dstr_of_string_exn "text/plain; charset=utf-8" ) ])
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["HttpClient::htmlContentType"]
    ; ins = []
    ; p = []
    ; r = TObj
    ; d = "Returns an object with 'Content-Type' for html"
    ; f =
        InProcess
          (function
          | _, [] ->
              DObj
                (DvalMap.of_alist_exn
                   [ ( "Content-Type"
                     , Dval.dstr_of_string_exn "text/html; charset=utf-8" ) ])
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["HttpClient::basicAuth"]
    ; ins = []
    ; p = [par "username" TStr; par "password" TStr]
    ; r = TObj
    ; d =
        "Returns an object with 'Authorization' created using HTTP basic auth"
    ; f = NotClientAvailable
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::bearerToken"]
    ; ins = []
    ; p = [par "token" TStr]
    ; r = TObj
    ; d = "Returns an object with 'Authorization' set to the passed token"
    ; f =
        InProcess
          (function
          | _, [DStr token] ->
              let auth_string =
                Unicode_string.append
                  (Unicode_string.of_string_exn "Bearer ")
                  token
              in
              DObj (DvalMap.of_alist_exn [("Authorization", DStr auth_string)])
          | args ->
              fail args)
    ; ps = true
    ; dep = false } ]
