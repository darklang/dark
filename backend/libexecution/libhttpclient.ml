open Core_kernel
open Runtime
open Lib
open Types.RuntimeT

let params =
  [par "uri" TStr; par "body" TAny; par "query" TObj; par "headers" TObj]


type headers = (string * string) list

let fns : Lib.shortfn list =
  [ { pns = ["HttpClient::post"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP POST call to `uri`"
    ; f = NotClientAvailable
    ; pr = None
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::put"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP PUT call to `uri`"
    ; f = NotClientAvailable
    ; pr = None
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::get"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP GET call to `uri`"
    ; f = NotClientAvailable
    ; pr = None
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::delete"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP DELETE call to `uri`"
    ; f = NotClientAvailable
    ; pr = None
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::options"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP OPTIONS call to `uri`"
    ; f = NotClientAvailable
    ; pr = None
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::head"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP HEAD call to `uri`"
    ; f = NotClientAvailable
    ; pr = None
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::patch"]
    ; ins = []
    ; p = params
    ; r = TObj
    ; d = "Make blocking HTTP PATCH call to `uri`"
    ; f = NotClientAvailable
    ; pr = None
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::formContentType"]
    ; ins = []
    ; p = []
    ; r = TObj
    ; d = ""
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
    ; pr = None
    ; ps = true
    ; dep = false }
  ; { pns = ["HttpClient::jsonContentType"]
    ; ins = []
    ; p = []
    ; r = TObj
    ; d = ""
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
    ; pr = None
    ; ps = true
    ; dep = false }
  ; { pns = ["HttpClient::plainTextContentType"]
    ; ins = []
    ; p = []
    ; r = TObj
    ; d = ""
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
    ; pr = None
    ; ps = true
    ; dep = false }
  ; { pns = ["HttpClient::htmlContentType"]
    ; ins = []
    ; p = []
    ; r = TObj
    ; d = ""
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
    ; pr = None
    ; ps = true
    ; dep = false }
  ; { pns = ["HttpClient::basicAuth"]
    ; ins = []
    ; p = [par "username" TStr; par "password" TStr]
    ; r = TObj
    ; d = ""
    ; f = NotClientAvailable
    ; pr = None
    ; ps = false
    ; dep = false }
  ; { pns = ["HttpClient::bearerToken"]
    ; ins = []
    ; p = [par "token" TStr]
    ; r = TObj
    ; d = ""
    ; f =
        InProcess
          (function
          | _, [DStr token] ->
              let auth_string = "Bearer " ^ token in
              DObj
                (DvalMap.of_alist_exn
                   [("Authorization", Dval.dstr_of_string_exn auth_string)])
          | args ->
              fail args)
    ; pr = None
    ; ps = true
    ; dep = false } ]
