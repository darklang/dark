open Core_kernel
open Runtime
open Lib
open Types.RuntimeT

let fns : Lib.shortfn list =
  [ { pns = ["HttpClient::formContentType"]
    ; ins = []
    ; p = []
    ; r = TObj
    ; d = "Returns an object with 'Content-Type' for url-encoded HTML forms"
    ; f =
        InProcess
          (function
          | _, [] ->
              DObj
                (DvalMap.singleton
                   "Content-Type"
                   (Dval.dstr_of_string_exn "application/x-www-form-urlencoded"))
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
                (DvalMap.singleton
                   "Content-Type"
                   (Dval.dstr_of_string_exn "application/json; charset=utf-8"))
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
                (DvalMap.singleton
                   "Content-Type"
                   (Dval.dstr_of_string_exn "text/plain; charset=utf-8"))
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
                (DvalMap.singleton
                   "Content-Type"
                   (Dval.dstr_of_string_exn "text/html; charset=utf-8"))
          | args ->
              fail args)
    ; ps = true
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
              DObj (DvalMap.singleton "Authorization" (DStr auth_string))
          | args ->
              fail args)
    ; ps = true
    ; dep = false } ]
