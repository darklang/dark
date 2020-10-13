open Core_kernel
open Runtime
open Lib
open Types.RuntimeT

let fns : fn list =
  [ { name = fn "HttpClient" "formContentType" 0
    ; infix_names = []
    ; parameters = []
    ; return_type = TObj
    ; description =
        "Returns an object with 'Content-Type' for url-encoded HTML forms"
    ; func =
        InProcess
          (function
          | _, [] ->
              DObj
                (DvalMap.singleton
                   "Content-Type"
                   (Dval.dstr_of_string_exn "application/x-www-form-urlencoded"))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { name = fn "HttpClient" "jsonContentType" 0
    ; infix_names = []
    ; parameters = []
    ; return_type = TObj
    ; description = "Returns an object with 'Content-Type' for JSON"
    ; func =
        InProcess
          (function
          | _, [] ->
              DObj
                (DvalMap.singleton
                   "Content-Type"
                   (Dval.dstr_of_string_exn "application/json; charset=utf-8"))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { name = fn "HttpClient" "plainTextContentType" 0
    ; infix_names = []
    ; parameters = []
    ; return_type = TObj
    ; description = "Returns an object with 'Content-Type' for plain text"
    ; func =
        InProcess
          (function
          | _, [] ->
              DObj
                (DvalMap.singleton
                   "Content-Type"
                   (Dval.dstr_of_string_exn "text/plain; charset=utf-8"))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { name = fn "HttpClient" "htmlContentType" 0
    ; infix_names = []
    ; parameters = []
    ; return_type = TObj
    ; description = "Returns an object with 'Content-Type' for html"
    ; func =
        InProcess
          (function
          | _, [] ->
              DObj
                (DvalMap.singleton
                   "Content-Type"
                   (Dval.dstr_of_string_exn "text/html; charset=utf-8"))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { name = fn "HttpClient" "bearerToken" 0
    ; infix_names = []
    ; parameters = [par "token" TStr]
    ; return_type = TObj
    ; description =
        "Returns an object with 'Authorization' set to the passed token"
    ; func =
        InProcess
          (function
          | _, [DStr token] ->
              let auth_string =
                Unicode_string.append_broken
                  (Unicode_string.of_string_exn "Bearer ")
                  token
              in
              DObj (DvalMap.singleton "Authorization" (DStr auth_string))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated =
        true (* Deprecated due to using Unicode_string.append_broken *) }
  ; { name = fn "HttpClient" "bearerToken" 1
    ; infix_names = []
    ; parameters = [par "token" TStr]
    ; return_type = TObj
    ; description =
        "Returns an object with 'Authorization' set to the passed token"
    ; func =
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
    ; preview_safety = Safe
    ; deprecated = false } ]
