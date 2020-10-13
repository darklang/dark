open Core_kernel
open Runtime
open Lib
open Types.RuntimeT

let fns : fn list =
  [ { name = fn "HttpClient" "formContentType" 0

    ; parameters = []
    ; returnType = TObj
    ; description =
        "Returns an object with 'Content-Type' for url-encoded HTML forms"
    ; fn =

          (function
          | _, [] ->
              DObj
                (DvalMap.singleton
                   "Content-Type"
                   (Dval.dstr_of_string_exn "application/x-www-form-urlencoded"))
          | args ->
              Error FnWrongType)
    ; previewable = Pure
    ; deprecated = NotDeprecated }
  ; { name = fn "HttpClient" "jsonContentType" 0

    ; parameters = []
    ; returnType = TObj
    ; description = "Returns an object with 'Content-Type' for JSON"
    ; fn =

          (function
          | _, [] ->
              DObj
                (DvalMap.singleton
                   "Content-Type"
                   (Dval.dstr_of_string_exn "application/json; charset=utf-8"))
          | args ->
              Error FnWrongType)
    ; previewable = Pure
    ; deprecated = NotDeprecated }
  ; { name = fn "HttpClient" "plainTextContentType" 0

    ; parameters = []
    ; returnType = TObj
    ; description = "Returns an object with 'Content-Type' for plain text"
    ; fn =

          (function
          | _, [] ->
              DObj
                (DvalMap.singleton
                   "Content-Type"
                   (Dval.dstr_of_string_exn "text/plain; charset=utf-8"))
          | args ->
              Error FnWrongType)
    ; previewable = Pure
    ; deprecated = NotDeprecated }
  ; { name = fn "HttpClient" "htmlContentType" 0

    ; parameters = []
    ; returnType = TObj
    ; description = "Returns an object with 'Content-Type' for html"
    ; fn =

          (function
          | _, [] ->
              DObj
                (DvalMap.singleton
                   "Content-Type"
                   (Dval.dstr_of_string_exn "text/html; charset=utf-8"))
          | args ->
              Error FnWrongType)
    ; previewable = Pure
    ; deprecated = NotDeprecated }
  ; { name = fn "HttpClient" "bearerToken" 0

    ; parameters = [Param.make "token" TStr]
    ; returnType = TObj
    ; description =
        "Returns an object with 'Authorization' set to the passed token"
    ; fn =

          (function
          | _, [DStr token] ->
              let auth_string =
                Unicode_string.append_broken
                  (Unicode_string.of_string_exn "Bearer ")
                  token
              in
              DObj (DvalMap.singleton "Authorization" (DStr auth_string))
          | args ->
              Error FnWrongType)
    ; previewable = Pure
    ; deprecated =
        true (* Deprecated due to using Unicode_string.append_broken *) }
  ; { name = fn "HttpClient" "bearerToken" 1

    ; parameters = [Param.make "token" TStr]
    ; returnType = TObj
    ; description =
        "Returns an object with 'Authorization' set to the passed token"
    ; fn =

          (function
          | _, [DStr token] ->
              let auth_string =
                Unicode_string.append
                  (Unicode_string.of_string_exn "Bearer ")
                  token
              in
              DObj (DvalMap.singleton "Authorization" (DStr auth_string))
          | args ->
              Error FnWrongType)
    ; previewable = Pure
    ; deprecated = NotDeprecated } ]
