open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : fn list =
  [ { name = fn "JSON" "read" 0

    ; parameters = [Param.make "json" TStr]
    ; return_type = TAny
    ; description =
        "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
    ; fn =

          (function
          | _, [DStr json] ->
            ( try json |> Unicode_string.to_string |> Dval.of_unknown_json_v0
              with _ -> DNull )
          | args ->
              fail args)
    ; previewable = Pure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "JSON" "read" 1

    ; parameters = [Param.make "json" TStr]
    ; return_type = TAny
    ; description =
        "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
    ; fn =

          (function
          | _, [DStr json] ->
              json |> Unicode_string.to_string |> Dval.of_unknown_json_v1
          | args ->
              fail args)
    ; previewable = Pure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "JSON" "parse" 0

    ; parameters = [Param.make "json" TStr]
    ; return_type = TAny
    ; description =
        "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
    ; fn =

          (function
          | _, [DStr json] ->
              json |> Unicode_string.to_string |> Dval.of_unknown_json_v1
          | args ->
              fail args)
    ; previewable = Pure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "JSON" "parse" 1

    ; parameters = [Param.make "json" TStr]
    ; return_type = TResult
    ; description =
        "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
    ; fn =

          (function
          | _, [DStr json] ->
            ( try
                let dval =
                  json |> Unicode_string.to_string |> Dval.of_unknown_json_v1
                in
                DResult (ResOk dval)
              with e ->
                DResult
                  (ResError
                     (e |> Exception.exn_to_string |> Dval.dstr_of_string_exn))
            )
          | args ->
              fail args)
    ; previewable = Pure
    ; deprecated = NotDeprecated } ]
