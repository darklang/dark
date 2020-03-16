open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : fn list =
  [ { prefix_names = ["JSON::read"]
    ; infix_names = []
    ; parameters = [par "json" TStr]
    ; return_type = TAny
    ; description =
        "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
    ; func =
        InProcess
          (function
          | _, [DStr json] ->
            ( try Dval.of_unknown_json_v0 (Unicode_string.to_string json)
              with _ -> DNull )
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = true }
  ; { prefix_names = ["JSON::read_v1"]
    ; infix_names = []
    ; parameters = [par "json" TStr]
    ; return_type = TAny
    ; description =
        "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
    ; func =
        InProcess
          (function
          | _, [DStr json] ->
              Dval.of_unknown_json_v1 (Unicode_string.to_string json)
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = true }
  ; { prefix_names = ["JSON::parse_v0"]
    ; infix_names = []
    ; parameters = [par "json" TStr]
    ; return_type = TAny
    ; description =
        "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
    ; func =
        InProcess
          (function
          | _, [DStr json] ->
              Dval.of_unknown_json_v1 (Unicode_string.to_string json)
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = true }
  ; { prefix_names = ["JSON::parse_v1"]
    ; infix_names = []
    ; parameters = [par "json" TStr]
    ; return_type = TResult
    ; description =
        "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
    ; func =
        InProcess
          (function
          | _, [DStr json] ->
            ( try
                let dval =
                  Dval.of_unknown_json_v1 (Unicode_string.to_string json)
                in
                DResult (ResOk dval)
              with e ->
                DResult
                  (ResError
                     (e |> Exception.exn_to_string |> Dval.dstr_of_string_exn))
            )
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false } ]
