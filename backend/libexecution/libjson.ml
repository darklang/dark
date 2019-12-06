open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : Lib.shortfn list =
  [ { pns = ["JSON::read"]
    ; ins = []
    ; p = [par "json" TStr]
    ; r = TAny
    ; d =
        "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
    ; f =
        InProcess
          (function
          | _, [DStr json] ->
            ( try Dval.of_unknown_json_v0 (Unicode_string.to_string json)
              with _ -> DNull )
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["JSON::read_v1"]
    ; ins = []
    ; p = [par "json" TStr]
    ; r = TAny
    ; d =
        "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
    ; f =
        InProcess
          (function
          | _, [DStr json] ->
              Dval.of_unknown_json_v1 (Unicode_string.to_string json)
          | args ->
              fail args)
    ; ps = true
    ; dep = false } ]
