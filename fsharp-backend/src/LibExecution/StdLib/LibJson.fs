module LibExecution.StdLib.LibJson

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks
open FSharpPlus

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibName

let err (str : string) = Value(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> = []
  // [ { name = fn "JSON" "read" 0
  //
  //   ; parameters = [Param.make "json" TStr ""]
  //   ; returnType = TAny
  //   ; description =
  //       "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
  //   ; fn =
  //         (function
  //         | _, [DStr json] ->
  //           ( try json |> Unicode_string.to_string |> Dval.of_unknown_json_v0
  //             with _ -> DNull )
  //         | _ ->
  //             incorrectArgs ())
  //   ; sqlSpec = NotYetImplementedTODO
  //   ; previewable = Pure
  //   ; deprecated = ReplacedBy(fn "" "" 0) }
  // ; { name = fn "JSON" "read" 1
  //   ; parameters = [Param.make "json" TStr ""]
  //   ; returnType = TAny
  //   ; description =
  //       "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
  //   ; fn =
  //         (function
  //         | _, [DStr json] ->
  //             json |> Unicode_string.to_string |> Dval.of_unknown_json_v1
  //         | _ ->
  //             incorrectArgs ())
  //   ; sqlSpec = NotYetImplementedTODO
  //   ; previewable = Pure
  //   ; deprecated = ReplacedBy(fn "" "" 0) }
  // ; { name = fn "JSON" "parse" 0
  //   ; parameters = [Param.make "json" TStr ""]
  //   ; returnType = TAny
  //   ; description =
  //       "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
  //   ; fn =
  //         (function
  //         | _, [DStr json] ->
  //             json |> Unicode_string.to_string |> Dval.of_unknown_json_v1
  //         | _ ->
  //             incorrectArgs ())
  //   ; sqlSpec = NotYetImplementedTODO
  //   ; previewable = Pure
  //   ; deprecated = ReplacedBy(fn "" "" 0) }
  // ; { name = fn "JSON" "parse" 1
  //   ; parameters = [Param.make "json" TStr ""]
  //   ; returnType = TResult
  //   ; description =
  //       "Parses a json string and returns its value. HTTPClient functions, and our request handler, automatically parse JSON into the `body` and `jsonbody` fields, so you probably won't need this. However, if you need to consume bad JSON, you can use string functions to fix the JSON and then use this function to parse it."
  //   ; fn =
  //         (function
  //         | _, [DStr json] ->
  //           ( try
  //               let dval =
  //                 json |> Unicode_string.to_string |> Dval.of_unknown_json_v1
  //               in
  //               DResult (ResOk dval)
  //             with e ->
  //               DResult
  //                 (ResError
  //                    (e |> Exception.exn_to_string |> DStr))
  //           )
  //         | _ ->
  //             incorrectArgs ())
  //   ; sqlSpec = NotYetImplementedTODO
  //   ; previewable = Pure
  //   ; deprecated = NotDeprecated } ]
