module BackendOnlyStdLib.LibHttpClient2

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"
let varB = TVariable "b"

// let parameters =
//   [Param.make "uri" TStr ""; Param.make "body" varA ""; Param.make "query" TObj ""; Param.make "headers" TObj ""]
//
//
// let parametersNoBody = [Param.make "uri" TStr ""; Param.make "query" TObj ""; Param.make "headers" TObj ""]
//
let fns : List<BuiltInFn> =
  [
  // ; { name = fn "HttpClient" "post" 2
//   ; parameters = params
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP POST call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
//   ; fn =
//       Legacy.LibhttpclientV0.wrapped_call
//         Httpclient.POST
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "put" 2
//   ; parameters = params
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP PUT call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
//   ; fn =
//       Legacy.LibhttpclientV0.wrapped_call
//         Httpclient.PUT
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "get" 2
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP GET call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
//   ; fn =
//       Legacy.LibhttpclientV0.wrapped_call_no_body
//         Httpclient.GET
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "delete" 2
//   ; infix_names =
//       []
//       (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
//        * the spec says it may have a body *)
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP DELETE call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
//   ; fn =
//       Legacy.LibhttpclientV0.wrapped_call_no_body
//         Httpclient.DELETE
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "options" 2
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
//   ; fn =
//       Legacy.LibhttpclientV0.wrapped_call_no_body
//         Httpclient.OPTIONS
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "head" 2
//   ; parameters = params_no_body
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP HEAD call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
//   ; fn =
//       Legacy.LibhttpclientV0.wrapped_call_no_body
//         Httpclient.HEAD
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "patch" 2
//   ; parameters = params
//   ; returnType = TResult
//   ; description =
//       "Make blocking HTTP PATCH call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
//   ; fn =
//       Legacy.LibhttpclientV0.wrapped_call
//         Httpclient.PATCH
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
  ]
