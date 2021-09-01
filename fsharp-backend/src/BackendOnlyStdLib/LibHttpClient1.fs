module BackendOnlyStdLib.LibHttpClient1

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
// type headers = (string * string) list

let fns : List<BuiltInFn> =
  [
  // ; { name = fn "HttpClient" "post" 1
//   ; parameters = parameters
//   ; returnType = TObj
//   ; description = "Make blocking HTTP POST call to `uri`"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.POST
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "put" 1
//   ; parameters = parameters
//   ; returnType = TObj
//   ; description = "Make blocking HTTP PUT call to `uri`"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.PUT
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "get" 1
//   ; parameters = parametersNoBody
//   ; returnType = TObj
//   ; description = "Make blocking HTTP GET call to `uri`"
//   ; fn =
//       Legacy.LibhttpclientV0.callNoBody
//         Httpclient.GET
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "delete" 1
//   ; infix_names =
//       []
//       (* https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE
//        * the spec says it may have a body *)
//   ; parameters = parametersNoBody
//   ; returnType = TObj
//   ; description = "Make blocking HTTP DELETE call to `uri`"
//   ; fn =
//       Legacy.LibhttpclientV0.callNoBody
//         Httpclient.DELETE
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "options" 1
//   ; parameters = parametersNoBody
//   ; returnType = TObj
//   ; description = "Make blocking HTTP OPTIONS call to `uri`"
//   ; fn =
//       Legacy.LibhttpclientV0.callNoBody
//         Httpclient.OPTIONS
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "head" 1
//   ; parameters = parametersNoBody
//   ; returnType = TObj
//   ; description = "Make blocking HTTP HEAD call to `uri`"
//   ; fn =
//       Legacy.LibhttpclientV0.callNoBody
//         Httpclient.HEAD
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "patch" 1
//   ; parameters = parameters
//   ; returnType = TObj
//   ; description = "Make blocking HTTP PATCH call to `uri`"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.PATCH
//         Dval.to_pretty_machine_json_v1
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
  ]
