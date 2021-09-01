module BackendOnlyStdLib.LibHttpClient0

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

let fns : List<BuiltInFn> =
  [
  // [ { name = fn "HttpClient" "post" 0
//
//   ; parameters = parameters
//   ; returnType = TObj
//   ; description =
//       "Make blocking HTTP POST call to `uri`. Uses broken JSON format"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.POST
//         Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "put" 0
//   ; parameters = parameters
//   ; returnType = TObj
//   ; description =
//       "Make blocking HTTP PUT call to `uri`. Uses broken JSON format"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.PUT
//         Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "get" 0
//   ; parameters = parameters
//   ; returnType = TObj
//   ; description =
//       "Make blocking HTTP GET call to `uri`. Uses broken JSON format"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.GET
//         Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "delete" 0
//   ; parameters = parameters
//   ; returnType = TObj
//   ; description =
//       "Make blocking HTTP DELETE call to `uri`. Uses broken JSON format"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.DELETE
//         Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "options" 0
//   ; parameters = parameters
//   ; returnType = TObj
//   ; description =
//       "Make blocking HTTP OPTIONS call to `uri`. Uses broken JSON format"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.OPTIONS
//         Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "head" 0
//   ; parameters = parameters
//   ; returnType = TObj
//   ; description =
//       "Make blocking HTTP HEAD call to `uri`. Uses broken JSON format"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.HEAD
//         Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
// ; { name = fn "HttpClient" "patch" 0
//   ; parameters = parameters
//   ; returnType = TObj
//   ; description =
//       "Make blocking HTTP PATCH call to `uri`. Uses broken JSON format"
//   ; fn =
//       Legacy.LibhttpclientV0.call
//         Httpclient.PATCH
//         Libexecution.Legacy.PrettyRequestJsonV0.to_pretty_request_json_v0
//   ; sqlSpec = NotYetImplementedTODO
//   ; previewable = Impure
//   ; deprecated = ReplacedBy(fn "" "" 0) }
  ]
