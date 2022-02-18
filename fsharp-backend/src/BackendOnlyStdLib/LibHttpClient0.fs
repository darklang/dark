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

module PrettyRequestJson =
  // This was a terrible format, I'm not even sure why this is called json
  let rec toPrettyRequestJson (dv : Dval) : string =
    let rec to_repr_ (indent : int) (dv : Dval) : string =
      let nl = "\n" + String.replicate indent " "
      let inl = "\n" + String.replicate (indent + 2) " "
      let indent = indent + 2
      match dv with
      | DFnVal _ -> "block"
      | DIncomplete _ -> "<incomplete: <incomplete>>"
      | DError _ -> "<error: error>"
      | DDate d -> $"<date: {(DDateTime.toInstant d).toIsoString ()}>"
      | DDB dbname -> $"<datastore: {dbname}>"
      | DUuid uuid -> $"<uuid: {uuid}>"
      | DPassword _ -> "<password: <password>>"
      | DInt i -> string i
      | DFloat f -> LibExecution.DvalReprExternal.ocamlStringOfFloat f
      | DBool true -> "true"
      | DBool false -> "false"
      | DNull -> "null"
      | DChar c -> $"'{c}'"
      | DStr str -> $"\"{str}\""
      | DHttpResponse (Redirect url) -> $"302 {url}{nl}null"
      | DHttpResponse (Response (code, headers, body)) ->
        let headerString =
          headers
          |> List.map (fun (k, v) -> k + ": " + v)
          |> String.concat ","
          |> fun s -> "{ " + s + " }"
        $"{code} {headerString}{nl}{to_repr_ indent body}"
      | DList l ->
        if l = [] then
          "[]"
        else
          "[ " + inl + String.concat ", " (List.map (to_repr_ indent) l) + nl + "]"
      | DObj o ->
        if Map.empty = o then
          "{}"
        else
          let strs =
            Map.fold
              (fun l key value -> (key + ": " + to_repr_ indent value) :: l)
              []
              o
          "{ " + inl + String.concat ("," + inl) strs + nl + "}"
      | DOption None -> "Nothing"
      | DOption (Some dv) -> "Just " + to_repr_ indent dv
      | DErrorRail dv -> "ErrorRail: " + to_repr_ indent dv
      | DResult _ ->
        Exception.raiseDeveloper
          "Unknown Err: (Failure \"printing an unprintable value:<result>\")"
      | DBytes _ ->
        Exception.raiseDeveloper
          "Unknown Err: (Failure \"printing an unprintable value:<bytes>\")"
    to_repr_ 0 dv



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
