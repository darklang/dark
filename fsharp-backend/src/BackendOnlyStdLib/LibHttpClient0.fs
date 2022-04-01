/// (deprecated) v0 StdLib functions in the HttpClient module
module BackendOnlyStdLib.LibHttpClient0

open System.Net.Http

open LibExecution.RuntimeTypes
open Prelude

/// Legacy "json" serialization of DVals
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
      | DDate d -> $"<date: {DDateTime.toIsoString d}>"
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
        Exception.raiseCode
          "Unknown Err: (Failure \"printing an unprintable value:<result>\")"
      | DBytes _ ->
        Exception.raiseCode
          "Unknown Err: (Failure \"printing an unprintable value:<bytes>\")"
    to_repr_ 0 dv

let fn = FQFnName.stdlibFnName
let err (str : string) = Ply(Dval.errStr str)

let varA = TVariable "a"

// CLEANUP: remove the obj type and use varA instead
// this^ comment was copied from LibDB.fs - reevaluate
let obj = TDict varA

let returnType = obj

let parameters =
  [ Param.make "uri" TStr ""
    Param.make "body" varA ""
    Param.make "query" (TDict TStr) ""
    Param.make "headers" (TDict TStr) "" ]

let jsonFn = PrettyRequestJson.toPrettyRequestJson

let fns : List<BuiltInFn> =
  [ { name = fn "HttpClient" "post" 0
      parameters = parameters
      returnType = returnType
      description = "Make blocking HTTP POST call to `uri`. Uses broken JSON format"
      fn = LegacyHttpClient0.call HttpMethod.Post jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "post" 1) }
    { name = fn "HttpClient" "put" 0
      parameters = parameters
      returnType = returnType
      description = "Make blocking HTTP PUT call to `uri`. Uses broken JSON format"
      fn = LegacyHttpClient0.call HttpMethod.Put jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "put" 1) }
    { name = fn "HttpClient" "get" 0
      parameters = parameters
      returnType = returnType
      description = "Make blocking HTTP GET call to `uri`. Uses broken JSON format"
      fn = LegacyHttpClient0.callIgnoreBody HttpMethod.Get jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "get" 1) }
    { name = fn "HttpClient" "delete" 0
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP DELETE call to `uri`. Uses broken JSON format"
      fn = LegacyHttpClient0.callIgnoreBody HttpMethod.Delete jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "delete" 1) }
    { name = fn "HttpClient" "options" 0
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP OPTIONS call to `uri`. Uses broken JSON format"
      fn = LegacyHttpClient0.callIgnoreBody HttpMethod.Options jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "options" 1) }
    { name = fn "HttpClient" "head" 0
      parameters = parameters
      returnType = returnType
      description = "Make blocking HTTP HEAD call to `uri`. Uses broken JSON format"
      fn = LegacyHttpClient0.callIgnoreBody HttpMethod.Head jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "head" 1) }
    { name = fn "HttpClient" "patch" 0
      parameters = parameters
      returnType = returnType
      description = "Make blocking HTTP PATCH call to `uri`. Uses broken JSON format"
      fn = LegacyHttpClient0.call HttpMethod.Patch jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "patch" 1) } ]
