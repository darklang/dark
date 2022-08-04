/// StdLib functions in the HttpBytesClient module
module BackendOnlyStdLib.LibHttpBytesClient

open System.Net.Http

open Prelude
open LibExecution
open LibExecution.RuntimeTypes

module Errors = LibExecution.Errors

let incorrectArgs = Errors.incorrectArgs

let fn = FQFnName.stdlibFnName

// Converts an object to (string, string) pairs
let toStringPairs (dv : Dval) : Result<List<string * string>, string> =
  match dv with
  | DList tpls ->
    tpls
    |> List.map (fun pair ->
      match pair with
      | DTuple (DStr k, DStr v, []) ->
        // TODO trim key and/or value here?
        // TODO error on empty key?
        Ok(k, v)
      | other ->
        Error
          $"Expected a (string * string), but got: {DvalReprDeveloper.toRepr other}")
    |> Tablecloth.Result.values
  | _ -> Error $"Expected a list of tuples, but got: {DvalReprDeveloper.toRepr dv}"

let call (method : HttpMethod) =
  (function
  | _, [ DStr uri; DBytes body; query; headers ] ->
    let headers = toStringPairs headers
    let query = HttpQueryEncoding.toQuery query
    match headers, query with
    | Ok headers, Ok query ->
      HttpBytesClient.sendRequest uri method body query headers
    | _ -> incorrectArgs ()
  | _ -> incorrectArgs ())


let headersType = TList(TTuple(TStr, TStr, []))

let parameters =
  [ Param.make "uri" TStr ""
    Param.make "body" TBytes ""
    Param.make "query" (TDict TStr) ""
    Param.make "headers" headersType "" ]

let returnType =
  TResult(TRecord [ "body", TBytes; "headers", headersType; "code", TInt ], TStr)

let fns : List<BuiltInFn> =
  [ { name = fn "HttpBytesClient" "post" 0
      parameters = parameters
      returnType = returnType
      description =
        // TODO better description
        "Make blocking HTTP POST call to `uri`. Returns a `Result` object where
        the response object is wrapped in `Ok` if a response was successfully
        received and parsed, and is wrapped in `Error` otherwise"
      fn = call HttpMethod.Post
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "HttpBytesClient" "get" 0
      parameters = parameters
      returnType = returnType
      description =
        // TODO better description
        "Make blocking HTTP GET call to `uri`. Returns a `Result` object where
        the response object is wrapped in `Ok` if a response was successfully
        received and parsed, and is wrapped in `Error` otherwise"
      fn = call HttpMethod.Get
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
