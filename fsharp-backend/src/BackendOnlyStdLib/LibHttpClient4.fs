module BackendOnlyStdLib.LibHttpClient4

open LibExecution.RuntimeTypes
open Prelude
open System.Net.Http

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"

let returnTypeOk = TVariable "result"
let returnTypeErr = TVariable "error" // FSTODO
let returnType = TResult(returnTypeOk, returnTypeErr)

let parameters =
  [ Param.make "uri" TStr ""
    Param.make "body" varA ""
    Param.make "query" (TDict TStr) ""
    Param.make "headers" (TDict TStr) "" ]

let parametersNoBody =
  [ Param.make "uri" TStr ""
    Param.make "query" (TDict TStr) ""
    Param.make "headers" (TDict TStr) "" ]

let fns : List<BuiltInFn> =
  [ { name = fn "HttpClient" "post" 4
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP POST call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
      fn = LegacyLibHttp.LibhttpclientV2.call HttpMethod.Post
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "post" 5) }
    { name = fn "HttpClient" "put" 4
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP PUT call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
      fn = LegacyLibHttp.LibhttpclientV2.call HttpMethod.Put
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "put" 5) }
    { name = fn "HttpClient" "get" 4
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP GET call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
      fn = LegacyLibHttp.LibhttpclientV2.callNoBody HttpMethod.Get
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "get" 5) }
    { name = fn "HttpClient" "delete" 4
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP DELETE call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
      fn = LegacyLibHttp.LibhttpclientV2.callNoBody HttpMethod.Delete
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "delete" 5) }
    { name = fn "HttpClient" "options" 4
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
      fn = LegacyLibHttp.LibhttpclientV2.callNoBody HttpMethod.Options
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "options" 5) }
    { name = fn "HttpClient" "head" 4
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP HEAD call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
      fn = LegacyLibHttp.LibhttpclientV2.callNoBody HttpMethod.Head
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "head" 5) }
    { name = fn "HttpClient" "patch" 4
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP PATCH call to `uri`. Returns a `Result` object where the response object is wrapped in `Ok` if the status code is in the 2xx range, and is wrapped in `Error` otherwise. Parsing errors/UTF-8 decoding errors are also `Error` wrapped response objects, with a message in the `body` and/or `raw` fields"
      fn = LegacyLibHttp.LibhttpclientV2.call HttpMethod.Patch
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "patch" 5) } ]
