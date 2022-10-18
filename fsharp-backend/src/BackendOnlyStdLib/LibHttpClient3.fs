/// (deprecated) v3 StdLib functions in the HttpClient module
module BackendOnlyStdLib.LibHttpClient3

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude
open System.Net.Http

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let err (str : string) = Ply(Dval.errStr str)

let incorrectArgs = LibExecution.Errors.incorrectArgs

let varA = TVariable "a"

let returnTypeOk = TVariable "result"
let returnTypeErr = TStr
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
  [ { name = fn "HttpClient" "post" 3
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP POST call to <param uri>. Returns a <type Result> object where the response <type Dict> is wrapped in {{ Ok }} if the status code is in the 2xx range, and is wrapped in {{ Error }} otherwise. Parsing errors/UTF-8 decoding errors are also {{ Error }} wrapped response objects, with a message in the <param body> and/or <param raw> fields"
      fn = LegacyHttpClient1.call HttpMethod.Post
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "post" 4) }
    { name = fn "HttpClient" "put" 3
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP PUT call to <param uri>. Returns a <type Result> object where the response <type Dict> is wrapped in {{ Ok }} if the status code is in the 2xx range, and is wrapped in {{ Error }} otherwise. Parsing errors/UTF-8 decoding errors are also {{ Error }} wrapped response objects, with a message in the <param body> and/or <param raw> fields"
      fn = LegacyHttpClient1.call HttpMethod.Put
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "put" 4) }
    { name = fn "HttpClient" "get" 3
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP GET call to <param uri>. Returns a <type Result> object where the response <type Dict> is wrapped in {{ Ok }} if the status code is in the 2xx range, and is wrapped in {{ Error }} otherwise. Parsing errors/UTF-8 decoding errors are also {{ Error }} wrapped response objects, with a message in the <param body> and/or <param raw> fields"
      fn = LegacyHttpClient1.callNoBody HttpMethod.Get
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "get" 4) }
    { name = fn "HttpClient" "delete" 3
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP DELETE call to <param uri>. Returns a <type Result> object where the response <type Dict> is wrapped in {{ Ok }} if the status code is in the 2xx range, and is wrapped in {{ Error }} otherwise. Parsing errors/UTF-8 decoding errors are also {{ Error }} wrapped response objects, with a message in the <param body> and/or <param raw> fields"
      fn = LegacyHttpClient1.callNoBody HttpMethod.Delete
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "delete" 4) }
    { name = fn "HttpClient" "options" 3
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP OPTIONS call to <param uri>. Returns a <type Result> object where the response <type Dict> is wrapped in {{ Ok }} if the status code is in the 2xx range, and is wrapped in {{ Error }} otherwise. Parsing errors/UTF-8 decoding errors are also {{ Error }} wrapped response objects, with a message in the <param body> and/or <param raw> fields"
      fn = LegacyHttpClient1.callNoBody HttpMethod.Options
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "options" 4) }
    { name = fn "HttpClient" "head" 3
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP HEAD call to <param uri>. Returns a <type Result> object where the response <type Dict> is wrapped in {{ Ok }} if the status code is in the 2xx range, and is wrapped in {{ Error }} otherwise. Parsing errors/UTF-8 decoding errors are also {{ Error }} wrapped response objects, with a message in the <param body> and/or <param raw> fields"
      fn = LegacyHttpClient1.callNoBody HttpMethod.Head
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "head" 4) }
    { name = fn "HttpClient" "patch" 3
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP PATCH call to <param uri>. Returns a <type Result> object where the response <type Dict> is wrapped in {{ Ok }} if the status code is in the 2xx range, and is wrapped in {{ Error }} otherwise. Parsing errors/UTF-8 decoding errors are also {{ Error }} wrapped response objects, with a message in the <param body> and/or <param raw> fields"
      fn = LegacyHttpClient1.call HttpMethod.Patch
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "patch" 4) } ]
