/// v5 StdLib functions in the HttpClient module
module BackendOnlyStdLib.LibHttpClient5

open System.Net.Http

open Prelude
open LibExecution.RuntimeTypes

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let incorrectArgs = Errors.incorrectArgs

let varA = TVariable "a"
let returnTypeOk = HttpClient.responseType
let returnTypeErr = HttpClient.responseType
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
  [ { name = fn "HttpClient" "post" 5
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP POST call to <param uri>. Returns a <type Result> object where the response <type Dict> is wrapped in {{ Ok }} if the status code is in the 2xx range, and is wrapped in {{ Error }} otherwise. Parsing errors/UTF-8 decoding errors are also {{ Error }} wrapped response <type Dict>s, with a message in the <param body> and/or <param raw> fields"
      fn = HttpClient.call HttpMethod.Post
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "HttpClient" "put" 5
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP PUT call to <param uri>. Returns a <type Result> object where the response <type Dict> is wrapped in {{ Ok }} if the status code is in the 2xx range, and is wrapped in {{ Error }} otherwise. Parsing errors/UTF-8 decoding errors are also {{ Error }} wrapped response <type Dict>s, with a message in the <param body> and/or <param raw> fields"
      fn = HttpClient.call HttpMethod.Put
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "HttpClient" "get" 5
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP GET call to <param uri>. Returns a <type Result> object where the response <type Dict> is wrapped in {{ Ok }} if the status code is in the 2xx range, and is wrapped in {{ Error }} otherwise. Parsing errors/UTF-8 decoding errors are also {{ Error }} wrapped response <type Dict>s, with a message in the <param body> and/or <param raw> fields"
      fn = HttpClient.callNoBody HttpMethod.Get
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "HttpClient" "delete" 5
      // https://developer.mozilla.org/en-US/docs/Web/HTTP/Methods/DELETE the spec
      // says it may have a body
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP DELETE call to <param uri>. Returns a <type Result> object where the response <type Dict> is wrapped in {{ Ok }} if the status code is in the 2xx range, and is wrapped in {{ Error }} otherwise. Parsing errors/UTF-8 decoding errors are also {{ Error }} wrapped response <type Dict>s, with a message in the <param body> and/or <param raw> fields"
      fn = HttpClient.callNoBody HttpMethod.Delete
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "HttpClient" "options" 5
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP OPTIONS call to <param uri>. Returns a <type Result> object where the response <type Dict> is wrapped in {{ Ok }} if the status code is in the 2xx range, and is wrapped in {{ Error }} otherwise. Parsing errors/UTF-8 decoding errors are also {{ Error }} wrapped response <type Dict>s, with a message in the <param body> and/or <param raw> fields"
      fn = HttpClient.callNoBody HttpMethod.Options
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "HttpClient" "head" 5
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP HEAD call to <param uri>. Returns a <type Result> object where the response <type Dict> is wrapped in {{ Ok }} if the status code is in the 2xx range, and is wrapped in {{ Error }} otherwise. Parsing errors/UTF-8 decoding errors are also {{ Error }} wrapped response <type Dict>s, with a message in the <param body> and/or <param raw> fields"
      fn = HttpClient.callNoBody HttpMethod.Head
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "HttpClient" "patch" 5
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP PATCH call to <param uri>. Returns a <type Result> object where the response <type Dict> is wrapped in {{ Ok }} if the status code is in the 2xx range, and is wrapped in {{ Error }} otherwise. Parsing errors/UTF-8 decoding errors are also {{ Error }} wrapped response <type Dict>s, with a message in the <param body> and/or <param raw> fields"
      fn = HttpClient.call HttpMethod.Patch
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = NotDeprecated } ]
