/// (deprecated) Library functions in the form "HttpClient::xy_v2"
module BackendOnlyStdLib.LibHttpClient2

open System.Threading.Tasks
open System.Numerics
open FSharp.Control.Tasks

open LibExecution.RuntimeTypes
open Prelude
open System.Net.Http
open LibExecution

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

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

let jsonFn = DvalReprExternal.toPrettyMachineJsonStringV1

let fns : List<BuiltInFn> =
  [ { name = fn "HttpClient" "post" 2
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP POST call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
      fn = LegacyHttpClient0.wrappedCall HttpMethod.Post jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "post" 3) }
    { name = fn "HttpClient" "put" 2
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP PUT call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
      fn = LegacyHttpClient0.wrappedCall HttpMethod.Put jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "put" 3) }
    { name = fn "HttpClient" "get" 2
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP GET call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
      fn = LegacyHttpClient0.wrappedCallNoBody HttpMethod.Get jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "get" 3) }
    { name = fn "HttpClient" "delete" 2
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP DELETE call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
      fn = LegacyHttpClient0.wrappedCallNoBody HttpMethod.Delete jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "delete" 3) }
    { name = fn "HttpClient" "options" 2
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP OPTIONS call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
      fn = LegacyHttpClient0.wrappedCallNoBody HttpMethod.Options jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "options" 3) }
    { name = fn "HttpClient" "head" 2
      parameters = parametersNoBody
      returnType = returnType
      description =
        "Make blocking HTTP HEAD call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
      fn = LegacyHttpClient0.wrappedCallNoBody HttpMethod.Head jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "head" 3) }
    { name = fn "HttpClient" "patch" 2
      parameters = parameters
      returnType = returnType
      description =
        "Make blocking HTTP PATCH call to `uri`. Returns a `Result` where `Ok` is a response Obj if successful and `Error` is an error message if not successful"
      fn = LegacyHttpClient0.wrappedCall HttpMethod.Patch jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "patch" 3) } ]
