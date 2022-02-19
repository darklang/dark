module BackendOnlyStdLib.LibHttpClient1

open System.Net.Http

open LibExecution.RuntimeTypes
open Prelude
open LibExecution

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

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

let parametersNoBody =
  [ Param.make "uri" TStr ""
    Param.make "query" (TDict TStr) ""
    Param.make "headers" (TDict TStr) "" ]

let jsonFn = DvalReprExternal.toPrettyMachineJsonStringV1

type headers = (string * string) list

let fns : List<BuiltInFn> =
  [ { name = fn "HttpClient" "post" 1
      parameters = parameters
      returnType = returnType
      description = "Make blocking HTTP POST call to `uri`"
      fn = LegacyHttpClient0.call HttpMethod.Post jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "post" 2) }
    { name = fn "HttpClient" "put" 1
      parameters = parameters
      returnType = returnType
      description = "Make blocking HTTP PUT call to `uri`"
      fn = LegacyHttpClient0.call HttpMethod.Put jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "put" 2) }
    { name = fn "HttpClient" "get" 1
      parameters = parametersNoBody
      returnType = returnType
      description = "Make blocking HTTP GET call to `uri`"
      fn = LegacyHttpClient0.callNoBody HttpMethod.Get jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "get" 2) }
    { name = fn "HttpClient" "delete" 1
      parameters = parametersNoBody
      returnType = returnType
      description = "Make blocking HTTP DELETE call to `uri`"
      fn = LegacyHttpClient0.callNoBody HttpMethod.Delete jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "delete" 2) }
    { name = fn "HttpClient" "options" 1
      parameters = parametersNoBody
      returnType = returnType
      description = "Make blocking HTTP OPTIONS call to `uri`"
      fn = LegacyHttpClient0.callNoBody HttpMethod.Options jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "options" 2) }
    { name = fn "HttpClient" "head" 1
      parameters = parametersNoBody
      returnType = returnType
      description = "Make blocking HTTP HEAD call to `uri`"
      fn = LegacyHttpClient0.callNoBody HttpMethod.Head jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "head" 2) }
    { name = fn "HttpClient" "patch" 1
      parameters = parameters
      returnType = returnType
      description = "Make blocking HTTP PATCH call to `uri`"
      fn = LegacyHttpClient0.call HttpMethod.Patch jsonFn
      sqlSpec = NotQueryable
      previewable = Impure
      deprecated = ReplacedBy(fn "HttpClient" "patch" 2) } ]
