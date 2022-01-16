module LibExecutionStdLib.LibHttpClient

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
  [ { name = fn "HttpClient" "formContentType" 0
      parameters = []
      returnType = TDict TStr
      description =
        "Returns an object with 'Content-Type' for url-encoded HTML forms"
      fn =
        (function
        | _, [] ->
          Ply(
            // CLEANUP why no charset?
            DObj(
              Map.ofList [ "Content-Type", DStr "application/x-www-form-urlencoded" ]
            )
          )
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "HttpClient" "jsonContentType" 0
      parameters = []
      returnType = TDict TStr
      description = "Returns an object with 'Content-Type' for JSON"
      fn =
        (function
        | _, [] ->
          Ply(
            DObj(
              Map.ofList [ "Content-Type", DStr "application/json; charset=utf-8" ]
            )
          )
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "HttpClient" "plainTextContentType" 0
      parameters = []
      returnType = TDict TStr
      description = "Returns an object with 'Content-Type' for plain text"
      fn =
        (function
        | _, [] ->
          Ply(DObj(Map.ofList [ "Content-Type", DStr "text/plain; charset=utf-8" ]))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "HttpClient" "htmlContentType" 0
      parameters = []
      returnType = TDict TStr
      description = "Returns an object with 'Content-Type' for html"
      fn =
        (function
        | _, [] ->
          Ply(DObj(Map.ofList [ "Content-Type", DStr "text/html; charset=utf-8" ]))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "HttpClient" "bearerToken" 0
      parameters = [ Param.make "token" TStr "" ]
      returnType = TDict TStr
      description = "Returns an object with 'Authorization' set to the passed token"
      fn =
        (function
        | _, [ DStr token ] ->
          let authString = "Bearer " + token
          Ply(DObj(Map.ofList [ "Authorization", DStr authString ]))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "HttpClient" "bearerToken" 1) }


    { name = fn "HttpClient" "bearerToken" 1
      parameters = [ Param.make "token" TStr "" ]
      returnType = TDict TStr
      description = "Returns an object with 'Authorization' set to the passed token"
      fn =
        (function
        | _, [ DStr token ] ->
          let authString = "Bearer " + token
          Ply(DObj(Map.ofList [ "Authorization", DStr authString ]))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
