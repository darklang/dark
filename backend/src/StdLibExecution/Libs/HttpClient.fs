/// <remarks>
/// The HttpClient module is shared between this file and several LibHttpV_.fs
/// files in StdLibCloudExecution, where the impure fns live.
/// </remarks>
module StdLibExecution.Libs.HttpClient

open System.Threading.Tasks
open FSharp.Control.Tasks

open System.Numerics

open Prelude
open LibExecution.RuntimeTypes

open LibExecution.StdLib.Shortcuts


let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "HttpClient" "formContentType" 0
      typeParams = []
      parameters = []
      returnType = TDict TString
      description =
        "Returns a header <type Dict> with {{Content-Type}} set for HTML form
         requests or responses"
      fn =
        (function
        | _, _, [] ->
          Ply(
            DDict(
              Map.ofList
                [ "Content-Type", DString "application/x-www-form-urlencoded" ]
            )
          )
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "HttpClient" "jsonContentType" 0
      typeParams = []
      parameters = []
      returnType = TDict TString
      description =
        "Returns a header <type dict> with {{Content-Type}} set for JSON requests or
         responses"
      fn =
        (function
        | _, _, [] ->
          Ply(
            DDict(
              Map.ofList
                [ "Content-Type", DString "application/json; charset=utf-8" ]
            )
          )
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "HttpClient" "plainTextContentType" 0
      typeParams = []
      parameters = []
      returnType = TDict TString
      description =
        "Returns a header <type Dict> with {{'Content-Type'}} set for plain text
         requests or responses"
      fn =
        (function
        | _, _, [] ->
          Ply(
            DDict(Map.ofList [ "Content-Type", DString "text/plain; charset=utf-8" ])
          )
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "HttpClient" "htmlContentType" 0
      typeParams = []
      parameters = []
      returnType = TDict TString
      description =
        "Returns a header <type Dict> with {{'Content-Type'}} set for html requests
         or responses"
      fn =
        (function
        | _, _, [] ->
          Ply(
            DDict(Map.ofList [ "Content-Type", DString "text/html; charset=utf-8" ])
          )
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "HttpClient" "bearerToken" 0
      typeParams = []
      parameters = [ Param.make "token" TString "" ]
      returnType = TDict TString
      description =
        "Returns a header <type Dict> with {{'Authorization'}} set to <param token>"
      fn =
        (function
        | _, _, [ DString token ] ->
          let authString = "Bearer " + token
          Ply(DDict(Map.ofList [ "Authorization", DString authString ]))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types)
