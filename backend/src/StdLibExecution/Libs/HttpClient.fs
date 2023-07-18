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

let modules = [ "HttpClient" ]
let constant = constant modules
let fn = fn modules

let constants : List<BuiltInConstant> =
  [ { name = constant "formContentType" 0
      typ = TDict TString
      description =
        "Returns a header <type Dict> with {{Content-Type}} set for HTML form
        requests or responses"
      body = DDict(Map.ofList [ "Content-Type", DString "application/x-www-form-urlencoded" ])
      deprecated = NotDeprecated }

    { name = constant "plainTextContentType" 0
      typ = TDict TString
      description =
        "Returns a header <type Dict> with {{'Content-Type'}} set for plain text
        requests or responses"
      body = DDict(Map.ofList [ "Content-Type", DString "text/plain; charset=utf-8" ])
      deprecated = NotDeprecated }

    { name = constant "htmlContentType" 0
      typ = TDict TString
      description =
        "Returns a header <type Dict> with {{'Content-Type'}} set for html requests
        or responses"
      body = DDict(Map.ofList [ "Content-Type", DString "text/html; charset=utf-8" ])
      deprecated = NotDeprecated }

    { name = constant "jsonContentType" 0
      typ = TDict TString
      description =
        "Returns a header <type dict> with {{Content-Type}} set for JSON requests or
        responses"
      body = DDict(Map.ofList [ "Content-Type", DString "application/json; charset=utf-8" ])
      deprecated = NotDeprecated }

    ]

let fns : List<BuiltInFn> =
  [

    { name = fn "bearerToken" 0
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

let contents = (fns, types, constants)
