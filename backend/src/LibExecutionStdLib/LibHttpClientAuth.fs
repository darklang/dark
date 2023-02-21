/// StdLib HttpClient Auth functions
module LibExecutionStdLib.LibHttpClientAuth

open FSharpPlus

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let incorrectArgs = Errors.incorrectArgs

/// Base64-encodes username/password combination for basic authentication
///
/// Deprecated in favor of [encodeBasicAuth u p] due to using non-unicode append
let encodeBasicAuthBroken (u : string) (p : string) : string =
  let input : byte [] =
    if u.Contains("-") then
      Exception.raiseCode "Username cannot contain a hyphen"
    else
      ([ (System.Text.Encoding.UTF8.GetBytes u)
         [| byte ':' |]
         (System.Text.Encoding.UTF8.GetBytes p) ]
       |> Array.concat)

  $"Basic {Base64.defaultEncodeToString input}"

/// Base64-encodes username/password combination for basic authentication
let encodeBasicAuth (u : string) (p : string) : string =
  let input : byte [] =
    if u.Contains("-") then
      Exception.raiseCode "Username cannot contain a hyphen"
    else
      toBytes $"{u}:{p}"

  $"Basic {Base64.defaultEncodeToString input}"

let fns : List<BuiltInFn> =
  [ { name = fn "HttpClient" "basicAuth" 1
      parameters = [ Param.make "username" TStr ""; Param.make "password" TStr "" ]
      returnType = TDict TStr
      description =
        "Returns a header <type Dict> with {{'Authorization'}} created using HTTP basic auth"
      fn =
        (function
        | _, [ DStr u; DStr p ] ->
          Ply(DObj(Map [ "Authorization", (DStr(encodeBasicAuth u p)) ]))
        | _ -> incorrectArgs ())
      previewable = Pure
      sqlSpec = NotQueryable
      deprecated = NotDeprecated } ]
