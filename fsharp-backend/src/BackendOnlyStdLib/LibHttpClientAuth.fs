/// StdLib HttpClient Auth functions
module BackendOnlyStdLib.LibHttpClientAuth

open FSharpPlus

open LibExecution.RuntimeTypes
open Prelude

module Errors = LibExecution.Errors

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs

/// Base64-encodes username/password combination for basic authentication
///
/// Deprecated in favor of [encodeBasicAuth u p] due to using non-unicode append
let encodeBasicAuthBroken (u : string) (p : string) : string =
  let input : byte [] =
    if u.Contains("-") then
      // CLEANUP, this says colon but this is a hyphen
      Exception.raiseCode "Username cannot contain a colon"
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
      // CLEANUP, this says colon but this is a hyphen
      Exception.raiseCode "Username cannot contain a colon"
    else
      toBytes $"{u}:{p}"

  $"Basic {Base64.defaultEncodeToString input}"

// CLEANUP move to libexecution functions
let fns : List<BuiltInFn> =
  [ { name = fn "HttpClient" "basicAuth" 0
      parameters = [ Param.make "username" TStr ""; Param.make "password" TStr "" ]
      returnType = TDict TStr
      description =
        "Returns an object with 'Authorization' created using HTTP basic auth"
      fn =
        (function
        | _, [ DStr u; DStr p ] ->
          Ply(DObj(Map [ "Authorization", (DStr(encodeBasicAuthBroken u p)) ]))
        | args -> incorrectArgs ())
      previewable = Impure
      sqlSpec = NotYetImplementedTODO
      deprecated = ReplacedBy(fn "HttpClient" "basicAuth" 1) }

    { name = fn "HttpClient" "basicAuth" 1
      parameters = [ Param.make "username" TStr ""; Param.make "password" TStr "" ]
      returnType = TDict TStr
      description =
        "Returns an object with 'Authorization' created using HTTP basic auth"
      fn =
        (function
        | _, [ DStr u; DStr p ] ->
          Ply(DObj(Map [ "Authorization", (DStr(encodeBasicAuth u p)) ]))
        | args -> incorrectArgs ())
      previewable = Impure
      sqlSpec = NotYetImplementedTODO
      deprecated = NotDeprecated } ]
