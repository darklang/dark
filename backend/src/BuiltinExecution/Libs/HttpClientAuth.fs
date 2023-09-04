/// StdLib HttpClient Auth functions
module BuiltinExecution.Libs.HttpClientAuth

open FSharpPlus

open Prelude
open LibExecution.RuntimeTypes

open LibExecution.Builtin.Shortcuts

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fns : List<BuiltInFn> =
  [ { name = fn [ "HttpClient" ] "basicAuth" 0
      typeParams = []
      parameters =
        [ Param.make "username" TString ""; Param.make "password" TString "" ]
      returnType = TTuple(TString, TString, [])
      description =
        "Returns a header <type (String*String)> with {{'authorization'}} created using HTTP basic auth"
      fn =
        (function
        | _, _, [ DString u; DString p ] ->
          /// Base64-encodes username/password combination for basic authentication
          let encodeBasicAuth (u : string) (p : string) : string =
            let input : byte[] =
              if u.Contains("-") then
                Exception.raiseCode "Username cannot contain a hyphen"
              else
                toBytes $"{u}:{p}"

            $"basic {Base64.defaultEncodeToString input}"

          Ply(DTuple(DString "authorization", DString(encodeBasicAuth u p), []))
        | _ -> incorrectArgs ())
      previewable = Pure
      sqlSpec = NotQueryable
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
