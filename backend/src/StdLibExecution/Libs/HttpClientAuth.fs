/// StdLib HttpClient Auth functions
module StdLibExecution.Libs.HttpClientAuth

open FSharpPlus

open Prelude
open LibExecution.RuntimeTypes

open LibExecution.StdLib.Shortcuts

let types : List<BuiltInType> = []

let fns : List<BuiltInFn> =
  [ { name = fn "HttpClient" "basicAuth" 1
      typeParams = []
      parameters =
        [ Param.make "username" TString ""; Param.make "password" TString "" ]
      returnType = TDict TString
      description =
        "Returns a header <type Dict> with {{'Authorization'}} created using HTTP basic auth"
      fn =
        (function
        | _, _, [ DString u; DString p ] ->
          /// Base64-encodes username/password combination for basic authentication
          let encodeBasicAuth (u : string) (p : string) : string =
            let input : byte [] =
              if u.Contains("-") then
                Exception.raiseCode "Username cannot contain a hyphen"
              else
                toBytes $"{u}:{p}"

            $"Basic {Base64.defaultEncodeToString input}"

          Ply(DDict(Map [ "Authorization", (DString(encodeBasicAuth u p)) ]))
        | _ -> incorrectArgs ())
      previewable = Pure
      sqlSpec = NotQueryable
      deprecated = NotDeprecated } ]

let contents = (fns, types)
