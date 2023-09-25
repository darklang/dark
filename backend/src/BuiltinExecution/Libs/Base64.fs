module BuiltinExecution.Libs.Base64

open System
open System.Text
open System.Text.RegularExpressions

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval


let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let modules = [ "Base64" ]
let fn = fn modules

let fns : List<BuiltInFn> =
  [ { name = fn "decode" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TypeReference.result TBytes TString
      description =
        "Base64 decodes a string. Works with both the URL-safe and standard Base64
         alphabets defined in [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648.html)
         sections [4](https://www.rfc-editor.org/rfc/rfc4648.html#section-4) and
         [5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)."
      fn =
        let resultOk = Dval.resultOk VT.bytes VT.string
        let resultError = Dval.resultError VT.bytes VT.string
        (function
        | _, _, [ DString s ] ->
          let base64FromUrlEncoded (str : string) : string =
            let initial = str.Replace('-', '+').Replace('_', '/')
            let length = initial.Length

            if length % 4 = 2 then $"{initial}=="
            else if length % 4 = 3 then $"{initial}="
            else initial

          if s = "" then
            // This seems like we should allow it
            [||] |> DBytes |> resultOk |> Ply
          elif Regex.IsMatch(s, @"\s") then
            // dotnet ignores whitespace but we don't allow it
            "Not a valid base64 string" |> DString |> resultError |> Ply
          else
            try
              s
              |> base64FromUrlEncoded
              |> Convert.FromBase64String
              |> DBytes
              |> resultOk
              |> Ply
            with e ->
              Ply(resultError (DString("Not a valid base64 string")))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "encode" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TString
      description =
        "Base64 encodes <param bytes> with {{=}} padding. Uses the standard
         alphabet defined in [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648.html)
         section [4](https://www.rfc-editor.org/rfc/rfc4648.html#section-4)."
      fn =
        (function
        | _, _, [ DBytes bytes ] ->
          System.Convert.ToBase64String(bytes) |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "urlEncode" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TString
      description =
        "Base64URL encodes <param bytes> with {{=}} padding. Uses URL-safe encoding
         with {{-}} and {{_}} instead of {{+}} and {{/}}, as defined in RFC 4648
         section [5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)."
      fn =
        (function
        | _, _, [ DBytes bytes ] ->
          // Differs from Base64.encodeToUrlSafe as this version has padding
          System.Convert.ToBase64String(bytes).Replace('+', '-').Replace('/', '_')
          |> DString
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
