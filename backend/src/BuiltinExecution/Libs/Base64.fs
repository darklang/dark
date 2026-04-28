module BuiltinExecution.Libs.Base64

open System.Text
open System.Text.RegularExpressions

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Blob = LibExecution.Blob


let fns () : List<BuiltInFn> =
  [ { name = fn "base64Decode" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TypeReference.result TBlob TString
      description =
        "Base64 decodes a string. Works with both the URL-safe and standard Base64
         alphabets defined in [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648.html)
         sections [4](https://www.rfc-editor.org/rfc/rfc4648.html#section-4) and
         [5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)."
      fn =
        let resultOk r = Dval.resultOk KTBlob KTString r |> Ply
        let resultError r = Dval.resultError KTBlob KTString r |> Ply
        (function
        | state, _, _, [ DString s ] ->
          let base64FromUrlEncoded (str : string) : string =
            let initial = str.Replace('-', '+').Replace('_', '/')
            let length = initial.Length

            if length % 4 = 2 then $"{initial}=="
            else if length % 4 = 3 then $"{initial}="
            else initial

          if s = "" then
            resultOk (Blob.newEphemeral state [||])
          elif Regex.IsMatch(s, @"\s") then
            // dotnet ignores whitespace but we don't allow it
            resultError (DString "Not a valid base64 string")
          else
            try
              let bytes =
                s |> base64FromUrlEncoded |> System.Convert.FromBase64String
              resultOk (Blob.newEphemeral state bytes)
            with _ ->
              resultError (DString "Not a valid base64 string")
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "base64Encode" 0
      typeParams = []
      parameters = [ Param.make "blob" TBlob "" ]
      returnType = TString
      description =
        "Base64 encodes <param blob> with {{=}} padding. Uses the standard
         alphabet defined in [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648.html)
         section [4](https://www.rfc-editor.org/rfc/rfc4648.html#section-4)."
      fn =
        (function
        | state, _, _, [ DBlob ref ] ->
          uply {
            let! bytes = Blob.readBytes state ref
            return DString(System.Convert.ToBase64String(bytes))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "base64UrlEncode" 0
      typeParams = []
      parameters = [ Param.make "blob" TBlob "" ]
      returnType = TString
      description =
        "Base64URL encodes <param blob> with {{=}} padding. Uses URL-safe encoding
         with {{-}} and {{_}} instead of {{+}} and {{/}}, as defined in RFC 4648
         section [5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)."
      fn =
        (function
        | state, _, _, [ DBlob ref ] ->
          uply {
            let! bytes = Blob.readBytes state ref
            // Differs from Base64.encodeToUrlSafe as this version has padding
            let encoded =
              System.Convert
                .ToBase64String(bytes)
                .Replace('+', '-')
                .Replace('/', '_')
            return DString encoded
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
