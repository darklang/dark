module BuiltinExecution.Libs.Base64

open System.Text
open System.Text.RegularExpressions

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval


let fns : List<BuiltInFn> =
  [ { name = fn "base64Decode" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TypeReference.result (TList TUInt8) TString
      description =
        "Base64 decodes a string. Works with both the URL-safe and standard Base64
         alphabets defined in [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648.html)
         sections [4](https://www.rfc-editor.org/rfc/rfc4648.html#section-4) and
         [5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)."
      fn =
        let resultOk r =
          Dval.resultOk (KTList(ValueType.Known KTUInt8)) KTString r |> Ply
        let resultError r =
          Dval.resultError (KTList(ValueType.Known KTUInt8)) KTString r |> Ply
        (function
        | _, _, _, [ DString s ] ->
          let base64FromUrlEncoded (str : string) : string =
            let initial = str.Replace('-', '+').Replace('_', '/')
            let length = initial.Length

            if length % 4 = 2 then $"{initial}=="
            else if length % 4 = 3 then $"{initial}="
            else initial

          if s = "" then
            // This seems like we should allow it
            DList(VT.uint8, []) |> resultOk
          elif Regex.IsMatch(s, @"\s") then
            // dotnet ignores whitespace but we don't allow it
            "Not a valid base64 string" |> DString |> resultError
          else
            try
              s
              |> base64FromUrlEncoded
              |> System.Convert.FromBase64String
              |> Dval.byteArrayToDvalList
              |> resultOk

            with e ->
              resultError (DString("Not a valid base64 string"))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "base64Encode" 0
      typeParams = []
      parameters = [ Param.make "bytes" (TList TUInt8) "" ]
      returnType = TString
      description =
        "Base64 encodes <param bytes> with {{=}} padding. Uses the standard
         alphabet defined in [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648.html)
         section [4](https://www.rfc-editor.org/rfc/rfc4648.html#section-4)."
      fn =
        (function
        | _, _, _, [ DList(_vt, bytes) ] ->
          let bytes = Dval.dlistToByteArray bytes
          System.Convert.ToBase64String(bytes) |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "base64UrlEncode" 0
      typeParams = []
      parameters = [ Param.make "bytes" (TList TUInt8) "" ]
      returnType = TString
      description =
        "Base64URL encodes <param bytes> with {{=}} padding. Uses URL-safe encoding
         with {{-}} and {{_}} instead of {{+}} and {{/}}, as defined in RFC 4648
         section [5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)."
      fn =
        (function
        | _, _, _, [ DList(_vt, bytes) ] ->
          let bytes = Dval.dlistToByteArray bytes
          // Differs from Base64.encodeToUrlSafe as this version has padding
          System.Convert.ToBase64String(bytes).Replace('+', '-').Replace('/', '_')
          |> DString
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let builtins = LibExecution.Builtin.make [] fns
