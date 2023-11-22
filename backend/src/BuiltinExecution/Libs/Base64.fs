module BuiltinExecution.Libs.Base64

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
      returnType = TypeReference.result (TList(TUInt8)) TString
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
        | _, _, [ DString s ] ->
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


    { name = fn "encode" 0
      typeParams = []
      parameters = [ Param.make "bytes" (TList(TUInt8)) "" ]
      returnType = TString
      description =
        "Base64 encodes <param bytes> with {{=}} padding. Uses the standard
         alphabet defined in [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648.html)
         section [4](https://www.rfc-editor.org/rfc/rfc4648.html#section-4)."
      fn =
        (function
        | _, _, [ DList(_vt, bytes) ] ->
          let bytes = Dval.DlistToByteArray bytes
          System.Convert.ToBase64String(bytes) |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "urlEncode" 0
      typeParams = []
      parameters = [ Param.make "bytes" (TList(TUInt8)) "" ]
      returnType = TString
      description =
        "Base64URL encodes <param bytes> with {{=}} padding. Uses URL-safe encoding
         with {{-}} and {{_}} instead of {{+}} and {{/}}, as defined in RFC 4648
         section [5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)."
      fn =
        (function
        | _, _, [ DList(_vt, bytes) ] ->
          let bytes = Dval.DlistToByteArray bytes
          // Differs from Base64.encodeToUrlSafe as this version has padding
          System.Convert.ToBase64String(bytes).Replace('+', '-').Replace('/', '_')
          |> DString
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "hexEncode" 0
      typeParams = []
      parameters = [ Param.make "bytes" (TList(TUInt8)) "" ]
      returnType = TString
      description =
        "Hex (Base16) encodes <param bytes> using an uppercase alphabet. Complies
        with [RFC 4648 section 8](https://www.rfc-editor.org/rfc/rfc4648.html#section-8)."
      fn =
        (function
        | _, _, [ DList(_, bytes) ] ->
          let hexUppercaseLookup = "0123456789ABCDEF"
          let len = bytes.Length
          let buf = new StringBuilder(len * 2)

          for i = 0 to len - 1 do
            match bytes[i] with
            | DUInt8 byte ->
              let byte = int byte
              buf
                .Append(hexUppercaseLookup[((byte >>> 4) &&& 0xF)])
                .Append(hexUppercaseLookup[(byte &&& 0xF)])
              |> ignore<StringBuilder>
            | _ -> Exception.raiseInternal "hexEncode: expected UInt8" []

          buf.ToString() |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
