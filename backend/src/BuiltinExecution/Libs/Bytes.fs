module BuiltinExecution.Libs.Bytes

open System
open System.Text
open System.Text.RegularExpressions

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Dval = LibExecution.Dval
module VT = ValueType

let types : List<BuiltInType> = []

let modules = [ "Bytes" ]
let fn = fn modules
let constant = constant modules

let constants : List<BuiltInConstant> =
  [ { name = constant "empty" 0
      typ = TBytes
      description = "Returns an empty list of bytes"
      body = DBytes [||]
      deprecated = NotDeprecated } ]

let fns : List<BuiltInFn> =
  [ { name = fn "base64Decode" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TypeReference.result TBytes TString
      description =
        "Base64 decodes a string. Works with both the URL-safe and standard Base64
         alphabets defined in [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648.html)
         sections [4](https://www.rfc-editor.org/rfc/rfc4648.html#section-4) and
         [5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)."
      fn =
        (function
        | _, _, [ DString s ] ->
          let okType = VT.bytes
          let errType = VT.string
          let base64FromUrlEncoded (str : string) : string =
            let initial = str.Replace('-', '+').Replace('_', '/')
            let length = initial.Length

            if length % 4 = 2 then $"{initial}=="
            else if length % 4 = 3 then $"{initial}="
            else initial

          if s = "" then
            // This seems like we should allow it
            [||] |> DBytes |> Dval.resultOk okType errType |> Ply
          elif Regex.IsMatch(s, @"\s") then
            // dotnet ignores whitespace but we don't allow it
            "Not a valid base64 string"
            |> DString
            |> Dval.resultError okType errType
            |> Ply
          else
            try
              s
              |> base64FromUrlEncoded
              |> Convert.FromBase64String
              |> DBytes
              |> Dval.resultOk okType errType
              |> Ply
            with e ->
              Ply(
                Dval.resultError
                  okType
                  errType
                  (DString("Not a valid base64 string"))
              )
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "base64Encode" 0
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
      deprecated = NotDeprecated }


    { name = fn "hexEncode" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TString
      description =
        "Hex (Base16) encodes <param bytes> using an uppercase alphabet. Complies
         with [RFC 4648 section 8](https://www.rfc-editor.org/rfc/rfc4648.html#section-8)."
      fn =
        (function
        | _, _, [ DBytes bytes ] ->
          let hexUppercaseLookup = "0123456789ABCDEF"
          let len = bytes.Length
          let buf = new StringBuilder(len * 2)

          for i = 0 to len - 1 do
            let byte = bytes[i] |> int

            buf
              .Append(hexUppercaseLookup[((byte >>> 4) &&& 0xF)])
              .Append(hexUppercaseLookup[(byte &&& 0xF)])
            |> ignore<StringBuilder>

          buf |> string |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "length" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TInt
      description = "Returns the number of bytes in <param bytes>"
      fn =
        (function
        | _, _, [ DBytes bytes ] -> bytes |> Array.length |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
