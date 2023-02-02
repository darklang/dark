module LibExecutionStdLib.LibBytes

open LibExecution.RuntimeTypes
open Prelude
open System
open System.Text

open System.Text.RegularExpressions

let fn = FQFnName.stdlibFnName

let incorrectArgs = LibExecution.Errors.incorrectArgs

let fns : List<BuiltInFn> =
  [ { name = fn "Bytes" "base64Decode" 0
      parameters = [ Param.make "s" TStr "" ]
      returnType = TResult(TBytes, TStr)
      description =
        "Base64 decodes a string. Works with both the URL-safe and standard Base64
         alphabets defined in [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648.html)
         sections [4](https://www.rfc-editor.org/rfc/rfc4648.html#section-4) and
         [5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)."
      fn =
        (function
        | _, [ DStr s ] ->
          let base64FromUrlEncoded (str : string) : string =
            let initial = str.Replace('-', '+').Replace('_', '/')
            let length = initial.Length

            if length % 4 = 2 then $"{initial}=="
            else if length % 4 = 3 then $"{initial}="
            else initial

          if s = "" then
            // This seems like we should allow it
            [||] |> DBytes |> Ok |> DResult |> Ply
          elif Regex.IsMatch(s, @"\s") then
            // dotnet ignores whitespace but we don't allow it
            "Not a valid base64 string" |> DStr |> Error |> DResult |> Ply
          else
            s
            |> base64FromUrlEncoded
            |> Convert.FromBase64String
            |> DBytes
            |> Ok
            |> DResult
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = ReplacedBy(fn "Bytes" "base64Decode" 1) }

    { name = fn "Bytes" "base64Decode" 1
      parameters = [ Param.make "s" TStr "" ]
      returnType = TResult(TBytes, TStr)
      description =
        "Base64 decodes a string. Works with both the URL-safe and standard Base64
         alphabets defined in [RFC 4648](https://www.rfc-editor.org/rfc/rfc4648.html)
         sections [4](https://www.rfc-editor.org/rfc/rfc4648.html#section-4) and
         [5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)."
      fn =
        (function
        | _, [ DStr s ] ->
          let base64FromUrlEncoded (str : string) : string =
            let initial = str.Replace('-', '+').Replace('_', '/')
            let length = initial.Length

            if length % 4 = 2 then $"{initial}=="
            else if length % 4 = 3 then $"{initial}="
            else initial

          if s = "" then
            // This seems like we should allow it
            [||] |> DBytes |> Ok |> DResult |> Ply
          elif Regex.IsMatch(s, @"\s") then
            // dotnet ignores whitespace but we don't allow it
            "Not a valid base64 string" |> DStr |> Error |> DResult |> Ply
          else
            try
              s
              |> base64FromUrlEncoded
              |> Convert.FromBase64String
              |> DBytes
              |> Ok
              |> DResult
              |> Ply
            with
            | e -> Ply(DResult(Error(DStr("Not a valid base64 string"))))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Bytes" "base64Encode" 0
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TStr
      description =
        "Base64URL encodes <param bytes> with {{=}} padding. Uses URL-safe encoding
         with {{-}} and {{_}} instead of {{+}} and {{/}}, as defined in RFC 4648
         section [5](https://www.rfc-editor.org/rfc/rfc4648.html#section-5)."
      fn =
        (function
        | _, [ DBytes bytes ] ->
          // Differs from Base64.encodeToUrlSafe as this version has padding
          System.Convert.ToBase64String(bytes).Replace('+', '-').Replace('/', '_')
          |> DStr
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Bytes" "hexEncode" 0
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TStr
      description =
        "Hex (Base16) encodes <param bytes> using an uppercase alphabet. Complies
         with [RFC 4648 section 8](https://www.rfc-editor.org/rfc/rfc4648.html#section-8)."
      fn =
        (function
        | _, [ DBytes bytes ] ->
          let hexUppercaseLookup = "0123456789ABCDEF"
          let len = bytes.Length
          let buf = new StringBuilder(len * 2)

          for i = 0 to len - 1 do
            let byte = bytes[i] |> int

            buf
              .Append(hexUppercaseLookup[((byte >>> 4) &&& 0xF)])
              .Append(hexUppercaseLookup[(byte &&& 0xF)])
            |> ignore<StringBuilder>

          buf |> string |> DStr |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "Bytes" "length" 0
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TInt
      description = "Returns the number of bytes in <param bytes>"
      fn =
        (function
        | _, [ DBytes bytes ] -> bytes |> Array.length |> Dval.int |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]
