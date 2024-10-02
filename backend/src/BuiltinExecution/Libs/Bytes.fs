module BuiltinExecution.Libs.Bytes

open System.Text

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts


let fns : List<BuiltInFn> =
  [ { name = fn "bytesHexEncode" 0
      typeParams = []
      parameters = [ Param.make "bytes" (TList TUInt8) "" ]
      returnType = TString
      description =
        "Hex (Base16) encodes <param bytes> using an uppercase alphabet. Complies
        with [RFC 4648 section 8](https://www.rfc-editor.org/rfc/rfc4648.html#section-8)."
      fn =
        (function
        | _, _, _, [ DList(_, bytes) ] ->
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

let builtins = LibExecution.Builtin.make [] fns
