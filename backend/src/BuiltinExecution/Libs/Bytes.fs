module BuiltinExecution.Libs.Bytes

open System
open System.Text
open System.Text.RegularExpressions

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

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
  [ { name = fn "hexEncode" 0
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
      deprecated = NotDeprecated }


    { name = fn "append" 0
      typeParams = []
      parameters = [ Param.make "bytes1" TBytes ""; Param.make "bytes2" TBytes "" ]
      returnType = TBytes
      description = "Returns the concatenation of <param bytes1> and <param bytes2>"
      fn =
        (function
        | _, _, [ DBytes bytes1; DBytes bytes2 ] ->
          bytes2 |> Array.append bytes1 |> DBytes |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "contains" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes ""; Param.make "segment" TBytes "" ]
      returnType = TBool
      description = "Returns true if <param bytes> contains <param segment>"
      fn =
        (function
        | _, _, [ DBytes bytes; DBytes segment ] ->
          let segmentLength = Array.length segment
          let limit = (Array.length bytes) - segmentLength

          let rec searchSegmentFromIndex idx =
            if idx > limit then false
            else if Array.sub bytes idx segmentLength = segment then true
            else searchSegmentFromIndex (idx + 1)

          (searchSegmentFromIndex 0) |> DBool |> Ply

        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
