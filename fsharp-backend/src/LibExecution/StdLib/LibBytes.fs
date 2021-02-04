module LibExecution.StdLib.LibBytes

open LibExecution.RuntimeTypes
open Prelude

let fn = FQFnName.stdlibName

let fns : List<BuiltInFn> =
  [ { name = fn "Bytes" "base64Encode" 0
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TStr
      description =
        "Base64URL encodes `bytes` with `=` padding. Uses URL-safe encoding with `-` and `_` instead of `+` and `/`, as defined in RFC 4648 section 5."
      fn =
        (function
        | _, [ DBytes bytes ] ->
            System.Convert.ToBase64String(bytes).Replace('+', '-').Replace('/', '_')
            |> DStr
            |> Value
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    // { name = fn "Bytes" "hexEncode" 0
    //
    //   parameters = [ Param.make "bytes" TBytes ]
    //   returnType = TStr
    //   description =
    //     "Hex (Base16) encodes `bytes` using an uppercase alphabet. Complies with RFC 4648 section 8."
    //   fn =
    //
    //     (function
    //     | _, [ DBytes bytes ] ->
    //         let hexUppercaseLookup = "0123456789ABCDEF"
    //         let len = Bytes.length bytes
    //         let buf = Buffer.create (len * 2)
    //         for i = 0 to len - 1 do
    //           let byte = Bytes.unsafe_get bytes i |> int_of_char
    //           Buffer.add_char
    //             buf
    //             (String.unsafe_get hexUppercaseLookup ((byte ``lsr`` 4) ``land`` 0xF))
    //           Buffer.add_char
    //             buf
    //             (String.unsafe_get hexUppercaseLookup (byte ``land`` 0xF))
    //         Buffer.contents buf |> Dval.dstr_of_string_exn
    //     | args -> incorrectArgs ())
    //   sqlSpec = NotYetImplementedTODO
    //   previewable = Pure
    //   deprecated = NotDeprecated }
    { name = fn "Bytes" "length" 0
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TInt
      description = "Length of encoded byte string"
      fn =
        (function
        | _, [ DBytes bytes ] -> bytes |> Array.length |> Dval.int |> Value
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated } ]
