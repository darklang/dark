module BuiltinExecution.Libs.Bytes

open System.Text

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval


/// Read the bytes of a DBlob, dereferencing ephemeral/persistent refs
/// via the ExecutionState's blob accessor. All Blob→X builtins funnel
/// through here.
let private readBlob (exeState : ExecutionState) (ref : BlobRef) : Ply<byte[]> =
  uply {
    match ref with
    | Ephemeral id ->
      let mutable bs : byte[] = null
      if exeState.blobStore.TryGetValue(id, &bs) then
        return bs
      else
        return
          Exception.raiseInternal "ephemeral blob not found in store" [ "id", id ]
    | Persistent(hash, _length) ->
      let! got = exeState.blobs.get hash
      match got with
      | Some bs -> return bs
      | None ->
        return
          Exception.raiseInternal
            "persistent blob not found in package_blobs"
            [ "hash", hash ]
  }


let fns () : List<BuiltInFn> =
  [

    { name = fn "bytesLength" 0
      typeParams = []
      parameters = [ Param.make "blob" TBlob "" ]
      returnType = TInt64
      description = "Returns the length of <param blob> in bytes."
      fn =
        (function
        | state, _, _, [ DBlob ref ] ->
          uply {
            let! bs = readBlob state ref
            return DInt64(int64 bs.Length)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "bytesFromString" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TBlob
      description = "Encodes <param s> as UTF-8 bytes in a fresh ephemeral Blob."
      fn =
        (function
        | state, _, _, [ DString s ] ->
          let bs = System.Text.Encoding.UTF8.GetBytes(s)
          Dval.newEphemeralBlob state bs |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "bytesToString" 0
      typeParams = []
      parameters = [ Param.make "blob" TBlob "" ]
      returnType = TypeReference.result TString TString
      description =
        "Decodes <param blob> as UTF-8. Returns Error with a message if the bytes aren't valid UTF-8."
      fn =
        let ok r = Dval.resultOk KTString KTString r
        let err r = Dval.resultError KTString KTString r
        (function
        | state, _, _, [ DBlob ref ] ->
          uply {
            let! bs = readBlob state ref
            try
              let s = (new System.Text.UTF8Encoding(false, true)).GetString(bs)
              return ok (DString s)
            with e ->
              return err (DString($"Invalid UTF-8: {e.Message}"))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "bytesToHex" 0
      typeParams = []
      parameters = [ Param.make "blob" TBlob "" ]
      returnType = TString
      description =
        "Hex (Base16) encodes <param blob> using an uppercase alphabet. Complies
         with [RFC 4648 section 8](https://www.rfc-editor.org/rfc/rfc4648.html#section-8)."
      fn =
        (function
        | state, _, _, [ DBlob ref ] ->
          uply {
            let! bs = readBlob state ref
            return DString(System.Convert.ToHexString(bs))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "bytesFromHex" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TypeReference.result TBlob TString
      description =
        "Parses <param s> as an uppercase or lowercase hex string into a fresh ephemeral Blob."
      fn =
        let ok b = Dval.resultOk KTBlob KTString b
        let err s = Dval.resultError KTBlob KTString (DString s)
        (function
        | state, _, _, [ DString s ] ->
          try
            let bs = System.Convert.FromHexString(s)
            ok (Dval.newEphemeralBlob state bs) |> Ply
          with e ->
            err $"Invalid hex string: {e.Message}" |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "bytesToBase64" 0
      typeParams = []
      parameters = [ Param.make "blob" TBlob "" ]
      returnType = TString
      description =
        "Base64 encodes <param blob> using the standard alphabet (RFC 4648 section 4) with {{=}} padding."
      fn =
        (function
        | state, _, _, [ DBlob ref ] ->
          uply {
            let! bs = readBlob state ref
            return DString(System.Convert.ToBase64String(bs))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "bytesFromBase64" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TypeReference.result TBlob TString
      description =
        "Parses <param s> as a Base64 string (standard or URL-safe alphabet; padding optional) into a fresh ephemeral Blob."
      fn =
        let ok b = Dval.resultOk KTBlob KTString b
        let err s = Dval.resultError KTBlob KTString (DString s)
        (function
        | state, _, _, [ DString s ] ->
          let normalized =
            // Accept URL-safe alphabet + optional padding — matches
            // the old base64Decode behaviour.
            let base0 = s.Replace('-', '+').Replace('_', '/')
            match base0.Length % 4 with
            | 2 -> base0 + "=="
            | 3 -> base0 + "="
            | _ -> base0
          try
            let bs = System.Convert.FromBase64String(normalized)
            ok (Dval.newEphemeralBlob state bs) |> Ply
          with e ->
            err $"Invalid base64 string: {e.Message}" |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "bytesConcat" 0
      typeParams = []
      parameters = [ Param.make "blobs" (TList TBlob) "" ]
      returnType = TBlob
      description =
        "Concatenates a list of Blobs into a single fresh ephemeral Blob."
      fn =
        (function
        | state, _, _, [ DList(_, items) ] ->
          uply {
            use collected = new System.IO.MemoryStream()
            for item in items do
              match item with
              | DBlob ref ->
                let! bs = readBlob state ref
                collected.Write(bs, 0, bs.Length)
              | _ ->
                return
                  Exception.raiseInternal
                    "bytesConcat: expected DBlob"
                    [ "item", item ]
            return Dval.newEphemeralBlob state (collected.ToArray())
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "bytesSlice" 0
      typeParams = []
      parameters =
        [ Param.make "blob" TBlob ""
          Param.make "start" TInt64 "zero-based, inclusive"
          Param.make "length" TInt64 "0 or more" ]
      returnType = TBlob
      description =
        "Copies <param length> bytes starting at <param start> into a fresh ephemeral Blob. Out-of-range slices are clamped."
      fn =
        (function
        | state, _, _, [ DBlob ref; DInt64 startL; DInt64 lenL ] ->
          uply {
            let! bs = readBlob state ref
            let len64 = int64 bs.Length
            let safeStart = max 0L (min startL len64)
            let safeLen = max 0L (min lenL (len64 - safeStart))
            let slice = Array.zeroCreate<byte> (int safeLen)
            if safeLen > 0L then
              System.Array.Copy(bs, int safeStart, slice, 0, int safeLen)
            return Dval.newEphemeralBlob state slice
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "bytesToList" 0
      typeParams = []
      parameters = [ Param.make "blob" TBlob "" ]
      returnType = TList TUInt8
      description =
        "Expands <param blob> to a List<UInt8>. Escape hatch for code that hasn't migrated yet — prefer the Bytes module for new code."
      fn =
        (function
        | state, _, _, [ DBlob ref ] ->
          uply {
            let! bs = readBlob state ref
            return Dval.byteArrayToDvalList bs
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "bytesFromList" 0
      typeParams = []
      parameters = [ Param.make "bytes" (TList TUInt8) "" ]
      returnType = TBlob
      description =
        "Collects a List<UInt8> into a fresh ephemeral Blob. Escape hatch for bridging old and new code."
      fn =
        (function
        | state, _, _, [ DList(_, items) ] ->
          let bs = Dval.dlistToByteArray items
          Dval.newEphemeralBlob state bs |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "bytesHexEncode" 0
      typeParams = []
      parameters = [ Param.make "bytes" (TList TUInt8) "" ]
      returnType = TString
      description = "Hex (Base16) encodes <param bytes> using an uppercase alphabet."
      fn =
        (function
        | _, _, _, [ DList(_, items) ] ->
          let bs = Dval.dlistToByteArray items
          System.Convert.ToHexString(bs) |> DString |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated =
        DeprecatedBecause
          "Use bytesToHex on a Blob instead. bytesHexEncode takes List<UInt8>, which the runtime materialises as one Dval per byte — prohibitively expensive past ~1 MB." } ]

let builtins () = LibExecution.Builtin.make [] (fns ())
