module Builtins.Pure.Libs.Bytes

open System.Text

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Bytes = LibExecution.Bytes


// Bytes dereferencing is value materialization, including reads of persistent
// bytes from the host-owned blob store. It has no call effect; see
// docs/effects.md for the local-storage boundary and its limits.
let fns () : List<BuiltInFn> =
  [

    { name = fn "bytesLength" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TInt
      description = "Returns the length of <param bytes> in bytes."
      fn =
        (function
        | _, _, _, [| DBytes ref |] -> Ply(Dval.int (bigint ref.Length))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bytesFromString" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TBytes
      description = "Encodes <param s> as UTF-8 bytes in Bytes."
      fn =
        (function
        | _, _, _, [| DString s |] ->
          let bs = System.Text.Encoding.UTF8.GetBytes(s)
          Bytes.newEphemeral bs |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bytesToString" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TypeReference.result TString TString
      description =
        "Decodes <param bytes> as UTF-8. Returns Error with a message if the bytes aren't valid UTF-8."
      fn =
        let ok r = Dval.resultOk KTString KTString r
        let err r = Dval.resultError KTString KTString r
        (function
        | state, _, _, [| DBytes ref |] ->
          Bytes.withBytes state ref (fun bs ->
            try
              let s = (new System.Text.UTF8Encoding(false, true)).GetString(bs)
              Ply(ok (DString s))
            with e ->
              Ply(err (DString($"Invalid UTF-8: {e.Message}"))))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bytesToHex" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TString
      description =
        "Hex (Base16) encodes <param bytes> using an uppercase alphabet. "
        + "Complies with [RFC 4648 section "
        + "8](https://www.rfc-editor.org/rfc/rfc4648.html#section-8)."
      fn =
        (function
        | state, _, _, [| DBytes ref |] ->
          Bytes.withBytes state ref (fun bs ->
            Ply(DString(System.Convert.ToHexString(bs))))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bytesPeek" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes ""; Param.make "count" TInt "" ]
      returnType = TypeReference.option TBytes
      description =
        "The first <param count> bytes of <param bytes>, or None when the bytes "
        + "are saved. Never reads the store, so it is cheap enough to call while "
        + "printing a value."
      fn =
        (function
        | _, _, _, [| DBytes(Ephemeral eph); DInt count |] ->
          let n =
            max (bigint 0) (min (DarkInt.toBigInt count) (bigint eph.bytes.Length))
          let prefix = Array.truncate (int n) eph.bytes
          Ply(Dval.optionSome KTBytes (Bytes.newEphemeral prefix))
        | _, _, _, [| DBytes(Persistent _); DInt _ |] -> Ply(Dval.optionNone KTBytes)
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bytesFromHex" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TypeReference.result TBytes TString
      description =
        "Parses <param s> as an uppercase or lowercase hex string into Bytes."
      fn =
        let ok b = Dval.resultOk KTBytes KTString b
        let err s = Dval.resultError KTBytes KTString (DString s)
        (function
        | _, _, _, [| DString s |] ->
          try
            let bs = System.Convert.FromHexString(s)
            ok (Bytes.newEphemeral bs) |> Ply
          with e ->
            err $"Invalid hex string: {e.Message}" |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bytesToBase64" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TString
      description =
        "Base64 encodes <param bytes> using the standard alphabet (RFC 4648 section 4) with {{=}} padding."
      fn =
        (function
        | state, _, _, [| DBytes ref |] ->
          Bytes.withBytes state ref (fun bs ->
            Ply(DString(System.Convert.ToBase64String(bs))))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bytesFromBase64" 0
      typeParams = []
      parameters = [ Param.make "s" TString "" ]
      returnType = TypeReference.result TBytes TString
      description =
        "Parses <param s> as a Base64 string (standard or URL-safe alphabet; padding optional) into Bytes."
      fn =
        let ok b = Dval.resultOk KTBytes KTString b
        let err s = Dval.resultError KTBytes KTString (DString s)
        (function
        | _, _, _, [| DString s |] ->
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
            ok (Bytes.newEphemeral bs) |> Ply
          with e ->
            err $"Invalid base64 string: {e.Message}" |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bytesConcat" 0
      typeParams = []
      parameters = [ Param.make "values" (TList TBytes) "" ]
      returnType = TBytes
      description = "Concatenates a list of Bytes into a single Bytes."
      fn =
        (function
        | state, _, _, [| DList(_, items) |] ->
          uply {
            use collected = new System.IO.MemoryStream()
            for item in items do
              match item with
              | DBytes ref ->
                let! bs = Bytes.readBytes state ref
                collected.Write(bs, 0, bs.Length)
              | _ ->
                return
                  Exception.raiseInternal
                    "bytesConcat: expected DBytes"
                    [ "item", item ]
            return Bytes.newEphemeral (collected.ToArray())
          }
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // TODO copy-free slicing via a `Subblob` BytesRef variant (see
    // RuntimeTypes.fs:BytesRef). Today's slice always allocates a
    // fresh byte[]; fine at current sizes, wasteful for 100MB chunked
    // processing. Reopen when a profile flags slice-copy as hot.
    { name = fn "bytesSlice" 0
      typeParams = []
      parameters =
        [ Param.make "bytes" TBytes ""
          Param.make "start" TInt "zero-based, inclusive"
          Param.make "length" TInt "0 or more" ]
      returnType = TBytes
      description =
        "Copies <param length> bytes starting at <param start> into Bytes. Out-of-range slices are clamped."
      fn =
        (function
        | state, _, _, [| DBytes ref; DInt startD; DInt lenD |] ->
          Bytes.withBytes state ref (fun bs ->
            let len = bigint bs.Length
            let safeStart = max (bigint 0) (min (DarkInt.toBigInt startD) len)
            let safeLen =
              max (bigint 0) (min (DarkInt.toBigInt lenD) (len - safeStart))
            let slice = Array.zeroCreate<byte> (int safeLen)
            if safeLen > bigint 0 then
              System.Array.Copy(bs, int safeStart, slice, 0, int safeLen)
            Ply(Bytes.newEphemeral slice))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bytesToList" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TList TUInt8
      description =
        "Expands <param bytes> to a List<UInt8>, copying every byte."
      fn =
        (function
        | state, _, _, [| DBytes ref |] ->
          Bytes.withBytes state ref (fun bs -> Ply(Dval.byteArrayToDvalList bs))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bytesFromList" 0
      typeParams = []
      parameters = [ Param.make "bytes" (TList TUInt8) "" ]
      returnType = TBytes
      description =
        "Collects a List<UInt8> into Bytes, copying every byte."
      fn =
        (function
        | _, _, _, [| DList(_, items) |] ->
          let bs = Dval.dlistToByteArray items
          Bytes.newEphemeral bs |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]

// TODO `bytesEmpty` is the only BuiltInValue we ship. The pattern is
// general — `intMaxInt64`, `uuidNil`, etc. could all live here — but
// today it's a sample size of one, so the surrounding machinery isn't
// well-exercised. Add a second use case before relying on it for
// anything load-bearing.
//
// TODO general "Builtin bytes constant" pattern: a `seeds : List<byte[]>`
// field on Builtins, with LocalExec-startup hashing + INSERT OR IGNORE
// into `package_blobs`. Lets `Builtin.someBytesConstant` ship bytes from
// F# source without per-constant plumbing. Worth doing once we have
// a second non-empty blob constant to motivate it.
let values () : List<BuiltInValue> =
  [ { name = LibExecution.Builtin.Shortcuts.value "bytesEmpty" 0
      typ = TBytes
      description = "The empty Bytes (zero bytes)."
      deprecated = NotDeprecated
      body = DBytes(Persistent(Bytes.emptyHash, 0L)) } ]


let builtins () = LibExecution.Builtin.make (values ()) (fns ())
