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


    // Native because record decoders call it once per byte of every field, and
    // the Dark version, a one-byte `slice`, allocated more than reading the
    // whole buffer as a list.
    { name = fn "bytesGetAt" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes ""; Param.make "index" TInt "" ]
      returnType = TypeReference.option TUInt8
      description =
        "Returns {{Some byte}} at <param index> in <param bytes>, or {{None}} if out of bounds."
      fn =
        (function
        | state, _, _, [| DBytes ref; DInt index |] ->
          Bytes.withBytes state ref (fun bs ->
            match DarkInt.toInt64 index with
            | Some i when i >= 0L && i < int64 bs.Length ->
              Ply(Dval.optionSome KTUInt8 (DUInt8 bs[int i]))
            | _ -> Ply(Dval.optionNone KTUInt8))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // Native because a scan written in Dark allocates for every byte it reads;
    // this allocates once per call.
    { name = fn "bytesIndexOf" 0
      typeParams = []
      parameters =
        [ Param.make "bytes" TBytes ""
          Param.make "byte" TUInt8 ""
          Param.make "start" TInt "zero-based, inclusive" ]
      returnType = TypeReference.option TInt
      description =
        "Returns {{Some index}} of the first <param byte> in <param bytes> at or after <param start>, or {{None}} if there is none or <param start> is out of range."
      fn =
        (function
        | state, _, _, [| DBytes ref; DUInt8 b; DInt startD |] ->
          Bytes.withBytes state ref (fun bs ->
            match DarkInt.toInt64 startD with
            | Some start when start >= 0L && start < int64 bs.Length ->
              match System.Array.IndexOf(bs, b, int start) with
              | -1 -> Ply(Dval.optionNone KTInt)
              | i -> Ply(Dval.optionSome KTInt (Dval.int (bigint i)))
            | _ -> Ply(Dval.optionNone KTInt))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bytesToList" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes "" ]
      returnType = TList TUInt8
      description = "Expands <param bytes> to a List<UInt8>, copying every byte."
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
      description = "Collects a List<UInt8> into Bytes, copying every byte."
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
