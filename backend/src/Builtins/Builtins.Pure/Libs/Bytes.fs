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
      returnType = TInt64
      description = "Returns the length of <param bytes> in bytes."
      fn =
        (function
        | _, _, _, [| DBytes ref |] -> Ply(Dval.dint64 ref.Length)
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "bytesPeek" 0
      typeParams = []
      parameters = [ Param.make "bytes" TBytes ""; Param.make "count" TInt64 "" ]
      returnType = TypeReference.option TBytes
      description =
        "The first <param count> bytes of <param bytes>, or None when the bytes "
        + "are saved. Never reads the store, so it is cheap enough to call while "
        + "printing a value."
      fn =
        (function
        | _, _, _, [| DBytes(Ephemeral eph); DInt64 count |] ->
          let n = max 0L (min count (int64 eph.count))
          let prefix = eph.Span.Slice(0, int n).ToArray()
          Ply(Dval.optionSome KTBytes (Bytes.newEphemeral prefix))
        | _, _, _, [| DBytes(Persistent _); DInt64 _ |] ->
          Ply(Dval.optionNone KTBytes)
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
              | DBytes(Ephemeral eph) -> collected.Write(eph.Span)
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


    // In-memory bytes are sliced without copying when the piece is big enough
    // (`Bytes.sliceEphemeral`), so `take`, `drop` and a cursor over a large buffer
    // don't copy the rest of it each step. Saved bytes are read whole and copied.
    { name = fn "bytesSlice" 0
      typeParams = []
      parameters =
        [ Param.make "bytes" TBytes ""
          Param.make "start" TInt64 "zero-based, inclusive"
          Param.make "length" TInt64 "0 or more" ]
      returnType = TBytes
      description =
        "Returns <param length> bytes starting at <param start>. Out-of-range slices are clamped."
      fn =
        (function
        | _, _, _, [| DBytes(Ephemeral eph); DInt64 startD; DInt64 lenD |] ->
          let len = int64 eph.count
          let safeStart = max 0L (min startD len)
          let safeLen = max 0L (min lenD (len - safeStart))
          Ply(Bytes.sliceEphemeral eph (int safeStart) (int safeLen))
        | state, _, _, [| DBytes ref; DInt64 startD; DInt64 lenD |] ->
          Bytes.withBytes state ref (fun bs ->
            let len = int64 bs.Length
            let safeStart = max 0L (min startD len)
            let safeLen = max 0L (min lenD (len - safeStart))
            let slice = Array.zeroCreate<byte> (int safeLen)
            if safeLen > 0L then
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
      parameters = [ Param.make "bytes" TBytes ""; Param.make "index" TInt64 "" ]
      returnType = TypeReference.option TUInt8
      description =
        "Returns {{Some byte}} at <param index> in <param bytes>, or {{None}} if out of bounds."
      fn =
        (function
        | _, _, _, [| DBytes(Ephemeral eph); DInt64 index |] ->
          if index >= 0L && index < int64 eph.count then
            Ply(Dval.optionSome KTUInt8 (DUInt8 eph.buffer[eph.offset + int index]))
          else
            Ply(Dval.optionNone KTUInt8)
        | state, _, _, [| DBytes ref; DInt64 index |] ->
          Bytes.withBytes state ref (fun bs ->
            if index >= 0L && index < int64 bs.Length then
              Ply(Dval.optionSome KTUInt8 (DUInt8 bs[int index]))
            else
              Ply(Dval.optionNone KTUInt8))
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
          Param.make "start" TInt64 "zero-based, inclusive" ]
      returnType = TypeReference.option TInt64
      description =
        "Returns {{Some index}} of the first <param byte> in <param bytes> at or after <param start>, or {{None}} if there is none or <param start> is out of range."
      fn =
        (function
        | _, _, _, [| DBytes(Ephemeral eph); DUInt8 b; DInt64 startD |] ->
          if startD >= 0L && startD < int64 eph.count then
            match System.MemoryExtensions.IndexOf(eph.Span.Slice(int startD), b) with
            | -1 -> Ply(Dval.optionNone KTInt64)
            | i -> Ply(Dval.optionSome KTInt64 (Dval.dint64 (startD + int64 i)))
          else
            Ply(Dval.optionNone KTInt64)
        | state, _, _, [| DBytes ref; DUInt8 b; DInt64 startD |] ->
          Bytes.withBytes state ref (fun bs ->
            if startD >= 0L && startD < int64 bs.Length then
              match System.Array.IndexOf(bs, b, int startD) with
              | -1 -> Ply(Dval.optionNone KTInt64)
              | i -> Ply(Dval.optionSome KTInt64 (Dval.dint64 (int64 i)))
            else
              Ply(Dval.optionNone KTInt64))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // Native because a Dark loop compares a slice at every candidate position:
    // on 1 MB of HTTP headers that took 520 ms, where this takes 0.1 ms.
    { name = fn "bytesIndexOfSegment" 0
      typeParams = []
      parameters =
        [ Param.make "bytes" TBytes ""
          Param.make "segment" TBytes ""
          Param.make "start" TInt64 "zero-based, inclusive" ]
      returnType = TypeReference.option TInt64
      description =
        "Returns {{Some index}} of the first <param segment> in <param bytes> at or after <param start>, or {{None}} if there is none or <param start> is out of range. An empty <param segment> is found at <param start>."
      fn =
        (function
        | _, _, _, [| DBytes(Ephemeral eph); DBytes(Ephemeral seg); DInt64 start |] ->
          if start >= 0L && start <= int64 eph.count then
            match
              System.MemoryExtensions.IndexOf(eph.Span.Slice(int start), seg.Span)
            with
            | -1 -> Ply(Dval.optionNone KTInt64)
            | i -> Ply(Dval.optionSome KTInt64 (Dval.dint64 (start + int64 i)))
          else
            Ply(Dval.optionNone KTInt64)
        | state, _, _, [| DBytes ref; DBytes segmentRef; DInt64 start |] ->
          Bytes.withBytes state ref (fun bs ->
            Bytes.withBytes state segmentRef (fun segment ->
              if start >= 0L && start <= int64 bs.Length then
                let rest =
                  System.ReadOnlySpan<byte>(bs, int start, bs.Length - int start)
                match
                  System.MemoryExtensions.IndexOf(
                    rest,
                    System.ReadOnlySpan<byte> segment
                  )
                with
                | -1 -> Ply(Dval.optionNone KTInt64)
                | i -> Ply(Dval.optionSome KTInt64 (Dval.dint64 (start + int64 i)))
              else
                Ply(Dval.optionNone KTInt64)))
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
