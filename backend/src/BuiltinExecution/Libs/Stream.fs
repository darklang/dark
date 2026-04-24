/// Stream builtins — see thinking/blobs-and-streams/30-phase-2.md.
///
/// Chunk 2.5: consumption-only API (`next`, `toList`, `toBlob`,
/// `close`) plus a `fromList` constructor for tests. Chunk 2.7 adds
/// lazy transforms (`map` / `filter` / `take` / `concat`) over the
/// `Mapped`/`Filtered`/`Take`/`Concat` StreamImpl nodes from 2.6.
///
/// CLEANUP revisit: considered alternatives we did not take.
/// - Channels (Go-style with separate reader/writer ends and buffering):
///   adds synchronization semantics Dark does not have elsewhere. Skip
///   for v1; revisit if fan-in/fan-out becomes a real use case.
/// - Actor-mailbox (Erlang-style, with a scheduler): out of scope;
///   Dark has no scheduler story.
module BuiltinExecution.Libs.Stream

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Exe = LibExecution.Execution


let varA = TVariable "a"


/// Synchronously turn a RT.TypeReference leaf into a ValueType for
/// builtin-side handling. TStream's element type tends to be a
/// concrete primitive here (e.g. TInt64, TUInt8), so we avoid the
/// async `TypeReference.toVT` and just cover the common shapes.
/// Unknown/complex types fall back to [ValueType.Unknown].
let rec private elemValueType (t : TypeReference) : ValueType =
  match t with
  | TUnit -> VT.unit
  | TBool -> VT.bool
  | TInt8 -> VT.int8
  | TUInt8 -> VT.uint8
  | TInt16 -> VT.int16
  | TUInt16 -> VT.uint16
  | TInt32 -> VT.int32
  | TUInt32 -> VT.uint32
  | TInt64 -> VT.int64
  | TUInt64 -> VT.uint64
  | TInt128 -> VT.int128
  | TUInt128 -> VT.uint128
  | TFloat -> VT.float
  | TChar -> VT.char
  | TString -> VT.string
  | TUuid -> VT.uuid
  | TDateTime -> VT.dateTime
  | TBlob -> VT.blob
  | TList inner -> VT.list (elemValueType inner)
  | TStream inner -> VT.stream (elemValueType inner)
  | _ -> VT.unknown


let private elemKnownType (t : TypeReference) : KnownType =
  match elemValueType t with
  | ValueType.Known kt -> kt
  | ValueType.Unknown -> KTUnit


let fns () : List<BuiltInFn> =
  [ { name = fn "streamFromList" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "items" (TList varA) "" ]
      returnType = TStream varA
      description =
        "Constructs a stream that yields the given list's items in order, then Done."
      fn =
        (function
        | _, _, [ elemType ], [ DList(elemVT, items) ] ->
          let remaining = ref items
          let nextFn () : Ply<Option<Dval>> =
            uply {
              match remaining.Value with
              | head :: tail ->
                remaining.Value <- tail
                return Some head
              | [] -> return None
            }
          // Prefer the runtime ValueType of the list elements; fall
          // back to the declared type parameter when the list was
          // empty (ValueType.Unknown).
          let inferredElem =
            match elemVT with
            | ValueType.Unknown -> elemValueType elemType
            | known -> known
          Dval.newStream inferredElem nextFn None |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "streamNext" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "stream" (TStream varA) "" ]
      returnType = TypeReference.option varA
      description =
        "Pulls the next element from <param stream>. Returns None when the stream is exhausted. Mutates the stream — subsequent calls after exhaustion keep returning None."
      fn =
        (function
        | _, _, [ elemType ], [ s ] ->
          uply {
            let! nextResult = Dval.readStreamNext s
            return Dval.option (elemKnownType elemType) nextResult
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "streamToList" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "stream" (TStream varA) "" ]
      returnType = TList varA
      description = "Drains <param stream> into a List, consuming it entirely."
      fn =
        (function
        | _, _, [ elemType ], [ s ] ->
          uply {
            let elemKT = elemKnownType elemType
            let collected = ResizeArray<Dval>()
            let mutable keepGoing = true
            while keepGoing do
              let! result = Dval.readStreamNext s
              match result with
              | Some item -> collected.Add item
              | None -> keepGoing <- false
            return Dval.list elemKT (List.ofSeq collected)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "streamToBlob" 0
      typeParams = []
      parameters = [ Param.make "stream" (TStream TUInt8) "" ]
      returnType = TBlob
      description =
        "Drains a byte stream into a single ephemeral Blob, consuming <param stream>."
      fn =
        (function
        | state, _, _, [ s ] ->
          uply {
            // CLEANUP sub-blob slicing (chunk L.5): we copy each byte
            // into a MemoryStream. Could share the source buffer on
            // promotion if profiles ever show this as hot. For now the
            // single contiguous byte[] matches Blob's storage model and
            // avoids a per-chunk Dval per byte.
            use collected = new System.IO.MemoryStream()
            let mutable keepGoing = true
            while keepGoing do
              let! result = Dval.readStreamNext s
              match result with
              | Some(DUInt8 b) -> collected.WriteByte b
              | Some other ->
                return
                  Exception.raiseInternal
                    "streamToBlob: expected Stream<UInt8> element"
                    [ "got", other ]
              | None -> keepGoing <- false
            return Dval.newEphemeralBlob state (collected.ToArray())
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "streamClose" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "stream" (TStream varA) "" ]
      returnType = TUnit
      description =
        "Marks <param stream> as fully consumed. Idempotent — calling on an
         already-closed stream is a no-op. Not strictly required (streams are
         GC-closed via their .NET finalizer), but useful when you want to release
         an IO source promptly."
      fn =
        (function
        | _, _, _, [ DStream(impl, disposed, lockObj) ] ->
          // Acquire the lock to avoid racing a mid-flight `next`; set
          // the flag so subsequent pulls return None immediately. Run
          // the disposer chain on the first close so IO sources
          // (HTTP response, file handle, …) are released promptly.
          System.Threading.Monitor.Enter(lockObj)
          try
            if not disposed.Value then
              disposed.Value <- true
              Dval.disposeStreamImpl impl
          finally
            System.Threading.Monitor.Exit(lockObj)
          DUnit |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    // Chunk 2.7 — lazy transforms. Each wraps the 2.6 StreamImpl
    // constructors. The source DStream's impl is extracted and placed
    // inside a new transform node under a fresh DStream — callers
    // should not pull from the original DStream afterwards (single-
    // consumer semantics; the shared impl underneath is unaware of
    // which wrapper is pulling).
    { name = fn "streamMap" 0
      typeParams = [ "a"; "b" ]
      parameters =
        [ Param.make "stream" (TStream varA) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.singleton varA, TVariable "b"))
            "Transforms each element of <param stream> by applying <param fn>."
            [ "elem" ] ]
      returnType = TStream(TVariable "b")
      description =
        "Returns a new stream whose elements are <param fn> applied to each
         element of <param stream>. Lazy — <param fn> runs only when the
         returned stream is drained."
      fn =
        (function
        | state, vm, [ _; outputType ], [ DStream(src, _, _); DApplicable app ] ->
          let elemType = elemValueType outputType
          let apply (dv : Dval) : Ply<Dval> =
            uply {
              let! result = Exe.executeApplicable state app (NEList.singleton dv)
              match result with
              | Ok v -> return v
              | Error(rte, _cs) -> return raiseRTE vm.threadID rte
            }
          DStream(Mapped(src, apply, elemType), ref false, obj ()) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "streamFilter" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.make "stream" (TStream varA) ""
          Param.makeWithArgs
            "pred"
            (TFn(NEList.singleton varA, TBool))
            "Predicate — returns true to keep an element, false to skip it."
            [ "elem" ] ]
      returnType = TStream varA
      description =
        "Returns a new stream that yields only the elements of <param stream>
         for which <param pred> returns true. Lazy — <param pred> runs as the
         result is drained, skipping rejected elements without buffering."
      fn =
        (function
        | state, vm, _, [ DStream(src, _, _); DApplicable app ] ->
          let pred (dv : Dval) : Ply<bool> =
            uply {
              let! result = Exe.executeApplicable state app (NEList.singleton dv)
              match result with
              | Ok(DBool b) -> return b
              | Ok other ->
                return
                  Exception.raiseInternal
                    "stream filter predicate returned non-Bool"
                    [ "got", other ]
              | Error(rte, _cs) -> return raiseRTE vm.threadID rte
            }
          DStream(Filtered(src, pred), ref false, obj ()) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "streamTake" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.make "stream" (TStream varA) ""
          Param.make "n" TInt64 "Maximum number of elements to yield." ]
      returnType = TStream varA
      description =
        "Returns a new stream that yields at most the first <param n> elements
         of <param stream>. If the source has fewer elements, yields them all.
         Terminates early without pulling the source past the limit."
      fn =
        (function
        | _, _, _, [ DStream(src, _, _); DInt64 n ] ->
          // Clamp negative n to 0 — pullStreamImpl treats remaining<=0
          // as done, so a negative here becomes an empty stream.
          let clamped = max 0L n
          DStream(Take(src, clamped, ref clamped), ref false, obj ()) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "streamConcat" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.make "streams" (TList(TStream varA)) "Streams to concatenate." ]
      returnType = TStream varA
      description =
        "Returns a new stream that drains <param streams> in list order,
         advancing to the next sub-stream when the current one is exhausted."
      fn =
        (function
        | _, _, _, [ DList(_, items) ] ->
          let impls =
            items
            |> List.map (fun dv ->
              match dv with
              | DStream(impl, _, _) -> impl
              | other ->
                Exception.raiseInternal
                  "streamConcat: expected List<Stream>"
                  [ "got", other ])
          DStream(Concat(ref impls), ref false, obj ()) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
