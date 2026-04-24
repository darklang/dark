/// Stream builtins — see thinking/blobs-and-streams/30-phase-2.md.
///
/// Chunk 2.5: consumption-only API (`next`, `toList`, `toBlob`,
/// `close`) plus a `fromList` constructor for tests. Lazy
/// transformations (`map` / `filter` / `take` / `concat`) arrive in
/// chunks 2.6 + 2.7, once StreamImpl grows `Mapped`/`Filtered`/
/// `Take`/`Concat` constructors.
///
/// CLEANUP revisit: considered alternatives we did not take.
/// - Channels (Go-style with separate reader/writer ends and buffering):
///   adds synchronization semantics Dark does not have elsewhere. Skip
///   for v1; revisit if fan-in/fan-out becomes a real use case.
/// - Actor-mailbox (Erlang-style, with a scheduler): out of scope;
///   Dark has no scheduler story.
module BuiltinExecution.Libs.Stream

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval


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
          Dval.newStream inferredElem nextFn |> Ply
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
        | _, _, _, [ DStream(_, disposed, lockObj) ] ->
          // Acquire the lock to avoid racing a mid-flight `next`; set
          // the flag so subsequent pulls return None immediately.
          System.Threading.Monitor.Enter(lockObj)
          try
            disposed.Value <- true
          finally
            System.Threading.Monitor.Exit(lockObj)
          DUnit |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
