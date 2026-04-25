/// Simple pass-through functions for creating Dvals
module LibExecution.Dval

open Prelude

open LibExecution.RuntimeTypes
module VT = ValueType


let int8 (i : int8) = DInt8 i
let uint8 (i : uint8) = DUInt8 i
let int16 (i : int16) = DInt16 i
let uint16 (i : uint16) = DUInt16 i
let int32 (i : int32) = DInt32 i
let uint32 (i : uint32) = DUInt32 i
let int64 (i : int64) = DInt64 i
let uint64 (i : uint64) = DUInt64 i
let int128 (i : System.Int128) = DInt128 i
let uint128 (i : System.UInt128) = DUInt128 i

let string (s : string) = DString s

let uuid (s : System.Guid) = DUuid s

let list (typ : KnownType) (list : List<Dval>) : Dval = DList(VT.known typ, list)

let dict (typ : KnownType) (entries : List<string * Dval>) : Dval =
  DDict(VT.known typ, Map entries)

let dictFromMap (typ : KnownType) (entries : Map<string, Dval>) : Dval =
  DDict(VT.known typ, entries)


let optionType () = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.option ())

let optionSome (innerType : KnownType) (dv : Dval) : Dval =
  DEnum(optionType (), optionType (), [ VT.known innerType ], "Some", [ dv ])

let optionNone (innerType : KnownType) : Dval =
  DEnum(optionType (), optionType (), [ VT.known innerType ], "None", [])

let option (innerType : KnownType) (dv : Option<Dval>) : Dval =
  match dv with
  | Some dv -> optionSome innerType dv
  | None -> optionNone innerType



let resultType () = FQTypeName.fqPackage (PackageRefs.Type.Stdlib.result ())


let resultOk (okType : KnownType) (errorType : KnownType) (dvOk : Dval) : Dval =
  DEnum(
    resultType (),
    resultType (),
    [ ValueType.Known okType; ValueType.Known errorType ],
    "Ok",
    [ dvOk ]
  )

let resultError
  (okType : KnownType)
  (errorType : KnownType)
  (dvError : Dval)
  : Dval =

  DEnum(
    resultType (),
    resultType (),
    [ ValueType.Known okType; ValueType.Known errorType ],
    "Error",
    [ dvError ]
  )

let result
  (okType : KnownType)
  (errorType : KnownType)
  (dv : Result<Dval, Dval>)
  : Dval =
  match dv with
  | Ok dv -> resultOk okType errorType dv
  | Error dv -> resultError okType errorType dv


/// Mint a fresh ephemeral blob: register the bytes in the exeState's
/// blob store and return a DBlob that references them. Caller retains
/// no direct handle on the byte[] past this point.
///
/// If a blob-scope is active (see [pushBlobScope]), the new UUID is
/// recorded in the top scope so [popBlobScope] can reclaim the bytes
/// when the scope exits. Runs without an active scope (CLI, tests)
/// leave the bytes in the ExecutionState for its full lifetime.
let newEphemeralBlob (exeState : ExecutionState) (bytes : byte[]) : Dval =
  let id = System.Guid.NewGuid()
  exeState.blobStore[id] <- bytes
  if exeState.blobScopes.Count > 0 then
    exeState.blobScopes.Peek().Add(id) |> ignore<bool>
  DBlob(Ephemeral id)


/// Push a fresh, empty blob-scope onto the stack. Each subsequent
/// [newEphemeralBlob] records its UUID in this scope until it's
/// popped. Used by long-lived VMs (http-server) around per-handler
/// work so ephemeral blobs don't accumulate across requests.
let pushBlobScope (exeState : ExecutionState) : unit =
  exeState.blobScopes.Push(System.Collections.Generic.HashSet<System.Guid>())


/// Pop the top blob-scope: drop every UUID it tracked from
/// [blobStore]. Safe to call without a push (no-op on empty stack).
/// Caller should wrap `pushBlobScope` / `popBlobScope` in a
/// `try/finally` so failures don't leak blob bytes.
///
/// Blobs promoted to `Persistent` inside this scope are unaffected —
/// promotion writes to `package_blobs` (a separate durable table)
/// and the DBlob reference swaps to `Persistent`. We only drop the
/// in-memory ephemeral byte cache.
let popBlobScope (exeState : ExecutionState) : unit =
  if exeState.blobScopes.Count > 0 then
    let scope = exeState.blobScopes.Pop()
    for id in scope do
      exeState.blobStore.TryRemove(id) |> ignore<bool * byte[]>


/// SHA-256 of zero bytes. The well-known canonical hash backing
/// [Builtin.blobEmpty]; short-circuited in [readBlobBytes] so the empty
/// blob doesn't need a `package_blobs` row.
let emptyBlobHash =
  "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"


/// Resolve a BlobRef to its bytes. Ephemerals read from the VM store;
/// persistent refs hit package_blobs via the ExecutionState's blob
/// accessor. Shared by every builtin that dereferences a DBlob.
let readBlobBytes (state : ExecutionState) (ref : BlobRef) : Ply.Ply<byte[]> =
  uply {
    match ref with
    | Ephemeral id ->
      let mutable bs : byte[] = null
      if state.blobStore.TryGetValue(id, &bs) then
        return bs
      else
        return Exception.raiseInternal "ephemeral blob not found" [ "id", id ]
    | Persistent(hash, _length) when hash = emptyBlobHash -> return [||]
    | Persistent(hash, _length) ->
      let! got = state.blobs.get hash
      match got with
      | Some bs -> return bs
      | None ->
        return
          Exception.raiseInternal
            "persistent blob missing in package_blobs"
            [ "hash", hash ]
  }


/// Walk a StreamImpl tree invoking any IO-source disposers. Wrapped
/// in try/with so a misbehaving disposer doesn't take the whole
/// runtime down — best-effort cleanup. Safe to call multiple times:
/// the disposers themselves must be idempotent (e.g. `response.Dispose()`
/// on .NET HttpResponseMessage is a no-op if already disposed).
let rec disposeStreamImpl (impl : StreamImpl) : unit =
  match impl with
  | FromIO(_, _, Some d, _) ->
    try
      d ()
    with _ ->
      ()
  | FromIO(_, _, None, _) -> ()
  | Mapped(src, _, _) -> disposeStreamImpl src
  | Filtered(src, _) -> disposeStreamImpl src
  | Take(src, _, _) -> disposeStreamImpl src
  | Concat streams -> streams.Value |> List.iter disposeStreamImpl


/// GC-triggered cleanup for DStreams that callers never explicitly
/// drain or close. Doubles as the DStream's lockObj so the lifetime
/// tracks the DStream itself — the GC can only finalize this object
/// when the DStream holding it is unreachable. On finalize, runs the
/// full [disposeStreamImpl] chain once (guarded by the shared
/// `disposed` ref, so no double-fire if streamClose/drain-to-EOF
/// already ran).
///
/// Swallows disposer exceptions — finalizers that throw crash the
/// process, and we'd rather leak on the pathological case than take
/// down everything.
type StreamFinalizer(impl : StreamImpl, disposed : bool ref) =
  override this.Finalize() =
    try
      if not disposed.Value then
        disposed.Value <- true
        disposeStreamImpl impl
    with _ ->
      ()


/// Wrap a [StreamImpl] in a fresh DStream with its own disposed flag
/// and a GC-backed finalizer. Single-consumer by construction — the
/// monitor on `lockObj` guards concurrent `next` calls. When the
/// DStream becomes unreachable, the GC finalizes the StreamFinalizer
/// (which is also the lockObj) and the disposer chain runs once.
///
/// Used by `newStream` and by the Stream transform builtins
/// (streamMap/streamFilter/streamTake/streamConcat) when they wrap
/// a source's impl into a new DStream.
let wrapStreamImpl (impl : StreamImpl) : Dval =
  let disposed = ref false
  DStream(impl, disposed, StreamFinalizer(impl, disposed) :> obj)


/// Mint a fresh DStream from a pull function. Convenience wrapper
/// over [wrapStreamImpl] for the common FromIO case. [disposer],
/// when `Some`, is called once when the stream is drained to
/// completion, `streamClose`d, or finalized by the GC — used by
/// IO-backed producers to release the underlying source
/// (HttpResponseMessage, FileStream, etc.). Use [newStreamChunked]
/// for byte streams that can efficiently yield a whole chunk per
/// pull (IO-backed producers like HttpClient.stream).
let newStream
  (elemType : ValueType)
  (next : unit -> Ply.Ply<Option<Dval>>)
  (disposer : (unit -> unit) option)
  : Dval =
  wrapStreamImpl (FromIO(next, elemType, disposer, None))


/// Mint a DStream<UInt8> that can be drained bulk-wise via
/// [readStreamChunk]. The `nextChunk` callback fills up to `maxBytes`
/// into a fresh byte[] and returns it (or None on exhaustion).
/// Consumers that want full-chunk bytes — `streamToBlob`, SSE byte
/// accumulators — bypass per-byte Ply/Dval boxing by calling
/// `readStreamChunk` instead of `readStreamNext`. The `next` path
/// stays available so `streamNext` returns one DUInt8 at a time as
/// before — the implementation synthesises single-byte pulls from
/// the chunk buffer.
let newStreamChunked
  (elemType : ValueType)
  (nextChunk : int -> Ply.Ply<Option<byte[]>>)
  (disposer : (unit -> unit) option)
  : Dval =
  // Maintain a small carry buffer so single-byte `next` pulls can
  // be served from the chunks that `nextChunk` returned.
  let carry = ref [||]
  let carryPos = ref 0
  let next () : Ply.Ply<Option<Dval>> =
    uply {
      if carryPos.Value >= carry.Value.Length then
        // Refill from the underlying chunked producer. 8 KB mirrors
        // the socket-read buffer size we use across the codebase.
        let! chunk = nextChunk 8192
        match chunk with
        | None -> return None
        | Some buf ->
          carry.Value <- buf
          carryPos.Value <- 0
          if buf.Length = 0 then
            return None
          else
            let b = buf[0]
            carryPos.Value <- 1
            return Some(DUInt8 b)
      else
        let b = carry.Value[carryPos.Value]
        carryPos.Value <- carryPos.Value + 1
        return Some(DUInt8 b)
    }
  wrapStreamImpl (FromIO(next, elemType, disposer, Some nextChunk))


/// Pull one element through a [StreamImpl]. Separate from
/// [readStreamNext] so the recursion can walk transform nodes
/// (Mapped/Filtered/Take/Concat) without re-entering the root's
/// disposed flag — nested transforms share the wrapping DStream's
/// lifecycle.
let rec private pullStreamImpl (impl : StreamImpl) : Ply.Ply<Option<Dval>> =
  uply {
    match impl with
    | FromIO(next, _elemType, _disposer, _nextChunk) -> return! next ()

    | Mapped(src, fn, _elemType) ->
      let! upstream = pullStreamImpl src
      match upstream with
      | None -> return None
      | Some v ->
        let! mapped = fn v
        return Some mapped

    | Filtered(src, pred) ->
      // Pull from source until the predicate accepts or the source
      // runs dry. Written as a mutable loop rather than tail recursion
      // so a long rejection run doesn't blow the Ply chain.
      let mutable result : Option<Dval> = None
      let mutable keepGoing = true
      while keepGoing do
        let! upstream = pullStreamImpl src
        match upstream with
        | None -> keepGoing <- false
        | Some v ->
          let! matches = pred v
          if matches then
            result <- Some v
            keepGoing <- false
      return result

    | Take(src, _n, remaining) ->
      if remaining.Value <= 0L then
        return None
      else
        let! upstream = pullStreamImpl src
        match upstream with
        | Some _ ->
          remaining.Value <- remaining.Value - 1L
          return upstream
        | None ->
          // Source dried up before the limit; clamp so future pulls
          // stay at zero and short-circuit without touching source.
          remaining.Value <- 0L
          return None

    | Concat streams ->
      // Pull from the head stream; when it's exhausted, drop it and
      // try the next. Mutating the ref means future pulls don't
      // re-enter a drained stream (which would call `None ()` on a
      // disposed IO source in pathological cases).
      let mutable result : Option<Dval> = None
      let mutable keepGoing = true
      while keepGoing do
        match streams.Value with
        | [] -> keepGoing <- false
        | head :: tail ->
          let! pulled = pullStreamImpl head
          match pulled with
          | Some _ ->
            result <- pulled
            keepGoing <- false
          | None -> streams.Value <- tail
      return result
  }


/// Pull the next element from a stream. Returns [None] when the
/// stream is exhausted; subsequent calls after exhaustion return
/// [None] (single-consumer — once drained, stays drained).
///
/// No thread-affine lock: `pullStreamImpl` awaits inside `fn v` /
/// `pred v` (via `Exe.executeApplicable`) and Ply continuations can
/// resume on a different thread, which makes `Monitor.Exit` throw
/// `SynchronizationLockException`. We rely on the single-threaded
/// Dark VM model for ordering instead — concurrent consumers of the
/// same stream have undefined output but never crash. The walk
/// through Mapped/Filtered/Take/Concat nodes happens in
/// [pullStreamImpl]; on exhaustion, any IO-source disposers attached
/// to FromIO nodes are invoked.
let readStreamNext (dv : Dval) : Ply.Ply<Option<Dval>> =
  uply {
    match dv with
    | DStream(impl, disposed, _lockObj) ->
      if disposed.Value then
        return None
      else
        let! result = pullStreamImpl impl
        match result with
        | Some _ -> return result
        | None ->
          disposed.Value <- true
          disposeStreamImpl impl
          return None
    | _ -> return Exception.raiseInternal "readStreamNext: expected DStream" []
  }


/// Pull up to `maxBytes` bytes from a byte stream as one chunk.
/// Returns [None] when exhausted; subsequent calls stay [None].
/// Prefers a FromIO's own `nextChunk` when present; falls back to
/// byte-wise pulls for streams that were built via [newStream] or
/// that walk transform nodes (Mapped/Filtered/Take/Concat) where a
/// chunk semantic isn't well-defined.
///
/// Used by `streamToBlob` and SSE byte accumulators to amortise the
/// Ply-continuation cost across whole chunks rather than paying it
/// per byte.
let readStreamChunk (maxBytes : int) (dv : Dval) : Ply.Ply<Option<byte[]>> =
  uply {
    match dv with
    | DStream(impl, disposed, _) ->
      if disposed.Value then
        return None
      else
        match impl with
        | FromIO(_, _, _, Some nextChunk) ->
          let! chunk = nextChunk maxBytes
          match chunk with
          | Some buf when buf.Length > 0 -> return Some buf
          | _ ->
            disposed.Value <- true
            disposeStreamImpl impl
            return None
        | _ ->
          // Fallback: pull byte-by-byte. Only pays off vs per-byte
          // `readStreamNext` if the caller really wants bulk bytes —
          // transform chains lose the chunk optimisation but still
          // drain correctly.
          use collected = new System.IO.MemoryStream()
          let mutable keepGoing = true
          let mutable bytesSoFar = 0
          while keepGoing && bytesSoFar < maxBytes do
            let! pulled = pullStreamImpl impl
            match pulled with
            | Some(DUInt8 b) ->
              collected.WriteByte b
              bytesSoFar <- bytesSoFar + 1
            | Some _ ->
              return
                Exception.raiseInternal
                  "readStreamChunk: expected Stream<UInt8> element"
                  []
            | None -> keepGoing <- false
          if bytesSoFar = 0 then
            disposed.Value <- true
            disposeStreamImpl impl
            return None
          else
            return Some(collected.ToArray())
    | _ -> return Exception.raiseInternal "readStreamChunk: expected DStream" []
  }


/// SHA-256 hex digest of the given bytes — content-addressing for
/// blob promotion.
let sha256Hex (bytes : byte[]) : string =
  use sha = System.Security.Cryptography.SHA256.Create()
  let digest = sha.ComputeHash(bytes)
  System.Convert.ToHexStringLower(digest)


/// Promote any ephemeral blobs reachable from [dv] to persistent:
/// hash the bytes (SHA-256), write them to the content-addressed
/// store via [insert], and swap the ref. Idempotent by design — the
/// insert path uses `INSERT OR IGNORE`, so promoting the same bytes
/// twice writes to `package_blobs` once.
///
/// Called before serializing a Dval through any persistence boundary
/// (val commit, User DB write, trace capture). Plain `DBlob` writers
/// (binary, JSON) still raise on ephemeral — promote first, serialize
/// second.
///
/// TODO: trace capture path should also call this.
let promoteBlobs
  (exeState : ExecutionState)
  (insert : string -> byte[] -> Ply.Ply<unit>)
  (dv : Dval)
  : Ply.Ply<Dval> =
  uply {
    let rec go (dv : Dval) : Ply.Ply<Dval> =
      uply {
        match dv with
        | DBlob(Ephemeral id) ->
          let mutable bs : byte[] = null
          if exeState.blobStore.TryGetValue(id, &bs) then
            let h = sha256Hex bs
            // NB: Dval.int64 is a Dval-builder in this module, so we
            // fully-qualify the int64 primitive conversion here.
            let n : int64 = System.Convert.ToInt64 bs.Length
            do! insert h bs
            return DBlob(Persistent(h, n))
          else
            return
              Exception.raiseInternal
                "Ephemeral blob not found in store during promotion"
                [ "id", id ]
        | DBlob(Persistent _)
        | DUnit
        | DBool _
        | DInt8 _
        | DUInt8 _
        | DInt16 _
        | DUInt16 _
        | DInt32 _
        | DUInt32 _
        | DInt64 _
        | DUInt64 _
        | DInt128 _
        | DUInt128 _
        | DFloat _
        | DChar _
        | DString _
        | DDateTime _
        | DUuid _
        | DApplicable _
        | DDB _
        | DStream _ -> return dv
        | DList(vt, items) ->
          let! items' = items |> Ply.List.mapSequentially go
          return DList(vt, items')
        | DDict(vt, entries) ->
          let! entries' =
            entries
            |> Map.toList
            |> Ply.List.mapSequentially (fun (k, v) ->
              uply {
                let! v' = go v
                return (k, v')
              })
          return DDict(vt, Map.ofList entries')
        | DTuple(a, b, rest) ->
          let! a' = go a
          let! b' = go b
          let! rest' = rest |> Ply.List.mapSequentially go
          return DTuple(a', b', rest')
        | DRecord(src, rt, typeArgs, fields) ->
          let! fields' =
            fields
            |> Map.toList
            |> Ply.List.mapSequentially (fun (k, v) ->
              uply {
                let! v' = go v
                return (k, v')
              })
          return DRecord(src, rt, typeArgs, Map.ofList fields')
        | DEnum(src, rt, typeArgs, caseName, fields) ->
          let! fields' = fields |> Ply.List.mapSequentially go
          return DEnum(src, rt, typeArgs, caseName, fields')
      }
    return! go dv
  }


/// Can this Dval be stored as the evaluated body of a package value?
///
/// The persist path in `Seed.fs` runs each package value's body and
/// writes the resulting Dval as `package_values.rt_dval` bytes. The
/// binary serializer raises on exactly two shapes — we'd rather
/// report "cannot store this kind of value in a val" up front than
/// surface a deep-stack serialize exception.
///
/// Rejected:
/// - `DStream` — the pull fn is a closure bound to this exeState.
/// - `DBlob(Ephemeral _)` — the raw bytes live in exeState.blobStore;
///   promote to `Persistent` first. Most call paths already promote
///   (see `promoteBlobs`); this branch is a safety net.
///
/// Walks containers so one bad leaf anywhere in the tree disqualifies
/// the whole value.
let rec isPersistable (dv : Dval) : bool =
  match dv with
  // These two shapes `raiseFormatError` in the Dval binary serializer
  // (see LibSerialization/Binary/Serializers/RT/Dval.fs). Catch them
  // here so the Seed.fs evaluator can report a clean reason instead
  // of surfacing a deep-stack raise.
  | DStream _ -> false
  | DBlob(Ephemeral _) -> false

  | DUnit
  | DBool _
  | DInt8 _
  | DUInt8 _
  | DInt16 _
  | DUInt16 _
  | DInt32 _
  | DUInt32 _
  | DInt64 _
  | DUInt64 _
  | DInt128 _
  | DUInt128 _
  | DFloat _
  | DChar _
  | DString _
  | DDateTime _
  | DUuid _
  // DApplicable and DDB serialize successfully — lambdas store their
  // instruction stream, DB handles store as a string identifier.
  // Demo handlers persist lambdas as vals; canvas-local DBs rely on
  // the DDB path.
  | DApplicable _
  | DDB _
  | DBlob(Persistent _) -> true

  | DList(_, items) -> items |> List.forall isPersistable
  | DTuple(a, b, rest) ->
    isPersistable a && isPersistable b && List.forall isPersistable rest
  | DDict(_, entries) -> entries |> Map.values |> Seq.forall isPersistable
  | DRecord(_, _, _, fields) -> fields |> Map.values |> Seq.forall isPersistable
  | DEnum(_, _, _, _, fields) -> fields |> List.forall isPersistable


/// Human-readable explanation of why [isPersistable] rejected a value.
/// Returns the first offending shape found — good enough for an error
/// message pointing the user at the problem.
let rec nonPersistableReason (dv : Dval) : Option<string> =
  match dv with
  | DStream _ ->
    Some "stream values can't be stored in a `val` — drain to a Blob or List first"
  | DBlob(Ephemeral _) ->
    Some
      "ephemeral blob can't be stored in a `val` — promote to persistent (serialize) first"

  | DList(_, items) -> items |> List.tryPick nonPersistableReason
  | DTuple(a, b, rest) -> [ a; b ] @ rest |> List.tryPick nonPersistableReason
  | DDict(_, entries) -> entries |> Map.values |> Seq.tryPick nonPersistableReason
  | DRecord(_, _, _, fields) ->
    fields |> Map.values |> Seq.tryPick nonPersistableReason
  | DEnum(_, _, _, _, fields) -> fields |> List.tryPick nonPersistableReason

  | _ -> None


let byteArrayToDvalList (bytes : byte[]) : Dval =
  bytes
  |> Array.toList
  |> List.map (fun b -> DUInt8(byte b))
  |> fun dvalList -> DList(VT.uint8, dvalList)

let dlistToByteArray (dvalList : List<Dval>) : byte[] =
  dvalList
  |> List.map (fun dval ->
    match dval with
    | DUInt8 b -> b
    | _ -> (Exception.raiseInternal "Invalid type in byte list") [])
  |> Array.ofList
