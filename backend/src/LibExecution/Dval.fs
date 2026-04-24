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
let newEphemeralBlob (exeState : ExecutionState) (bytes : byte[]) : Dval =
  let id = System.Guid.NewGuid()
  exeState.blobStore[id] <- bytes
  DBlob(Ephemeral id)


/// Resolve a BlobRef to its bytes. Ephemerals read from the VM store;
/// persistent refs hit package_blobs via the PM.
let readBlobBytes
  (exeState : ExecutionState)
  (pm : PackageManager)
  (ref : BlobRef)
  : Ply.Ply<byte[]> =
  uply {
    match ref with
    | Ephemeral id ->
      match exeState.blobStore.TryGetValue id with
      | true, bytes -> return bytes
      | false, _ ->
        return
          Exception.raiseInternal "Ephemeral blob not found in store" [ "id", id ]
    | Persistent(hash, _length) ->
      let! bytes = pm.getBlob hash
      match bytes with
      | Some b -> return b
      | None ->
        return
          Exception.raiseInternal
            "Persistent blob not found in package_blobs"
            [ "hash", hash ]
  }


/// Walk a StreamImpl tree invoking any IO-source disposers. Wrapped
/// in try/with so a misbehaving disposer doesn't take the whole
/// runtime down — best-effort cleanup. Safe to call multiple times:
/// the disposers themselves must be idempotent (e.g. `response.Dispose()`
/// on .NET HttpResponseMessage is a no-op if already disposed).
let rec disposeStreamImpl (impl : StreamImpl) : unit =
  match impl with
  | FromIO(_, _, Some d) ->
    try
      d ()
    with _ ->
      ()
  | FromIO(_, _, None) -> ()
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
/// (HttpResponseMessage, FileStream, etc.). See
/// thinking/blobs-and-streams/30-phase-2.md.
let newStream
  (elemType : ValueType)
  (next : unit -> Ply.Ply<Option<Dval>>)
  (disposer : (unit -> unit) option)
  : Dval =
  wrapStreamImpl (FromIO(next, elemType, disposer))


/// Pull one element through a [StreamImpl]. Separate from
/// [readStreamNext] so the recursion can walk transform nodes without
/// re-entering the root's monitor lock — nested transforms share the
/// wrapping DStream's lock, per the design note in
/// thinking/blobs-and-streams/30-phase-2.md.
let rec private pullStreamImpl (impl : StreamImpl) : Ply.Ply<Option<Dval>> =
  uply {
    match impl with
    | FromIO(next, _elemType, _disposer) -> return! next ()

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
/// Thread-safe via the stream's monitor lock; concurrent callers
/// serialize. The walk through Mapped/Filtered/Take/Concat nodes
/// happens in [pullStreamImpl]. On exhaustion, any IO-source
/// disposers attached to FromIO nodes are invoked (HttpResponseMessage,
/// FileStream, etc.).
let readStreamNext (dv : Dval) : Ply.Ply<Option<Dval>> =
  uply {
    match dv with
    | DStream(impl, disposed, lockObj) ->
      // Monitor ensures single-consumer semantics.
      let taken = System.Threading.Monitor.TryEnter(lockObj, 0)
      if not taken then
        return
          Exception.raiseInternal "stream: concurrent consumers not supported" []
      try
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
      finally
        System.Threading.Monitor.Exit(lockObj)
    | _ -> return Exception.raiseInternal "readStreamNext: expected DStream" []
  }


/// SHA-256 hex digest of the given bytes — content-addressing for
/// blob promotion. See thinking/blobs-and-streams/00-design.md.
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
/// TODO chunk L.1: trace capture path should also call this.
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
