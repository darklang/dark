/// Runtime helpers for the `Stream<'a>` Dval type.
///
/// Streams are lazy, single-consumer, non-persistable. This module
/// owns the construction, drain, and disposal mechanics; the
/// transform-tree shape (`StreamImpl`'s `Mapped` / `Filtered` /
/// `Take` / `Concat`) is defined alongside the types in
/// `RuntimeTypes.fs`.
module LibExecution.Stream

open Prelude

open LibExecution.RuntimeTypes


/// Walk a StreamImpl tree invoking any IO-source disposers. Wrapped
/// in try/with so a misbehaving disposer doesn't take the whole
/// runtime down — best-effort cleanup. Safe to call multiple times:
/// the disposers themselves must be idempotent (e.g. `response.Dispose()`
/// on .NET HttpResponseMessage is a no-op if already disposed).
let rec disposeImpl (impl : StreamImpl) : unit =
  match impl with
  | FromIO(_, _, Some d, _) ->
    try
      d ()
    with _ ->
      ()
  | FromIO(_, _, None, _) -> ()
  | Mapped(src, _, _) -> disposeImpl src
  | Filtered(src, _) -> disposeImpl src
  | Take(src, _, _) -> disposeImpl src
  | Concat streams -> streams.Value |> List.iter disposeImpl


/// GC-triggered cleanup for DStreams that callers never explicitly
/// drain or close. Doubles as the DStream's lockObj so the lifetime
/// tracks the DStream itself — the GC can only finalize this object
/// when the DStream holding it is unreachable. On finalize, runs the
/// full [disposeImpl] chain once (guarded by the shared `disposed`
/// ref, so no double-fire if streamClose/drain-to-EOF already ran).
///
/// Swallows disposer exceptions — finalizers that throw crash the
/// process, and we'd rather leak on the pathological case than take
/// down everything.
type Finalizer(impl : StreamImpl, disposed : bool ref) =
  override this.Finalize() =
    try
      if not disposed.Value then
        disposed.Value <- true
        disposeImpl impl
    with _ ->
      ()


/// Wrap a [StreamImpl] in a fresh DStream with its own disposed flag
/// and a GC-backed finalizer. When the DStream becomes unreachable,
/// the GC finalizes the Finalizer (which is also the lockObj) and
/// the disposer chain runs once.
///
/// Used by `newFromIO` and by the Stream transform builtins
/// (streamMap/streamFilter/streamTake/streamConcat) when they wrap a
/// source's impl into a new DStream.
let wrapImpl (impl : StreamImpl) : Dval =
  let disposed = ref false
  DStream(impl, disposed, Finalizer(impl, disposed) :> obj)


/// Mint a fresh DStream from a pull function. Convenience wrapper
/// over [wrapImpl] for the common FromIO case. [disposer], when
/// `Some`, is called once when the stream is drained to completion,
/// `streamClose`d, or finalized by the GC — used by IO-backed
/// producers to release the underlying source (HttpResponseMessage,
/// FileStream, etc.). Use [newChunked] for byte streams that can
/// efficiently yield a whole chunk per pull.
let newFromIO
  (elemType : ValueType)
  (next : unit -> Ply.Ply<Option<Dval>>)
  (disposer : (unit -> unit) option)
  : Dval =
  wrapImpl (FromIO(next, elemType, disposer, None))


/// Mint a DStream<UInt8> that can be drained bulk-wise via
/// [readChunk]. The `nextChunk` callback fills up to `maxBytes` into
/// a fresh byte[] and returns it (or None on exhaustion). Consumers
/// that want full-chunk bytes — `streamToBlob`, SSE byte
/// accumulators — bypass per-byte Ply/Dval boxing by calling
/// `readChunk` instead of `readNext`. The `next` path stays
/// available so `streamNext` returns one DUInt8 at a time as before
/// — the implementation synthesises single-byte pulls from the
/// chunk buffer.
let newChunked
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
  wrapImpl (FromIO(next, elemType, disposer, Some nextChunk))


/// Pull one element through a [StreamImpl]. Separate from [readNext]
/// so the recursion can walk transform nodes
/// (Mapped/Filtered/Take/Concat) without re-entering the root's
/// disposed flag — nested transforms share the wrapping DStream's
/// lifecycle.
let rec private pullImpl (impl : StreamImpl) : Ply.Ply<Option<Dval>> =
  uply {
    match impl with
    | FromIO(next, _elemType, _disposer, _nextChunk) -> return! next ()

    | Mapped(src, fn, _elemType) ->
      let! upstream = pullImpl src
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
        let! upstream = pullImpl src
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
        let! upstream = pullImpl src
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
      // re-enter a drained stream.
      let mutable result : Option<Dval> = None
      let mutable keepGoing = true
      while keepGoing do
        match streams.Value with
        | [] -> keepGoing <- false
        | head :: tail ->
          let! pulled = pullImpl head
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
/// No thread-affine lock: `pullImpl` awaits inside `fn v` / `pred v`
/// (via `Exe.executeApplicable`) and Ply continuations can resume on
/// a different thread, which makes `Monitor.Exit` throw
/// `SynchronizationLockException`. We rely on the single-threaded
/// Dark VM model for ordering instead — concurrent consumers of the
/// same stream have undefined output but never crash.
///
/// TODO LATENT BUG: the single-consumer invariant is unenforced.
/// `disposed` only short-circuits AFTER drain; two callers entering
/// `readNext` concurrently on the same DStream interleave silently.
/// Cheap fix: a semaphore-with-permit-1 + raise on contention.
/// Proper fix: a Ply-aware lock that survives continuation awaits —
/// that's a Ply-internals refactor. Nothing in the codebase shares a
/// DStream across consumers today, but the type system doesn't
/// prevent it.
///
/// TODO per-element Ply continuation cost: every `next` allocates a
/// state machine. A 1000-element pipeline with three transforms is
/// ~3000 allocations. The chunked `nextChunk` fast path covers byte
/// streams; element-wise streams pay full cost. Real fix is replacing
/// Ply with something cheaper — either a custom `Future<'a>` struct
/// or a CPS interpreter with a fiber scheduler.
let readNext (dv : Dval) : Ply.Ply<Option<Dval>> =
  uply {
    match dv with
    | DStream(impl, disposed, _lockObj) ->
      if disposed.Value then
        return None
      else
        let! result = pullImpl impl
        match result with
        | Some _ -> return result
        | None ->
          disposed.Value <- true
          disposeImpl impl
          return None
    | _ -> return Exception.raiseInternal "readNext: expected DStream" []
  }


/// Pull up to `maxBytes` bytes from a byte stream as one chunk.
/// Returns [None] when exhausted; subsequent calls stay [None].
/// Prefers a FromIO's own `nextChunk` when present; falls back to
/// byte-wise pulls for streams that were built via [newFromIO] or
/// that walk transform nodes (Mapped/Filtered/Take/Concat) where a
/// chunk semantic isn't well-defined.
///
/// Used by `streamToBlob` and SSE byte accumulators to amortise the
/// Ply-continuation cost across whole chunks rather than paying it
/// per byte.
let readChunk (maxBytes : int) (dv : Dval) : Ply.Ply<Option<byte[]>> =
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
            disposeImpl impl
            return None
        | _ ->
          // Fallback: pull byte-by-byte. Only pays off vs per-byte
          // `readNext` if the caller really wants bulk bytes —
          // transform chains lose the chunk optimisation but still
          // drain correctly.
          use collected = new System.IO.MemoryStream()
          let mutable keepGoing = true
          let mutable bytesSoFar = 0
          while keepGoing && bytesSoFar < maxBytes do
            let! pulled = pullImpl impl
            match pulled with
            | Some(DUInt8 b) ->
              collected.WriteByte b
              bytesSoFar <- bytesSoFar + 1
            | Some _ ->
              return
                Exception.raiseInternal
                  "readChunk: expected Stream<UInt8> element"
                  []
            | None -> keepGoing <- false
          if bytesSoFar = 0 then
            disposed.Value <- true
            disposeImpl impl
            return None
          else
            return Some(collected.ToArray())
    | _ -> return Exception.raiseInternal "readChunk: expected DStream" []
  }
