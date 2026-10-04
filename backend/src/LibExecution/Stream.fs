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
  | Unfold _ -> ()
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


/// One pull through a stream, as far as it can go without the puller: what the puller has
/// to do next. A node that runs Dark code (`Unfold`, `Mapped`, `Filtered`) cannot run it
/// here; it hands the callable back, and the builtin pulling asks the interpreter to apply
/// it as a frame of the pulling process (`Interpreter.requestApply`), then continues the
/// pull with the answer. Native IO that has to wait is handed back as a wait for the same
/// reason: the puller decides how to wait.
type PullStep =
  /// This pull is done: the element, or `None` for exhausted.
  | Pulled of Option<Dval>
  /// Apply the callable to the argument, then continue with what it answered (a value, not a
  /// read still in flight: the puller forces it first).
  | Apply of applicable : Applicable * arg : Dval * next : (Dval -> PullStep)
  /// Native IO under way; continue with what it lands.
  | Wait of Ply.Ply<PullStep>

/// Continue a step with what to do once its pull is done.
let rec private andThen (step : PullStep) (k : Option<Dval> -> PullStep) : PullStep =
  match step with
  | Pulled r -> k r
  | Apply(app, arg, next) -> Apply(app, arg, (fun dv -> andThen (next dv) k))
  | Wait ply ->
    Wait(
      uply {
        let! landed = ply
        return andThen landed k
      }
    )

/// Pull one element through a [StreamImpl] tree, as a step.
///
/// A `Filtered` node's rejections and a `Concat` node's exhausted heads recurse here, but not
/// on the F# stack for long: a rejection goes back to the puller as an `Apply` and comes back
/// through the continuation, so each turn starts fresh.
let rec private pull (impl : StreamImpl) : PullStep =
  match impl with
  | FromIO(next, _elemType, _disposer, _nextChunk) ->
    let p = next ()
    match Ply.trySync p with
    | ValueSome r -> Pulled r
    | ValueNone ->
      Wait(
        uply {
          let! r = p
          return Pulled r
        }
      )

  | Unfold(step, state, _elemType) ->
    Apply(
      step,
      state.Value,
      fun result ->
        match result with
        | DEnum(_, _, _, "Some", [ DTuple(elem, newState, []) ]) ->
          state.Value <- newState
          Pulled(Some elem)
        | DEnum(_, _, _, "None", _) -> Pulled None
        | other ->
          Exception.raiseInternal
            "streamUnfold step must return Option<(a, s)>"
            [ "got", other ]
    )

  | Mapped(src, fn, _elemType) ->
    andThen (pull src) (fun upstream ->
      match upstream with
      | None -> Pulled None
      | Some v -> Apply(fn, v, (fun mapped -> Pulled(Some mapped))))

  | Filtered(src, pred) ->
    andThen (pull src) (fun upstream ->
      match upstream with
      | None -> Pulled None
      | Some v ->
        Apply(
          pred,
          v,
          fun answer ->
            match answer with
            | DBool true -> Pulled(Some v)
            | DBool false -> pull impl
            | other ->
              Exception.raiseInternal
                "stream filter predicate returned non-Bool"
                [ "got", other ]
        ))

  | Take(src, _n, remaining) ->
    if remaining.Value <= 0L then
      Pulled None
    else
      andThen (pull src) (fun upstream ->
        match upstream with
        | Some _ ->
          remaining.Value <- remaining.Value - 1L
          Pulled upstream
        | None ->
          // Source dried up before the limit; clamp so future pulls
          // stay at zero and short-circuit without touching source.
          remaining.Value <- 0L
          Pulled None)

  | Concat streams ->
    // Pull from the head stream; when it's exhausted, drop it and
    // try the next. Mutating the ref means future pulls don't
    // re-enter a drained stream.
    match streams.Value with
    | [] -> Pulled None
    | head :: tail ->
      andThen (pull head) (fun pulled ->
        match pulled with
        | Some _ -> Pulled pulled
        | None ->
          streams.Value <- tail
          pull impl)


/// Pull the next element from a stream, as a step. `None` when the stream is exhausted, and
/// from then on (single-consumer: once drained, stays drained); the first `None` runs the
/// disposers.
let pullNext (dv : Dval) : PullStep =
  match dv with
  | DStream(impl, disposed, _lockObj) ->
    if disposed.Value then
      Pulled None
    else
      andThen (pull impl) (fun result ->
        match result with
        | Some _ -> Pulled result
        | None ->
          disposed.Value <- true
          disposeImpl impl
          Pulled None)
  | _ -> Exception.raiseInternal "pullNext: expected DStream" []


/// A pull driven to its end here, for F# callers with no process to run Dark code in (tests,
/// the benchmark scenarios, the HTTP client's own byte streams). Loud on a node that needs Dark
/// code: that stream has to be pulled from a Dark process, through the stream builtins.
let rec private runNative (step : PullStep) : Ply.Ply<Option<Dval>> =
  match step with
  | Pulled r -> Ply r
  | Wait ply ->
    uply {
      let! landed = ply
      return! runNative landed
    }
  | Apply _ ->
    Exception.raiseInternal
      "this stream runs Dark code and has to be pulled from a Dark process"
      []


/// Pull the next element from a stream that runs no Dark code. Returns [None] when the
/// stream is exhausted; subsequent calls after exhaustion return [None]. The stream
/// builtins drive [pullNext] themselves; this is for F# code that owns a native stream.
let readNext (dv : Dval) : Ply.Ply<Option<Dval>> = runNative (pullNext dv)


/// Pull up to `maxBytes` bytes from a byte stream as one chunk.
/// Returns [None] when exhausted; subsequent calls stay [None].
/// Prefers a FromIO's own `nextChunk` when present; falls back to
/// byte-wise pulls for streams that were built via [newFromIO] or
/// that walk transform nodes (Take/Concat) where a chunk semantic
/// isn't well-defined. Like [readNext], for streams that run no Dark
/// code; `streamToBlob` drives the transform case itself.
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
          // `readNext` if the caller really wants bulk bytes --
          // transform chains lose the chunk optimisation but still
          // drain correctly.
          use collected = new System.IO.MemoryStream()
          let mutable keepGoing = true
          let mutable bytesSoFar = 0
          while keepGoing && bytesSoFar < maxBytes do
            let! pulled = runNative (pull impl)
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
