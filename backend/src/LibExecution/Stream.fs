/// Runtime helpers for the `Stream<'a>` Dval type.
///
/// A stream is a value: a pull answers the element and the rest, and pulling the same stream
/// again answers the same. This module owns construction, the pull, and releasing IO
/// sources; the node shapes are in `RuntimeTypes.fs` (`StreamImpl`).
module LibExecution.Stream

open System.Threading.Tasks

open Prelude

open LibExecution.RuntimeTypes


/// How much a chunked source is asked for per read.
let chunkSize = 64 * 1024


let rec private cellOf
  (source : StreamSource)
  (read : unit -> ValueTask<'r>)
  (landed : StreamCell -> 'r -> StreamRead)
  : StreamCell =
  { source = source
    read =
      lazy
        (if source.Released then
           Task.FromResult Ended
         else
           task {
             let! r = read ()
             match landed (cellOf source read landed) r with
             | Ended ->
               source.Release()
               return Ended
             | answer -> return answer
           }) }


/// A stream over a native element-at-a-time source. [disposer], when `Some`, runs once: when
/// the source reaches its end, when a stream over it is closed, or when the GC finds nothing
/// left that could read it. Use [newChunked] for byte sources that can yield whole buffers.
let newFromIO
  (elemType : ValueType)
  (next : unit -> ValueTask<Option<Dval>>)
  (disposer : (unit -> unit) option)
  : Dval =
  let landed (after : StreamCell) (r : Option<Dval>) =
    match r with
    | Some v -> Item(v, after)
    | None -> Ended
  DStream(FromIO(cellOf (StreamSource disposer) next landed, elemType))


/// A byte stream over a source that fills up to `maxBytes` into a fresh byte[] per read (or
/// None at its end). It keeps whole buffers, so draining it to Bytes copies buffers rather
/// than boxing a `DUInt8` per byte; `Stream.next` still answers one byte at a time.
let newChunked
  (elemType : ValueType)
  (nextChunk : int -> ValueTask<Option<byte[]>>)
  (disposer : (unit -> unit) option)
  : Dval =
  let landed (after : StreamCell) (r : Option<byte[]>) =
    match r with
    | Some buf when buf.Length > 0 -> Chunk(buf, after)
    | _ -> Ended
  let read () = nextChunk chunkSize
  DStream(FromIO(cellOf (StreamSource disposer) read landed, elemType))


/// Release every IO source a stream would still read from. What was read stays readable;
/// cells not read yet end. A step's state or captures are not looked into. Idempotent.
let rec close (impl : StreamImpl) : unit =
  match impl with
  | FromIO(cell, _)
  | InChunk(_, _, cell) -> cell.source.Release()
  | FromList _
  | Unfold _ -> ()
  | Mapped(src, _, _)
  | Filtered(src, _)
  | Take(src, _) -> close src
  | Concat streams -> streams |> List.iter close


/// One pull through a stream, as far as it can go without the puller: what the puller has
/// to do next. A node that runs Dark code (`Unfold`, `Mapped`, `Filtered`) hands the callable
/// back, and the builtin pulling asks the interpreter to apply it as a frame of the pulling
/// process (`Interpreter.requestApply`), then continues the pull with the answer. Native IO
/// that has to wait is handed back as a wait for the same reason: the puller decides how.
type PullStep =
  /// This pull is done: the element and the rest of the stream, or `None` at the end.
  | Pulled of Option<Dval * StreamImpl>
  /// Apply the callable to the argument, then continue with what it answered. Where `looks`,
  /// `next` looks at the answer, so the puller waits for a read in flight first; otherwise
  /// `next` only carries it along (`Mapped`) and it stays in flight.
  | Apply of
    applicable : Applicable *
    arg : Dval *
    next : (Dval -> PullStep) *
    looks : bool
  /// Native IO under way; continue with what it lands.
  | Wait of Task<PullStep>

/// Continue a step with what to do once its pull is done.
let rec private andThen
  (step : PullStep)
  (k : Option<Dval * StreamImpl> -> PullStep)
  : PullStep =
  match step with
  | Pulled r -> k r
  | Apply(app, arg, next, looks) ->
    Apply(app, arg, (fun dv -> andThen (next dv) k), looks)
  | Wait t ->
    Wait(
      task {
        let! landed = t
        return andThen landed k
      }
    )


let private inChunk (buf : byte[]) (offset : int) (rest : StreamCell) : PullStep =
  Pulled(Some(RuntimeTypes.Dval.duint8 buf[offset], InChunk(buf, offset + 1, rest)))

let private readCell (cell : StreamCell) (elemType : ValueType) : PullStep =
  let landed (r : StreamRead) =
    match r with
    | Item(v, after) -> Pulled(Some(v, FromIO(after, elemType)))
    | Chunk(buf, after) -> inChunk buf 0 after
    | Ended -> Pulled None
  let t = cell.read.Force()
  if t.IsCompletedSuccessfully then
    landed t.Result
  else
    Wait(
      task {
        let! r = t
        return landed r
      }
    )


/// Pull one element through a stream, as a step.
///
/// A `Filtered` node's rejections and a `Concat` node's ended heads recurse here, but not on
/// the F# stack for long: a rejection goes back to the puller as an `Apply` and comes back
/// through the continuation, so each turn starts fresh.
let rec pull (impl : StreamImpl) : PullStep =
  match impl with
  | FromIO(cell, elemType) -> readCell cell elemType

  | InChunk(buf, offset, rest) ->
    if offset < buf.Length then
      inChunk buf offset rest
    else
      readCell rest (ValueType.Known KTUInt8)

  | FromList(items, elemType) ->
    match items with
    | [] -> Pulled None
    | head :: tail -> Pulled(Some(head, FromList(tail, elemType)))

  | Unfold(step, state, elemType) ->
    Apply(
      step,
      state,
      (fun result ->
        match result with
        | DEnum(_, _, _, "Some", [ DTuple(elem, newState, []) ]) ->
          Pulled(Some(elem, Unfold(step, newState, elemType)))
        | DEnum(_, _, _, "None", _) -> Pulled None
        | other ->
          Exception.raiseInternal
            "streamUnfold step must return Option<(a, s)>"
            [ "got", other ]),
      true
    )

  | Mapped(src, fn, elemType) ->
    andThen (pull src) (fun upstream ->
      match upstream with
      | None -> Pulled None
      | Some(v, rest) ->
        Apply(
          fn,
          v,
          (fun mapped -> Pulled(Some(mapped, Mapped(rest, fn, elemType)))),
          false
        ))

  | Filtered(src, pred) ->
    andThen (pull src) (fun upstream ->
      match upstream with
      | None -> Pulled None
      | Some(v, rest) ->
        Apply(
          pred,
          v,
          (fun answer ->
            match answer with
            | DBool true -> Pulled(Some(v, Filtered(rest, pred)))
            | DBool false -> pull (Filtered(rest, pred))
            | other ->
              Exception.raiseInternal
                "stream filter predicate returned non-Bool"
                [ "got", other ]),
          true
        ))

  | Take(src, n) ->
    if n <= 0L then
      Pulled None
    else
      andThen (pull src) (fun upstream ->
        match upstream with
        | Some(v, rest) -> Pulled(Some(v, Take(rest, n - 1L)))
        | None -> Pulled None)

  | Concat streams ->
    match streams with
    | [] -> Pulled None
    | head :: tail ->
      andThen (pull head) (fun pulled ->
        match pulled with
        | Some(v, rest) -> Pulled(Some(v, Concat(rest :: tail)))
        | None -> pull (Concat tail))


/// Pull the next element from a stream, as a step.
let pullNext (dv : Dval) : PullStep =
  match dv with
  | DStream impl -> pull impl
  | _ -> Exception.raiseInternal "pullNext: expected DStream" []


/// A pull driven to its end here, for F# callers with no process to run Dark code in (tests,
/// the benchmark scenarios). Loud on a node that needs Dark code: that stream has to be pulled
/// from a Dark process, through the stream builtins.
let rec private runNative (step : PullStep) : Task<Option<Dval * StreamImpl>> =
  match step with
  | Pulled r -> Task.FromResult r
  | Wait t ->
    task {
      let! landed = t
      return! runNative landed
    }
  | Apply _ ->
    Exception.raiseInternal
      "this stream runs Dark code and has to be pulled from a Dark process"
      []


/// The next element of a stream that runs no Dark code, and the rest of the stream.
let readNext (dv : Dval) : Task<Option<Dval * Dval>> =
  task {
    match! runNative (pullNext dv) with
    | Some(v, rest) -> return Some(v, DStream rest)
    | None -> return None
  }


/// All the bytes of a byte stream read straight off an IO source, a whole chunk at a time;
/// `None` for any other stream, which has to be pulled element by element.
let readAllBytes (dv : Dval) : Option<Task<byte[]>> =
  let fromCell (start : StreamCell) (prefix : byte[]) =
    task {
      use collected = new System.IO.MemoryStream()
      collected.Write(prefix, 0, prefix.Length)
      let mutable cell = start
      let mutable reading = true
      while reading do
        match! cell.read.Force() with
        | Chunk(buf, after) ->
          collected.Write(buf, 0, buf.Length)
          cell <- after
        | Item(DUInt8 b, after) ->
          collected.WriteByte b
          cell <- after
        | Item _ ->
          Exception.raiseInternal "readAllBytes: expected Stream<UInt8> element" []
        | Ended -> reading <- false
      return collected.ToArray()
    }
  match dv with
  | DStream(FromIO(cell, _)) -> Some(fromCell cell [||])
  | DStream(InChunk(buf, offset, rest)) ->
    Some(fromCell rest (Array.sub buf offset (max 0 (buf.Length - offset))))
  | _ -> None
