/// Stream builtins. Surfaces the lazy, single-consumer `DStream`
/// abstraction from `LibExecution.RuntimeTypes` to user code:
/// `fromList`, `next`, `toList`, `toBlob`, `close`, and the transforms
/// `map` / `filter` / `take` / `concat`.
///
/// Alternatives considered but not taken:
/// - Channels (Go-style with separate reader/writer ends and
///   buffering) would add synchronization semantics Dark does not
///   have elsewhere; revisit if fan-in/fan-out becomes a real use
///   case.
/// - Actor-mailbox (Erlang-style, with a scheduler) is out of scope
///   given Dark has no scheduler story.
module BuiltinExecution.Libs.Stream

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Blob = LibExecution.Blob
module Exe = LibExecution.Execution
module Stream = LibExecution.Stream


let varA = TVariable "a"


/// Resolve the declared element TypeReference to a concrete
/// ValueType using the program's type table. This handles primitives
/// as well as custom types — the earlier sync-only version fell back
/// to Unknown for anything non-leaf, which broke `streamToList<T>`
/// when T was a package type (empty-result case got KTUnit tagged,
/// diverging from the return annotation).
let private resolveElemVT
  (state : ExecutionState)
  (t : TypeReference)
  : Ply<ValueType> =
  LibExecution.RuntimeTypes.TypeReference.toVT state.types Map.empty t


/// Fallback-aware KnownType helper for callsites that need a
/// KnownType directly (Dval.list, Dval.option). Unknown -> KTUnit,
/// matching prior behaviour.
let private resolveElemKT
  (state : ExecutionState)
  (t : TypeReference)
  : Ply<KnownType> =
  uply {
    let! vt = resolveElemVT state t
    match vt with
    | ValueType.Known kt -> return kt
    | ValueType.Unknown -> return KTUnit
  }


let fns () : List<BuiltInFn> =
  [ { name = fn "streamFromList" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "items" (TList varA) "" ]
      returnType = TStream varA
      description =
        "Constructs a stream that yields the given list's items in order, then Done."
      fn =
        (function
        | state, _, [ elemType ], [ DList(elemVT, items) ] ->
          uply {
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
            // empty (ValueType.Unknown). Goes through the full type
            // table so custom types resolve correctly.
            let! inferredElem =
              match elemVT with
              | ValueType.Unknown -> resolveElemVT state elemType
              | known -> Ply known
            return Stream.newFromIO inferredElem nextFn None
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "streamUnfold" 0
      typeParams = [ "s"; "a" ]
      parameters =
        [ Param.make "initial" (TVariable "s") "Initial state."
          Param.makeWithArgs
            "step"
            (TFn(
              NEList.singleton (TVariable "s"),
              TypeReference.option (TTuple(TVariable "a", TVariable "s", []))
            ))
            "Called with the current state; returns Some of a (next element, next
             state) pair to emit another element, or None to end the stream."
            [ "state" ] ]
      returnType = TStream(TVariable "a")
      description =
        "Constructs a stream from a seed state and a step function. The step is
         called once per pull — return Some of a (value, nextState) tuple to
         yield an element, or None to end the stream. Useful for writing custom
         lazy producers in Dark: file line readers, paginated API iterators,
         protocol parsers like SSE, etc."
      fn =
        (function
        | state, vm, [ _; outputType ], [ initialState; DApplicable app ] ->
          uply {
            let! elemType = resolveElemVT state outputType
            let currentState = ref initialState
            let next () : Ply<Option<Dval>> =
              uply {
                let! result =
                  Exe.executeApplicable
                    state
                    app
                    (NEList.singleton currentState.Value)
                match result with
                | Ok(DEnum(_, _, _, "Some", [ DTuple(elem, newState, []) ])) ->
                  currentState.Value <- newState
                  return Some elem
                | Ok(DEnum(_, _, _, "None", _)) -> return None
                | Ok other ->
                  return
                    Exception.raiseInternal
                      "streamUnfold step must return Option<(a, s)>"
                      [ "got", other ]
                | Error(rte, _cs) -> return raiseRTE vm.threadID rte
              }
            return Stream.newFromIO elemType next None
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "streamNext" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "stream" (TStream varA) "" ]
      returnType = TypeReference.option varA
      description =
        "Pulls the next element from <param stream>. Returns None when the stream is exhausted. Mutates the stream — subsequent calls after exhaustion keep returning None."
      fn =
        (function
        | state, _, [ elemType ], [ s ] ->
          uply {
            let! nextResult = Stream.readNext s
            let! elemKT = resolveElemKT state elemType
            return Dval.option elemKT nextResult
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
        | state, _, [ elemType ], [ s ] ->
          uply {
            let collected = ResizeArray<Dval>()
            let mutable keepGoing = true
            while keepGoing do
              let! result = Stream.readNext s
              match result with
              | Some item -> collected.Add item
              | None -> keepGoing <- false
            // Prefer the first drained element's actual ValueType — it
            // captures the lambda's real return type even when the
            // wrapper couldn't tell us via a `'b` bind. Fall back to
            // the declared type-arg for empty results.
            let! elemVT =
              if collected.Count > 0 then
                Ply(Dval.toValueType collected[0])
              else
                resolveElemVT state elemType
            return DList(elemVT, List.ofSeq collected)
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
            // Drain via `readStreamChunk` so IO-backed byte streams
            // (HttpClient.stream) hand back a whole buffer per pull
            // instead of boxing one DUInt8 per byte. Falls back to
            // byte-wise pulls for streams without a `nextChunk`
            // (Mapped/Filtered/Take/Concat transforms, in-memory
            // fromList streams).
            use collected = new System.IO.MemoryStream()
            let mutable keepGoing = true
            while keepGoing do
              let! chunk = Stream.readChunk (64 * 1024) s
              match chunk with
              | Some buf -> collected.Write(buf, 0, buf.Length)
              | None -> keepGoing <- false
            return Blob.newEphemeral state (collected.ToArray())
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
        | _, _, _, [ DStream(impl, disposed, _lockObj) ] ->
          // Flip disposed and run the disposer chain on the first
          // close so IO sources (HTTP response, file handle, ...) are
          // released promptly. No Monitor: the Dark VM is single-
          // threaded per pull, and concurrent streamClose+readStream
          // races would fight Ply's thread-hopping continuations
          // (Monitor.Exit throws across threads). Idempotent via the
          // disposed flag.
          if not disposed.Value then
            disposed.Value <- true
            Stream.disposeImpl impl
          DUnit |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    // Lazy transforms. Each wraps the corresponding StreamImpl
    // constructor. The source DStream's impl is extracted and placed
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
          uply {
            let! elemType = resolveElemVT state outputType
            let apply (dv : Dval) : Ply<Dval> =
              uply {
                let! result = Exe.executeApplicable state app (NEList.singleton dv)
                match result with
                | Ok v -> return v
                | Error(rte, _cs) -> return raiseRTE vm.threadID rte
              }
            return Stream.wrapImpl (Mapped(src, apply, elemType))
          }
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
          Stream.wrapImpl (Filtered(src, pred)) |> Ply
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
          Stream.wrapImpl (Take(src, clamped, ref clamped)) |> Ply
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
          Stream.wrapImpl (Concat(ref impls)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
