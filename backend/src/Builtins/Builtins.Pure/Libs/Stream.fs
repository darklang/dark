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
/// - Actor mailboxes are not streams: `Exec.spawn` and `Host.await`
///   are the process story, this is the lazy-sequence one.
module Builtins.Pure.Libs.Stream

open System.Threading.Tasks
open FSharp.Control.Tasks

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Blob = LibExecution.Blob
module Interpreter = LibExecution.Interpreter
module Permissions = LibExecution.Permissions
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
  LibExecution.RuntimeTypes.TypeReference.toVT
    state.types
    LibExecution.RuntimeTypes.TST.empty
    t


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


/// A transform's callable, with the access of the frame building the transform folded in.
/// The callable runs later, as a frame of whoever pulls, under that frame's access narrowed by
/// what the callable captured (`Interpreter.requestApply`); folding the builder's in keeps a
/// narrow producer's transform narrow when a wider consumer drains it, and it is done once,
/// here, rather than on every pull.
let private narrowedBy
  (access : Permissions.Access)
  (app : Applicable)
  : Applicable =
  match app with
  | AppLambda l ->
    AppLambda { l with access = l.access |> Permissions.Access.constrainBy access }
  | AppNamedFn n ->
    let captured =
      match n.access with
      | Some a -> a |> Permissions.Access.constrainBy access
      | None -> access
    AppNamedFn { n with access = Some captured }


/// Drive a pull to its end from a builtin: a callable the stream hands back is applied as a
/// frame of this process, its answer forced (a read still in flight is waited for) and handed
/// back to the pull; native IO is waited for. `k` gets the pulled element.
let rec private drivePull
  (vm : VMState)
  (k : Option<Dval> -> Ply<Dval>)
  (step : Stream.PullStep)
  : Ply<Dval> =
  match step with
  | Stream.Pulled r -> k r
  | Stream.Apply(app, arg, next) ->
    Interpreter.requestApply vm app arg [] (fun dv ->
      Interpreter.withValue vm dv (fun v -> drivePull vm k (next v)))
  | Stream.Wait ply ->
    uply {
      let! landed = ply
      return! drivePull vm k landed
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
        | state, _, [ elemType ], [| DList(elemVT, items) |] ->
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
      callEffects = Set.empty
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
        "Constructs a stream from a seed state and a step function. The step is "
        + "called once per pull — return Some of a (value, nextState) tuple to "
        + "yield an element, or None to end the stream. Useful for writing custom "
        + "lazy producers in Dark: file line readers, paginated API iterators, "
        + "protocol parsers like SSE, etc."
      fn =
        (function
        | state, vm, [ _; outputType ], [| initialState; DApplicable app |] ->
          uply {
            let! elemType = resolveElemVT state outputType
            // The step runs on later pulls, as a frame of whoever is pulling; the access this
            // frame holds now is folded into it (`narrowedBy`).
            let step = narrowedBy vm.activeAccess app
            return Stream.wrapImpl (Unfold(step, ref initialState, elemType))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "streamNext" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "stream" (TStream varA) "" ]
      returnType = TypeReference.option varA
      description =
        "Pulls the next element from <param stream>. Returns None when the stream is exhausted. Mutates the stream — subsequent calls after exhaustion keep returning None."
      fn =
        (function
        | state, vm, [ elemType ], [| s |] ->
          // The element type first: it can wait (a package type), and a request has to be the
          // body's first move or come after the pull's own wait, never after this one.
          match Ply.trySync (resolveElemKT state elemType) with
          | ValueSome elemKT ->
            drivePull vm (fun r -> Ply(Dval.option elemKT r)) (Stream.pullNext s)
          | ValueNone ->
            uply {
              let! elemKT = resolveElemKT state elemType
              return!
                drivePull vm (fun r -> Ply(Dval.option elemKT r)) (Stream.pullNext s)
            }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "streamToList" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "stream" (TStream varA) "" ]
      returnType = TList varA
      description = "Drains <param stream> into a List, consuming it entirely."
      fn =
        (function
        | state, vm, [ elemType ], [| s |] ->
          let collected = ResizeArray<Dval>()
          // Prefer the first drained element's actual ValueType: it captures the lambda's
          // real return type even when the wrapper couldn't tell us via a `'b` bind. Fall
          // back to the declared type-arg for empty results.
          let finish () : Ply<Dval> =
            if collected.Count > 0 then
              Ply(DList(Dval.toValueType collected[0], List.ofSeq collected))
            else
              uply {
                let! elemVT = resolveElemVT state elemType
                return DList(elemVT, [])
              }
          let rec onPulled (r : Option<Dval>) : Ply<Dval> =
            match r with
            | Some item ->
              collected.Add item
              drivePull vm onPulled (Stream.pullNext s)
            | None -> finish ()
          drivePull vm onPulled (Stream.pullNext s)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "streamToBlob" 0
      typeParams = []
      parameters = [ Param.make "stream" (TStream TUInt8) "" ]
      returnType = TBlob
      description =
        "Drains a byte stream into a single ephemeral Blob, consuming <param stream>."
      fn =
        (function
        | _, _, _, [| DStream(FromIO(_, _, _, Some _), _, _) as s |] ->
          uply {
            // An IO-backed byte stream (HttpClient.stream) hands back a whole buffer per
            // pull instead of boxing one DUInt8 per byte.
            use collected = new System.IO.MemoryStream()
            let mutable keepGoing = true
            while keepGoing do
              let! chunk = Stream.readChunk (64 * 1024) s
              match chunk with
              | Some buf -> collected.Write(buf, 0, buf.Length)
              | None -> keepGoing <- false
            return Blob.newEphemeral (collected.ToArray())
          }
        | _, vm, _, [| DStream _ as s |] ->
          // Anything else (a transform chain, an in-memory stream) is pulled a byte at a time;
          // a transform's callable runs as a frame of this process.
          let collected = new System.IO.MemoryStream()
          let rec onPulled (r : Option<Dval>) : Ply<Dval> =
            match r with
            | Some(DUInt8 b) ->
              collected.WriteByte b
              drivePull vm onPulled (Stream.pullNext s)
            | Some _ ->
              Exception.raiseInternal
                "streamToBlob: expected Stream<UInt8> element"
                []
            | None ->
              let bytes = collected.ToArray()
              collected.Dispose()
              Ply(Blob.newEphemeral bytes)
          drivePull vm onPulled (Stream.pullNext s)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "streamClose" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "stream" (TStream varA) "" ]
      returnType = TUnit
      description =
        "Marks <param stream> as fully consumed. Idempotent — calling on an "
        + "already-closed stream is a no-op. Not strictly required (streams are "
        + "GC-closed via their .NET finalizer), but useful when you want to "
        + "release an IO source promptly."
      fn =
        (function
        | _, _, _, [| DStream(impl, disposed, _lockObj) |] ->
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
      callEffects = Set.empty
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
        "Returns a new stream whose elements are <param fn> applied to each "
        + "element of <param stream>. Lazy — <param fn> runs only when the "
        + "returned stream is drained."
      fn =
        (function
        | state, vm, [ _; outputType ], [| DStream(src, _, _); DApplicable app |] ->
          uply {
            let! elemType = resolveElemVT state outputType
            // The callable runs as a frame of whoever drains; this frame's access is folded
            // into it now (`narrowedBy`).
            return
              Stream.wrapImpl (Mapped(src, narrowedBy vm.activeAccess app, elemType))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      callEffects = Set.empty
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
        "Returns a new stream that yields only the elements of <param stream> "
        + "for which <param pred> returns true. Lazy — <param pred> runs as the "
        + "result is drained, skipping rejected elements without buffering."
      fn =
        (function
        | _, vm, _, [| DStream(src, _, _); DApplicable app |] ->
          // As `streamMap`.
          Stream.wrapImpl (Filtered(src, narrowedBy vm.activeAccess app)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "streamTake" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.make "stream" (TStream varA) ""
          Param.make "n" TInt "Maximum number of elements to yield." ]
      returnType = TStream varA
      description =
        "Returns a new stream that yields at most the first <param n> elements "
        + "of <param stream>. If the source has fewer elements, yields them all. "
        + "Terminates early without pulling the source past the limit."
      fn =
        (function
        | _, _, _, [| DStream(src, _, _); DInt n |] ->
          // Clamp on the arbitrary-precision value before narrowing: a negative n
          // (however large) becomes an empty stream, and an n past Int64 just
          // takes everything. pullStreamImpl treats remaining<=0 as done.
          let n = DarkInt.toBigInt n
          let clamped =
            if n < bigint 0 then 0L
            elif n > bigint System.Int64.MaxValue then System.Int64.MaxValue
            else int64 n
          Stream.wrapImpl (Take(src, clamped, ref clamped)) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "streamConcat" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.make "streams" (TList(TStream varA)) "Streams to concatenate." ]
      returnType = TStream varA
      description =
        "Returns a new stream that drains <param streams> in list order, "
        + "advancing to the next sub-stream when the current one is exhausted."
      fn =
        (function
        | _, _, _, [| DList(_, items) |] ->
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
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
