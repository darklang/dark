/// Stream builtins. Surfaces the lazy `DStream` value from `LibExecution.RuntimeTypes` to
/// user code:
/// `fromList`, `next`, `toList`, `close`, and the transforms
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

open Prelude

open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Interpreter = LibExecution.Interpreter
module Permissions = LibExecution.Permissions
module Stream = LibExecution.Stream
module Pending = LibExecution.Pending


let varA = TVariable "a"


/// Resolve the declared element TypeReference to a concrete
/// ValueType using the program's type table, so a package `T` in
/// `streamToList<T>` resolves too, even for an empty result.
let private resolveElemVT (state : ExecutionState) (t : TypeReference) : ValueType =
  LibExecution.RuntimeTypes.TypeReference.toVT
    state.types
    LibExecution.RuntimeTypes.TST.empty
    t


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
/// frame of this process and its answer handed back to the pull, waited for first where the
/// pull looks at it (a `filter` predicate, an `unfold` step) and in flight where it does not (a
/// `map`); native IO is waited for. `k` gets the pulled element, which can be a read in flight.
let rec private drivePull
  (vm : VMState)
  (k : Option<Dval * StreamImpl> -> Dval)
  (step : Stream.PullStep)
  : Dval =
  match step with
  | Stream.Pulled r -> k r
  | Stream.Apply(app, arg, next, looks) ->
    Interpreter.requestApply vm app arg ValueNone (fun dv ->
      if looks then
        Interpreter.withValue vm dv (fun v -> drivePull vm k (next v))
      else
        drivePull vm k (next dv))
  | Stream.Wait t -> t |> Pending.map (drivePull vm k)


let fns () : List<BuiltInFn> =
  [ { name = fn "streamFromList" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "items" (TList varA) "" ]
      returnType = TStream varA
      description =
        "Constructs a stream that yields the given list's items in order, then Done."
      fn =
        (function
        | state, _, [ elemType ], [| DList(elemVT, Items(items)) |] ->
          // Prefer the runtime ValueType of the list elements; fall
          // back to the declared type parameter when the list was
          // empty (ValueType.Unknown). Goes through the full type
          // table so custom types resolve correctly.
          let inferredElem =
            match elemVT with
            | ValueType.Unknown -> resolveElemVT state elemType
            | known -> known
          DStream(FromList(items, inferredElem))
        | _ -> incorrectArgs ())
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
          let elemType = resolveElemVT state outputType
          // The step runs on later pulls, as a frame of whoever is pulling; the access this
          // frame holds now is folded into it (`narrowedBy`).
          let step = narrowedBy vm.activeAccess app
          DStream(Unfold(step, initialState, elemType))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "streamNext" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "stream" (TStream varA) "" ]
      returnType = TypeReference.option (TTuple(varA, TStream varA, []))
      description =
        "The first element of <param stream> and the rest of it, or None when it is empty. Does not change <param stream>: calling again on it answers the same."
      fn =
        (function
        | state, vm, [ elemType ], [| s |] ->
          // The element type first: it can wait (a package type), and a request has to be the
          // body's first move or come after the pull's own wait, never after this one.
          let elemVT = resolveElemVT state elemType
          let pairKT = KTTuple(elemVT, ValueType.Known(KTStream elemVT), [])
          let answer r =
            r
            |> Option.map (fun (v, rest) -> DTuple(v, DStream rest, []))
            |> Dval.option pairKT
          drivePull vm answer (Stream.pullNext s)
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "streamToList" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "stream" (TStream varA) "" ]
      returnType = TList varA
      description = "All the elements of <param stream>, as a List."
      fn =
        (function
        | state, vm, [ elemType ], [| s |] ->
          // Filled in place: threading a list through the continuation instead costs a
          // closure per element and a reversal.
          let collected = ResizeArray<Dval>()
          // Prefer the first drained element's actual ValueType: it captures the lambda's
          // real return type even when the wrapper couldn't tell us via a `'b` bind. Fall
          // back to the declared type-arg for empty results.
          let finish () : Dval =
            if collected.Count > 0 then
              (DList(Dval.toValueType collected[0], Items(List.ofSeq collected)))
            else
              let elemVT = resolveElemVT state elemType
              DList(elemVT, Items([]))
          let rec onPulled (r : Option<Dval * StreamImpl>) : Dval =
            match r with
            | Some(item, rest) ->
              collected.Add item
              drivePull vm onPulled (Stream.pull rest)
            | None -> finish ()
          drivePull vm onPulled (Stream.pullNext s)
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "streamClose" 0
      typeParams = [ "a" ]
      parameters = [ Param.make "stream" (TStream varA) "" ]
      returnType = TUnit
      description =
        "Releases the IO source behind <param stream> (an HTTP response, ...) now, rather "
        + "than at its end or when nothing holds it any more. What was already read stays; "
        + "the rest of the stream ends here. Does nothing to a stream with no IO source, "
        + "and nothing the second time."
      fn =
        (function
        | _, _, _, [| DStream impl |] ->
          Stream.close impl
          DUnit
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // Lazy transforms: each is a new node over its source, which stays as it was.
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
        | state, vm, [ _; outputType ], [| DStream src; DApplicable app |] ->
          let elemType = resolveElemVT state outputType
          // The callable runs as a frame of whoever drains; this frame's access is folded
          // into it now (`narrowedBy`).
          DStream(Mapped(src, narrowedBy vm.activeAccess app, elemType))
        | _ -> incorrectArgs ())
      previewable = Pure
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
        | _, vm, _, [| DStream src; DApplicable app |] ->
          // As `streamMap`.
          DStream(Filtered(src, narrowedBy vm.activeAccess app))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "streamTake" 0
      typeParams = [ "a" ]
      parameters =
        [ Param.make "stream" (TStream varA) ""
          Param.make "n" TInt64 "Maximum number of elements to yield." ]
      returnType = TStream varA
      description =
        "Returns a new stream that yields at most the first <param n> elements "
        + "of <param stream>. If the source has fewer elements, yields them all. "
        + "Terminates early without pulling the source past the limit."
      fn =
        (function
        | _, _, _, [| DStream src; DInt64 n |] -> DStream(Take(src, max 0L n))
        | _ -> incorrectArgs ())
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
        | _, _, _, [| DList(_, Items(items)) |] ->
          let impls =
            items
            |> List.map (fun dv ->
              match dv with
              | DStream impl -> impl
              | other ->
                Exception.raiseInternal
                  "streamConcat: expected List<Stream>"
                  [ "got", other ])
          DStream(Concat impls)
        | _ -> incorrectArgs ())
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated } ]


let builtins () = LibExecution.Builtin.make [] (fns ())
