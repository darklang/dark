module Builtins.Pure.Libs.List

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Interpreter = LibExecution.Interpreter
module TypeChecker = LibExecution.TypeChecker
module Spread = LibExecution.Spread
module Pending = LibExecution.Pending
module RTE = RuntimeError


module DvalComparator =
  type Order =
    | Less
    | Equal
    | Greater

  // Typed, not generic: a generic `compare` boxes both ints, two allocations on every comparison
  // a sort makes.
  let order (v1 : int) (v2 : int) : Order =
    if v1 < v2 then Less
    elif v1 > v2 then Greater
    else Equal


  /// Runs one comparison, turning what `DvalOrdering` raises into a Dark error.
  let private guarded
    (compare : Dval -> Dval -> int)
    (dv1 : Dval)
    (dv2 : Dval)
    : int =
    try
      compare dv1 dv2
    with
    | :? DvalComparisonException as e ->
      RuntimeError.Error.EqualityCheckOnIncompatibleTypes(
        Dval.toValueType e.Left,
        Dval.toValueType e.Right
      )
      |> raiseUntargetedRTE
    | :? DvalUnsortableException as e ->
      RuntimeError.Error.List(RuntimeError.Lists.CannotSort e.Value)
      |> raiseUntargetedRTE

  let compareDval (dv1 : Dval) (dv2 : Dval) : Order =
    order (guarded DvalOrdering.compareForSort dv1 dv2) 0

  let compareDvalInt v1 v2 =
    match compareDval v1 v2 with
    | Greater -> 1
    | Less -> -1
    | Equal -> 0

  /// .NET's sort wraps whatever a comparer raises in an InvalidOperationException,
  /// so a Dark runtime error from a comparison (Bytes, a lambda) would surface as
  /// an uncaught .NET one. Rethrow what the comparer raised.
  let sortWith (cmp : 'a -> 'a -> int) (list : List<'a>) : List<'a> =
    try
      List.sortWith cmp list
    with :? System.InvalidOperationException as e when not (isNull e.InnerException) ->
      System.Runtime.ExceptionServices.ExceptionDispatchInfo
        .Capture(e.InnerException)
        .Throw()
      []

  let sortArrayWith (cmp : 'a -> 'a -> int) (arr : 'a[]) : 'a[] =
    try
      Array.sortWith cmp arr
    with :? System.InvalidOperationException as e when not (isNull e.InnerException) ->
      System.Runtime.ExceptionServices.ExceptionDispatchInfo
        .Capture(e.InnerException)
        .Throw()
      [||]


let varA = TVariable "a"
let varB = TVariable "b"
let varC = TVariable "c"


/// Sort (key, value) pairs the way the Dark `sortBy` did.
///
/// It was `map (fun x -> (fn x, x)) |> sort |> map Tuple2.second`, so it sorted the *tuple*, and
/// comparing a tuple compares its fields in order. The tie-break on the value is therefore not
/// incidental -- dropping it would silently reorder elements with equal keys.
let private sortedByKey
  (vt : ValueType)
  (keyed : List<struct (Dval * Dval)>)
  : Dval =
  keyed
  |> DvalComparator.sortWith (fun (struct (k1, v1)) (struct (k2, v2)) ->
    let c = DvalComparator.compareDvalInt k1 k2
    if c <> 0 then c else DvalComparator.compareDvalInt v1 v2)
  |> List.map (fun (struct (_, v)) -> v)
  |> fun l -> DList(vt, Items(l))


/// `sortedByKey`, for elements that may be reads in flight. The tie-break compares elements, so
/// one whose key ties another's is waited for first; every other element stays in flight.
let private sortedByKeyLanding
  (vm : VMState)
  (vt : ValueType)
  (keyed : List<struct (Dval * Dval)>)
  : Ply<Dval> =
  if not (List.exists (fun (struct (_, v)) -> Pending.isPromise v) keyed) then
    Ply(sortedByKey vt keyed)
  else
    let entries = Array.ofList keyed
    let keyAt i =
      let (struct (k, _)) = entries[i]
      k
    let byKey =
      Array.init entries.Length (fun i -> i)
      |> DvalComparator.sortArrayWith (fun a b ->
        DvalComparator.compareDvalInt (keyAt a) (keyAt b))
    let tied = Array.zeroCreate<bool> entries.Length
    for j in 1 .. byKey.Length - 1 do
      if DvalComparator.compareDvalInt (keyAt byKey[j - 1]) (keyAt byKey[j]) = 0 then
        tied[byKey[j - 1]] <- true
        tied[byKey[j]] <- true
    let rec landTied (i : int) : Ply<Dval> =
      if i = entries.Length then
        Ply(sortedByKey vt (List.ofArray entries))
      else
        match entries[i] with
        | struct (k, (DPromise _ as v)) when tied[i] ->
          Interpreter.withValue vm v (fun landed ->
            entries[i] <- struct (k, landed)
            landTied (i + 1))
        | _ -> landTied (i + 1)
    landTied 0


/// The result of a `map`: its element type comes from the values the lambda returned.
///
/// Same shape as `listFlatten`. Merge the element ValueTypes where they agree and build the list
/// directly; where they do not, hand it to `DvalCreator.list`, which is the general path and reports
/// the mismatch properly. Accumulating with `prepend`, which is what the Dark version did, merged the
/// same types one element at a time.
let private mappedList (vm : VMState) (items : DvalList) : Dval =
  let merged =
    items
    |> DvalList.fold
      (fun acc item ->
        match acc with
        | Ok accVt ->
          match VT.mergeDval accVt item with
          | Ok m -> Ok m
          | Error() -> Error()
        | Error() -> Error())
      (Ok VT.unknown)

  match merged with
  | Ok vt -> DList(vt, items)
  | Error() ->
    TypeChecker.DvalCreator.list vm.threadID VT.unknown (DvalList.toList items)


/// A `filterMap` function that returned something other than an `Option`.
///
/// The Dark version matched `Some`/`None` and would fail with "No matching case found" on anything
/// else; this reports the type instead, which is strictly more useful and is what the other
/// wrong-return-type helpers here do.
let private notAnOption (actual : Dval) =
  RuntimeError.UncaughtException(
    "filterMap's function must return an Option",
    [ "actual", actual ]
  )


/// A `filter` predicate returned something other than a bool.
///
/// The same error the Dark version raised, since its body was `if f elem then ... else ...` and this
/// is what the interpreter says about an `if` on a non-bool.
let private predicateNotBool (actual : Dval) =
  RuntimeError.Bool(
    RuntimeError.Bools.ConditionRequiresBool(Dval.toValueType actual, actual)
  )


/// A list op over one list whose callable takes one element (`Spread.Walk`). A chunk is
/// `builtinName` over its stretch of the list.
[<AbstractClass>]
type private OverList<'acc>
  (
    state : ExecutionState,
    vm : VMState,
    app : Applicable,
    vt : ValueType,
    builtinName : string,
    floor : int64
  ) =
  inherit Spread.Walk<'acc>(state, vm, app, builtinName, floor)
  override _.Apply(_, elem, next) = Interpreter.requestApply vm app elem [] next
  override _.ChunkArgs(_, chunk) = [ DList(vt, chunk) ]

/// What a map has so far, back to front, once a spread came back: the chunks' results onto it.
let private resultsOnto (acc : List<Dval>) (spread : Spread.Outcome) : List<Dval> =
  DvalList.fold (fun acc result -> result :: acc) acc spread.results


// Every builtin here that takes a callable applies it as a frame on the caller's own stack
// (`Interpreter.requestApply`), not a nested VM: `ps` sees the lambda, the budget can preempt
// it, and a read in it parks the process. Where the builtin looks at the answer (a predicate,
// a key, the value built so far) the answer is forced; where it only collects it (`map`), a
// read still in flight stays in flight and the list comes back as one promise.
/// A stable merge sort that asks `lessThan` for every comparison: `List.sort` for an element type
/// with its own `Compare`, so the order is the one `<` gives. Written as continuations because each
/// comparison is an interpreted call (`requestApply`), which also keeps a long sort preemptible and
/// visible in `ps`. The sorted list exists only in the last continuation, so a sort stopped halfway
/// stops with its process rather than answering a half-sorted list.
let private sortByLessThan
  (vm : VMState)
  (lessThan : Applicable)
  (vt : ValueType)
  (items : List<Dval>)
  : Ply<Dval> =
  let rec sortK (xs : List<Dval>) (k : List<Dval> -> Ply<Dval>) : Ply<Dval> =
    match xs with
    | []
    | [ _ ] -> k xs
    | _ ->
      let (left, right) = List.splitAt (List.length xs / 2) xs
      sortK left (fun sortedLeft ->
        sortK right (fun sortedRight -> mergeK sortedLeft sortedRight [] k))
  and mergeK
    (left : List<Dval>)
    (right : List<Dval>)
    (acc : List<Dval>)
    (k : List<Dval> -> Ply<Dval>)
    : Ply<Dval> =
    match left, right with
    | [], rest
    | rest, [] -> k (List.rev acc @ rest)
    | l :: lt, r :: rt ->
      // The right one goes first only when it is strictly less, which keeps equal values in order.
      Interpreter.requestApply vm lessThan r [ l ] (fun answer ->
        Interpreter.withValue vm answer (fun answer ->
          match answer with
          | DBool true -> mergeK left rt (r :: acc) k
          | DBool false -> mergeK lt right (l :: acc) k
          | other ->
            Exception.raiseInternal
              "Compare.lessThan answered a non-Bool during List.sort"
              [ "answer", other ]))
  sortK items (fun sorted -> Ply(DList(vt, Items(sorted))))


let fns () : List<BuiltInFn> =
  [ { name = fn "listFold" 0
      typeParams = []
      parameters =
        [ Param.moved "list" (TList varA) ""
          Param.moved "init" varB ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.ofList varB [ varA ], varB))
            ""
            [ "acc"; "elem" ] ]
      returnType = varB
      description =
        "Folds <param list> into a single value, by calling <param fn> on each element with the "
        + "value built so far"
      fn =
        (function
        | _, vm, [], [| DList(_, items); init; DApplicable app |] ->
          // The value so far goes to the next step as it is: calling the lambda with a read
          // in flight is not a use of it.
          if DvalList.isEmpty items then
            Ply init
          else
            let mutable rest = DvalList.tail items
            let rec next (stepped : Dval) : Ply<Dval> =
              if DvalList.isEmpty rest then
                Ply stepped
              else
                let elem = DvalList.head rest
                rest <- DvalList.tail rest
                Interpreter.requestApply vm app stepped [ elem ] next
            Interpreter.requestApply vm app init [ DvalList.head items ] next
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listTake" 0
      typeParams = []
      parameters =
        [ Param.moved "list" (TList varA) ""; Param.make "count" TInt64 "" ]
      returnType = TList varA
      description =
        "Returns the first <param count> values of <param list>, or all of them if there are fewer"
      fn =
        (function
        | _, vm, [], [| DList(_, items); DInt64 count |] ->
          if count <= 0L then
            Ply(DList(VT.unknown, DvalList.empty))
          else
            // The element ValueType is merged from what was actually taken, not inherited from the
            // source list. That is what the Dark version did -- it rebuilt the prefix with `prepend` --
            // and a prefix can be narrower than the whole.
            let taken = int (min count (int64 (DvalList.length items)))
            Ply(mappedList vm (DvalList.take taken items))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listIndexedMap" 0
      typeParams = []
      parameters =
        [ Param.moved "list" (TList varA) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.doubleton TInt64 varA, varB))
            ""
            [ "index"; "elem" ] ]
      returnType = TList varB
      description =
        "Calls <param fn> on every value in <param list> with its index, returning a list of the "
        + "results"
      fn =
        (function
        | state, vm, [], [| DList(vt, items); DApplicable app |] ->
          // Built back to front and reversed once. A read a lambda hands back stays in flight
          // in the list, which the interpreter holds as a promise. Spread as `listMap` is; a
          // chunk is `listMap2shortest` over its indices and its elements, which is the same call.
          Spread.walk
            { new Spread.Walk<List<Dval>>(state, vm, app, "listMap2shortest", 0L) with
                member _.Apply(index, elem, next) =
                  Interpreter.requestApply vm app (Dval.dint64 index) [ elem ] next
                member _.Step(acc, _, mapped) = Spread.Next(mapped :: acc)
                member _.ChunkArgs(index, chunk) =
                  let indices =
                    Array.init (DvalList.length chunk) (fun k ->
                      Dval.dint64 (index + int64 k))
                  [ DList(VT.int64, DvalList.ofArray indices); DList(vt, chunk) ]
                member _.Absorb(acc, _, spread) = resultsOnto acc spread
                member _.Finish acc = Ply(mappedList vm (DvalList.ofRevList acc)) }
            []
            items
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listSortBy" 0
      typeParams = []
      parameters =
        [ Param.moved "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn(NEList.singleton varA, varB)) "" [ "elem" ] ]
      returnType = TList varA
      description =
        "Returns a copy of <param list>, sorted by the value <param fn> returns for each element"
      fn =
        (function
        | state, vm, [], [| DList(vt, items); DApplicable app |] ->
          // Was two interpreted passes and a tuple per element around a native sort: one `map` to
          // build `(key, value)`, the sort, then a second `map` of `Tuple2.second`, which is why
          // `Tuple2.second` showed up in profiles of code that never mentions it. Only the key
          // function needs interpreting.
          //
          // The result keeps the source list's ValueType: sorting is a permutation, so the elements
          // are exactly the ones already merged into it. `listSort` does the same.
          //
          // The keys are spread as `listMap` is, a chunk being `listMap` over its elements; the
          // sort itself runs here, once every key is in, as it always did.
          Spread.walk
            { new OverList<List<struct (Dval * Dval)>>(state,
                                                       vm,
                                                       app,
                                                       vt,
                                                       "listMap",
                                                       Spread.minPerElement) with
                member this.Settle key = this.Value key
                member _.Step(keyed, elem, key) =
                  Spread.Next(struct (key, elem) :: keyed)
                member _.Absorb(keyed, spread, outcome) =
                  let elems = DvalList.take outcome.leftoverIndex spread
                  List.fold2
                    (fun acc k e -> struct (k, e) :: acc)
                    keyed
                    (DvalList.toList outcome.results)
                    (DvalList.toList elems)
                member _.Finish keyed = sortedByKeyLanding vm vt (List.rev keyed) }
            []
            items
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listFilterMap" 0
      typeParams = []
      parameters =
        [ Param.moved "list" (TList varA) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.singleton varA, TypeReference.option varB))
            ""
            [ "elem" ] ]
      returnType = TList varB
      description =
        "Calls <param fn> on every value in <param list>, keeping the values it returns "
        + "{{Some}} for and dropping the rest"
      fn =
        (function
        | state, vm, [], [| DList(vt, items); DApplicable app |] ->
          // The Dark version recursed a package call, an Option match and a `prepend` per element on
          // top of the lambda application, and it is used widely enough for that to show up in a
          // profile of anything.
          //
          // Built back to front and reversed once, and spread as `listMap` is.
          Spread.walk
            { new OverList<List<Dval>>(state, vm, app, vt, "listFilterMap", 0L) with
                // Only the case is looked at, so an answer built around a read (`Some (get x)`)
                // is matched through its shell and the read inside stays in flight. Anything else
                // is waited for: it is either an Option once it lands or an error that shows it.
                member this.Settle answer =
                  match Pending.peek answer with
                  | DEnum(_, _, _, ("Some" | "None"), _) as shell ->
                    this.Landed shell
                  | _ -> this.Value answer
                member _.Step(acc, _, answer) =
                  match answer with
                  | DEnum(_, _, _, "Some", [ v ]) -> Spread.Next(v :: acc)
                  | DEnum(_, _, _, "None", []) -> Spread.Next acc
                  | other -> raiseRTE vm.threadID (notAnOption other)
                member _.Absorb(acc, _, spread) = resultsOnto acc spread
                member _.Finish acc = Ply(mappedList vm (DvalList.ofRevList acc)) }
            []
            items
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listZipShortest" 0
      typeParams = []
      parameters =
        [ Param.moved "as" (TList varA) ""; Param.moved "bs" (TList varB) "" ]
      returnType = TList(TTuple(varA, varB, []))
      description =
        "Returns a list of parallel pairs from <param as> and <param bs>, stopping when either "
        + "runs out"
      fn =
        (function
        | _, vm, [], [| DList(_, listA); DList(_, listB) |] ->
          let count = min (DvalList.length listA) (DvalList.length listB)
          let pairs =
            Array.map2
              (fun a b -> DTuple(a, b, []))
              (DvalList.toArray (DvalList.take count listA))
              (DvalList.toArray (DvalList.take count listB))
          // `mappedList` merges the tuples' own ValueTypes, so the element type comes out
          // `KTTuple(a, b)` without naming it here.
          Ply(mappedList vm (DvalList.ofArray pairs))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listMap2shortest" 0
      typeParams = []
      parameters =
        [ Param.moved "as" (TList varA) ""
          Param.moved "bs" (TList varB) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.doubleton varA varB, varC))
            ""
            [ "a"; "b" ] ]
      returnType = TList varC
      description =
        "Maps <param fn> over <param as> and <param bs> in parallel, stopping when either runs "
        + "out"
      fn =
        (function
        | state, vm, [], [| DList(vtA, listA); DList(vtB, listB); DApplicable app |] ->
          // The Dark version recursed with `append`, which copies the accumulator every element,
          // so it was quadratic on top of the package call and the two-argument lambda application.
          // Built back to front and reversed once, as `listMap` and `listIndexedMap` do.
          //
          // Spread as `listMap` is, a chunk being this same call over its stretch of both lists.
          match DvalList.uncons listA, DvalList.uncons listB with
          | ValueSome(struct (a, tailA)), ValueSome(struct (b, tailB)) ->
            let mutable acc = []
            let mutable restA = tailA
            let mutable restB = tailB
            let probe = Spread.probeCostly state vm app
            let rec next (mapped : Dval) : Ply<Dval> =
              acc <- mapped :: acc
              match DvalList.uncons restA, DvalList.uncons restB with
              | ValueSome(struct (a, tA)), ValueSome(struct (b, tB)) ->
                // `Spread.run` keeps a list holding reads in flight here; this is the other list.
                if
                  not (isNull probe)
                  && probe.Ask(DvalList.length restA)
                  && not (DvalList.exists Pending.isPromise restB)
                then
                  probe.Stop()
                  // Only as far as both go. The other list's stretch for a chunk is found by
                  // where the chunk begins.
                  let count = min (DvalList.length restA) (DvalList.length restB)
                  let firstA = DvalList.take count restA
                  let bs = DvalList.take count restB
                  uply {
                    let! spread =
                      Spread.run
                        state
                        vm
                        "listMap2shortest"
                        (fun offset chunk ->
                          let chunkB =
                            DvalList.take
                              (DvalList.length chunk)
                              (DvalList.drop offset bs)
                          [ DList(vtA, chunk); DList(vtB, chunkB) ])
                        app
                        probe.PerElement
                        firstA
                    acc <- resultsOnto acc spread
                    restA <- DvalList.drop spread.leftoverIndex restA
                    restB <- DvalList.drop spread.leftoverIndex restB
                    match DvalList.uncons restA, DvalList.uncons restB with
                    | ValueSome(struct (a, tA)), ValueSome(struct (b, tB)) ->
                      restA <- tA
                      restB <- tB
                      return! Interpreter.requestApply vm app a [ b ] next
                    | _ -> return mappedList vm (DvalList.ofRevList acc)
                  }
                else
                  restA <- tA
                  restB <- tB
                  Interpreter.requestApply vm app a [ b ] next
              | _ -> Ply(mappedList vm (DvalList.ofRevList acc))
            Interpreter.requestApply vm app a [ b ] next
          | _ -> Ply(mappedList vm DvalList.empty)
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listMap" 0
      typeParams = []
      parameters =
        [ Param.moved "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn(NEList.singleton varA, varB)) "" [ "elem" ] ]
      returnType = TList varB
      description =
        "Calls <param fn> on every value in <param list>, returning a list of the results"
      fn =
        (function
        | state, vm, [], [| DList(vt, items); DApplicable app |] ->
          // Built back to front and reversed once at the end. A read a lambda hands back stays
          // in flight in the list, which the interpreter holds as a promise.
          //
          // Once the rest looks worth it, it is spread across cores (`LibExecution.Spread`), and
          // whatever a spread could not finish carries on here exactly as before.
          Spread.walk
            { new OverList<List<Dval>>(state, vm, app, vt, "listMap", 0L) with
                member _.Step(acc, _, mapped) = Spread.Next(mapped :: acc)
                member _.Absorb(acc, _, spread) = resultsOnto acc spread
                member _.Finish acc = Ply(mappedList vm (DvalList.ofRevList acc)) }
            []
            items
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listFilter" 0
      typeParams = []
      parameters =
        [ Param.moved "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn(NEList.singleton varA, TBool)) "" [ "elem" ] ]
      returnType = TList varA
      description =
        "Calls <param fn> on every value in <param list>, returning a list of the values for which "
        + "it returned true"
      fn =
        (function
        | state, vm, [], [| DList(vt, items); DApplicable app |] ->
          // The result holds a subset of the values that came in, so it keeps their ValueType
          // exactly. Nothing to merge, and nothing that can fail to. Spread as `listMap` is.
          Spread.walk
            { new OverList<List<Dval>>(state, vm, app, vt, "listFilter", 0L) with
                member this.Settle answer = this.Value answer
                member _.Step(acc, elem, answer) =
                  match answer with
                  | DBool keep -> Spread.Next(if keep then elem :: acc else acc)
                  | other -> raiseRTE vm.threadID (predicateNotBool other)
                member _.Absorb(acc, _, spread) = resultsOnto acc spread
                member _.Finish acc = Ply(DList(vt, DvalList.ofRevList acc)) }
            []
            items
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listRange" 0
      typeParams = []
      parameters = [ Param.make "lowest" TInt64 ""; Param.make "highest" TInt64 "" ]
      returnType = TList TInt64
      description =
        "Returns a list of the Int64s from <param lowest> to <param highest>, inclusive of both"
      fn =
        (function
        | _, _, [], [| DInt64 lowest; DInt64 highest |] ->
          // An empty range keeps an unknown element type, which is what the Dark version did: it
          // returned the literal `[]`, and only `prepend` ever made it a `List<Int64>`.
          if highest < lowest then
            Ply(DList(VT.unknown, DvalList.empty))
          else
            let span = uint64 (highest - lowest)
            // Past what an array holds, a range ran out of memory as a cons list too.
            if span >= uint64 System.Array.MaxLength then
              raise (System.OutOfMemoryException())
            Ply(
              DList(
                VT.int64,
                DvalList.init (int span + 1) (fun i ->
                  Dval.dint64 (lowest + int64 i))
              )
            )
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listAny" 0
      typeParams = []
      parameters =
        [ Param.moved "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn(NEList.singleton varA, TBool)) "" [ "elem" ] ]
      returnType = TBool
      description =
        "Returns true if <param fn> returns true for any value in <param list>, stopping at the "
        + "first one that does"
      fn =
        (function
        | state, vm, [], [| DList(vt, items); DApplicable app |] ->
          // Spread as `listMap` is, as a search (`Spread.search`): a chunk is this same call over
          // its stretch, and the first chunk in input order to answer true is the answer. A search
          // keeps nothing as it goes, so what it has so far is `DUnit`.
          Spread.walk
            { new OverList<Dval>(state, vm, app, vt, "listAny", Spread.minPerElement) with
                member this.Settle answer = this.Value answer
                member _.Step(_, _, answer) =
                  match answer with
                  | DBool true -> Spread.Done(DBool true)
                  | DBool false -> Spread.Next DUnit
                  | other -> raiseRTE vm.threadID (predicateNotBool other)
                member _.AnswerOf answer =
                  match answer with
                  | DBool true -> Spread.Found answer
                  | DBool false -> Spread.Finished DvalList.empty
                  | _ -> Spread.NotAnAnswer
                member _.Absorb(nothing, _, _) = nothing
                member _.Found _ = DBool true
                member _.Finish _ = Ply(DBool false) }
            DUnit
            items
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listContains" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) ""; Param.make "value" varA "" ]
      returnType = TBool
      description = "Returns true if <param value> is in <param list>"
      fn =
        (function
        | _, vm, [], [| DList(_, items); value |] ->
          // The Dark version of this was `findFirst list (fun e -> e == value)`, which pays a
          // lambda application per element to run what is only an equality test.
          //
          // `==` type-checks before comparing, and raises rather than answering false on
          // incompatible types; so does this, so a heterogeneous list still reports the same
          // error it always did: at the first element whose type does not merge.
          match Dval.listContains items value with
          | ValueSome found -> Ply(DBool found)
          | ValueNone ->
            let vtValue = Dval.toValueType value
            match
              items
              |> DvalList.toList
              |> List.map Dval.toValueType
              |> List.find (fun vt -> Result.isError (VT.merge vt vtValue))
            with
            | Some vtElem ->
              RTE.EqualityCheckOnIncompatibleTypes(vtElem, vtValue)
              |> raiseRTE vm.threadID
            | None ->
              Exception.raiseInternal
                "listContains declined a list it could answer"
                []
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listFindFirst" 0
      typeParams = []
      parameters =
        [ Param.moved "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn(NEList.singleton varA, TBool)) "" [ "elem" ] ]
      returnType = TypeReference.option varA
      description =
        "Returns the first value in <param list> for which <param fn> returns true, stopping there"
      fn =
        (function
        | state, vm, [], [| DList(vt, items); DApplicable app |] ->
          // Spread as `listAny` is. A chunk's `Some` is the element its own search stopped at.
          let option found = TypeChecker.DvalCreator.option vm.threadID vt found
          Spread.walk
            { new OverList<Dval>(state,
                                 vm,
                                 app,
                                 vt,
                                 "listFindFirst",
                                 Spread.minPerElement) with
                member this.Settle answer = this.Value answer
                member _.Step(_, elem, answer) =
                  match answer with
                  | DBool true -> Spread.Done(option (Some elem))
                  | DBool false -> Spread.Next DUnit
                  | other -> raiseRTE vm.threadID (predicateNotBool other)
                member _.AnswerOf answer =
                  match answer with
                  | DEnum(_, _, _, "Some", [ found ]) -> Spread.Found found
                  | DEnum(_, _, _, "None", []) -> Spread.Finished DvalList.empty
                  | _ -> Spread.NotAnAnswer
                member _.Absorb(nothing, _, _) = nothing
                member _.Found found = option (Some found)
                member _.Finish _ = Ply(option None) }
            DUnit
            items
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listIsEmpty" 0
      typeParams = []
      parameters = [ Param.moved "list" (TList varA) "" ]
      returnType = TBool
      description = "Returns true if <param list> has no values"
      fn =
        (function
        | _, _, _, [| DList(_, l) |] -> Ply(DBool(DvalList.isEmpty l))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listLength" 0
      typeParams = []
      parameters = [ Param.moved "list" (TList varA) "" ]
      returnType = TInt64
      description = "Returns the number of values in <param list>"
      fn =
        (function
        | _, _, _, [| DList(_, l) |] -> Ply(Dval.dint64 (int64 (DvalList.length l)))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listUnique" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TList varA
      description =
        "Returns the passed list, with only unique values. The first occurrence "
        + "of each value is kept, in the order it appeared."
      fn =
        (function
        | _, _, _, [| DList(vt, l) |] ->
          let unique =
            if DvalList.length l < 2 then
              l
            else
              let seen =
                System.Collections.Generic.HashSet<Dval>(Dval.equalityComparer)
              l |> DvalList.toArray |> Array.filter seen.Add |> DvalList.ofArray
          DList(vt, unique) |> Ply
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    // Consistent with `<`: a custom element type with its own `Compare` is sorted by that
    // implementation's `lessThan`, one interpreted call per comparison, a cost only such a type
    // pays. Every other list keeps the native structural sort, which builtin types' `Compare`
    // agrees with, and which is the only order a type without an implementation has.
    { name = fn "listSort" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TList varA
      description =
        "Returns a copy of <param list> with every value sorted in ascending "
        + "order.\n\nUse this if the values have types Dark knows how to "
        + "sort.\n\nConsider <fn List.sortBy> or <fn List.sortWith> if "
        + "you need more control over the sorting process."
      fn =
        (function
        | exeState, vm, _, [| DList(vt, Items(list)) |] ->
          let structural () =
            list
            |> DvalComparator.sortWith DvalComparator.compareDvalInt
            |> (fun l -> DList(vt, Items(l)))
            |> Ply
          match vt, list with
          | ValueType.Known(KTCustomType _ as self), _ :: _ :: _ ->
            uply {
              let compareTrait =
                Hash(LibExecution.PackageRefs.Trait.Stdlib.Traits.compare ())
              let! candidates =
                exeState.fns.implCandidates exeState.branchId compareTrait
              match LibExecution.Traits.select candidates self with
              | LibExecution.Traits.Selected c ->
                match Map.tryFind "lessThan" c.methods with
                | Some fnHash ->
                  let lessThan =
                    AppNamedFn
                      { name = FQFnName.Package fnHash
                        typeSymbolTable = TST.empty
                        typeArgs = []
                        access = None
                        argsSoFar = []
                        boundImpls = [] }
                  return! sortByLessThan vm lessThan vt list
                | None -> return! structural ()
              | LibExecution.Traits.NoImpl -> return! structural ()
              | LibExecution.Traits.Ambiguous cs ->
                return
                  RTE.Trait(
                    RTE.Traits.DispatchAmbiguous(
                      FQTraitName.Package compareTrait,
                      vt,
                      cs |> List.map (fun c -> c.source)
                    )
                  )
                  |> raiseRTE vm.threadID
            }
          | _ -> structural ()
        | _ -> incorrectArgs ())
      // Impure, like `sortBy`: it can call a person's `Compare` implementation.
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listConcat" 0
      typeParams = []
      parameters =
        [ Param.moved "as" (TList varA) ""; Param.moved "bs" (TList varA) "" ]
      returnType = TList varA
      description =
        "Returns a new list with all values in <param as> followed by all "
        + "values in <param bs>, preserving the order."
      fn =
        (function
        | _, vm, _, [| DList(vt1, l1); DList(vt2, l2) |] ->
          // When the types conflict, `DvalCreator.list` re-checks the elements to name the one at
          // fault.
          match TypeChecker.DvalCreator.tryConcat vt1 l1 vt2 l2 with
          | ValueSome concatenated -> Ply concatenated
          | ValueNone ->
            Ply(
              TypeChecker.DvalCreator.list
                vm.threadID
                vt1
                (DvalList.toList (DvalList.append l1 l2))
            )
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listPrepend" 0
      typeParams = []
      parameters =
        [ Param.moved "list" (TList varA) ""; Param.moved "value" varA "" ]
      returnType = TList varA
      description = "Adds <param value> to the front of <param list>."
      fn =
        (function
        | _, vm, _, [| DList(vt, l); value |] ->
          // Native because every list built by a fold goes through here, once per element, and the
          // Dark version had to build a one-element list to append.
          match TypeChecker.DvalCreator.tryPrepend vt l value with
          | ValueSome prepended -> Ply prepended
          | ValueNone ->
            Ply(
              TypeChecker.DvalCreator.list
                vm.threadID
                vt
                (DvalList.toList (DvalList.cons value l))
            )
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listAppend" 0
      typeParams = []
      parameters =
        [ Param.moved "list" (TList varA) ""; Param.moved "value" varA "" ]
      returnType = TList varA
      description = "Adds <param value> to the back of <param list>."
      fn =
        (function
        | _, vm, _, [| DList(vt, l); value |] ->
          // Native for the same reason as `prepend`.
          match VT.mergeDval vt value with
          | Ok merged -> Ply(DList(merged, DvalList.snoc l value))
          | Error() ->
            Ply(
              TypeChecker.DvalCreator.list
                vm.threadID
                vt
                (DvalList.toList (DvalList.snoc l value))
            )
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listFlatten" 0
      typeParams = []
      parameters = [ Param.make "list" (TList(TList varA)) "" ]
      returnType = TList varA
      description =
        "Returns a single list containing the values of every list directly in "
        + "<param list> (does not recursively flatten nested lists)."
      fn =
        (function
        | _, vm, _, [| DList(_, sublists) |] ->
          let inner =
            sublists
            |> DvalList.fold
              (fun acc sub ->
                match acc, sub with
                | Ok accVt, DList(vt, _) ->
                  match VT.merge accVt vt with
                  | Ok merged -> Ok merged
                  | Error() -> Error()
                | _ -> Error())
              (Ok VT.unknown)
          let items =
            sublists
            |> DvalList.fold
              (fun acc sub ->
                match sub with
                | DList(_, items) -> DvalList.append acc items
                | other -> DvalList.snoc acc other)
              DvalList.empty
          match inner with
          | Ok vt -> Ply(DList(vt, items))
          | Error() ->
            Ply(
              TypeChecker.DvalCreator.list
                vm.threadID
                VT.unknown
                (DvalList.toList items)
            )
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listGetAt" 0
      typeParams = []
      parameters =
        [ Param.moved "list" (TList varA) ""; Param.make "index" TInt64 "" ]
      returnType = TypeReference.option varA
      description =
        "Returns {{Some value}} at <param index> in <param list>, or {{None}} if out of bounds."
      fn =
        (function
        | _, vm, _, [| DList(vt, l); DInt64 index |] ->
          Ply(
            TypeChecker.DvalCreator.option vm.threadID vt (DvalList.tryItem index l)
          )
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listReverse" 0
      typeParams = []
      parameters = [ Param.moved "list" (TList varA) "" ]
      returnType = TList varA
      description = "Returns a reversed copy of <param list>."
      fn =
        (function
        | _, _, _, [| DList(vt, l) |] ->
          // Reordering can't change the element type and every element was checked on the way in, so no
          // type check is needed. Native because `map` and `filter` both end in a reverse.
          Ply(DList(vt, DvalList.rev l))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    ]


let builtins () = LibExecution.Builtin.make [] (fns ())
