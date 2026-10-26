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

  /// For `List.unique`, which sorts only to group equal values, so Bytes are
  /// allowed here and take the dict-key order.
  let compareDvalIntForDedupe (v1 : Dval) (v2 : Dval) : int =
    guarded DvalOrdering.compareForDedupe v1 v2

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
  |> fun l -> DList(vt, l)


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
let private mappedList (vm : VMState) (items : List<Dval>) : Dval =
  let merged =
    items
    |> List.fold
      (fun acc item ->
        match acc with
        | Ok accVt ->
          match VT.merge accVt (Dval.toValueType item) with
          | Ok m -> Ok m
          | Error() -> Error()
        | Error() -> Error())
      (Ok VT.unknown)

  match merged with
  | Ok vt -> DList(vt, items)
  | Error() -> TypeChecker.DvalCreator.list vm.threadID VT.unknown items


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
  sortK items (fun sorted -> Ply(DList(vt, sorted)))


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
          match items with
          | [] -> Ply init
          | first :: tail ->
            let mutable rest = tail
            let rec next (stepped : Dval) : Ply<Dval> =
              match rest with
              | [] -> Ply stepped
              | elem :: elemTail ->
                rest <- elemTail
                Interpreter.requestApply vm app stepped [ elem ] next
            Interpreter.requestApply vm app init [ first ] next
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listTake" 0
      typeParams = []
      parameters = [ Param.moved "list" (TList varA) ""; Param.make "count" TInt "" ]
      returnType = TList varA
      description =
        "Returns the first <param count> values of <param list>, or all of them if there are fewer"
      fn =
        (function
        | _, vm, [], [| DList(_, items); DInt count |] ->
          // The Dark version recursed one package call, one match and one `prepend` per element. There
          // is no lambda here at all -- it is pure structure -- so nothing about it needed to be
          // interpreted.
          let n =
            match count with
            | DarkInt.Finite i -> i
            // Past either end of any list. `intToInt32` would raise instead, and `take` with an
            // absurd count is defined to return the whole list rather than fail.
            | DarkInt.Infinite b -> if b.Sign > 0 then System.Int64.MaxValue else 0L

          if n <= 0L then
            Ply(DList(VT.unknown, []))
          else
            let mutable acc = []
            let mutable rest = items
            let mutable taken = 0L

            while taken < n && not (List.isEmpty rest) do
              match rest with
              | elem :: tail ->
                acc <- elem :: acc
                rest <- tail
                taken <- taken + 1L
              | [] -> ()

            // The element ValueType is merged from what was actually taken, not inherited from the
            // source list. That is what the Dark version did -- it rebuilt the prefix with `prepend` --
            // and a prefix can be narrower than the whole.
            Ply(mappedList vm (List.rev acc))
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
            (TFn(NEList.doubleton TInt varA, varB))
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
          match items with
          | [] -> Ply(mappedList vm [])
          | first :: tail ->
            let mutable acc = []
            let mutable rest = tail
            let mutable i = 1L
            let probe = Spread.probe state vm app
            let rec next (mapped : Dval) : Ply<Dval> =
              acc <- mapped :: acc
              match rest with
              | [] -> Ply(mappedList vm (List.rev acc))
              | elem :: elemTail ->
                if not (isNull probe) && probe.Ask rest then
                  probe.Stop()
                  let start = i
                  uply {
                    let! spread =
                      Spread.run
                        state
                        vm
                        "listMap2shortest"
                        (fun offset chunk ->
                          let first = start + int64 offset
                          let indices =
                            List.init (List.length chunk) (fun k ->
                              Dval.int (bigint (first + int64 k)))
                          [ DList(VT.int, indices); DList(vt, chunk) ])
                        app
                        probe.PerElement
                        rest
                    acc <- List.rev spread.results @ acc
                    i <- start + int64 spread.leftoverIndex
                    match spread.leftover with
                    | [] -> return mappedList vm (List.rev acc)
                    | elem :: elemTail ->
                      rest <- elemTail
                      let index = Dval.int (bigint i)
                      i <- i + 1L
                      return! Interpreter.requestApply vm app index [ elem ] next
                  }
                else
                  rest <- elemTail
                  let index = Dval.int (bigint i)
                  i <- i + 1L
                  Interpreter.requestApply vm app index [ elem ] next
            Interpreter.requestApply vm app (Dval.int (bigint 0)) [ first ] next
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
        | _, vm, [], [| DList(vt, items); DApplicable app |] ->
          // Was two interpreted passes and a tuple per element around a native sort: one `map` to
          // build `(key, value)`, the sort, then a second `map` of `Tuple2.second`, which is why
          // `Tuple2.second` showed up in profiles of code that never mentions it. Only the key
          // function needs interpreting.
          //
          // The result keeps the source list's ValueType: sorting is a permutation, so the elements
          // are exactly the ones already merged into it. `listSort` does the same.
          match items with
          | [] -> Ply(sortedByKey vt [])
          | first :: tail ->
            let mutable keyed = []
            let mutable current = first
            let mutable rest = tail
            let rec next (key : Dval) : Ply<Dval> =
              Interpreter.withValue vm key (fun key ->
                keyed <- struct (key, current) :: keyed
                match rest with
                | [] -> sortedByKeyLanding vm vt (List.rev keyed)
                | elem :: elemTail ->
                  current <- elem
                  rest <- elemTail
                  Interpreter.requestApply vm app elem [] next)
            Interpreter.requestApply vm app first [] next
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
          match items with
          | [] -> Ply(mappedList vm [])
          | first :: tail ->
            let mutable acc = []
            let mutable rest = tail
            let probe = Spread.probe state vm app
            // Only the case is looked at, so an answer built around a read (`Some (get x)`) is
            // matched through its shell and the read inside stays in flight. Anything else is
            // waited for: it is either an Option once it lands or an error that shows it.
            let rec next (answer : Dval) : Ply<Dval> =
              match Pending.peek answer with
              | DEnum(_, _, _, ("Some" | "None"), _) as shell -> step shell
              | _ -> Interpreter.withValue vm answer step
            and step (answer : Dval) : Ply<Dval> =
              match answer with
              | DEnum(_, _, _, "Some", [ v ]) -> acc <- v :: acc
              | DEnum(_, _, _, "None", []) -> ()
              | other -> raiseRTE vm.threadID (notAnOption other)
              match rest with
              | [] -> Ply(mappedList vm (List.rev acc))
              | elem :: elemTail ->
                if not (isNull probe) && probe.Ask rest then
                  probe.Stop()
                  uply {
                    let! spread =
                      Spread.run
                        state
                        vm
                        "listFilterMap"
                        (fun _ chunk -> [ DList(vt, chunk) ])
                        app
                        probe.PerElement
                        rest
                    acc <- List.rev spread.results @ acc
                    match spread.leftover with
                    | [] -> return mappedList vm (List.rev acc)
                    | elem :: elemTail ->
                      rest <- elemTail
                      return! Interpreter.requestApply vm app elem [] next
                  }
                else
                  rest <- elemTail
                  Interpreter.requestApply vm app elem [] next
            Interpreter.requestApply vm app first [] next
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
          // The Dark version recursed a package call, two matches and a `prepend` per element. There is
          // no lambda here at all -- pairing is pure structure -- so unlike `map2shortest` this
          // needs no application per element either.
          let mutable acc = []
          let mutable restA = listA
          let mutable restB = listB

          while not (List.isEmpty restA) && not (List.isEmpty restB) do
            match restA, restB with
            | a :: tailA, b :: tailB ->
              acc <- DTuple(a, b, []) :: acc
              restA <- tailA
              restB <- tailB
            | _ -> ()

          // `mappedList` merges the tuples' own ValueTypes, so the element type comes out
          // `KTTuple(a, b)` without naming it here.
          Ply(mappedList vm (List.rev acc))
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
        | _, vm, [], [| DList(_, listA); DList(_, listB); DApplicable app |] ->
          // The Dark version recursed with `append`, which copies the accumulator every element,
          // so it was quadratic on top of the package call and the two-argument lambda application.
          // Built back to front and reversed once, as `listMap` and `listIndexedMap` do.
          match listA, listB with
          | a :: tailA, b :: tailB ->
            let mutable acc = []
            let mutable restA = tailA
            let mutable restB = tailB
            let rec next (mapped : Dval) : Ply<Dval> =
              acc <- mapped :: acc
              match restA, restB with
              | a :: tA, b :: tB ->
                restA <- tA
                restB <- tB
                Interpreter.requestApply vm app a [ b ] next
              | _ -> Ply(mappedList vm (List.rev acc))
            Interpreter.requestApply vm app a [ b ] next
          | _ -> Ply(mappedList vm [])
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
          // in flight in the list, which the interpreter holds as a promise. One continuation
          // for the whole list, over two
          // mutable cells, rather than a closure per element.
          //
          // Once the rest looks worth it, it is spread across cores (`LibExecution.Spread`), and
          // whatever a spread could not finish carries on here exactly as before.
          match items with
          | [] -> Ply(mappedList vm [])
          | first :: tail ->
            let mutable acc = []
            let mutable rest = tail
            let probe = Spread.probe state vm app
            let rec next (mapped : Dval) : Ply<Dval> =
              acc <- mapped :: acc
              match rest with
              | [] -> Ply(mappedList vm (List.rev acc))
              | elem :: elemTail ->
                if not (isNull probe) && probe.Ask rest then
                  probe.Stop()
                  uply {
                    let! spread =
                      Spread.run
                        state
                        vm
                        "listMap"
                        (fun _ chunk -> [ DList(vt, chunk) ])
                        app
                        probe.PerElement
                        rest
                    acc <- List.rev spread.results @ acc
                    match spread.leftover with
                    | [] -> return mappedList vm (List.rev acc)
                    | elem :: elemTail ->
                      rest <- elemTail
                      return! Interpreter.requestApply vm app elem [] next
                  }
                else
                  rest <- elemTail
                  Interpreter.requestApply vm app elem [] next
            Interpreter.requestApply vm app first [] next
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
          match items with
          | [] -> Ply(DList(vt, []))
          | first :: tail ->
            let mutable acc = []
            let mutable current = first
            let mutable rest = tail
            let probe = Spread.probe state vm app
            let rec next (answer : Dval) : Ply<Dval> =
              Interpreter.withValue vm answer (fun answer ->
                match answer with
                | DBool keep -> if keep then acc <- current :: acc
                | other -> raiseRTE vm.threadID (predicateNotBool other)
                match rest with
                | [] -> Ply(DList(vt, List.rev acc))
                | elem :: elemTail ->
                  if not (isNull probe) && probe.Ask rest then
                    probe.Stop()
                    uply {
                      let! spread =
                        Spread.run
                          state
                          vm
                          "listFilter"
                          (fun _ chunk -> [ DList(vt, chunk) ])
                          app
                          probe.PerElement
                          rest
                      acc <- List.rev spread.results @ acc
                      match spread.leftover with
                      | [] -> return DList(vt, List.rev acc)
                      | elem :: elemTail ->
                        current <- elem
                        rest <- elemTail
                        return! Interpreter.requestApply vm app elem [] next
                    }
                  else
                    current <- elem
                    rest <- elemTail
                    Interpreter.requestApply vm app elem [] next)
            Interpreter.requestApply vm app first [] next
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listRange" 0
      typeParams = []
      parameters = [ Param.make "lowest" TInt ""; Param.make "highest" TInt "" ]
      returnType = TList TInt
      description =
        "Returns a list of the Ints from <param lowest> to <param highest>, inclusive of both"
      fn =
        (function
        | _, _, [], [| DInt lowest; DInt highest |] ->
          // Counted down, so the list is built in order without a reverse.
          //
          // On `int64` where both ends are `Finite`, which is every range anyone writes. Doing it in
          // `bigint` throughout boxes a `BigInteger` per element, which dominated the allocation when
          // this was first written that way. A range that needs the wide path would not fit in memory
          // anyway, but it keeps its own arithmetic.
          let mutable items = []

          match lowest, highest with
          | DarkInt.Finite low, DarkInt.Finite high ->
            let mutable current = high
            // `current = low` ends it rather than `current - 1L >= low`, which underflows at
            // `Int64.MinValue`.
            let mutable more = high >= low
            while more do
              items <- DInt(DarkInt.Finite current) :: items
              if current = low then more <- false else current <- current - 1L
          | _ ->
            let low = DarkInt.toBigInt lowest
            let mutable current = DarkInt.toBigInt highest
            while current >= low do
              items <- Dval.int current :: items
              current <- current - bigint 1

          // An empty range keeps an unknown element type, which is what the Dark version did: it
          // returned the literal `[]`, and only `prepend` ever made it a `List<Int>`.
          if List.isEmpty items then
            Ply(DList(VT.unknown, []))
          else
            Ply(DList(VT.int, items))
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
        | _, vm, [], [| DList(_, items); DApplicable app |] ->
          match items with
          | [] -> Ply(DBool false)
          | first :: tail ->
            let mutable rest = tail
            let rec next (answer : Dval) : Ply<Dval> =
              Interpreter.withValue vm answer (fun answer ->
                match answer with
                | DBool true -> Ply(DBool true)
                | DBool false ->
                  match rest with
                  | [] -> Ply(DBool false)
                  | elem :: elemTail ->
                    rest <- elemTail
                    Interpreter.requestApply vm app elem [] next
                | other -> raiseRTE vm.threadID (predicateNotBool other))
            Interpreter.requestApply vm app first [] next
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
          // lambda application per element to run what is only an equality test. Here the
          // comparison is direct, and the value's type is read once rather than per element.
          //
          // `==` type-checks before comparing, and raises rather than answering false on
          // incompatible types; this keeps that, so a heterogeneous list still reports the
          // same error it always did.
          let vtValue = Dval.toValueType value

          let rec search (rest : List<Dval>) : bool =
            match rest with
            | [] -> false
            | elem :: tail ->
              let vtElem = Dval.toValueType elem
              match VT.merge vtElem vtValue with
              | Error _ ->
                RTE.EqualityCheckOnIncompatibleTypes(vtElem, vtValue)
                |> raiseRTE vm.threadID
              | Ok _ -> if Dval.equals elem value then true else search tail

          Ply(DBool(search items))
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
        | _, vm, [], [| DList(vt, items); DApplicable app |] ->
          match items with
          | [] -> Ply(TypeChecker.DvalCreator.option vm.threadID vt None)
          | first :: tail ->
            let mutable current = first
            let mutable rest = tail
            let rec next (answer : Dval) : Ply<Dval> =
              Interpreter.withValue vm answer (fun answer ->
                match answer with
                | DBool true ->
                  Ply(TypeChecker.DvalCreator.option vm.threadID vt (Some current))
                | DBool false ->
                  match rest with
                  | [] -> Ply(TypeChecker.DvalCreator.option vm.threadID vt None)
                  | elem :: elemTail ->
                    current <- elem
                    rest <- elemTail
                    Interpreter.requestApply vm app elem [] next
                | other -> raiseRTE vm.threadID (predicateNotBool other))
            Interpreter.requestApply vm app first [] next
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
        | _, _, _, [| DList(_, l) |] -> Ply(DBool(List.isEmpty l))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listLength" 0
      typeParams = []
      parameters = [ Param.moved "list" (TList varA) "" ]
      returnType = TInt
      description = "Returns the number of values in <param list>"
      fn =
        (function
        | _, _, _, [| DList(_, l) |] -> Ply(Dval.int (bigint l.Length))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listUnique" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TList varA
      description =
        "Returns the passed list, with only unique values. Only one of each "
        + "value will be returned, but the order will not be maintained."
      fn =
        (function
        | _, _, _, [| DList(vt, l) |] ->
          List.distinct l
          |> DvalComparator.sortWith DvalComparator.compareDvalIntForDedupe
          |> fun l -> DList(vt, l)
          |> Ply
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
        + "sort.\n\nConsider <fn List.sortBy> or <fn List.sortByComparator> if "
        + "you need more control over the sorting process."
      fn =
        (function
        | exeState, vm, _, [| DList(vt, list) |] ->
          let structural () =
            list
            |> DvalComparator.sortWith DvalComparator.compareDvalInt
            |> (fun l -> DList(vt, l))
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
          // Both lists are already valid, so the result is valid exactly when their element types
          // merge, which is O(1). Re-checking every element would cost O(n) per call and make a list
          // built by repeated concatenation O(n²). When the types conflict, `DvalCreator.list`
          // re-checks the elements to name the one at fault.
          match VT.merge vt1 vt2 with
          | Ok merged -> Ply(DList(merged, List.append l1 l2))
          | Error() ->
            Ply(TypeChecker.DvalCreator.list vm.threadID vt1 (List.append l1 l2))
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
          match VT.merge vt (Dval.toValueType value) with
          | Ok merged -> Ply(DList(merged, value :: l))
          | Error() -> Ply(TypeChecker.DvalCreator.list vm.threadID vt (value :: l))
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
          // Still O(n): a cons list has to be copied to append. Native for the same reason as `prepend`.
          match VT.merge vt (Dval.toValueType value) with
          | Ok merged -> Ply(DList(merged, l @ [ value ]))
          | Error() ->
            Ply(TypeChecker.DvalCreator.list vm.threadID vt (l @ [ value ]))
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
          // One concat, rather than an append per sublist, which re-copies the accumulator each step.
          let inner =
            sublists
            |> List.fold
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
            |> List.collect (fun sub ->
              match sub with
              | DList(_, items) -> items
              | other -> [ other ])
          match inner with
          | Ok vt -> Ply(DList(vt, items))
          | Error() -> Ply(TypeChecker.DvalCreator.list vm.threadID VT.unknown items)
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listGetAt" 0
      typeParams = []
      parameters = [ Param.moved "list" (TList varA) ""; Param.make "index" TInt "" ]
      returnType = TypeReference.option varA
      description =
        "Returns {{Some value}} at <param index> in <param list>, or {{None}} if out of bounds."
      fn =
        (function
        | _, vm, _, [| DList(vt, l); DInt index |] ->
          // Still a walk, since that's what a cons list costs, but one walk rather than an interpreted
          // call per position. Deliberately not `List.length` then `List.item`: that is O(n) even for
          // index 0, and callers ask for low indices of long lists.
          let rec walk (remaining : int64) (rest : List<Dval>) : Option<Dval> =
            match rest with
            | [] -> None
            | head :: tail ->
              if remaining = 0L then Some head else walk (remaining - 1L) tail
          let item =
            match DarkInt.toInt64 index with
            | Some i when i >= 0L -> walk i l
            | _ -> None
          Ply(TypeChecker.DvalCreator.option vm.threadID vt item)
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
          Ply(DList(vt, List.rev l))
        | _ -> incorrectArgs ())
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    ]


let builtins () = LibExecution.Builtin.make [] (fns ())
