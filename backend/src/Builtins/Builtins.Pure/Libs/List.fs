module Builtins.Pure.Libs.List

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Exe = LibExecution.Execution
module Interpreter = LibExecution.Interpreter
module TypeChecker = LibExecution.TypeChecker
module ValueType = LibExecution.ValueType
module RTE = RuntimeError


module DvalComparator =
  type Order =
    | Less
    | Equal
    | Greater

  let order v1 v2 =
    let result = compare v1 v2
    if result < 0 then Less
    elif result > 0 then Greater
    else Equal


  let compareDval (dv1 : Dval) (dv2 : Dval) : Order =
    try
      order (DvalOrdering.compareForSort dv1 dv2) 0
    with :? DvalComparisonException as e ->
      RuntimeError.Error.EqualityCheckOnIncompatibleTypes(
        Dval.toValueType e.Left,
        Dval.toValueType e.Right
      )
      |> raiseUntargetedRTE

  let compareDvalInt v1 v2 =
    match compareDval v1 v2 with
    | Greater -> 1
    | Less -> -1
    | Equal -> 0

// Based on https://github.com/dotnet/runtime/blob/57bfe474518ab5b7cfe6bf7424a79ce3af9d6657/src/coreclr/tools/Common/Sorting/MergeSortCore.cs#L55
module Sort =
  exception InvalidSortComparatorInt of int64

  type Comparer = Dval -> Dval -> Ply<int>

  type Array = array<Dval>

  let copy
    (source : Array)
    (sourceIndex : int)
    (target : Array)
    (destIndex : int)
    (length : int)
    : unit =
    System.Array.Copy(source, sourceIndex, target, destIndex, length)

  let merge
    (localCopyofHalfOfArray : Array)
    (arrayToSort : Array)
    (index : int)
    (halfLen : int)
    (length : int)
    (comparer : Comparer)
    : Ply<unit> =
    uply {
      let mutable leftHalfIndex = 0
      let mutable rightHalfIndex = index + halfLen
      let rightHalfEnd = index + length

      // this whole thing is just a hacky for-loop with breaks
      let mutable i' = 0
      let mutable cont = true

      while (cont && i' < length) do
        // Advance the array here to make sure we do it, but use `i` for the calculations
        let i = i'
        i' <- i' + 1

        if (leftHalfIndex = halfLen) then
          // All of the remaining elements must be from the right half, and thus must already be in position
          cont <- false // break
        elif rightHalfIndex = rightHalfEnd then
          // Copy remaining elements from the local copy
          copy
            localCopyofHalfOfArray
            leftHalfIndex
            arrayToSort
            (index + i)
            (length - i)

          cont <- false // break
        else
          let v0 = localCopyofHalfOfArray[leftHalfIndex]
          let v1 = arrayToSort[rightHalfIndex]
          let! comparisonResult = comparer v0 v1

          if comparisonResult <= 0 then
            arrayToSort[i + index] <- v0
            leftHalfIndex <- leftHalfIndex + 1
          else
            arrayToSort[i + index] <- v1
            rightHalfIndex <- rightHalfIndex + 1
    }

  let rec mergeSortHelper
    (arrayToSort : Array)
    (index : int)
    (length : int)
    (comparer : Comparer)
    (scratchSpace : Array)
    : Ply<unit> =
    uply {
      if length <= 1 then
        return ()
      elif length = 2 then
        let v0 = arrayToSort[index]
        let v1 = arrayToSort[index + 1]
        let! result = comparer v0 v1

        if result > 0 then
          arrayToSort[index] <- v1
          arrayToSort[index + 1] <- v0

      else
        let halfLen = length / 2
        do! mergeSortHelper arrayToSort index halfLen comparer scratchSpace

        let nextIndex = index + halfLen
        let nextLength = length - halfLen
        do! mergeSortHelper arrayToSort nextIndex nextLength comparer scratchSpace

        copy arrayToSort index scratchSpace 0 halfLen
        return! merge scratchSpace arrayToSort index halfLen length comparer
    }

  let sequentialSort
    (arrayToSort : Array)
    (index : int)
    (length : int)
    (comparer : Comparer)
    : Ply<unit> =
    let scratchSpace =
      System.Array.CreateInstance(typeof<Dval>, arrayToSort.Length / 2) :?> Array

    mergeSortHelper arrayToSort index length comparer scratchSpace

  let sort (comparer : Comparer) (arrayToSort : Array) : Ply<unit> =
    sequentialSort arrayToSort 0 arrayToSort.Length comparer

let varA = TVariable "a"
let varB = TVariable "b"
let varC = TVariable "c"


/// The result of a `map`: its element type comes from the values the lambda returned.
///
/// Same shape as `listFlatten`. Merge the element ValueTypes where they agree and build the list
/// directly; where they do not, hand it to `DvalCreator.list`, which is the general path and reports
/// the mismatch properly. Accumulating with `push`, which is what the Dark version did, merged the
/// same types one element at a time.
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
  |> List.sortWith (fun (struct (k1, v1)) (struct (k2, v2)) ->
    let c = DvalComparator.compareDvalInt k1 k2
    if c <> 0 then c else DvalComparator.compareDvalInt v1 v2)
  |> List.map (fun (struct (_, v)) -> v)
  |> fun l -> DList(vt, l)


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


/// A `filter` predicate returned something other than a bool.
///
/// The same error the Dark version raised, since its body was `if f elem then ... else ...` and this
/// is what the interpreter says about an `if` on a non-bool.
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


let private predicateNotBool (actual : Dval) =
  RuntimeError.Bool(
    RuntimeError.Bools.ConditionRequiresBool(Dval.toValueType actual, actual)
  )


let fns () : List<BuiltInFn> =
  [ { name = fn "listFold" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.make "init" varB ""
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
        | state, vm, [], [| DList(_, items); init; DApplicable app |] ->
          // Walked without a computation expression while the lambda answers synchronously, which is
          // nearly always: an arithmetic body, a comparison, a push. A `uply` around the whole fold
          // costs a state machine per *call*, and awaiting inside it costs one per *element*.
          let mutable acc = init
          let mutable rest = items
          let mutable pending = ValueNone

          while ValueOption.isNone pending && not (List.isEmpty rest) do
            match rest with
            | elem :: tail ->
              let call = Exe.executeApplicable2 state app acc elem
              match Ply.trySync call with
              | ValueSome(Ok next) ->
                acc <- next
                rest <- tail
              | ValueSome(Error(rte, cs)) -> Exe.raiseFromApplied vm rte cs
              // Hand the unfinished call and what is left of the list to the awaiting path.
              | ValueNone -> pending <- ValueSome(struct (call, tail))
            | [] -> ()

          match pending with
          | ValueNone -> Ply acc
          | ValueSome(struct (call, tail)) ->
            uply {
              let! first = call
              match first with
              | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
              | Ok next ->
                let mutable acc = next
                let mutable rest = tail
                while not (List.isEmpty rest) do
                  match rest with
                  | elem :: elemTail ->
                    match! Exe.executeApplicable2 state app acc elem with
                    | Ok stepped ->
                      acc <- stepped
                      rest <- elemTail
                    | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
                  | [] -> ()
                return acc
            }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listTake" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) ""; Param.make "count" TInt "" ]
      returnType = TList varA
      description =
        "Returns the first <param count> values of <param list>, or all of them if there are fewer"
      fn =
        (function
        | _, vm, [], [| DList(_, items); DInt count |] ->
          // The Dark version recursed one package call, one match and one `push` per element. There
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
            // source list. That is what the Dark version did -- it rebuilt the prefix with `push` --
            // and a prefix can be narrower than the whole.
            Ply(mappedList vm (List.rev acc))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listIndexedMap" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
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
        | state, vm, [], [| DList(_, items); DApplicable app |] ->
          // The Dark version folded with `pushBack`, which copies the accumulator per element, so it
          // was quadratic on top of the two lambda applications a fold costs. Built back to front
          // and reversed once, as `listMap` does.
          let mutable acc = []
          let mutable rest = items
          let mutable i = 0L
          let mutable pending = ValueNone

          while ValueOption.isNone pending && not (List.isEmpty rest) do
            match rest with
            | elem :: tail ->
              let call = Exe.executeApplicable2 state app (Dval.int (bigint i)) elem
              match Ply.trySync call with
              | ValueSome(Ok mapped) ->
                acc <- mapped :: acc
                rest <- tail
                i <- i + 1L
              | ValueSome(Error(rte, cs)) -> Exe.raiseFromApplied vm rte cs
              | ValueNone -> pending <- ValueSome(struct (call, tail, i + 1L))
            | [] -> ()

          match pending with
          | ValueNone -> Ply(mappedList vm (List.rev acc))
          | ValueSome(struct (call, tail, nextI)) ->
            uply {
              let! first = call
              match first with
              | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
              | Ok mapped ->
                let mutable acc = mapped :: acc
                let mutable rest = tail
                let mutable i = nextI
                while not (List.isEmpty rest) do
                  match rest with
                  | elem :: elemTail ->
                    match!
                      Exe.executeApplicable2 state app (Dval.int (bigint i)) elem
                    with
                    | Ok stepped ->
                      acc <- stepped :: acc
                      rest <- elemTail
                      i <- i + 1L
                    | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
                  | [] -> ()
                return mappedList vm (List.rev acc)
            }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listSortBy" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
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
          let mutable keyed = []
          let mutable rest = items
          let mutable pending = ValueNone

          while ValueOption.isNone pending && not (List.isEmpty rest) do
            match rest with
            | elem :: tail ->
              let call = Exe.executeApplicable1 state app elem
              match Ply.trySync call with
              | ValueSome(Ok key) ->
                keyed <- struct (key, elem) :: keyed
                rest <- tail
              | ValueSome(Error(rte, cs)) -> Exe.raiseFromApplied vm rte cs
              | ValueNone -> pending <- ValueSome(struct (call, elem, tail))
            | [] -> ()

          match pending with
          | ValueNone -> Ply(sortedByKey vt (List.rev keyed))
          | ValueSome(struct (call, elem, tail)) ->
            uply {
              let! first = call
              match first with
              | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
              | Ok key ->
                let mutable keyed = struct (key, elem) :: keyed
                let mutable rest = tail
                while not (List.isEmpty rest) do
                  match rest with
                  | e :: elemTail ->
                    match! Exe.executeApplicable1 state app e with
                    | Ok k ->
                      keyed <- struct (k, e) :: keyed
                      rest <- elemTail
                    | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
                  | [] -> ()
                return sortedByKey vt (List.rev keyed)
            }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listFilterMap" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
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
        | state, vm, [], [| DList(_, items); DApplicable app |] ->
          // The Dark version recursed a package call, an Option match and a `push` per element on
          // top of the lambda application, and it is used widely enough for that to show up in a
          // profile of anything.
          //
          // Built back to front and reversed once, as `listMap` does.
          let mutable acc = []
          let mutable rest = items
          let mutable pending = ValueNone

          while ValueOption.isNone pending && not (List.isEmpty rest) do
            match rest with
            | elem :: tail ->
              let call = Exe.executeApplicable1 state app elem
              match Ply.trySync call with
              | ValueSome(Ok(DEnum(_, _, _, "Some", [ v ]))) ->
                acc <- v :: acc
                rest <- tail
              | ValueSome(Ok(DEnum(_, _, _, "None", []))) -> rest <- tail
              | ValueSome(Ok other) -> raiseRTE vm.threadID (notAnOption other)
              | ValueSome(Error(rte, cs)) -> Exe.raiseFromApplied vm rte cs
              | ValueNone -> pending <- ValueSome(struct (call, tail))
            | [] -> ()

          match pending with
          | ValueNone -> Ply(mappedList vm (List.rev acc))
          | ValueSome(struct (call, tail)) ->
            uply {
              let mutable acc = acc
              let mutable rest = tail
              let mutable first = ValueSome call
              let mutable go = true

              while go do
                let! stepped =
                  match first with
                  | ValueSome c ->
                    first <- ValueNone
                    c
                  | ValueNone ->
                    match rest with
                    | elem :: tl ->
                      rest <- tl
                      Exe.executeApplicable1 state app elem
                    | [] -> Ply(Ok DUnit)

                match stepped with
                | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
                | Ok(DEnum(_, _, _, "Some", [ v ])) -> acc <- v :: acc
                | Ok(DEnum(_, _, _, "None", [])) -> ()
                | Ok DUnit -> go <- false
                | Ok other -> return raiseRTE vm.threadID (notAnOption other)

              return mappedList vm (List.rev acc)
            }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listZipShortest" 0
      typeParams = []
      parameters =
        [ Param.make "as" (TList varA) ""; Param.make "bs" (TList varB) "" ]
      returnType = TList(TTuple(varA, varB, []))
      description =
        "Returns a list of parallel pairs from <param as> and <param bs>, stopping when either "
        + "runs out"
      fn =
        (function
        | _, vm, [], [| DList(_, listA); DList(_, listB) |] ->
          // The Dark version recursed a package call, two matches and a `push` per element. There is
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
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listMap2shortest" 0
      typeParams = []
      parameters =
        [ Param.make "as" (TList varA) ""
          Param.make "bs" (TList varB) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.doubleton varA varB, TVariable "c"))
            ""
            [ "a"; "b" ] ]
      returnType = TList(TVariable "c")
      description =
        "Maps <param fn> over <param as> and <param bs> in parallel, stopping when either runs "
        + "out"
      fn =
        (function
        | state, vm, [], [| DList(_, listA); DList(_, listB); DApplicable app |] ->
          // The Dark version recursed with `pushBack`, which copies the accumulator every element,
          // so it was quadratic on top of the package call and the two-argument lambda application.
          // Built back to front and reversed once, as `listMap` and `listIndexedMap` do.
          let mutable acc = []
          let mutable restA = listA
          let mutable restB = listB
          let mutable pending = ValueNone

          while ValueOption.isNone pending
                && not (List.isEmpty restA)
                && not (List.isEmpty restB) do
            match restA, restB with
            | a :: tailA, b :: tailB ->
              let call = Exe.executeApplicable2 state app a b
              match Ply.trySync call with
              | ValueSome(Ok mapped) ->
                acc <- mapped :: acc
                restA <- tailA
                restB <- tailB
              | ValueSome(Error(rte, cs)) -> Exe.raiseFromApplied vm rte cs
              | ValueNone -> pending <- ValueSome(struct (call, tailA, tailB))
            | _ -> ()

          match pending with
          | ValueNone -> Ply(mappedList vm (List.rev acc))
          | ValueSome(struct (call, tailA, tailB)) ->
            uply {
              let! first = call
              match first with
              | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
              | Ok mapped ->
                let mutable acc = mapped :: acc
                let mutable restA = tailA
                let mutable restB = tailB
                while not (List.isEmpty restA) && not (List.isEmpty restB) do
                  match restA, restB with
                  | a :: tA, b :: tB ->
                    match! Exe.executeApplicable2 state app a b with
                    | Ok stepped ->
                      acc <- stepped :: acc
                      restA <- tA
                      restB <- tB
                    | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
                  | _ -> ()
                return mappedList vm (List.rev acc)
            }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listMap" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn(NEList.singleton varA, varB)) "" [ "elem" ] ]
      returnType = TList varB
      description =
        "Calls <param fn> on every value in <param list>, returning a list of the results"
      fn =
        (function
        | state, vm, [], [| DList(_, items); DApplicable app |] ->
          // Built back to front and reversed once at the end, rather than appending, which would
          // re-copy the accumulator per element.
          let mutable acc = []
          let mutable rest = items
          let mutable pending = ValueNone

          while ValueOption.isNone pending && not (List.isEmpty rest) do
            match rest with
            | elem :: tail ->
              let call = Exe.executeApplicable1 state app elem
              match Ply.trySync call with
              | ValueSome(Ok mapped) ->
                acc <- mapped :: acc
                rest <- tail
              | ValueSome(Error(rte, cs)) -> Exe.raiseFromApplied vm rte cs
              | ValueNone -> pending <- ValueSome(struct (call, tail))
            | [] -> ()

          match pending with
          | ValueNone -> Ply(mappedList vm (List.rev acc))
          | ValueSome(struct (call, tail)) ->
            uply {
              let! first = call
              match first with
              | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
              | Ok mapped ->
                let mutable acc = mapped :: acc
                let mutable rest = tail
                while not (List.isEmpty rest) do
                  match rest with
                  | elem :: elemTail ->
                    match! Exe.executeApplicable1 state app elem with
                    | Ok stepped ->
                      acc <- stepped :: acc
                      rest <- elemTail
                    | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
                  | [] -> ()
                return mappedList vm (List.rev acc)
            }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listFilter" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn(NEList.singleton varA, TBool)) "" [ "elem" ] ]
      returnType = TList varA
      description =
        "Calls <param fn> on every value in <param list>, returning a list of the values for which "
        + "it returned true"
      fn =
        (function
        | state, vm, [], [| DList(vt, items); DApplicable app |] ->
          // The result holds a subset of the values that came in, so it keeps their ValueType
          // exactly. Nothing to merge, and nothing that can fail to.
          let mutable acc = []
          let mutable rest = items
          let mutable pending = ValueNone

          while ValueOption.isNone pending && not (List.isEmpty rest) do
            match rest with
            | elem :: tail ->
              let call = Exe.executeApplicable1 state app elem
              match Ply.trySync call with
              | ValueSome(Ok(DBool keep)) ->
                if keep then acc <- elem :: acc
                rest <- tail
              | ValueSome(Ok other) -> raiseRTE vm.threadID (predicateNotBool other)
              | ValueSome(Error(rte, cs)) -> Exe.raiseFromApplied vm rte cs
              | ValueNone -> pending <- ValueSome(struct (call, elem, tail))
            | [] -> ()

          match pending with
          | ValueNone -> Ply(DList(vt, List.rev acc))
          | ValueSome(struct (call, elem, tail)) ->
            uply {
              let! first = call
              let mutable acc = acc
              match first with
              | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
              | Ok(DBool keep) ->
                if keep then acc <- elem :: acc
                let mutable rest = tail
                while not (List.isEmpty rest) do
                  match rest with
                  | next :: elemTail ->
                    match! Exe.executeApplicable1 state app next with
                    | Ok(DBool keepNext) ->
                      if keepNext then acc <- next :: acc
                      rest <- elemTail
                    | Ok other ->
                      return raiseRTE vm.threadID (predicateNotBool other)
                    | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
                  | [] -> ()
                return DList(vt, List.rev acc)
              | Ok other -> return raiseRTE vm.threadID (predicateNotBool other)
            }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
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
          // returned the literal `[]`, and only `push` ever made it a `List<Int>`.
          if List.isEmpty items then
            Ply(DList(VT.unknown, []))
          else
            Ply(DList(VT.int, items))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listAny" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn(NEList.singleton varA, TBool)) "" [ "elem" ] ]
      returnType = TBool
      description =
        "Returns true if <param fn> returns true for any value in <param list>, stopping at the "
        + "first one that does"
      fn =
        (function
        | state, vm, [], [| DList(_, items); DApplicable app |] ->
          let mutable found = false
          let mutable rest = items
          let mutable pending = ValueNone

          while ValueOption.isNone pending && not found && not (List.isEmpty rest) do
            match rest with
            | elem :: tail ->
              let call = Exe.executeApplicable1 state app elem
              match Ply.trySync call with
              | ValueSome(Ok(DBool true)) -> found <- true
              | ValueSome(Ok(DBool false)) -> rest <- tail
              | ValueSome(Ok other) -> raiseRTE vm.threadID (predicateNotBool other)
              | ValueSome(Error(rte, cs)) -> Exe.raiseFromApplied vm rte cs
              | ValueNone -> pending <- ValueSome(struct (call, tail))
            | [] -> ()

          match pending with
          | ValueNone -> Ply(DBool found)
          | ValueSome(struct (call, tail)) ->
            uply {
              let! first = call
              match first with
              | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
              | Ok other ->
                match other with
                | DBool true -> return DBool true
                | DBool false ->
                  let mutable found = false
                  let mutable rest = tail
                  while not found && not (List.isEmpty rest) do
                    match rest with
                    | elem :: elemTail ->
                      match! Exe.executeApplicable1 state app elem with
                      | Ok(DBool true) -> found <- true
                      | Ok(DBool false) -> rest <- elemTail
                      | Ok bad -> return raiseRTE vm.threadID (predicateNotBool bad)
                      | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
                    | [] -> ()
                  return DBool found
                | bad -> return raiseRTE vm.threadID (predicateNotBool bad)
            }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listMember" 0
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
              match ValueType.merge vtElem vtValue with
              | Error _ ->
                RTE.EqualityCheckOnIncompatibleTypes(vtElem, vtValue)
                |> raiseRTE vm.threadID
              | Ok _ -> if Dval.equals elem value then true else search tail

          Ply(DBool(search items))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listFindFirst" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn(NEList.singleton varA, TBool)) "" [ "elem" ] ]
      returnType = TypeReference.option varA
      description =
        "Returns the first value in <param list> for which <param fn> returns true, stopping there"
      fn =
        (function
        | state, vm, [], [| DList(vt, items); DApplicable app |] ->
          let mutable hit = None
          let mutable rest = items
          let mutable pending = ValueNone

          while ValueOption.isNone pending
                && Option.isNone hit
                && not (List.isEmpty rest) do
            match rest with
            | elem :: tail ->
              let call = Exe.executeApplicable1 state app elem
              match Ply.trySync call with
              | ValueSome(Ok(DBool true)) -> hit <- Some elem
              | ValueSome(Ok(DBool false)) -> rest <- tail
              | ValueSome(Ok other) -> raiseRTE vm.threadID (predicateNotBool other)
              | ValueSome(Error(rte, cs)) -> Exe.raiseFromApplied vm rte cs
              | ValueNone -> pending <- ValueSome(struct (call, elem, tail))
            | [] -> ()

          match pending with
          | ValueNone -> Ply(TypeChecker.DvalCreator.option vm.threadID vt hit)
          | ValueSome(struct (call, elem, tail)) ->
            uply {
              let! first = call
              match first with
              | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
              | Ok(DBool true) ->
                return TypeChecker.DvalCreator.option vm.threadID vt (Some elem)
              | Ok(DBool false) ->
                let mutable hit = None
                let mutable rest = tail
                while Option.isNone hit && not (List.isEmpty rest) do
                  match rest with
                  | next :: elemTail ->
                    match! Exe.executeApplicable1 state app next with
                    | Ok(DBool true) -> hit <- Some next
                    | Ok(DBool false) -> rest <- elemTail
                    | Ok bad -> return raiseRTE vm.threadID (predicateNotBool bad)
                    | Error(rte, cs) -> return Exe.raiseFromApplied vm rte cs
                  | [] -> ()
                return TypeChecker.DvalCreator.option vm.threadID vt hit
              | Ok bad -> return raiseRTE vm.threadID (predicateNotBool bad)
            }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Impure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listIsEmpty" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TBool
      description = "Returns true if <param list> has no values"
      fn =
        (function
        | _, _, _, [| DList(_, l) |] -> Ply(DBool(List.isEmpty l))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listLength" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TInt
      description = "Returns the number of values in <param list>"
      fn =
        (function
        | _, _, _, [| DList(_, l) |] -> Ply(Dval.int (bigint l.Length))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
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
          |> List.sortWith DvalComparator.compareDvalInt
          |> fun l -> DList(vt, l)
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


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
        | _, _, _, [| DList(vt, list) |] ->
          list
          |> List.sortWith DvalComparator.compareDvalInt
          |> (fun l -> DList(vt, l))
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listAppend" 0
      typeParams = []
      parameters =
        [ Param.make "as" (TList varA) ""; Param.make "bs" (TList varA) "" ]
      returnType = TList varA
      description =
        "Returns a new list with all values in <param as> followed by all "
        + "values in <param bs>, preserving the order."
      fn =
        (function
        | _, vm, _, [| DList(vt1, l1); DList(vt2, l2) |] ->
          // Both inputs are already-validated DLists, so their concatenation is valid iff their element
          // types merge (O(1)) — no need to re-typecheck every element. This keeps `push` (= `append
          // [x] list`), and therefore `map`/`filter`/`reverse`/every fold-built list op, O(n) instead of
          // O(n²): re-validating the whole growing list on each push was quadratic. On a genuine type
          // conflict, fall back to `DvalCreator.list` for the precise element-level error.
          match VT.merge vt1 vt2 with
          | Ok merged -> Ply(DList(merged, List.append l1 l2))
          | Error() ->
            Ply(TypeChecker.DvalCreator.list vm.threadID vt1 (List.append l1 l2))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listPush" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) ""; Param.make "value" varA "" ]
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
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listPushBack" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) ""; Param.make "value" varA "" ]
      returnType = TList varA
      description = "Adds <param value> to the back of <param list>."
      fn =
        (function
        | _, vm, _, [| DList(vt, l); value |] ->
          // Still O(n): a cons list has to be copied to append. Native for the same reason as `push`.
          match VT.merge vt (Dval.toValueType value) with
          | Ok merged -> Ply(DList(merged, l @ [ value ]))
          | Error() ->
            Ply(TypeChecker.DvalCreator.list vm.threadID vt (l @ [ value ]))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
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
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listGetAt" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) ""; Param.make "index" TInt "" ]
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
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    { name = fn "listReverse" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TList varA
      description = "Returns a reversed copy of <param list>."
      fn =
        (function
        | _, _, _, [| DList(vt, l) |] ->
          // Reordering can't change the element type and every element was checked on the way in, so no
          // type check is needed. Native because `map` and `filter` both end in a reverse.
          Ply(DList(vt, List.rev l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      callEffects = Set.empty
      deprecated = NotDeprecated }


    ]


let builtins () = LibExecution.Builtin.make [] (fns ())
