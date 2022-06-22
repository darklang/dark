module LibExecutionStdLib.LibList

open LibExecution.RuntimeTypes
open Prelude

module Interpreter = LibExecution.Interpreter
module Errors = LibExecution.Errors
module DvalReprExternal = LibExecution.DvalReprExternal

let fn = FQFnName.stdlibFnName

let incorrectArgs = Errors.incorrectArgs

let err (str : string) = Ply(Dval.errStr str)

// Based on https://github.com/dotnet/runtime/blob/57bfe474518ab5b7cfe6bf7424a79ce3af9d6657/src/coreclr/tools/Common/Sorting/MergeSortCore.cs#L55
module Sort =
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

let fns : List<BuiltInFn> =
  [ { name = fn "List" "singleton" 0
      parameters = [ Param.make "val" (TVariable "a") "" ]
      returnType = TList(TVariable "a")
      description = "Returns a one-element list containing the given `val`."
      fn =
        (function
        | _, [ v ] -> Ply(DList [ v ])
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "head" 0
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = varA
      description =
        "Returns the head of a list. Returns null if the empty list is passed."
      fn =
        (function
        | _, [ DList l ] -> List.tryHead l |> Option.defaultValue DNull |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "List" "head" 1) }


    { name = fn "List" "head" 1
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TOption varA
      description = "Fetches the head of the list and returns an option"
      fn =
        (function
        | _, [ DList l ] -> Ply(DOption(List.tryHead l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "List" "head" 2) }


    { name = fn "List" "head" 2
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TOption varA
      description =
        "Returns `Just` the head (first value) of a list. Returns `Nothing` if the list is empty."
      fn =
        (function
        | _, [ DList l ] -> l |> List.tryHead |> Dval.option |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "tail" 0
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TOption(TList varA)
      description =
        "If the list contains at least one value, returns `Just` a list of every value other than the first. Otherwise, returns `Nothing`."
      fn =
        // This matches Elm's implementation, with the added benefit that the error rail
        // means you don't need to handle unwrapping the option
        // unless the passed list is truly empty (which shouldn't happen in most practical uses).
        (function
        | _, [ DList l ] ->
          (if List.isEmpty l then None else Some(DList l.Tail)) |> DOption |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "empty" 0
      parameters = []
      returnType = TList varA
      description = "Returns an empty list."
      fn =
        (function
        | _, [] -> Ply(DList [])
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "push" 0
      parameters = [ Param.make "list" (TList varA) ""; Param.make "val" varA "" ]
      returnType = TList varA
      description = "Add element `val` to front of list `list`"
      fn =
        // fakeval handled by call
        (function
        | _, [ DList l; i ] -> Ply(DList(i :: l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "pushBack" 0
      parameters = [ Param.make "list" (TList varA) ""; Param.make "val" varA "" ]
      returnType = TList varA
      description = "Add element `val` to back of list `list`"
      fn =
        (function
        | _, [ DList l; i ] -> Ply(DList(l @ [ i ]))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "last" 0
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = varA
      description =
        "Returns the last value in `list`. Returns null if the list is empty."
      fn =
        (function
        | _, [ DList l ] -> (if List.isEmpty l then DNull else List.last l) |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "List" "last" 1) }


    { name = fn "List" "last" 1
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TOption varA
      description =
        "Returns the last value in `list`, wrapped in an option (`Nothing` if the list is empty)."
      fn =
        (function
        | _, [ DList l ] -> Ply(DOption(List.tryLast l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "List" "last" 2) }


    { name = fn "List" "last" 2
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TOption varA
      description =
        "Returns the last value in `list`, wrapped in an option (`Nothing` if the list is empty)."
      fn =
        (function
        | _, [ DList l ] -> l |> List.tryLast |> Dval.option |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "reverse" 0
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TList varA
      description = "Returns a reversed copy of `list`."
      fn =
        (function
        | _, [ DList l ] -> Ply(DList(List.rev l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "findFirst" 0
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "f" (TFn([ varA ], TBool)) "" [ "val" ] ]
      returnType = varA
      description =
        "Returns the first value of `list` for which `f val` returns `true`. Returns null if no such value exists."
      fn =
        (function
        | state, [ DList l; DFnVal fn ] ->
          uply {
            let f (dv : Dval) : Ply<bool> =
              uply {
                let! result =
                  Interpreter.applyFnVal state (id 0) fn [ dv ] NotInPipe NoRail

                return result = DBool true
              }

            let! result = Ply.List.findSequentially f l
            return Option.defaultValue DNull result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "List" "findFirst" 1) }


    { name = fn "List" "findFirst" 1
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "f" (TFn([ varA ], TBool)) "" [ "val" ] ]
      returnType = TOption varA
      description =
        "Returns the first value of `list` for which `f val` returns `true`. Returns `Nothing` if no such value exists."
      fn =
        (function
        | state, [ DList l; DFnVal fn ] ->
          uply {
            let f (dv : Dval) : Ply<bool> =
              uply {
                let! result =
                  Interpreter.applyFnVal state (id 0) fn [ dv ] NotInPipe NoRail

                return result = DBool true
              }

            let! result = Ply.List.findSequentially f l
            return DOption result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "List" "findFirst" 2) }


    { name = fn "List" "findFirst" 2
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "f" (TFn([ varA ], TBool)) "" [ "val" ] ]
      returnType = TOption varA
      description =
        "Returns `Just firstMatch` where `firstMatch` is the first value of the list for which `f` returns `true`. Returns `Nothing` if no such value exists."
      fn =
        (function
        | state, [ DList l; DFnVal fn ] ->
          uply {
            let f (dv : Dval) : Ply<bool> =
              uply {
                let! result =
                  Interpreter.applyFnVal state (id 0) fn [ dv ] NotInPipe NoRail

                return result = DBool true
              }

            let! result = Ply.List.findSequentially f l
            return Dval.option result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "contains" 0
      parameters = [ Param.make "list" (TList varA) ""; Param.make "val" varA "" ]
      returnType = TBool
      description = "Returns `true` if `val` is in the list."
      fn =
        (function
        | _, [ DList l; i ] -> Ply(DBool(List.contains i l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      // Deprecated in favor of List::member for consistency with Elm's naming
      deprecated = ReplacedBy(fn "List" "member" 0) }


    { name = fn "List" "member" 0
      parameters = [ Param.make "list" (TList varA) ""; Param.make "val" varA "" ]
      returnType = TBool
      description = "Returns `true` if `val` is in the list."
      fn =
        (function
        | _, [ DList l; i ] -> Ply(DBool(List.contains i l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "repeat" 0
      parameters = [ Param.make "times" TInt ""; Param.make "val" varA "" ]
      returnType = TList varA
      description = "Returns a new list containing `val` repeated `times` times."
      fn =
        (function
        | _, [ DInt times; v ] ->
          if times < 0L then
            err (Errors.argumentWasnt "positive" "times" (DInt times))
          else if times > 2147483647L then
            err (Errors.argumentWasnt "less than 2147483647" "times" (DInt times))
          else
            List.replicate (int times) v |> DList |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "length" 0
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TInt
      description = "Returns the number of values in `list`."
      fn =
        (function
        | _, [ DList l ] -> Ply(Dval.int (l.Length))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "range" 0
      parameters =
        [ Param.make "lowest" TInt "First, smallest number in the list"
          Param.make "highest" TInt "Last, largest number in the list" ]
      returnType = TList TInt
      description =
        "Returns a list of numbers where each element is 1 larger than the previous. You provide the `lowest` and `highest` numbers in the list. If `lowest` is greater than `highest`, returns the empty list."
      fn =
        (function
        | _, [ DInt start; DInt stop ] ->
          [ start..stop ] |> List.map DInt |> DList |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "fold" 0
      parameters =
        [ Param.make "list" (TList varA) "The list of items to process one at a time"
          Param.make "init" varB "The initial starting value"
          Param.makeWithArgs
            "f"
            (TFn([ varB; varA ], varB))
            "the function taking the accumulated value and the next list item, returning the next accumulated item."
            [ "accum"; "curr" ] ]
      returnType = varB
      description =
        "Folds `list` into a single value, by repeatedly applying `f` to any two pairs."
      fn =
        (function
        | state, [ DList l; init; DFnVal b ] ->
          (* Fake cf should be propagated by the blocks so we dont need to check *)
          uply {
            let f (accum : DvalTask) (item : Dval) : DvalTask =
              uply {
                let! accum = accum

                return!
                  Interpreter.applyFnVal
                    state
                    (id 0) // CLEANUP should use real id here
                    b
                    [ accum; item ]
                    NotInPipe
                    NoRail
              }

            return! List.fold f (Ply init) l
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "flatten" 0
      parameters = [ Param.make "list" (TList(TList varA)) "" ]
      returnType = TList varA
      description =
        "Returns a single list containing the values of every list directly in `list` (does not recursively flatten nested lists)."
      fn =
        (function
        | _, [ DList l ] ->
          let f acc i =
            match i with
            | DList l -> List.append acc l
            | _ -> Exception.raiseCode "Flattening non-lists"

          List.fold f [] l |> DList |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "interpose" 0
      parameters = [ Param.make "list" (TList varA) ""; Param.make "sep" varA "" ]
      returnType = TList varA
      description =
        "Returns a single list containing the values of `list` separated by `sep`."
      fn =
        (function
        | _, [ DList l; i ] ->
          let rec join ls =
            match ls with
            | [] -> []
            | h :: t ->
              (match t with
               | [] -> [ h ]
               | t -> [ h ] @ [ i ] @ join t)

          Ply(DList(join l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "interleave" 0
      parameters =
        [ Param.make "as" (TList varA) ""; Param.make "bs" (TList varB) "" ]
      returnType = TList varA
      description =
        "Returns a new list with the first value from <param as> then the first value from <param bs>, then the second value from <param as> then the second value from <param bs>, etc, until one list ends, then the remaining items from the other list."
      fn =
        (function
        | _, [ DList l1; DList l2 ] ->
          let rec f l1 l2 =
            match l1 with
            | [] -> l2
            | x :: xs ->
              (match l2 with
               | [] -> l1
               | y :: ys -> x :: y :: f xs ys)

          Ply(DList(f l1 l2))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "uniqueBy" 0
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "f" (TFn([ varA ], varB)) "" [ "val" ] ]
      returnType = TList varA
      description =
        "Returns the passed list, with only unique values, where uniqueness is based on the result of `f`. Only one of each value will be returned, but the order will not be maintained."
      fn =
        (function
        | state, [ DList l; DFnVal b ] ->
          uply {
            let! projected =
              Ply.List.mapSequentially
                (fun dv ->
                  uply {
                    let! key =
                      Interpreter.applyFnVal state (id 0) b [ dv ] NotInPipe NoRail

                    return (dv, key)
                  })
                l

            let distinct = List.distinctBy snd projected

            return distinct |> List.sortBy fst |> List.map fst |> DList
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "isEmpty" 0
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TBool
      description = "Returns true if `list` has no values."
      fn =
        (function
        | _, [ DList l ] -> Ply(DBool(List.isEmpty l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "sort" 0
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TList varA
      description =
        "Returns a copy of `list` with every value sorted in ascending order. Use this if the values have types Dark knows how to sort.
         Consider `List::sortBy` or `List::sortByComparator` if you need more control over the sorting process."
      fn =
        (function
        | _, [ DList list ] -> list |> List.sort |> DList |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "sortBy" 0
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "f" (TFn([ varA ], varB)) "" [ "val" ] ]
      returnType = TList varA
      description =
        "Returns a copy of `list`, sorted in ascending order, as if each value evaluated to `f val`.
           For example, `List::sortBy [\"x\",\"jkl\",\"ab\"] \\val -> String::length val` returns `[ \"x\", \"ab\", \"jkl\" ]`.
           Consider `List::sort` if the list values can be directly compared, or `List::sortByComparator` if you want more control over the sorting process."
      fn =
        (function
        | state, [ DList list; DFnVal b ] ->
          uply {
            let fn dv = Interpreter.applyFnVal state 0UL b [ dv ] NotInPipe NoRail
            // FSNOTE: This isn't exactly the same as the ocaml one. We get all the keys in one pass.
            let! withKeys =
              list
              |> Ply.List.mapSequentially (fun v ->
                uply {
                  let! key = fn v
                  return (key, v)
                })

            return withKeys |> List.sortBy fst |> List.map snd |> DList
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "sortByComparator" 0
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "f" (TFn([ varA; varA ], TInt)) "" [ "a"; "b" ] ]
      returnType = TResult(varA, TStr)
      description =
        "Returns a copy of `list`, sorted using `f a b` to compare values `a` and `b`.
        `f` must return `-1` if `a` should appear before `b`, `1` if `a` should appear after `b`, and `0` if the order of `a` and `b` doesn't matter.
        Consider `List::sort` or `List::sortBy` if you don't need this level of control."
      fn =
        (function
        | state, [ DList list; DFnVal f ] ->
          let fn (dv1 : Dval) (dv2 : Dval) : Ply<int> =
            uply {
              let! result =
                Interpreter.applyFnVal state (id 0) f [ dv1; dv2 ] NotInPipe NoRail

              match result with
              | DInt i when i = 1L || i = 0L || i = -1L -> return int i
              | _ ->
                return
                  Exception.raiseCode (
                    Errors.expectedLambdaValue "f" "-1, 0, 1" result
                  )
            }

          uply {
            try
              let array = List.toArray list
              do! Sort.sort fn array
              // CLEANUP: check fakevals
              return array |> Array.toList |> DList |> Ok |> DResult
            with
            | e -> return DResult(Error(DStr e.Message))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "append" 0
      parameters =
        [ Param.make "as" (TList varA) ""; Param.make "bs" (TList varA) "" ]
      returnType = TList varA
      description =
        "Returns a new list with all values in `as` followed by all values in `bs`, preserving the order."
      fn =
        (function
        | _, [ DList l1; DList l2 ] ->
          Ply(DList(List.append l1 l2)) (* no checking for fake cf required *)
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "filter" 0
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "f" (TFn([ varA ], TBool)) "" [ "val" ] ]
      returnType = TList varA
      description =
        "Return only values in `list` which meet the function's criteria. The function should return true to keep the entry or false to remove it."
      fn =
        (function
        | state, [ DList l; DFnVal fn ] ->
          uply {
            let incomplete = ref false

            let f (dv : Dval) : Ply<bool> =
              uply {
                let! result =
                  Interpreter.applyFnVal state (id 0) fn [ dv ] NotInPipe NoRail

                match result with
                | DBool b -> return b
                | DIncomplete _ ->
                  incomplete.Value <- true
                  return false
                | v ->
                  return Exception.raiseCode (Errors.expectedLambdaType "f" TBool v)
              }

            if incomplete.Value then
              return DIncomplete SourceNone
            else
              let! result = Ply.List.filterSequentially f l
              return DList(result)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "List" "filter" 1) }
    // { name = fn "List" "all" 0 // CLEANUP: not in the ocaml version, add it back
    //   parameters =
    //     [ Param.make "list" (TList varA) ""
    //       Param.make
    //         "fn"
    //         (TFn([ varA ], TBool))
    //         "Function to be applied on all list elements;" ]
    //   returnType = TBool
    //   description =
    //     "Return true if all elements in the list meet the function's criteria, else false."
    //   fn =
    //     (function
    //     | state, [ DList l; DFnVal b ] ->
    //         uply {
    //           let incomplete = ref false
    //
    //           let f (dv : Dval) : Ply<bool> =
    //             uply {
    //               let! r =
    //                 LibExecution.Interpreter.applyFnVal
    //                   state
    //                   (id 0)
    //                   b
    //                   [ dv ]
    //                   NotInPipe
    //                   NoRail
    //
    //               match r with
    //               | DBool b -> return b
    //               | DIncomplete _ ->
    //                   incomplete := true
    //                   return false
    //               | v ->
    //                   Exception.raiseCode (Errors.expectedLambdaType TBool dv)
    //                   return false
    //             }
    //
    //           if !incomplete then
    //             return DIncomplete SourceNone
    //           else
    //             let! result = Ply.List.filterSequentially f l
    //             return DBool((result.Length) = (l.Length))
    //         }
    //     | _ -> incorrectArgs ())
    //   sqlSpec = NotYetImplementedTODO
    //   previewable = Pure
    //   deprecated = NotDeprecated }


    { name = fn "List" "filter" 1
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "f" (TFn([ varA ], TBool)) "" [ "val" ] ]
      returnType = TList varA
      description =
        "Calls `f` on every `val` in `list`, returning a list of only those values for which `f val` returns `true`.
        Preserves the order of values that were not dropped.
        Consider `List::filterMap` if you also want to transform the values."
      fn =
        (function
        | state, [ DList l; DFnVal fn ] ->
          uply {
            let fakecf = ref None

            let f (dv : Dval) : Ply<bool> =
              uply {
                let run = fakecf.Value = None

                if run then
                  let! result =
                    Interpreter.applyFnVal state (id 0) fn [ dv ] NotInPipe NoRail

                  match result with
                  | DBool b -> return b
                  | (DIncomplete _
                  | DErrorRail _) as dv ->
                    fakecf.Value <- Some dv
                    return false
                  | v ->
                    return
                      Exception.raiseCode (Errors.expectedLambdaType "f" TBool v)
                else
                  return false
              }

            let! result = Ply.List.filterSequentially f l

            match fakecf.Value with
            | None -> return DList(result)
            | Some v -> return v
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "List" "filter" 2) }


    { name = fn "List" "filter" 2
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "f" (TFn([ varA ], TBool)) "" [ "val" ] ]
      returnType = TList varA
      description =
        "Calls `f` on every `val` in `list`, returning a list of only those values for which `f val` returns `true`.
        Preserves the order of values that were not dropped.
        Consider `List::filterMap` if you also want to transform the values."
      fn =
        (function
        | state, [ DList l; DFnVal fn ] ->
          uply {
            let abortReason = ref None

            let f (dv : Dval) : Ply<bool> =
              uply {
                let run = abortReason.Value = None

                if run then
                  let! result =
                    Interpreter.applyFnVal state (id 0) fn [ dv ] NotInPipe NoRail

                  match result with
                  | DBool b -> return b
                  | (DIncomplete _
                  | DErrorRail _
                  | DError _) as dv ->
                    abortReason.Value <- Some dv
                    return false
                  | v ->
                    return
                      Exception.raiseCode (Errors.expectedLambdaType "f" TBool v)
                else
                  return false
              }

            let! result = Ply.List.filterSequentially f l

            match abortReason.Value with
            | None -> return DList(result)
            | Some v -> return v
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "filterMap" 0
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "f" (TFn([ varA ], TOption varB)) "" [ "val" ] ]
      returnType = TList varB
      description =
        "Calls `f` on every `val` in `list`, returning a new list that drops some values (filter) and transforms others (map).
        If `f val` returns `Nothing`, drops `val` from the list.
        If `f val` returns `Just newValue`, replaces `val` with `newValue`.
        Preserves the order of values that were not dropped.
        This function combines `List::filter` and `List::map`."
      fn =
        (function
        | state, [ DList l; DFnVal b ] ->
          uply {
            let abortReason = ref None

            let f (dv : Dval) : Ply<Dval option> =
              uply {
                let run = abortReason.Value = None

                if run then
                  let! result =
                    Interpreter.applyFnVal state (id 0) b [ dv ] NotInPipe NoRail

                  match result with
                  | DOption (Some o) -> return Some o
                  | DOption None -> return None
                  | (DIncomplete _
                  | DErrorRail _
                  | DError _) as dv ->
                    abortReason.Value <- Some dv
                    return None
                  | v ->
                    return
                      Exception.raiseCode (
                        Errors.expectedLambdaType "f" (TOption varB) v
                      )
                else
                  return None
              }

            let! result = Ply.List.filterMapSequentially f l

            match abortReason.Value with
            | None -> return DList result
            | Some v -> return v
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "drop" 0
      parameters = [ Param.make "list" (TList varA) ""; Param.make "count" TInt "" ]
      returnType = TList varA
      description = "Drops the first `count` values from `list`."
      fn =
        (function
        | _, [ DList l; DInt c ] ->
          if c < 0L then Ply(DList l)
          elif c > int64 (List.length l) then Ply(DList [])
          else Ply(DList(List.skip (int c) l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "dropWhile" 0
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "f" (TFn([ varA ], varB)) "" [ "val" ] ]
      returnType = TList varB
      description =
        "Drops the longest prefix of `list` which satisfies the predicate `val`"
      fn =
        (function
        | state, [ DList l; DFnVal b ] ->
          uply {
            let mutable abortReason = None

            let rec f (list : List<Dval>) : Ply<List<Dval>> =
              uply {
                match list with
                | [] -> return []
                | dv :: dvs ->
                  let run = abortReason = None

                  if run then
                    let! result =
                      Interpreter.applyFnVal state (id 0) b [ dv ] NotInPipe NoRail

                    match result with
                    | DBool true -> return! f dvs
                    | DBool false -> return dv :: dvs
                    | (DIncomplete _
                    | DErrorRail _
                    | DError _) as dv ->
                      abortReason <- Some dv
                      return []
                    | v ->
                      return
                        Exception.raiseCode (Errors.expectedLambdaType "f" TBool v)
                  else
                    return []
              }

            let! result = f l

            match abortReason with
            | None -> return DList result
            | Some v -> return v
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "take" 0
      parameters = [ Param.make "list" (TList varA) ""; Param.make "count" TInt "" ]
      returnType = TList varA
      description = "Drops all but the first `count` values from `list`."
      fn =
        (function
        | _, [ DList l; DInt c ] ->
          if c < 0L then Ply(DList [])
          elif c >= int64 (List.length l) then Ply(DList l)
          else Ply(DList(List.take (int c) l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "takeWhile" 0
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "f" (TFn([ varA ], varB)) "" [ "val" ] ]
      returnType = TList varA
      description =
        "Return the longest prefix of `list` which satisfies the predicate `val`"
      fn =
        (function
        | state, [ DList l; DFnVal b ] ->
          uply {
            let mutable abortReason = None

            let rec f (list : List<Dval>) : Ply<List<Dval>> =
              uply {
                match list with
                | [] -> return []
                | dv :: dvs ->
                  let run = abortReason = None

                  if run then
                    let! result =
                      Interpreter.applyFnVal state (id 0) b [ dv ] NotInPipe NoRail

                    match result with
                    | DBool true ->
                      let! tail = f dvs
                      return dv :: tail
                    | DBool false -> return []
                    | (DIncomplete _
                    | DErrorRail _
                    | DError _) as dv ->
                      abortReason <- Some dv
                      return []
                    | v ->
                      return
                        Exception.raiseCode (Errors.expectedLambdaType "f" TBool v)
                  else
                    return []
              }

            let! result = f l

            match abortReason with
            | None -> return DList result
            | Some v -> return v
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "foreach" 0
      parameters =
        [ Param.make "list" (TList varA) ""
          // CLEANUP rename these args to "fn"
          Param.makeWithArgs "f" (TFn([ varA ], varB)) "" [ "val" ] ]
      returnType = TList varB
      description =
        "Call `f` on every `val` in the list, returning a list of the results of
         those calls"
      fn =
        (function
        | state, [ DList l; DFnVal b ] ->
          uply {
            let! result =
              Ply.List.mapSequentially
                (fun dv ->
                  Interpreter.applyFnVal state (id 0) b [ dv ] NotInPipe NoRail)
                l

            return DList result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "List" "map" 0) }


    { name = fn "List" "map" 0
      parameters =
        [ Param.make "list" (TList varA) "The list to be operated on"
          Param.makeWithArgs "f" (TFn([ varA ], varB)) "" [ "val" ] ]
      description =
        "Calls `f` on every `val` in `list`, returning a list of the results of those calls.
        Consider `List::filterMap` if you also want to drop some of the values."
      returnType = TList varB
      fn =
        (function
        | state, [ DList l; DFnVal b ] ->
          uply {
            let! result =
              Ply.List.mapSequentially
                (fun dv ->
                  Interpreter.applyFnVal state (id 0) b [ dv ] NotInPipe NoRail)
                l

            return Dval.list result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "indexedMap" 0
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "f" (TFn([ TInt; varA ], varB)) "" [ "index"; "val" ] ]
      returnType = TList varB
      description =
        "Calls `f` on every `val` and its `index` in `list`, returning a list of the results of those calls.
        Consider `List::map` if you don't need the index."
      fn =
        (function
        | state, [ DList l; DFnVal b ] ->
          uply {
            let list = List.mapi (fun i v -> (i, v)) l

            let! result =
              Ply.List.mapSequentially
                (fun ((i, dv) : int * Dval) ->
                  Interpreter.applyFnVal
                    state
                    (id 0)
                    b
                    [ DInt(int64 i); dv ]
                    NotInPipe
                    NoRail)
                list

            return Dval.list result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "map2shortest" 0
      parameters =
        [ Param.make "as" (TList varA) ""
          Param.make "bs" (TList varB) ""
          Param.makeWithArgs "f" (TFn([ varA; varB ], varC)) "" [ "a"; "b" ] ]
      returnType = TList varC
      description =
        "Maps `f` over `as` and `bs` in parallel, calling `f a b` on every pair of values from `as` and `bs`.
        If the lists differ in length, values from the longer list are dropped.
        For example, if `as` is `[1,2]` and `bs` is `[\"x\",\"y\",\"z\"]`, returns `[(f 1 \"x\"), (f 2 \"y\")]`.
        Use `List::map2` if you want to enforce equivalent lengths for `as` and `bs`."
      fn =
        (function
        | state, [ DList l1; DList l2; DFnVal b ] ->
          uply {
            let len = min (List.length l1) (List.length l2)
            let l1 = List.take (int len) l1
            let l2 = List.take (int len) l2

            let list = List.zip l1 l2

            let! result =
              Ply.List.mapSequentially
                (fun ((dv1, dv2) : Dval * Dval) ->
                  Interpreter.applyFnVal state (id 0) b [ dv1; dv2 ] NotInPipe NoRail)
                list

            return Dval.list result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "map2" 0
      parameters =
        [ Param.make "as" (TList varA) ""
          Param.make "bs" (TList varB) ""
          Param.makeWithArgs "f" (TFn([ varA; varB ], varC)) "" [ "a"; "b" ] ]
      returnType = TOption varC
      description =
        "If the lists are the same length, returns `Just list` formed by mapping `f` over `as` and `bs` in parallel,
         calling `f a b` on every pair of values from `as` and `bs`.
         For example, if `as` is `[1,2,3]` and `bs` is `[\"x\",\"y\",\"z\"]`, returns `[(f 1 \"x\"), (f 2 \"y\"), (f 3 \"z\")]`.
         If the lists differ in length, returns `Nothing` (consider `List::map2shortest` if you want to drop values from the longer list instead)."
      fn =
        (function
        | state, [ DList l1; DList l2; DFnVal b ] ->
          uply {
            if List.length l1 <> List.length l2 then
              return DOption None
            else
              let list = List.zip l1 l2

              let! result =
                Ply.List.mapSequentially
                  (fun ((dv1, dv2) : Dval * Dval) ->
                    Interpreter.applyFnVal
                      state
                      (id 0)
                      b
                      [ dv1; dv2 ]
                      NotInPipe
                      NoRail)
                  list

              return DOption(Some(Dval.list result))
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "zipShortest" 0
      parameters =
        [ Param.make "as" (TList varA) ""; Param.make "bs" (TList varB) "" ]
      returnType = TList varA
      description =
        "Returns a list of parallel pairs from `as` and `bs`.
        If the lists differ in length, values from the longer list are dropped.
        For example, if `as` is `[1,2]` and `bs` is `[\"x\",\"y\",\"z\"]`, returns `[[1,\"x\"], [2,\"y\"]]`.
        Use `List::zip` if you want to enforce equivalent lengths for `as` and `bs`.
        See `List::unzip` if you want to deconstruct the result into `as` and `bs` again."
      fn =
        (function
        | state, [ DList l1; DList l2 ] ->
          // We have to do this munging because OCaml's map2
          // and Fsharp's zip enforces lists of the same length
          let len = min (List.length l1) (List.length l2)
          let l1 = List.take (int len) l1
          let l2 = List.take (int len) l2

          List.zip l1 l2
          |> List.map (fun (val1, val2) -> Dval.list [ val1; val2 ])
          |> Dval.list
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "zip" 0
      parameters =
        [ Param.make "as" (TList varA) ""; Param.make "bs" (TList varB) "" ]
      returnType = TOption(TList(TList varA))
      description =
        "If the lists have the same length, returns `Just list` formed from parallel pairs in `as` and `bs`.
        For example, if `as` is `[1,2,3]` and `bs` is `[\"x\",\"y\",\"z\"]`, returns `[[1,\"x\"], [2,\"y\"], [3,\"z\"]]`.
        See `List::unzip` if you want to deconstruct `list` into `as` and `bs` again.
        If the lists differ in length, returns `Nothing` (consider `List::zipShortest` if you want to drop values from the longer list instead)."
      fn =
        (function
        | state, [ DList l1; DList l2 ] ->
          if List.length l1 <> List.length l2 then
            Ply(DOption None)
          else
            List.zip l1 l2
            |> List.map (fun (val1, val2) -> Dval.list [ val1; val2 ])
            |> Dval.list
            |> Some
            |> DOption
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "unzip" 0
      parameters = [ Param.make "pairs" (TList(TList varA)) "" ]
      returnType = TList(TList varA)
      description =
        "Given a `pairs` list where each value is a list of two values (such lists are constructed by `List::zip` and `List::zipShortest`), returns a list of two lists,
        one with every first value, and one with every second value. For example, if `pairs` is `[[1,\"x\"], [2,\"y\"], [3,\"z\"]]`, returns `[[1,2,3], [\"x\",\"y\",\"z\"]]`."
      fn =
        (function
        | state, [ DList l ] ->

          let f (acc1, acc2) i =
            match i with
            | DList [ a; b ] -> (a :: acc1, b :: acc2)
            | (DIncomplete _
            | DErrorRail _
            | DError _) as dv -> Errors.foundFakeDval dv
            | v ->
              let errDetails =
                match v with
                | DList l ->
                  $". It has length {List.length l} but should have length 2"
                | nonList ->
                  $". It is of type {DvalReprExternal.prettyTypename v} instead of `List`"

              Exception.raiseCode (
                Errors.argumentWasnt "a list with exactly two values" "pairs" v
                + errDetails
              )

          let result = l |> List.rev |> List.fold f ([], [])

          match result with
          | (l, l2) -> Ply(DList [ DList l; DList l2 ])
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      // CLEANUP deprecate and replace with tuples
      deprecated = NotDeprecated }


    { name = fn "List" "getAt" 0
      parameters = [ Param.make "list" (TList varA) ""; Param.make "index" TInt "" ]
      returnType = TOption varA
      description =
        "Returns `Just value` at `index` in `list` if `index` is less than the length of the list. Otherwise returns `Nothing`."
      fn =
        (function
        | _, [ DList l; DInt index ] ->
          if index > int64 (List.length l) then
            Ply(DOption None)
          else
            Ply(DOption(List.tryItem (int index) l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "List" "getAt" 1) }


    { name = fn "List" "getAt" 1
      parameters = [ Param.make "list" (TList varA) ""; Param.make "index" TInt "" ]
      returnType = TOption varA
      description =
        "Returns `Just value` at `index` in `list` if `index` is less than the length of the list otherwise returns `Nothing`."
      fn =
        (function
        | _, [ DList l; DInt index ] ->
          (List.tryItem (int index) l) |> Dval.option |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "List" "randomElement" 0
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TOption varA
      description =
        "Returns {{Just <var randomValue>}}, where <var randomValue> is a randomly selected value in <param list>. Returns {{Nothing}} if <param list> is empty."
      fn =
        (function
        | _, [ DList [] ] -> Ply(DOption None)
        | _, [ DList l ] ->
          // Will return <= (length - 1)
          // Maximum value is Int64.MaxValue which is half of UInt64.MaxValue, but
          // that won't affect this as we won't have a list that big for a long long
          // long time.
          let index = RNG.GetInt32(l.Length)
          (List.tryItem index l) |> Dval.option |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated } ]
