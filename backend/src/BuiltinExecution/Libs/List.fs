module BuiltinExecution.Libs.List

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = LibExecution.ValueType
module Dval = LibExecution.Dval
module Interpreter = LibExecution.Interpreter
module TypeChecker = LibExecution.TypeChecker


// CLEANUP something like type ComparatorResult = Higher | Lower | Same
// rather than 0/1/-2

module DvalComparator =
  // should this take a vmstate?
  let rec compareDval (dv1 : Dval) (dv2 : Dval) : int =
    match dv1, dv2 with
    | DUnit, DUnit -> 0

    | DBool b1, DBool b2 -> compare b1 b2

    | DInt8 i1, DInt8 i2 -> compare i1 i2
    | DUInt8 i1, DUInt8 i2 -> compare i1 i2
    | DInt16 i1, DInt16 i2 -> compare i1 i2
    | DUInt16 i1, DUInt16 i2 -> compare i1 i2
    | DInt32 i1, DInt32 i2 -> compare i1 i2
    | DUInt32 i1, DUInt32 i2 -> compare i1 i2
    | DInt64 i1, DInt64 i2 -> compare i1 i2
    | DUInt64 i1, DUInt64 i2 -> compare i1 i2
    | DInt128 i1, DInt128 i2 -> compare i1 i2
    | DUInt128 i1, DUInt128 i2 -> compare i1 i2

    | DFloat f1, DFloat f2 -> compare f1 f2

    | DChar c1, DChar c2 -> compare c1 c2
    | DString s1, DString s2 -> compare s1 s2

    | DDateTime dt1, DDateTime dt2 -> compare dt1 dt2

    | DUuid u1, DUuid u2 -> compare u1 u2

    | DList(_, l1), DList(_, l2) -> compareLists l1 l2

    | DTuple(a1, b1, l1), DTuple(a2, b2, l2) ->
      compareLists (a1 :: b1 :: l1) (a2 :: b2 :: l2)


    | DDict(_vtTODO1, o1), DDict(_vtTODO2, o2) ->
      compareMaps (Map.toList o1) (Map.toList o2)

    | DRecord(tn1, _, _typeArgsTODO1, o1), DRecord(tn2, _, _typeArgsTODO2, o2) ->
      let c = compare tn1 tn2
      if c = 0 then compareMaps (Map.toList o1) (Map.toList o2) else c

    | DEnum(typeName1, _, _typeArgsTODO1, case1, fields1),
      DEnum(typeName2, _, _typeArgsTODO2, case2, fields2) ->
      let c = compare typeName1 typeName2
      if c = 0 then
        let c = compare case1 case2
        if c = 0 then compareLists fields1 fields2 else c
      else
        c

    // CLEANUP consider supporting sorting of `DApplicable`s
    // | DApplicable app1, DApplicable app2 ->
    //   match app1, app2 with
    //   | AppLambda l1, AppLambda l2 -> ...
    //   | AppNamedFn n1, AppNamedFn n2 -> ...
    //   | _ -> ...

    // | DFnVal(Lambda l1), DFnVal(Lambda l2) ->
    //   let l1' = NEList.toList l1.parameters
    //   let l2' = NEList.toList l2.parameters
    //   let c = compareLetPatternsLists l1' l2'
    //   if c = 0 then compareExprs l1.body l2.body else c

    | DDB name1, DDB name2 -> compare name1 name2

    // exhaustiveness check
    | DUnit, _
    | DBool _, _
    | DInt8 _, _
    | DUInt8 _, _
    | DInt16 _, _
    | DUInt16 _, _
    | DInt32 _, _
    | DUInt32 _, _
    | DInt64 _, _
    | DUInt64 _, _
    | DInt128 _, _
    | DUInt128 _, _
    | DFloat _, _
    | DChar _, _
    | DString _, _
    | DList _, _
    | DDict _, _
    | DTuple _, _
    | DDateTime _, _
    | DUuid _, _
    | DRecord _, _
    | DEnum _, _
    | DApplicable _, _
    | DDB _, _ ->
      // TODO: Feels like this should hook into typechecker and ValueTypes somehow
      RuntimeError.Error.EqualityCheckOnIncompatibleTypes(
        Dval.toValueType dv1,
        Dval.toValueType dv2
      )
      |> raiseUntargetedRTE



  and compareLists (l1 : List<Dval>) (l2 : List<Dval>) : int =
    match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | h1 :: t1, h2 :: t2 ->
      let c = compareDval h1 h2
      if c = 0 then compareLists t1 t2 else c

  and compareMaps (o1 : List<string * Dval>) (o2 : List<string * Dval>) : int =
    match o1, o2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | (k1, v1) :: t1, (k2, v2) :: t2 ->
      let c = compare k1 k2
      if c = 0 then
        let c = compareDval v1 v2
        if c = 0 then compareMaps t1 t2 else c
      else
        c



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


let fns : List<BuiltInFn> =
  [ { name = fn "listLength" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TInt64
      description = "Returns the number of values in <param list>"
      fn =
        (function
        | _, _, _, [ DList(_, l) ] -> Ply(Dval.int64 (l.Length))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "listUnique" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TList varA
      description =
        "Returns the passed list, with only unique values.
         Only one of each value will be returned, but the
         order will not be maintained."
      fn =
        (function
        | _, _, _, [ DList(vt, l) ] ->
          List.distinct l
          |> List.sortWith DvalComparator.compareDval
          |> fun l -> DList(vt, l)
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "listSort" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TList varA
      description =
        "Returns a copy of <param list> with every value sorted in ascending order.

         Use this if the values have types Dark knows how to sort.

         Consider <fn List.sortBy> or <fn List.sortByComparator> if you need more
         control over the sorting process."
      fn =
        (function
        | _, _, _, [ DList(vt, list) ] ->
          list
          |> List.sortWith DvalComparator.compareDval
          |> (fun l -> DList(vt, l))
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "listAppend" 0
      typeParams = []
      parameters =
        [ Param.make "as" (TList varA) ""; Param.make "bs" (TList varA) "" ]
      returnType = TList varA
      description =
        "Returns a new list with all values in <param as> followed by all values in <param bs>,
         preserving the order."
      fn =
        (function
        | _, vm, _, [ DList(vt1, l1); DList(_vt2, l2) ] ->
          // VTTODO should fail here in the case of vt1 conflicting with vt2?
          // (or is this handled by the interpreter?)
          Ply(TypeChecker.DvalCreator.list vm.threadID vt1 (List.append l1 l2))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "listRandomElement" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TypeReference.option varA
      description =
        "Returns {{Some <var randomValue>}}, where <var randomValue> is a randomly
         selected value in <param list>. Returns {{None}} if <param list> is
         empty."
      fn =
        let optType = VT.unknownTODO
        (function
        | _, _, _, [ DList(_, []) ] ->
          TypeChecker.DvalCreator.optionNone optType |> Ply
        | _, vm, _, [ DList(_, l) ] ->
          // Will return <= (length - 1)
          // Maximum value is Int64.MaxValue which is half of UInt64.MaxValue, but
          // that won't affect this as we won't have a list that big for a long long
          // long time.
          let index = RNG.GetInt32(l.Length)
          (List.tryItem index l)
          |> TypeChecker.DvalCreator.option vm.threadID optType
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
