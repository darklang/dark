module BuiltinExecution.Libs.List

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module VT = ValueType
module Dval = LibExecution.Dval
module Interpreter = LibExecution.Interpreter
module TypeChecker = LibExecution.TypeChecker


// CLEANUP something like type ComparatorResult = Higher | Lower | Same
// rather than 0/1/-2

module DvalComparator =
  let rec compareDval (dv1 : Dval) (dv2 : Dval) : int =
    match dv1, dv2 with
    | DInt64 i1, DInt64 i2 -> compare i1 i2
    | DUInt64 i1, DUInt64 i2 -> compare i1 i2
    | DInt8 i1, DInt8 i2 -> compare i1 i2
    | DUInt8 i1, DUInt8 i2 -> compare i1 i2
    | DInt16 i1, DInt16 i2 -> compare i1 i2
    | DUInt16 i1, DUInt16 i2 -> compare i1 i2
    | DInt32 i1, DInt32 i2 -> compare i1 i2
    | DUInt32 i1, DUInt32 i2 -> compare i1 i2
    | DInt128 i1, DInt128 i2 -> compare i1 i2
    | DUInt128 i1, DUInt128 i2 -> compare i1 i2
    | DFloat f1, DFloat f2 -> compare f1 f2
    | DBool b1, DBool b2 -> compare b1 b2
    | DUnit, DUnit -> 0
    | DString s1, DString s2 -> compare s1 s2
    | DChar c1, DChar c2 -> compare c1 c2
    | DList(_, l1), DList(_, l2) -> compareLists l1 l2
    | DTuple(a1, b1, l1), DTuple(a2, b2, l2) ->
      compareLists (a1 :: b1 :: l1) (a2 :: b2 :: l2)
    | DFnVal(Lambda l1), DFnVal(Lambda l2) ->
      let l1' = NEList.toList l1.parameters
      let l2' = NEList.toList l2.parameters
      let c = compareLetPatternsLists l1' l2'
      if c = 0 then compareExprs l1.body l2.body else c

    | DDB name1, DDB name2 -> compare name1 name2
    | DDateTime dt1, DDateTime dt2 -> compare dt1 dt2
    | DUuid u1, DUuid u2 -> compare u1 u2
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

    // exhaustiveness check
    | DInt64 _, _
    | DUInt64 _, _
    | DInt8 _, _
    | DUInt8 _, _
    | DInt16 _, _
    | DUInt16 _, _
    | DInt32 _, _
    | DUInt32 _, _
    | DInt128 _, _
    | DUInt128 _, _
    | DFloat _, _
    | DBool _, _
    | DUnit, _
    | DString _, _
    | DChar _, _
    | DList _, _
    | DTuple _, _
    | DFnVal _, _
    | DDB _, _
    | DDateTime _, _
    | DUuid _, _
    | DDict _, _
    | DRecord _, _
    | DEnum _, _ ->
      // TODO: Feels like this should hook into typechecker and ValueTypes somehow
      raiseUntargetedString "Comparing different types" [ "dv1", dv1; "dv2", dv2 ]
  and compareLetPatternsLists (l1 : List<LetPattern>) (l2 : List<LetPattern>) : int =

    let rec equalsLetPattern (pattern1 : LetPattern) (pattern2 : LetPattern) : int =
      match pattern1, pattern2 with
      | LPVariable(_, name1), LPVariable(_, name2) -> compare name1 name2
      | LPUnit _, LPUnit _ -> 0

      | LPTuple(_, first, second, theRest), LPTuple(_, first', second', theRest') ->
        let all = first :: second :: theRest
        let all' = first' :: second' :: theRest'
        if all.Length <> all'.Length then
          compare all.Length all'.Length
        else
          let c = equalsLetPattern first first'
          if c = 0 then
            let c = equalsLetPattern second second'
            if c = 0 then compareLetPatternsLists theRest theRest' else c
          else
            c

      | LPTuple _, LPVariable _ -> 1
      | LPTuple _, LPUnit _ -> 1
      | LPUnit _, LPVariable _ -> -1
      | LPVariable _, LPUnit _ -> 1
      | LPVariable _, LPTuple _ -> -1
      | _, _ -> -1

    match l1, l2 with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | h1 :: t1, h2 :: t2 ->
      let c = equalsLetPattern h1 h2
      if c = 0 then compareLetPatternsLists t1 t2 else c



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

  and compareExprs (_e1 : Expr) (_e2 : Expr) : int = 0 // CLEANUP



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
  [ { name = fn "listUniqueBy" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn(NEList.singleton varA, varB)) "" [ "val" ] ]
      returnType = TList varA
      description =
        "Returns the passed list, with only unique values, where uniqueness is based
         on the result of <param fn>. Only one of each value will be returned, but the
         order will not be maintained."
      fn =
        (function
        | state, _, [ DList(vt, l); DFnVal b ] ->
          uply {
            let! projected =
              Ply.List.mapSequentially
                (fun dv ->
                  uply {
                    let args = NEList.singleton dv
                    let! key = Interpreter.applyFnVal state b [] args

                    // TODO: type check to ensure `varB` is "comparable"
                    return (dv, key)
                  })
                l

            return
              projected
              |> List.distinctBy snd
              |> List.map fst
              |> List.sortWith DvalComparator.compareDval
              |> fun l -> DList(vt, l)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "listLength" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TInt64
      description = "Returns the number of values in <param list>"
      fn =
        (function
        | _, _, [ DList(_, l) ] -> Ply(Dval.int64 (l.Length))
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
        | _, _, [ DList(vt, l) ] ->
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
        | _, _, [ DList(vt, list) ] ->
          list
          |> List.sortWith DvalComparator.compareDval
          |> (fun l -> DList(vt, l))
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "listSortBy" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn(NEList.singleton varA, varB)) "" [ "val" ] ]
      returnType = TList varA
      description =
        "Returns a copy of <param list>, sorted in ascending order, as if each value
         evaluated to {{fn val}}.

         For example, {{List.sortBy [\"x\",\"jkl\",\"ab\"] \\val -> String.length
         val}} returns {{[ \"x\", \"ab\", \"jkl\" ]}}.

         Consider <fn List.sort> if the list values can be directly compared, or <fn
         List.sortByComparator> if you want more control over the sorting process."
      fn =
        (function
        | state, _, [ DList(vt, list); DFnVal b ] ->
          uply {
            let fn dv =
              let args = NEList.singleton dv
              Interpreter.applyFnVal state b [] args
            let! withKeys =
              list
              |> Ply.List.mapSequentially (fun v ->
                uply {
                  let! key = fn v
                  return (key, v)
                })

            return
              withKeys
              |> List.sortWith (fun (k1, _) (k2, _) ->
                DvalComparator.compareDval k1 k2)
              |> List.map snd
              |> fun l -> DList(vt, l)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "listSortByComparator" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.doubleton varA varA, TInt64))
            ""
            [ "a"; "b" ] ]
      returnType = TypeReference.result varA TString
      description =
        "Returns a copy of <param list>, sorted using {{fn a b}} to compare values
         <var a> and <var b>.

         <param f> must return {{-1}} if <var a> should appear before <var b>, {{1}}
         if <var a> should appear after <var b>, and {{0}} if the order of <var a>
         and <var b> doesn't matter.

         Consider <fn List.sort> or <fn List.sortBy> if you don't need this level
         of control."
      fn =

        (function
        | state, _, [ DList(vt, list); DFnVal f ] ->
          let okType = VT.unknownTODO
          let resultOk =
            TypeChecker.DvalCreator.resultOk state.tracing.callStack okType VT.string
          let resultError =
            TypeChecker.DvalCreator.resultError
              state.tracing.callStack
              okType
              VT.string


          let fn (dv1 : Dval) (dv2 : Dval) : Ply<int> =
            uply {
              let args = NEList.doubleton dv1 dv2
              let! result = Interpreter.applyFnVal state f [] args

              match result with
              | DInt64 i when i = 1L || i = 0L || i = -1L -> return int i
              | DInt64 i -> return raise (Sort.InvalidSortComparatorInt i)
              | v ->
                return!
                  TypeChecker.raiseFnValResultNotExpectedType
                    state.tracing.callStack
                    v
                    TInt64
            }

          uply {
            try
              let array = List.toArray list
              do! Sort.sort fn array
              return array |> Array.toList |> (fun l -> DList(vt, l)) |> resultOk
            with Sort.InvalidSortComparatorInt i ->
              let message =
                $"Expected comparator function to return -1, 0, or 1, but it returned {i}"
              return resultError (DString message)
          }
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
        | state, _, [ DList(vt1, l1); DList(_vt2, l2) ] ->
          // VTTODO should fail here in the case of vt1 conflicting with vt2?
          // (or is this handled by the interpreter?)
          Ply(
            TypeChecker.DvalCreator.list
              state.tracing.callStack
              vt1
              (List.append l1 l2)
          )
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "listIndexedMap" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.doubleton TInt64 varA, varB))
            ""
            [ "index"; "val" ] ]
      returnType = TList varB
      description =
        "Calls <fn fn> on every <var val> and its <var index> in <param list>,
         returning a list of the results of those calls.

         Consider <fn List.map> if you don't need the index."
      fn =
        (function
        | state, _, [ DList(_vtTODO, l); DFnVal b ] ->
          uply {
            let list = List.mapi (fun i v -> (i, v)) l

            let! result =
              Ply.List.mapSequentially
                (fun ((i, dv) : int * Dval) ->
                  let args = NEList.doubleton (DInt64(int64 i)) dv
                  Interpreter.applyFnVal state b [] args)
                list

            return
              TypeChecker.DvalCreator.list
                state.tracing.callStack
                VT.unknownTODO
                result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "listMap2shortest" 0
      typeParams = []
      parameters =
        [ Param.make "as" (TList varA) ""
          Param.make "bs" (TList varB) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.doubleton varA varB, varC))
            ""
            [ "a"; "b" ] ]
      returnType = TList varC
      description =
        "Maps <param fn> over <param as> and <param bs> in parallel, calling {{fn a
         b}} on every pair of values from <param as> and <param bs>.

         If the lists differ in length, values from the longer list are dropped.

         For example, if <param as> is {{[1,2]}} and <param bs> is
         {{[\"x\",\"y\",\"z\"]}}, returns {{[(f 1 \"x\"), (f 2 \"y\")]}}

         Use <fn List.map2> if you want to enforce equivalent lengths for <param as>
         and <param bs>."
      fn =
        (function
        | state, _, [ DList(_vtTODO1, l1); DList(_vtTODO2, l2); DFnVal b ] ->
          uply {
            let len = min (List.length l1) (List.length l2)
            let l1 = List.take (int len) l1
            let l2 = List.take (int len) l2

            let list = List.zip l1 l2

            let! result =
              Ply.List.mapSequentially
                (fun ((dv1, dv2) : Dval * Dval) ->
                  let args = NEList.doubleton dv1 dv2
                  Interpreter.applyFnVal state b [] args)
                list

            return
              TypeChecker.DvalCreator.list
                state.tracing.callStack
                VT.unknownTODO
                result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "listMap2" 0
      typeParams = []
      parameters =
        [ Param.make "as" (TList varA) ""
          Param.make "bs" (TList varB) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.doubleton varA varB, varC))
            ""
            [ "a"; "b" ] ]
      returnType = TypeReference.option varC
      description =
        "If the lists are the same length, returns {{Some list}} formed by mapping
         <param fn> over <param as> and <param bs> in parallel, calling {{fn a b}} on
         every pair of values from <param as> and <param bs>.

         For example, if <param as> is {{[1,2,3]}} and <param bs> is
         {{[\"x\",\"y\",\"z\"]}}, returns {{[(fn 1 \"x\"), (f 2 \"y\"), (f 3
         \"z\")]}}.

         If the lists differ in length, returns {{None}} (consider <fn
         List.map2shortest> if you want to drop values from the longer list
         instead)."
      fn =
        let optType = VT.unknownTODO
        (function
        | state, _, [ DList(_vtTODO1, l1); DList(_vtTODO2, l2); DFnVal b ] ->
          uply {
            if List.length l1 <> List.length l2 then
              return TypeChecker.DvalCreator.optionNone optType
            else
              let list = List.zip l1 l2

              let! result =
                Ply.List.mapSequentially
                  (fun ((dv1, dv2) : Dval * Dval) ->
                    let args = NEList.doubleton dv1 dv2
                    Interpreter.applyFnVal state b [] args)
                  list

              let callStack = state.tracing.callStack

              return
                TypeChecker.DvalCreator.list callStack VT.unknownTODO result
                |> TypeChecker.DvalCreator.optionSome callStack optType
          }
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
        | _, _, [ DList(_, []) ] -> TypeChecker.DvalCreator.optionNone optType |> Ply
        | state, _, [ DList(_, l) ] ->
          // Will return <= (length - 1)
          // Maximum value is Int64.MaxValue which is half of UInt64.MaxValue, but
          // that won't affect this as we won't have a list that big for a long long
          // long time.
          let index = RNG.GetInt32(l.Length)
          (List.tryItem index l)
          |> TypeChecker.DvalCreator.option state.tracing.callStack optType
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "listGroupByWithKey" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn(NEList.singleton varA, varB)) "" [ "item" ] ]
      returnType = TList(TTuple(varB, TList varA, []))
      description =
        "Groups <param list> into tuples (key, elements), where the key is computed by applying
         <param fn> to each element in the list.

         For example, if <param list> is {{[1, 2, 3, 4, 5]}} and <param fn>
         is {{fn item -> Int64.mod item 2}}, returns {{[(1, [1, 3, 5]), (0, [2, 4])]}}.

          Preserves the order of values and of the keys."
      fn =
        (function
        | state, _, [ DList(listType, l); DFnVal fn ] ->
          uply {
            let applyFn (dval : Dval) : DvalTask =
              let args = NEList.singleton dval
              Interpreter.applyFnVal state fn [] args

            // apply the function to each element in the list
            let! result =
              Ply.List.mapSequentially
                (fun dval ->
                  uply {
                    let! key = applyFn dval
                    return (key, dval)
                  })
                l

            return
              result
              |> Seq.groupBy fst
              |> Seq.toList
              |> List.map (fun (key, elementsWithKey) ->
                DTuple(
                  key,
                  DList(listType, Seq.map snd elementsWithKey |> Seq.toList),
                  []
                ))
              |> fun pairs ->
                  DList(VT.tuple VT.unknownTODO (VT.list listType) [], pairs)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]


let builtins = LibExecution.Builtin.make [] fns
