module BuiltinExecution.Libs.List

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.Builtin.Shortcuts

module Errors = LibExecution.Errors
module VT = ValueType
module Dval = LibExecution.Dval
module Interpreter = LibExecution.Interpreter
module DvalReprDeveloper = LibExecution.DvalReprDeveloper


// CLEANUP something like type ComparatorResult = Higher | Lower | Same
// rather than 0/1/-2

module DvalComparator =
  let rec compareDval (dv1 : Dval) (dv2 : Dval) : int =
    match dv1, dv2 with
    | DInt i1, DInt i2 -> compare i1 i2
    | DFloat f1, DFloat f2 -> compare f1 f2
    | DBool b1, DBool b2 -> compare b1 b2
    | DUnit, DUnit -> 0
    | DString s1, DString s2 -> compare s1 s2
    | DChar c1, DChar c2 -> compare c1 c2
    | DList(_, l1), DList(_, l2) -> compareLists l1 l2
    | DTuple(a1, b1, l1), DTuple(a2, b2, l2) ->
      compareLists (a1 :: b1 :: l1) (a2 :: b2 :: l2)
    | DFnVal(Lambda l1), DFnVal(Lambda l2) ->
      let c = compare (NEList.map snd l1.parameters) (NEList.map snd l2.parameters)
      if c = 0 then
        let c = compareExprs l1.body l2.body
        if c = 0 then
          compareMaps (Map.toList l1.symtable) (Map.toList l2.symtable)
        else
          c
      else
        c

    | DDB name1, DDB name2 -> compare name1 name2
    | DDateTime dt1, DDateTime dt2 -> compare dt1 dt2
    | DUuid u1, DUuid u2 -> compare u1 u2
    | DBytes b1, DBytes b2 -> compare b1 b2
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
    | DInt _, _
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
    | DBytes _, _
    | DDict _, _
    | DRecord _, _
    | DEnum _, _ ->
      raiseString "Comparing different types" [ "dv1", dv1; "dv2", dv2 ]

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

  and compareExprs (e1 : Expr) (e2 : Expr) : int = 0 // CLEAP



// Based on https://github.com/dotnet/runtime/blob/57bfe474518ab5b7cfe6bf7424a79ce3af9d6657/src/coreclr/tools/Common/Sorting/MergeSortCore.cs#L55
module Sort =

  exception InvalidSortComparator of int64

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

let modules = [ "List" ]
let fn = fn modules
let constant = constant modules

let types : List<BuiltInType> = []
let constants : List<BuiltInConstant> = []

let fns : List<BuiltInFn> =
  [ { name = fn "uniqueBy" 0
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
                    let! key = Interpreter.applyFnVal state 0UL b [] args

                    // TODO: type check to ensure `varB` is "comparable"
                    return (dv, key)
                  })
                l

            return
              projected
              |> List.distinctBy snd
              |> List.map fst
              |> List.sortWith DvalComparator.compareDval
              |> Dval.list vt
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "length" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TInt
      description = "Returns the number of values in <param list>"
      fn =
        (function
        | _, _, [ DList(vt, l) ] -> Ply(Dval.int (l.Length))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "unique" 0
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
          |> Dval.list vt
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "sort" 0
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
          list |> List.sortWith DvalComparator.compareDval |> Dval.list vt |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "sortBy" 0
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
              Interpreter.applyFnVal state 0UL b [] args
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
              |> Dval.list vt
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "sortByComparator" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.doubleton varA varA, TInt))
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
        let okType = VT.unknownTODO
        let resultOk = Dval.resultOk okType VT.string
        let resultError = Dval.resultError okType VT.string

        (function
        | state, _, [ DList(vt, list); DFnVal f ] ->
          let fn (dv1 : Dval) (dv2 : Dval) : Ply<int> =
            uply {
              let args = NEList.doubleton dv1 dv2
              let! result = Interpreter.applyFnVal state 0UL f [] args

              match result with
              | DInt i when i = 1L || i = 0L || i = -1L -> return int i
              | DInt i -> return raise (Sort.InvalidSortComparator i)
              | v ->
                // CLEANUP this yields pretty confusing error messages
                return raiseString (Errors.expectedLambdaValue "fn" "-1, 0, 1" v)
            }

          uply {
            try
              let array = List.toArray list
              do! Sort.sort fn array
              return array |> Array.toList |> Dval.list vt |> resultOk
            with Sort.InvalidSortComparator i ->
              // CLEANUP this yields pretty confusing error messages
              let message = Errors.expectedLambdaValue "fn" "-1, 0, 1" (DInt i)
              return resultError (DString message)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "append" 0
      typeParams = []
      parameters =
        [ Param.make "as" (TList varA) ""; Param.make "bs" (TList varA) "" ]
      returnType = TList varA
      description =
        "Returns a new list with all values in <param as> followed by all values in <param bs>,
         preserving the order."
      fn =
        (function
        | _, _, [ DList(vt1, l1); DList(vt2, l2) ] ->
          // VTTODO should fail here in the case of vt1 conflicting with vt2?
          // (or is this handled by the interpreter?)
          Ply(Dval.list vt1 (List.append l1 l2))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "filter" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn(NEList.singleton varA, TBool)) "" [ "val" ] ]
      returnType = TList varA
      description =
        "Calls <param f> on every <var val> in <param list>, returning a list of only
         those values for which {{fn val}} returns {{true}}.

         Preserves the order of values that were not dropped. Consider <fn
         List.filterMap> if you also want to transform the values."
      fn =
        (function
        | state, _, [ DList(vt, l); DFnVal fn ] ->
          uply {
            let f (dv : Dval) : Ply<bool> =
              uply {
                let args = NEList.singleton dv
                let! result = Interpreter.applyFnVal state 0UL fn [] args

                match result with
                | DBool b -> return b
                | v -> return raiseString (Errors.expectedLambdaType "fn" TBool v)
              }

            let! result = Ply.List.filterSequentially f l
            return Dval.list vt result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "filterMap" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.singleton varA, TypeReference.option varB))
            ""
            [ "val" ] ]
      returnType = TList varB
      description =
        "Calls <param fn> on every <var val> in <param list>, returning a list that
         drops some values (filter) and transforms others (map).

         If {{fn val}} returns {{None}}, drops <var val> from the list.

         If {{fn val}} returns {{Some newValue}}, replaces <var val> with <var newValue>.

         Preserves the order of values that were not dropped.

         This function combines <fn List.filter> and <fn List.map>."
      fn =
        (function
        | state, _, [ DList(_, l); DFnVal b ] ->
          uply {
            let f (dv : Dval) : Ply<Option<Dval>> =
              uply {
                let args = NEList.singleton dv
                let! result = Interpreter.applyFnVal state 0UL b [] args

                match result with
                | DEnum(FQName.Package { owner = "Darklang"
                                         modules = [ "Stdlib"; "Option" ]
                                         name = TypeName.TypeName "Option"
                                         version = 0 },
                        _,
                        _typeArgsDEnumTODO,
                        "Some",
                        [ o ]) -> return Some o
                | DEnum(FQName.Package { owner = "Darklang"
                                         modules = [ "Stdlib"; "Option" ]
                                         name = TypeName.TypeName "Option"
                                         version = 0 },
                        _,
                        _typeArgsDEnumTODO,
                        "None",
                        []) -> return None
                | v ->
                  return
                    raiseString (
                      Errors.expectedLambdaType "fn" (TypeReference.option varB) v
                    )
              }

            let! result = Ply.List.filterMapSequentially f l
            return Dval.list VT.unknownTODO result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "indexedMap" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs
            "fn"
            (TFn(NEList.doubleton TInt varA, varB))
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
                  let args = NEList.doubleton (DInt(int64 i)) dv
                  Interpreter.applyFnVal state 0UL b [] args)
                list

            return Dval.list VT.unknownTODO result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "map2shortest" 0
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
                  Interpreter.applyFnVal state 0UL b [] args)
                list

            return Dval.list VT.unknownTODO result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "map2" 0
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
              return Dval.optionNone optType
            else
              let list = List.zip l1 l2

              let! result =
                Ply.List.mapSequentially
                  (fun ((dv1, dv2) : Dval * Dval) ->
                    let args = NEList.doubleton dv1 dv2
                    Interpreter.applyFnVal state 0UL b [] args)
                  list

              return Dval.optionSome optType (Dval.list VT.unknownTODO result)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "randomElement" 0
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
        | _, _, [ DList(_, []) ] -> Ply(Dval.optionNone optType)
        | _, _, [ DList(_, l) ] ->
          // Will return <= (length - 1)
          // Maximum value is Int64.MaxValue which is half of UInt64.MaxValue, but
          // that won't affect this as we won't have a list that big for a long long
          // long time.
          let index = RNG.GetInt32(l.Length)
          (List.tryItem index l) |> Dval.option optType |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "groupByWithKey" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn(NEList.singleton varA, varB)) "" [ "item" ] ]
      returnType = TList(TTuple(varB, TList varA, []))
      description =
        "Groups <param list> into tuples (key, elements), where the key is computed by applying
         <param fn> to each element in the list.

         For example, if <param list> is {{[1, 2, 3, 4, 5]}} and <param fn>
         is {{fn item -> Int.mod_v0 item 2}}, returns {{[(1, [1, 3, 5]), (0, [2, 4])]}}.

          Preserves the order of values and of the keys."
      fn =
        (function
        | state, _, [ DList(_vtTODO, l); DFnVal fn ] ->
          uply {
            let applyFn (dval : Dval) : DvalTask =
              let args = NEList.singleton dval
              Interpreter.applyFnVal state 0UL fn [] args

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
                let elements = Seq.map snd elementsWithKey |> Seq.toList
                DTuple(key, Dval.list VT.unknownTODO elements, []))
              |> Dval.list VT.unknownTODO
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated } ]

let contents = (fns, types, constants)
