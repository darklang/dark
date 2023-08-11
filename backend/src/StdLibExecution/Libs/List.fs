module StdLibExecution.Libs.List

open Prelude
open LibExecution.RuntimeTypes
open LibExecution.StdLib.Shortcuts

module Errors = LibExecution.Errors
module Interpreter = LibExecution.Interpreter
module DvalReprDeveloper = LibExecution.DvalReprDeveloper

module DvalComparator =
  let rec compareDval (dv1 : Dval) (dv2 : Dval) : int =
    match dv1, dv2 with
    | DInt i1, DInt i2 -> compare i1 i2
    | DFloat f1, DFloat f2 -> compare f1 f2
    | DBool b1, DBool b2 -> compare b1 b2
    | DUnit, DUnit -> 0
    | DString s1, DString s2 -> compare s1 s2
    | DChar c1, DChar c2 -> compare c1 c2
    | DList l1, DList l2 -> compareLists l1 l2
    | DTuple(a1, b1, l1), DTuple(a2, b2, l2) ->
      compareLists (a1 :: b1 :: l1) (a2 :: b2 :: l2)
    | DFnVal(Lambda l1), DFnVal(Lambda l2) ->
      let c = compare (List.map snd l1.parameters) (List.map snd l2.parameters)
      if c = 0 then
        let c = compareExprs l1.body l2.body
        if c = 0 then
          compareMaps (Map.toList l1.symtable) (Map.toList l2.symtable)
        else
          c
      else
        c
    | DError(_, str1), DError(_, str2) -> compare str1 str2
    | DIncomplete _, DIncomplete _ -> 0
    | DDB name1, DDB name2 -> compare name1 name2
    | DDateTime dt1, DDateTime dt2 -> compare dt1 dt2
    | DPassword _, DPassword _ -> 0 // CLEANUP - how do we handle this?
    | DUuid u1, DUuid u2 -> compare u1 u2
    | DBytes b1, DBytes b2 -> compare b1 b2
    | DDict o1, DDict o2 -> compareMaps (Map.toList o1) (Map.toList o2)
    | DRecord(tn1, _, o1), DRecord(tn2, _, o2) ->
      let c = compare tn1 tn2
      if c = 0 then compareMaps (Map.toList o1) (Map.toList o2) else c
    | DEnum(tn1, _, c1, f1), DEnum(tn2, _, c2, f2) ->
      let c = compare tn1 tn2
      if c = 0 then
        let c = compare c1 c2
        if c = 0 then compareLists f1 f2 else c
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
    | DError _, _
    | DIncomplete _, _
    | DDB _, _
    | DDateTime _, _
    | DPassword _, _
    | DUuid _, _
    | DBytes _, _
    | DDict _, _
    | DRecord _, _
    | DEnum _, _ ->
      Exception.raiseCode "Comparing different types" [ "dv1", dv1; "dv2", dv2 ]

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
let constants : List<BuiltInConstant> =
  [ { name = constant "empty" 0
      typ = TList varA
      description = "Returns an empty list"
      body = DList []
      deprecated = NotDeprecated } ]

let fns : List<BuiltInFn> =
  [ { name = fn "singleton" 0
      typeParams = []
      parameters = [ Param.make "val" (TVariable "a") "" ]
      returnType = TList(TVariable "a")
      description = "Returns a one-element list containing the given <param val>"
      fn =
        (function
        | _, _, [ v ] -> Ply(DList [ v ])
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "findFirst" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn([ varA ], TBool)) "" [ "val" ] ]
      returnType = TypeReference.option varA
      description =
        "Returns {{Some firstMatch}} where <var firstMatch> is the first value of the
         list for which <param fn> returns {{true}}. Returns {{None}} if no such
         value exists"
      fn =
        (function
        | state, _, [ DList l; DFnVal fn ] ->
          uply {
            let f (dv : Dval) : Ply<bool> =
              uply {
                let! result = Interpreter.applyFnVal state 0UL fn [] [ dv ]

                return result = DBool true
              }

            let! result = Ply.List.findSequentially f l
            return Dval.option result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "member" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) ""; Param.make "val" varA "" ]
      returnType = TBool
      description = "Returns {{true}} if <param val> is in the list"
      fn =
        (function
        | _, _, [ DList l; i ] -> Ply(DBool(List.contains i l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "repeat" 0
      typeParams = []
      parameters = [ Param.make "times" TInt ""; Param.make "val" varA "" ]
      returnType = TypeReference.result (TList varA) TString
      description =
        "Returns a list containing <param val> repeated <param times> times"
      fn =
        (function
        | _, _, [ DInt times; v ] ->
          let errPipe e = e |> DString |> Dval.resultError |> Ply
          if times < 0L then
            Errors.argumentWasnt "positive" "times" (DInt times) |> errPipe
          else if times > 2147483647L then
            Errors.argumentWasnt "less than 2147483647" "times" (DInt times)
            |> errPipe
          else
            List.replicate (int times) v |> DList |> Dval.resultOk |> Ply
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
        | _, _, [ DList l ] -> Ply(Dval.int (l.Length))
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "range" 0
      typeParams = []
      parameters =
        [ Param.make "lowest" TInt "First, smallest number in the list"
          Param.make "highest" TInt "Last, largest number in the list" ]
      returnType = TList TInt
      description =
        "Returns a list of numbers where each element is {{1}} larger than the
         previous. You provide the <param lowest> and <param highest> numbers in the
         list."
      fn =
        (function
        | _, _, [ DInt start; DInt stop ] ->
          [ start..stop ] |> List.map DInt |> DList |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "fold" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) "The list of items to process one at a time"
          Param.make "init" varB "The initial starting value"
          Param.makeWithArgs
            "fn"
            (TFn([ varB; varA ], varB))
            "the function taking the accumulated value and the next list item, returning the next accumulated item."
            [ "accum"; "curr" ] ]
      returnType = varB
      description =
        "Folds <param list> into a single value, by repeatedly applying <param fn> to
         any two pairs."
      fn =
        (function
        | state, _, [ DList l; init; DFnVal b ] ->
          // Fake cf should be propagated by the blocks so we dont need to check
          uply {
            let f (accum : DvalTask) (item : Dval) : DvalTask =
              uply {
                let! accum = accum
                return! Interpreter.applyFnVal state 0UL b [] [ accum; item ]
              }

            return! List.fold f (Ply init) l
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    // CLEANUP: This can't be moved to packages until the package manager is live and stable.
    // we can't use PACKAGE functions during the "load from disk into DB" flow
    { name = fn "flatten" 0
      typeParams = []
      parameters = [ Param.make "list" (TList(TList varA)) "" ]
      returnType = TList varA
      description =
        "Returns a single list containing the values of every list directly in <param
         list> (does not recursively flatten nested lists)"
      fn =
        (function
        | _, _, [ DList l ] ->
          let f acc i =
            match i with
            | DList l -> List.append acc l
            | _ -> Exception.raiseCode "Flattening non-lists"

          List.fold f [] l |> DList |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "interpose" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) ""; Param.make "sep" varA "" ]
      returnType = TList varA
      description =
        "Returns a single list containing the values of <param list> separated by <param sep>"
      fn =
        (function
        | _, _, [ DList l; i ] ->
          let rec join ls =
            match ls with
            | [] -> []
            | h :: t ->
              (match t with
               | [] -> [ h ]
               | t -> [ h ] @ [ i ] @ join t)

          Ply(DList(join l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "interleave" 0
      typeParams = []
      parameters =
        [ Param.make "as" (TList varA) ""; Param.make "bs" (TList varB) "" ]
      returnType = TList varA
      description =
        "Returns a list with the first value from <param as> then the first value
         from <param bs>, then the second value from <param as> then the second value
         other list."
      fn =
        (function
        | _, _, [ DList l1; DList l2 ] ->
          let rec f l1 l2 =
            match l1 with
            | [] -> l2
            | x :: xs ->
              (match l2 with
               | [] -> l1
               | y :: ys -> x :: y :: f xs ys)

          Ply(DList(f l1 l2))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "uniqueBy" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn([ varA ], varB)) "" [ "val" ] ]
      returnType = TList varA
      description =
        "Returns the passed list, with only unique values, where uniqueness is based
         on the result of <param fn>. Only one of each value will be returned, but the
         order will not be maintained."
      fn =
        (function
        | state, _, [ DList l; DFnVal b ] ->
          uply {
            try
              let! projected =
                Ply.List.mapSequentially
                  (fun dv ->
                    uply {
                      let! key = Interpreter.applyFnVal state 0UL b [] [ dv ]

                      // TODO: type check to ensure `varB` is "comparable"
                      return (dv, key)
                    })
                  l

              return
                projected
                |> List.distinctBy snd
                |> List.map fst
                |> List.sortWith DvalComparator.compareDval
                |> DList
            with _ ->
              // TODO: we should prevent this as soon as the different types are added
              // Ideally we'd catch the exception thrown during comparison but the sort
              // catches it so we lose the error message
              return
                "List.uniqueBy: Unable to sort list, perhaps the list elements are different types"
                |> Dval.errStr
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    // TODO: type check to ensure `varA` is "comparable"
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
        | _, _, [ DList l ] ->
          try
            List.distinct l
            |> List.sortWith DvalComparator.compareDval
            |> DList
            |> Ply
          with _ ->
            // TODO: we should prevent this as soon as the different types are added
            // Ideally we'd catch the exception thrown during comparison but the sort
            // catches it so we lose the error message
            "List.unique: Unable to sort list, perhaps the list elements are different types"
            |> Dval.errStr
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
        | _, _, [ DList list ] ->
          try
            list |> List.sortWith DvalComparator.compareDval |> DList |> Ply
          with _ ->
            // TODO: we should prevent this as soon as the different types are added
            // Ideally we'd catch the exception thrown during comparison but the sort
            // catches it so we lose the error message
            "List.sort: Unable to sort list, perhaps the list elements are different types"
            |> Dval.errStr
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "sortBy" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn([ varA ], varB)) "" [ "val" ] ]
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
        | state, _, [ DList list; DFnVal b ] ->
          uply {
            try
              let fn dv = Interpreter.applyFnVal state 0UL b [] [ dv ]
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
                |> DList
            with _ ->
              // TODO: we should prevent this as soon as the different types are added
              // Ideally we'd catch the exception thrown during comparison but the sort
              // catches it so we lose the error message
              return
                "List.sortBy: Unable to sort list, perhaps the list elements are different types"
                |> Dval.errStr
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "sortByComparator" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn([ varA; varA ], TInt)) "" [ "a"; "b" ] ]
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
        | state, _, [ DList list; DFnVal f ] ->
          let fn (dv1 : Dval) (dv2 : Dval) : Ply<int> =
            uply {
              let! result = Interpreter.applyFnVal state 0UL f [] [ dv1; dv2 ]

              match result with
              | DInt i when i = 1L || i = 0L || i = -1L -> return int i
              | _ ->
                return
                  Exception.raiseCode (
                    Errors.expectedLambdaValue "fn" "-1, 0, 1" result
                  )
            }

          uply {
            try
              let array = List.toArray list
              do! Sort.sort fn array
              // CLEANUP: check fakevals
              return array |> Array.toList |> DList |> Dval.resultOk
            with e ->
              return Dval.resultError (DString e.Message)
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
        | _, _, [ DList l1; DList l2 ] -> Ply(DList(List.append l1 l2)) // no checking for DError required
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "all" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs
            "fn"
            (TFn([ varA ], TBool))
            "Function to be applied on all list elements"
            [ "val" ] ]
      returnType = TBool
      description =
        "Return {{true}} if all elements in the list meet the function's criteria, else {{false}}"
      fn =
        (function
        | state, _, [ DList l; DFnVal b ] ->
          uply {
            let mutable incomplete = false

            let f (dv : Dval) : Ply<bool> =
              uply {
                let! r = Interpreter.applyFnVal state 0UL b [] [ dv ]

                match r with
                | DBool b -> return b
                | DIncomplete _ ->
                  incomplete <- true
                  return false
                | _ ->
                  Exception.raiseCode (Errors.expectedLambdaType "fn" TBool dv)
                  return false
              }

            if incomplete then
              return DIncomplete SourceNone
            else
              let! result = Ply.List.filterSequentially f l
              return DBool(result.Length = l.Length)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "filter" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn([ varA ], TBool)) "" [ "val" ] ]
      returnType = TList varA
      description =
        "Calls <param f> on every <var val> in <param list>, returning a list of only
         those values for which {{fn val}} returns {{true}}.

         Preserves the order of values that were not dropped. Consider <fn
         List.filterMap> if you also want to transform the values."
      fn =
        (function
        | state, _, [ DList l; DFnVal fn ] ->
          uply {
            let abortReason = ref None

            let f (dv : Dval) : Ply<bool> =
              uply {
                let run = abortReason.Value = None

                if run then
                  let! result = Interpreter.applyFnVal state 0UL fn [] [ dv ]

                  match result with
                  | DBool b -> return b
                  | (DIncomplete _ | DError _) as dv ->
                    abortReason.Value <- Some dv
                    return false
                  | v ->
                    return
                      Exception.raiseCode (Errors.expectedLambdaType "fn" TBool v)
                else
                  return false
              }

            let! result = Ply.List.filterSequentially f l

            match abortReason.Value with
            | None -> return DList(result)
            | Some v -> return v
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
            (TFn([ varA ], TypeReference.option varB))
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
        | state, _, [ DList l; DFnVal b ] ->
          uply {
            let abortReason = ref None

            let f (dv : Dval) : Ply<Option<Dval>> =
              uply {
                let run = abortReason.Value = None

                if run then
                  let! result = Interpreter.applyFnVal state 0UL b [] [ dv ]

                  match result with
                  | DEnum(FQName.Package { owner = "Darklang"
                                           modules = { head = "Stdlib"
                                                       tail = [ "Option" ] }
                                           name = TypeName.TypeName "Option"
                                           version = 0 },
                          _,
                          "Some",
                          [ o ]) -> return Some o
                  | DEnum(FQName.Package { owner = "Darklang"
                                           modules = { head = "Stdlib"
                                                       tail = [ "Option" ] }
                                           name = TypeName.TypeName "Option"
                                           version = 0 },
                          _,
                          "None",
                          []) -> return None
                  | (DIncomplete _ | DError _) as dv ->
                    abortReason.Value <- Some dv
                    return None
                  | v ->
                    return
                      Exception.raiseCode (
                        Errors.expectedLambdaType "fn" (TypeReference.option varB) v
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
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "drop" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) ""; Param.make "count" TInt "" ]
      returnType = TList varA
      description = "Drops the first <param count> values from <param list>"
      fn =
        (function
        | _, _, [ DList l; DInt c ] ->
          if c < 0L then Ply(DList l)
          elif c > int64 (List.length l) then Ply(DList [])
          else Ply(DList(List.skip (int c) l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "dropWhile" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn([ varA ], varB)) "" [ "val" ] ]
      returnType = TList varB
      description =
        "Drops the longest prefix of <param list> which satisfies the predicate <param val>"
      fn =
        (function
        | state, _, [ DList l; DFnVal b ] ->
          uply {
            let mutable abortReason = None

            let rec f (list : List<Dval>) : Ply<List<Dval>> =
              uply {
                match list with
                | [] -> return []
                | dv :: dvs ->
                  let run = abortReason = None

                  if run then
                    let! result = Interpreter.applyFnVal state 0UL b [] [ dv ]

                    match result with
                    | DBool true -> return! f dvs
                    | DBool false -> return dv :: dvs
                    | (DIncomplete _ | DError _) as dv ->
                      abortReason <- Some dv
                      return []
                    | v ->
                      return
                        Exception.raiseCode (Errors.expectedLambdaType "fn" TBool v)
                  else
                    return []
              }

            let! result = f l

            match abortReason with
            | None -> return DList result
            | Some v -> return v
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "take" 0
      typeParams = []
      parameters = [ Param.make "list" (TList varA) ""; Param.make "count" TInt "" ]
      returnType = TList varA
      description = "Drops all but the first <param count> values from <param list>"
      fn =
        (function
        | _, _, [ DList l; DInt c ] ->
          if c < 0L then Ply(DList [])
          elif c >= int64 (List.length l) then Ply(DList l)
          else Ply(DList(List.take (int c) l))
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "takeWhile" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn([ varA ], varB)) "" [ "val" ] ]
      returnType = TList varA
      description =
        "Return the longest prefix of <param list> which satisfies the predicate <param fn>"
      fn =
        (function
        | state, _, [ DList l; DFnVal b ] ->
          uply {
            let mutable abortReason = None

            let rec f (list : List<Dval>) : Ply<List<Dval>> =
              uply {
                match list with
                | [] -> return []
                | dv :: dvs ->
                  let run = abortReason = None

                  if run then
                    let! result = Interpreter.applyFnVal state 0UL b [] [ dv ]

                    match result with
                    | DBool true ->
                      let! tail = f dvs
                      return dv :: tail
                    | DBool false -> return []
                    | (DIncomplete _ | DError _) as dv ->
                      abortReason <- Some dv
                      return []
                    | v ->
                      return
                        Exception.raiseCode (Errors.expectedLambdaType "fn" TBool v)
                  else
                    return []
              }

            let! result = f l

            match abortReason with
            | None -> return DList result
            | Some v -> return v
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "map" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) "The list to be operated on"
          Param.makeWithArgs "fn" (TFn([ varA ], varB)) "" [ "val" ] ]
      description =
        "Calls <param fn> on every <var val> in <param list>, returning a list of the
         results of those calls.

         Consider <fn List.filterMap> if you also want to drop some of the values."
      returnType = TList varB
      fn =
        (function
        | state, _, [ DList l; DFnVal b ] ->
          uply {
            let! result =
              Ply.List.mapSequentially
                (fun dv -> Interpreter.applyFnVal state 0UL b [] [ dv ])
                l

            return Dval.list result
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "indexedMap" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn([ TInt; varA ], varB)) "" [ "index"; "val" ] ]
      returnType = TList varB
      description =
        "Calls <fn fn> on every <var val> and its <var index> in <param list>,
         returning a list of the results of those calls.

         Consider <fn List.map> if you don't need the index."
      fn =
        (function
        | state, _, [ DList l; DFnVal b ] ->
          uply {
            let list = List.mapi (fun i v -> (i, v)) l

            let! result =
              Ply.List.mapSequentially
                (fun ((i, dv) : int * Dval) ->
                  Interpreter.applyFnVal state 0UL b [] [ DInt(int64 i); dv ])
                list

            return Dval.list result
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
          Param.makeWithArgs "fn" (TFn([ varA; varB ], varC)) "" [ "a"; "b" ] ]
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
        | state, _, [ DList l1; DList l2; DFnVal b ] ->
          uply {
            let len = min (List.length l1) (List.length l2)
            let l1 = List.take (int len) l1
            let l2 = List.take (int len) l2

            let list = List.zip l1 l2

            let! result =
              Ply.List.mapSequentially
                (fun ((dv1, dv2) : Dval * Dval) ->
                  Interpreter.applyFnVal state 0UL b [] [ dv1; dv2 ])
                list

            return Dval.list result
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
          Param.makeWithArgs "fn" (TFn([ varA; varB ], varC)) "" [ "a"; "b" ] ]
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
        (function
        | state, _, [ DList l1; DList l2; DFnVal b ] ->
          uply {
            if List.length l1 <> List.length l2 then
              return Dval.optionNone
            else
              let list = List.zip l1 l2

              let! result =
                Ply.List.mapSequentially
                  (fun ((dv1, dv2) : Dval * Dval) ->
                    Interpreter.applyFnVal state 0UL b [] [ dv1; dv2 ])
                  list

              return Dval.optionSome (Dval.list result)
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "zipShortest" 0
      typeParams = []
      parameters =
        [ Param.make "as" (TList varA) ""; Param.make "bs" (TList varB) "" ]
      returnType = TList(TTuple(varA, varB, []))
      description =
        "Returns a list of parallel pairs from <param as> and <param bs>.

        If the lists differ in length, values from the longer list are dropped.

        For example, if <param as> is {{[1,2]}} and <param bs> is
        {{[\"x\",\"y\",\"z\"]}}, returns {{[(1,\"x\"), (2,\"y\")]}}.

        Use <fn List.zip> if you want to enforce equivalent lengths for <param as>
        and <param bs>.

        See <fn List.unzip> if you want to deconstruct the result into <param as>
        and <param bs> again."
      fn =
        (function
        | _, _, [ DList l1; DList l2 ] ->
          // We have to do this munging because OCaml's map2
          // and Fsharp's zip enforces lists of the same length
          let len = min (List.length l1) (List.length l2)
          let l1 = List.take (int len) l1
          let l2 = List.take (int len) l2

          List.zip l1 l2
          |> List.map (fun (val1, val2) -> DTuple(val1, val2, []))
          |> Dval.list
          |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "zip" 0
      typeParams = []
      parameters =
        [ Param.make "as" (TList varA) ""; Param.make "bs" (TList varB) "" ]
      returnType = TypeReference.option (TList(TTuple(varA, varB, [])))
      description =
        "If the lists have the same length, returns {{Some list of tuples}} formed from
        parallel pairs in <param as> and <param bs>.

        For example, if <param as> is {{[1,2,3]}} and <param bs> is
        {{[\"x\",\"y\",\"z\"]}}, returns {{[(1,\"x\"), (2,\"y\"), (3,\"z\")]}}.

        See <fn List.unzip> if you want to deconstruct <var list> into <param as>
        and <param bs> again.

        If the lists differ in length, returns {{None}} (consider
        <fn List.zipShortest> if you want to drop values from the longer list
        instead)."
      fn =
        (function
        | _, _, [ DList l1; DList l2 ] ->
          if List.length l1 <> List.length l2 then
            Ply(Dval.optionNone)
          else
            List.zip l1 l2
            |> List.map (fun (val1, val2) -> DTuple(val1, val2, []))
            |> Dval.list
            |> Dval.optionSome
            |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "unzip" 0
      typeParams = []
      parameters = [ Param.make "pairs" (TList(TTuple(varA, varB, []))) "" ]
      returnType = TTuple(TList varA, TList varB, [])
      description =
        "Given a <param pairs> list where each value is a tuple of two values (such
         lists are constructed by <fn List.zip> and <fn List.zipShortest>), returns
         a tuple of two lists, one with every first value, and one with every second
         value.

         For example, if <fn pairs> is {{[(1,\"x\"), (2,\"y\"), (3,\"z\")]}}, returns
         {{([1,2,3], [\"x\",\"y\",\"z\"])}}."
      fn =
        (function
        | _, _, [ DList l ] ->

          let f (acc1, acc2) i =
            match i with
            | DTuple(a, b, []) -> (a :: acc1, b :: acc2)
            | (DIncomplete _ | DError _) as dv -> Errors.foundFakeDval dv
            | v ->
              let errDetails =
                match v with
                | DTuple(_, _, xs) ->
                  $". It has length {2 + List.length xs} but should have length 2"
                | _ ->
                  $". It is of type {DvalReprDeveloper.dvalTypeName v} instead of `Tuple`"

              Exception.raiseCode (
                Errors.argumentWasnt "a tuple with exactly two values" "pairs" v
                + errDetails
              )

          let result = l |> List.rev |> List.fold f ([], [])

          match result with
          | (l, l2) -> Ply(DTuple(DList l, DList l2, []))
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
        (function
        | _, _, [ DList [] ] -> Ply(Dval.optionNone)
        | _, _, [ DList l ] ->
          // Will return <= (length - 1)
          // Maximum value is Int64.MaxValue which is half of UInt64.MaxValue, but
          // that won't affect this as we won't have a list that big for a long long
          // long time.
          let index = RNG.GetInt32(l.Length)
          (List.tryItem index l) |> Dval.option |> Ply
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Impure
      deprecated = NotDeprecated }


    { name = fn "groupByWithKey" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn([ varA ], varB)) "" [ "item" ] ]
      returnType = TList(TTuple(varB, TList varA, []))
      description =
        "Groups <param list> into tuples (key, elements), where the key is computed by applying
         <param fn> to each element in the list.

         For example, if <param list> is {{[1, 2, 3, 4, 5]}} and <param fn>
         is {{fn item -> Int.mod_v0 item 2}}, returns {{[(1, [1, 3, 5]), (0, [2, 4])]}}.

          Preserves the order of values and of the keys."
      fn =
        (function
        | state, _, [ DList l; DFnVal fn ] ->
          uply {
            let applyFn (dval : Dval) : DvalTask =
              Interpreter.applyFnVal state 0UL fn [] [ dval ]

            // apply the function to each element in the list
            let! result =
              Ply.List.mapSequentially
                (fun dval ->
                  uply {
                    let! key = applyFn dval
                    return (key, dval)
                  })
                l

            let badKey = List.tryFind (fun (k, _) -> Dval.isFake k) result

            match badKey with
            | Some(key, _) -> return key
            | None ->
              let groups =
                result
                |> List.groupBy fst
                |> List.map (fun (key, elementsWithKey) ->
                  let elements = List.map snd elementsWithKey
                  DTuple(key, DList elements, []))
                |> DList

              return groups
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    { name = fn "partition" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn([ varA ], TBool)) "" [ "val" ] ]
      returnType = TTuple((TList varA), (TList varA), [])
      description =
        "Calls <param f> on every <var val> in <param list>, splitting the list into
         two - those values for which {{fn val}} returns {{true}}, and those that
         return {{false}}.

         Preserves the order of values."
      fn =
        (function
        | state, _, [ DList l; DFnVal fn ] ->
          uply {
            let partition l =
              let applyFn dval = Interpreter.applyFnVal state 0UL fn [] [ dval ]

              let rec loop acc l =
                uply {
                  match acc, l with
                  | (a, b), item :: tail ->
                    let! fnResult = applyFn item

                    match fnResult with
                    | DBool true -> return! loop (item :: a, b) tail
                    | DBool false -> return! loop (a, item :: b) tail

                    | (DIncomplete _ | DError _) as dv ->
                      // fake dvals
                      return Error dv

                    | v ->
                      return
                        Exception.raiseCode (Errors.expectedLambdaType "fn" TBool v)
                  | (a, b), [] -> return Ok(List.rev a, List.rev b)
                }

              loop ([], []) l

            match! partition l with
            | Ok(a, b) -> return DTuple(DList a, DList b, [])
            | Error fakeDval -> return fakeDval
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplemented
      previewable = Pure
      deprecated = NotDeprecated }


    // CLEANUP: This can't be moved to packages until the package manager is live and stable.
    // we can't use PACKAGE functions during the "load from disk into DB" flow
    { name = fn "iter" 0
      typeParams = []
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.makeWithArgs "fn" (TFn([ varA ], TUnit)) "" [ "element" ] ]
      returnType = TUnit
      description =
        "Applies the given function <param fn> to each element of the <param list>."
      fn =
        (function
        | state, _, [ DList l; DFnVal b ] ->
          uply {
            do!
              l
              |> Ply.List.iterSequentially (fun e ->
                uply {
                  match! Interpreter.applyFnVal state 0UL b [] [ e ] with
                  | DUnit -> return ()
                  | DError _ as dv -> return Errors.foundFakeDval dv
                  | v ->
                    Exception.raiseCode (Errors.expectedLambdaValue "fn" "unit" v)
                })
            return DUnit
          }
        | _ -> incorrectArgs ())
      sqlSpec = NotQueryable
      previewable = Pure
      deprecated = NotDeprecated }


    ]

let contents = (fns, types, constants)
