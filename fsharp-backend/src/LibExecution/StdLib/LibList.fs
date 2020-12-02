module LibExecution.LibList

open System.Threading.Tasks
open FSharp.Control.Tasks
open LibExecution.Runtime
open FSharpPlus
open Prelude

let fn = FnDesc.stdFnDesc

let varA = TVariable "a"
let varB = TVariable "b"

let fns : List<BuiltInFn> =
  [ { name = fn "List" "singleton" 0
      parameters = [ Param.make "val" (TVariable "a") "" ]
      returnType = TList(TVariable "a")
      description = "Returns a one-element list containing the given `val`."
      fn =
        (function
        | _, [ v ] -> Value(DList [ v ])
        | args -> incorrectArgs ())
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
        | _, [ DList l ] -> List.tryHead l |> Option.defaultValue DNull |> Value
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "" "" 0) }
    { name = fn "List" "head" 1
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TOption varA
      description = "Fetches the head of the list and returns an option"
      fn =
        (function
        | _, [ DList l ] -> Value(DOption(List.tryHead l))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "" "" 0) }
    //   ; { name = fn "List" "head" 2
//
//     ; parameters = [Param.make "list" TList]
//     ; returnType = TOption
//     ; description =
//         "Returns `Just` the head (first value) of a list. Returns `Nothing` if the list is empty."
//     ; fn =
//
//           (function
//           | _, [DList l] ->
//             ( match List.hd l with
//             | Some dv ->
//                 Dval.to_opt_just dv
//             | None ->
//                 DOption OptNothing )
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "tail" 0
//
//     ; parameters = [Param.make "list" TList]
//     ; returnType = TOption
//     ; description =
//         "If the list contains at least one value, returns `Just` a list of every value other than the first. Otherwise, returns `Nothing`."
//     ; fn =
//         (* This matches Elm's implementation, with the added benefit that the error rail
//          * means you don't need to handle unwrapping the option
//          * unless the passed list is truly empty (which shouldn't happen in most practical uses). *)
//
//           (function
//           | _, [DList l] ->
//             ( match List.tl l with
//             | Some dv ->
//                 DList dv |> Dval.to_opt_just
//             | None ->
//                 DOption OptNothing )
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "empty" 0
//
//     ; parameters = []
//     ; returnType = TList
//     ; description = "Returns an empty list."
//     ; fn =  (function _, [] -> DList [] | args -> incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
    { name = fn "List" "push" 0
      parameters = [ Param.make "list" (TList varA) ""; Param.make "val" varA "" ]
      returnType = TList varA
      description = "Add element {{val}} to front of list {{list}}"
      fn =
        // fakeval handled by call *)
        (function
        | _, [ DList l; i ] -> Value(DList(i :: l))
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    // ; { name = fn "List" "pushBack" 0
//     ; parameters = [Param.make "list" TList; Param.make "val" TAny]
//     ; returnType = TList
//     ; description = "Add element `val` to back of list `list`"
//     ; fn =
//
//           (function _, [DList l; i] -> DList (l @ [i]) | args -> incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "last" 0
//
//     ; parameters = [Param.make "list" TList]
//     ; returnType = TAny
//     ; description =
//         "Returns the last value in `list`. Returns null if the list is empty."
//     ; fn =
//
//           (function
//           | _, [DList []] ->
//               DNull
//           | _, [DList l] ->
//               List.last_exn l
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = ReplacedBy(fn "" "" 0) }
//   ; { name = fn "List" "last" 1
//
//     ; parameters = [Param.make "list" TList]
//     ; returnType = TOption
//     ; description =
//         "Returns the last value in `list`, wrapped in an option (`Nothing` if the list is empty)."
//     ; fn =
//
//           (function
//           | _, [DList []] ->
//               DOption OptNothing
//           | _, [DList l] ->
//               DOption (OptJust (List.last_exn l))
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = ReplacedBy(fn "" "" 0) }
//   ; { name = fn "List" "last" 2
//
//     ; parameters = [Param.make "list" TList]
//     ; returnType = TOption
//     ; description =
//         "Returns the last value in `list`, wrapped in an option (`Nothing` if the list is empty)."
//     ; fn =
//
//           (function
//           | _, [DList []] ->
//               DOption OptNothing
//           | _, [DList l] ->
//               Dval.to_opt_just (List.last_exn l)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "reverse" 0
//
//     ; parameters = [Param.make "list" TList]
//     ; returnType = TList
//     ; description = "Returns a reversed copy of `list`."
//     ; fn =
//
//           (function _, [DList l] -> DList (List.rev l) | args -> incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "findFirst" 0
//
//     ; parameters = [Param.make "list" TList; func ["val"]]
//     ; returnType = TAny
//     ; description =
//         "Returns the first value of `list` for which `f val` returns `true`. Returns `Nothing` if no such value exists."
//     ; fn =
//
//           (function
//           | state, [DList l; DLambda b] ->
//               let f (dv : dval) : bool =
//                 DBool true = Ast.execute_dblock ~state b [dv]
//               in
//               (match List.find ~f l with None -> DNull | Some dv -> dv)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = ReplacedBy(fn "" "" 0) }
//   ; { name = fn "List" "findFirst" 1
//
//     ; parameters = [Param.make "list" TList; func ["val"]]
//     ; returnType = TOption
//     ; description =
//         "Returns the first value of `list` for which `f val` returns `true`. Returns `Nothing` if no such value exists."
//     ; fn =
//
//           (function
//           | state, [DList l; DLambda b] ->
//               let f (dv : dval) : bool =
//                 DBool true = Ast.execute_dblock ~state b [dv]
//               in
//               ( match List.find ~f l with
//               | None ->
//                   DOption OptNothing
//               | Some dv ->
//                   DOption (OptJust dv) )
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = ReplacedBy(fn "" "" 0) }
//   ; { name = fn "List" "findFirst" 2
//
//     ; parameters = [Param.make "list" TList; func ["val"]]
//     ; returnType = TOption
//     ; description =
//         "Returns `Just firstMatch` where `firstMatch` is the first value of the list for which `f` returns `true`. Returns `Nothing` if no such value exists."
//     ; fn =
//
//           (function
//           | state, [DList l; DLambda b] ->
//               let f (dv : Types.RuntimeT.dval) : bool =
//                 DBool true = Ast.execute_dblock ~state b [dv]
//               in
//               ( match List.find ~f l with
//               | None ->
//                   DOption OptNothing
//               | Some dv ->
//                   Dval.to_opt_just dv )
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "contains" 0
//
//     ; parameters = [Param.make "list" TList; Param.make "val" TAny]
//     ; returnType = TBool
//     ; description = "Returns `true` if `val` is in the list."
//     ; fn =
//
//           (function
//           | _, [DList l; i] ->
//               DBool (List.mem equal_dval l i)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable =
//         Pure
//         (* Deprecated in favor of List::member for consistency with Elm's naming *)
//     ; deprecated = ReplacedBy(fn "" "" 0) }
//   ; { name = fn "List" "member" 0
//
//     ; parameters = [Param.make "list" TList; Param.make "val" TAny]
//     ; returnType = TBool
//     ; description = "Returns `true` if `val` is in the list."
//     ; fn =
//
//           (function
//           | _, [DList l; i] ->
//               DBool (List.mem equal_dval l i)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "repeat" 0
//
//     ; parameters = [Param.make "times" TInt; Param.make "val" TAny]
//     ; returnType = TList
//     ; description =
//         "Returns a new list containing `val` repeated `times` times."
//     ; fn =
//
//           (function
//           | _, [DInt t; dv] ->
//               DList (List.init (Dint.to_int_exn t) (fun _ -> dv))
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "length" 0
//
//     ; parameters = [Param.make "list" TList]
//     ; returnType = TInt
//     ; description = "Returns the number of values in `list`."
//     ; fn =
//
//           (function
//           | _, [DList l] -> Dval.dint (List.length l) | args -> incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "range" 0
//
//     ; parameters =
//         [ Param.make "lowest" TInt "First, smallest number in the list"
//         ; Param.make "highest" TInt "Last, largest number in the list" ]
//     ; returnType = TList
//     ; description =
//         "Returns a list of numbers where each element is 1 larger than the previous. You provide the `lowest` and `highest` numbers in the list. If `lowest` is greater than `highest`, returns the empty list."
//     ; fn =
//
//           (function
//           | _, [DInt start; DInt stop] ->
//               DList
//                 ( List.range (Dint.to_int_exn start) (Dint.to_int_exn stop + 1)
//                 |> List.map (fun i -> Dval.dint i) )
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "fold" 0
//
//     ; parameters = [Param.make "list" TList; Param.make "init" TAny; func ["accum"; "curr"]]
//     ; returnType = TAny
//     ; description =
//         "Folds `list` into a single value, by repeatedly applying `f` to any two pairs."
//     ; fn =
//
//           (function
//           | state, [DList l; init; DLambda b] ->
//               (* Fake cf should be propagated by the blocks so we dont need to check *)
//               let f (dv1 : dval) (dv2 : dval) : dval =
//                 Ast.execute_dblock ~state b [dv1; dv2]
//               in
//               List.fold ~f ~init l
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "flatten" 0
//
//     ; parameters = [Param.make "list" TList]
//     ; returnType = TList
//     ; description =
//         "Returns a single list containing the values of every list directly in `list` (does not recursively flatten nested lists)."
//     ; fn =
//
//           (function
//           | _, [DList l] ->
//               let f a b =
//                 match (a, b) with
//                 | DList a, DList b ->
//                     DList (List.append a b)
//                 | _ ->
//                     RT.error (DList [a; b]) "Flattening non-lists"
//               in
//               List.fold (DList []) ~f l
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "interpose" 0
//
//     ; parameters = [Param.make "list" TList; Param.make "sep" TAny]
//     ; returnType = TList
//     ; description =
//         "Returns a single list containing the values of `list` separated by `sep`."
//     ; fn =
//
//           (function
//           | _, [DList l; i] ->
//               let rec join ls =
//                 match ls with
//                 | [] ->
//                     []
//                 | h :: t ->
//                   (match t with [] -> [h] | t -> [h] @ [i] @ join t)
//               in
//               DList (join l)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "interleave" 0
//
//     ; parameters = [Param.make "as" TList; Param.make "bs" TList]
//     ; returnType = TList
//     ; description =
//         "Returns a new list with the first value from <param as> then the first value from <param bs>, then the second value from <param as> then the second value from <param bs>, etc, until one list ends, then the remaining items from the other list."
//     ; fn =
//
//           (function
//           | _, [DList l1; DList l2] ->
//               let rec f l1 l2 =
//                 match l1 with
//                 | [] ->
//                     l2
//                 | x :: xs ->
//                   (match l2 with [] -> l1 | y :: ys -> x :: y :: f xs ys)
//               in
//               DList (f l1 l2)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "uniqueBy" 0
//
//     ; parameters = [Param.make "list" TList; func ["val"]]
//     ; returnType = TList
//     ; description =
//         "Returns the passed list, with only unique values, where uniqueness is based on the result of `f`. Only one of each value will be returned, but the order will not be maintained."
//     ; fn =
//
//           (function
//           | state, [DList l; DLambda b] ->
//               let fn dv = Ast.execute_dblock ~state b [dv] in
//               DList
//                 (List.dedup_and_sort l (fun a b ->
//                      compare_dval (fn a) (fn b)))
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "isEmpty" 0
//
//     ; parameters = [Param.make "list" TList]
//     ; returnType = TBool
//     ; description = "Returns true if `list` has no values."
//     ; fn =
//
//           (function
//           | _, [DList l] -> DBool (List.is_empty l) | args -> incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "sort" 0
//
//     ; parameters = [Param.make "list" TList]
//     ; returnType = TList
//     ; description =
//         "Returns a copy of `list` with every value sorted in ascending order. Use this if the values have types Dark knows how to sort.
//          Consider `List::sortBy` or `List::sortByComparator` if you need more control over the sorting process."
//     ; fn =
//
//           (function
//           | _, [DList list] ->
//               list |> List.sort compare_dval |> DList
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "sortBy" 0
//
//     ; parameters = [Param.make "list" TList; func ["val"]]
//     ; returnType = TList
//     ; description =
//         {|Returns a copy of `list`, sorted in ascending order, as if each value evaluated to `f val`.
//           For example, `List::sortBy ["x","jkl","ab"] \val -> String::length val` returns `[ "x", "ab", "jkl" ]`.
//           Consider `List::sort` if the list values can be directly compared, or `List::sortByComparator` if you want more control over the sorting process.|}
//     ; fn =
//
//           (function
//           | state, [DList list; DLambda b] ->
//               let fn dv = Ast.execute_dblock ~state b [dv] in
//               list
//               |> List.sort (fun a b -> compare_dval (fn a) (fn b))
//               |> DList
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "sortByComparator" 0
//
//     ; parameters = [Param.make "list" TList; func ["a"; "b"]]
//     ; returnType = TResult
//     ; description =
//         "Returns a copy of `list`, sorted using `f a b` to compare values `a` and `b`.
//         `f` must return `-1` if `a` should appear before `b`, `1` if `a` should appear after `b`, and `0` if the order of `a` and `b` doesn't matter.
//         Consider `List::sort` or `List::sortBy` if you don't need this level of control."
//     ; fn =
//
//           (function
//           | state, [DList list; DLambda b] ->
//               let fn dv1 dv2 = Ast.execute_dblock ~state b [dv1; dv2] in
//               ( try
//                   list
//                   |> List.sort (fun a b ->
//                          match fn a b with
//                          | DInt i ->
//                              (* to_int_exn is just
//                               * Int63.to_int_exn; from docs
//                               * (https://ocaml.janestreet.com/ocaml-core/latest/doc/base/Base/Int63/index.html),
//                               * "The size of Int63 is always 63 bits. On a 64-bit
//                               * platform it is just an int (63-bits), and on a
//                               * 32-bit platform it is an int64 wrapped to respect
//                               * the semantics of 63-bit integers."
//                               *
//                               * We run these fns in two environments: native
//                               * ocaml, in our containers, which are a 64-bit
//                               * platform, and jsoo, which is a 32-bit platform.
//                               * But you'll only get an _exn there if you manage to
//                               * get a DInt constructed that is more than 32 bits.
//                               *
//                               * Not worrying about that because:
//                               * - you'd have to really try to get here with such a
//                               *   value, I think (constructing a DInt with a
//                               *   >32bit int ...)
//                               * - if you do, it'll only affect the editor
//                               *   runtime, not bwd execution *)
//                              let i = Dint.to_int_exn i in
//                              ( match i with
//                              | 0 | 1 | -1 ->
//                                  i
//                              | _ ->
//                                  Exception.code
//                                    ( "`f` must return one of -1, 0, 1, but returned another int: "
//                                      ^ string_of_int i
//                                    |> String.substr_replace_all
//                                         "\n"
//                                         ~with_:"" ) )
//                          | nonInt ->
//                              Exception.code
//                                ( "`f` must return one of -1, 0, 1, but returned non-int: "
//                                  ^ Dval.to_developer_repr_v0 nonInt
//                                |> String.substr_replace_all
//                                     "\n"
//                                     ~with_:"" ))
//                   |> DList
//                   |> ResOk
//                   |> DResult
//                 with Exception.DarkException e ->
//                   DResult (ResError (Dval.dstr_of_string_exn e.short)) )
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "append" 0
//
//     ; parameters = [Param.make "as" TList; Param.make "bs" TList]
//     ; returnType = TList
//     ; description =
//         "Returns a new list with all values in `as` followed by all values in `bs`, preserving the order."
//     ; fn =
//
//           (function
//           | _, [DList l1; DList l2] ->
//               DList (List.append l1 l2) (* no checking for fake cf required *)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
    { name = fn "List" "filter" 0
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.make
            "fn"
            (TFn([ varA ], TBool))
            "Function to be applied on all list elements; if the result it true then the element is kept" ]
      returnType = TList varA
      description =
        "Return only values in `list` which meet the function's criteria. The function should return true to keep the entry or false to remove it."
      fn =
        (function
        | state, [ DList l; DLambda b ] ->
            taskv {
              let incomplete = ref false

              let f (dv : Dval) : TaskOrValue<bool> =
                taskv {
                  match! Interpreter.eval_lambda state b [ dv ] with
                  | DBool b -> return b
                  | DFakeVal (DIncomplete _) ->
                      incomplete := true
                      return false
                  | v ->
                      raise (RuntimeException(LambdaResultHasWrongType(dv, TBool)))
                      return false
                }

              if !incomplete then
                return DFakeVal(DIncomplete SourceNone)
              else
                let! result = filter_s f l
                return DList(result)
            }
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "" "" 0) }
    //   ; { name = fn "List" "filter" 1
//
//     ; parameters = [Param.make "list" TList; func ["val"]]
//     ; returnType = TList
//     ; description =
//         "Calls `f` on every `val` in `list`, returning a list of only those values for which `f val` returns `true`.
//         Preserves the order of values that were not dropped.
//         Consider `List::filterMap` if you also want to transform the values."
//     ; fn =
//
//           (function
//           | state, [DList l; DLambda b] ->
//               let fakecf = ref None in
//               let f (dv : dval) : bool =
//                 let run = !fakecf = None in
//                 run
//                 &&
//                 match Ast.execute_dblock ~state b [dv] with
//                 | DBool b ->
//                     b
//                 | (DIncomplete _ | DErrorRail _) as dv ->
//                     fakecf := Some dv ;
//                     false
//                 | v ->
//                     RT.error "Expecting fn to return bool" v dv
//               in
//               let result = List.filter ~f l in
//               (match !fakecf with None -> DList result | Some v -> v)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = ReplacedBy(fn "" "" 0) }
//   ; { name = fn "List" "filter" 2
//
//     ; parameters = [Param.make "list" TList; func ["val"]]
//     ; returnType = TList
//     ; description =
//         "Calls `f` on every `val` in `list`, returning a list of only those values for which `f val` returns `true`.
//         Preserves the order of values that were not dropped.
//         Consider `List::filterMap` if you also want to transform the values."
//     ; fn =
//
//           (function
//           | state, [DList l; DLambda b] ->
//               let abortReason = ref None in
//               let f (dv : dval) : bool =
//                 !abortReason = None
//                 &&
//                 match Ast.execute_dblock ~state b [dv] with
//                 | DBool b ->
//                     b
//                 | (DIncomplete _ | DErrorRail _ | DError _) as dv ->
//                     abortReason := Some dv ;
//                     false
//                 | v ->
//                     abortReason :=
//                       Some
//                         (DError
//                            ( SourceNone
//                            , "Expected the argument `f` passed to `"
//                              ^ state.executing_fnname
//                              ^ "` to return `true` or `false` for every value in `list`. However, it returned `"
//                              ^ Dval.to_developer_repr_v0 v
//                              ^ "` for the input `"
//                              ^ Dval.to_developer_repr_v0 dv
//                              ^ "`." )) ;
//                     false
//               in
//               let result = List.filter ~f l in
//               (match !abortReason with None -> DList result | Some v -> v)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "filterMap" 0
//
//     ; parameters = [Param.make "list" TList; func ["val"]]
//     ; returnType = TList
//     ; description =
//         "Calls `f` on every `val` in `list`, returning a new list that drops some values (filter) and transforms others (map).
//         If `f val` returns `Nothing`, drops `val` from the list.
//         If `f val` returns `Just newValue`, replaces `val` with `newValue`.
//         Preserves the order of values that were not dropped.
//         This function combines `List::filter` and `List::map`."
//     ; fn =
//
//           (function
//           | state, [DList l; DLambda b] ->
//               let abortReason = ref None in
//               let f (dv : dval) : dval option =
//                 if !abortReason = None
//                 then (
//                   match Ast.execute_dblock ~state b [dv] with
//                   | DOption (OptJust o) ->
//                       Some o
//                   | DOption OptNothing ->
//                       None
//                   | (DIncomplete _ | DErrorRail _ | DError _) as dv ->
//                       abortReason := Some dv ;
//                       None
//                   | v ->
//                       abortReason :=
//                         Some
//                           (DError
//                              ( SourceNone
//                              , "Expected the argument `f` passed to `"
//                                ^ state.executing_fnname
//                                ^ "` to return `Just` or `Nothing` for every value in `list`. However, it returned `"
//                                ^ Dval.to_developer_repr_v0 v
//                                ^ "` for the input `"
//                                ^ Dval.to_developer_repr_v0 dv
//                                ^ "`." )) ;
//                       None )
//                 else None
//               in
//               let result = List.filter_map ~f l in
//               (match !abortReason with None -> DList result | Some v -> v)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "drop" 0
//
//     ; parameters = [Param.make "list" TList; Param.make "count" TInt]
//     ; returnType = TList
//     ; description = "Drops the first `count` values from `list`."
//     ; fn =
//
//           (function
//           | _, [DList l; DInt c] ->
//               DList (List.drop l (Dint.to_int_exn c))
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "dropWhile" 0
//
//     ; parameters = [Param.make "list" TList; func ["val"]]
//     ; returnType = TList
//     ; description =
//         "Drops the longest prefix of `list` which satisfies the predicate `val`"
//     ; fn =
//
//           (function
//           | state, [DList l; DLambda b] ->
//               let abortReason = ref None in
//               let rec f = function
//                 | [] ->
//                     []
//                 | dv :: dvs ->
//                     if !abortReason = None
//                     then (
//                       match Ast.execute_dblock ~state b [dv] with
//                       | DBool true ->
//                           f dvs
//                       | DBool false ->
//                           dv :: dvs
//                       | (DIncomplete _ | DErrorRail _ | DError _) as dv ->
//                           abortReason := Some dv ;
//                           []
//                       | v ->
//                           abortReason :=
//                             Some
//                               (DError
//                                  ( SourceNone
//                                  , "Expected the argument `f` passed to `"
//                                    ^ state.executing_fnname
//                                    ^ "` to return a boolean value for every value in `list`. However, it returned `"
//                                    ^ Dval.to_developer_repr_v0 v
//                                    ^ "` for the input `"
//                                    ^ Dval.to_developer_repr_v0 dv
//                                    ^ "`." )) ;
//                           [] )
//                     else []
//               in
//               let result = f l in
//               (match !abortReason with None -> DList result | Some v -> v)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "take" 0
//
//     ; parameters = [Param.make "list" TList; Param.make "count" TInt]
//     ; returnType = TList
//     ; description = "Drops all but the first `count` values from `list`."
//     ; fn =
//
//           (function
//           | _, [DList l; DInt c] ->
//               DList (List.take l (Dint.to_int_exn c))
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "takeWhile" 0
//
//     ; parameters = [Param.make "list" TList; func ["val"]]
//     ; returnType = TList
//     ; description =
//         "Return the longest prefix of `list` which satisfies the predicate `val`"
//     ; fn =
//
//           (function
//           | state, [DList l; DLambda b] ->
//               let abortReason = ref None in
//               let rec f = function
//                 | [] ->
//                     []
//                 | dv :: dvs ->
//                     if !abortReason = None
//                     then (
//                       match Ast.execute_dblock ~state b [dv] with
//                       | DBool true ->
//                           dv :: f dvs
//                       | DBool false ->
//                           []
//                       | (DIncomplete _ | DErrorRail _ | DError _) as dv ->
//                           abortReason := Some dv ;
//                           []
//                       | v ->
//                           abortReason :=
//                             Some
//                               (DError
//                                  ( SourceNone
//                                  , "Expected the argument `f` passed to `"
//                                    ^ state.executing_fnname
//                                    ^ "` to return a boolean value for every value in `list`. However, it returned `"
//                                    ^ Dval.to_developer_repr_v0 v
//                                    ^ "` for the input `"
//                                    ^ Dval.to_developer_repr_v0 dv
//                                    ^ "`." )) ;
//                           [] )
//                     else []
//               in
//               let result = f l in
//               (match !abortReason with None -> DList result | Some v -> v)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
    { name = fn "List" "foreach" 0
      parameters =
        [ Param.make "list" (TList varA) ""
          Param.make "fn" (TFn([ varA ], varB)) "" ]
      returnType = TList varB
      description =
        "Call `f` on every `val` in the list, returning a list of the results of those calls"
      fn =
        (function
        | state, [ DList l; DLambda b ] ->
            taskv {
              let! result =
                map_s (fun dv -> Interpreter.eval_lambda state b [ dv ]) l

              return Dval.list result
            }
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = ReplacedBy(fn "List" "map" 0) }
    { name = FnDesc.stdFnDesc "List" "map" 0
      parameters =
        [ Param.make "list" (TList varA) "The list to be operated on"
          Param.make
            "fn"
            (TFn([ varA ], varB))
            "Function to be called on each member" ]
      description =
        "Returns a list created by the elements of `list` with `fn` called on each of them in order"
      returnType = TList varB
      fn =
        (function
        | state, [ DList l; DLambda b ] ->
            taskv {
              let! result =
                map_s (fun dv -> Interpreter.eval_lambda state b [ dv ]) l

              return Dval.list result
            }
        | _ -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Pure
      deprecated = NotDeprecated }
    //   ; { name = fn "List" "indexedMap" 0
//
//     ; parameters = [Param.make "list" TList; func ["index"; "val"]]
//     ; returnType = TList
//     ; description =
//         "Calls `f` on every `val` and its `index` in `list`, returning a list of the results of those calls.
//         Consider `List::map` if you don't need the index."
//     ; fn =
//
//           (function
//           | state, [DList l; DLambda b] ->
//               let f (idx : int) (dv : dval) : dval =
//                 Ast.execute_dblock ~state b [Dval.dint idx; dv]
//               in
//               Dval.to_list (List.mapi ~f l)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "map2shortest" 0
//
//     ; parameters = [Param.make "as" TList; Param.make "bs" TList; func ["a"; "b"]]
//     ; returnType = TList
//     ; description =
//         {|Maps `f` over `as` and `bs` in parallel, calling `f a b` on every pair of values from `as` and `bs`.
//         If the lists differ in length, values from the longer list are dropped.
//         For example, if `as` is `[1,2]` and `bs` is `["x","y","z"]`, returns `[(f 1 "x"), (f 2 "y")]`.
//         Use `List::map2` if you want to enforce equivalent lengths for `as` and `bs`.|}
//     ; fn =
//
//           (function
//           | state, [DList l1; DList l2; DLambda b] ->
//               (* We have to do this munging because OCaml's map2 enforces lists of the same length *)
//               let len = min (List.length l1) (List.length l2) in
//               let l1 = List.take l1 len in
//               let l2 = List.take l2 len in
//               let f (l1Item : dval) (l2Item : dval) : dval =
//                 Ast.execute_dblock ~state b [l1Item; l2Item]
//               in
//               Dval.to_list (List.map2_exn ~f l1 l2)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "map2" 0
//
//     ; parameters = [Param.make "as" TList; Param.make "bs" TList; func ["a"; "b"]]
//     ; returnType = TOption
//     ; description =
//         {|If the lists are the same length, returns `Just list` formed by mapping `f` over `as` and `bs` in parallel,
//          calling `f a b` on every pair of values from `as` and `bs`.
//          For example, if `as` is `[1,2,3]` and `bs` is `["x","y","z"]`, returns `[(f 1 "x"), (f 2 "y"), (f 3 "z")]`.
//          If the lists differ in length, returns `Nothing` (consider `List::map2shortest` if you want to drop values from the longer list instead).|}
//     ; fn =
//
//           (function
//           | state, [DList l1; DList l2; DLambda b] ->
//               let f (l1Item : dval) (l2Item : dval) : dval =
//                 Ast.execute_dblock ~state b [l1Item; l2Item]
//               in
//               DOption
//                 ( match List.map2 ~f l1 l2 with
//                 | Ok res ->
//                     OptJust (Dval.to_list res)
//                 | Unequal_lengths ->
//                     OptNothing )
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "zipShortest" 0
//
//     ; parameters = [Param.make "as" TList; Param.make "bs" TList]
//     ; returnType = TList
//     ; description =
//         {|Returns a list of parallel pairs from `as` and `bs`.
//         If the lists differ in length, values from the longer list are dropped.
//         For example, if `as` is `[1,2]` and `bs` is `["x","y","z"]`, returns `[[1,"x"], [2,"y"]]`.
//         Use `List::zip` if you want to enforce equivalent lengths for `as` and `bs`.
//         See `List::unzip` if you want to deconstruct the result into `as` and `bs` again.|}
//     ; fn =
//
//           (function
//           | state, [DList l1; DList l2] ->
//               (* We have to do this munging because OCaml's map2 enforces lists of the same length *)
//               let len = min (List.length l1) (List.length l2) in
//               let l1 = List.take l1 len in
//               let l2 = List.take l2 len in
//               let f (l1Item : dval) (l2Item : dval) : dval =
//                 Dval.to_list [l1Item; l2Item]
//               in
//               Dval.to_list (List.map2_exn ~f l1 l2)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "zip" 0
//
//     ; parameters = [Param.make "as" TList; Param.make "bs" TList]
//     ; returnType = TOption
//     ; description =
//         {|If the lists have the same length, returns `Just list` formed from parallel pairs in `as` and `bs`.
//         For example, if `as` is `[1,2,3]` and `bs` is `["x","y","z"]`, returns `[[1,"x"], [2,"y"], [3,"z"]]`.
//         See `List::unzip` if you want to deconstruct `list` into `as` and `bs` again.
//         If the lists differ in length, returns `Nothing` (consider `List::zipShortest` if you want to drop values from the longer list instead).|}
//     ; fn =
//
//           (function
//           | state, [DList l1; DList l2] ->
//               let f (l1Item : dval) (l2Item : dval) : dval =
//                 Dval.to_list [l1Item; l2Item]
//               in
//               DOption
//                 ( match List.map2 ~f l1 l2 with
//                 | Ok res ->
//                     OptJust (Dval.to_list res)
//                 | Unequal_lengths ->
//                     OptNothing )
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "unzip" 0
//
//     ; parameters = [Param.make "pairs" TList]
//     ; returnType = TList
//     ; description =
//         {|Given a `pairs` list where each value is a list of two values (such lists are constructed by `List::zip` and `List::zipShortest`), returns a list of two lists,
//         one with every first value, and one with every second value. For example, if `pairs` is `[[1,"x"], [2,"y"], [3,"z"]]`, returns `[[1,2,3], ["x","y","z"]]`.|}
//     ; fn =
//         (* We should deprecate this once we have tuples and homogenous lists *)
//
//           (function
//           | state, [DList l] ->
//               let idx_from_rev_idx (rev_idx : int) (l : 'a list) : int =
//                 List.length l - 1 - rev_idx
//               in
//               let fold_fn
//                   (rev_idx : int)
//                   (acc : (dval list * dval list, dval (* type error *)) result)
//                   (dv : dval) :
//                   (dval list * dval list, dval (* type error *)) result =
//                 Result.bind acc (fun (acc_a, acc_b) ->
//                     match dv with
//                     | DList [a; b] ->
//                         Ok (a :: acc_a, b :: acc_b)
//                     | (DIncomplete _ | DErrorRail _ | DError _) as dv ->
//                         Error dv
//                     | v ->
//                         let err_details =
//                           match v with
//                           | DList l ->
//                               Printf.sprintf
//                                 "It has length %i but must have length 2."
//                                 (List.length l)
//                           | non_list ->
//                               let tipe =
//                                 non_list
//                                 |> Dval.tipe_of
//                                 |> Dval.tipe_to_developer_repr_v0
//                               in
//                               Printf.sprintf
//                                 "It is of type `%s` instead of `List`."
//                                 tipe
//                         in
//                         Error
//                           (DError
//                              ( SourceNone
//                              , Printf.sprintf
//                                  "Expected every value within the `pairs` argument passed to `%s` to be a list with exactly two values. However, that is not the case for the value at index %i: %s. %s"
//                                  state.executing_fnname
//                                  (idx_from_rev_idx rev_idx l)
//                                  (Dval.to_developer_repr_v0 v)
//                                  err_details )))
//               in
//               let result =
//                 (* We reverse here so that the [foldi] consing happens in the correct order.
//                 * It does mean that the index passed by [foldi] counts from the end *)
//                 l |> List.rev |> List.foldi (Ok ([], [])) fold_fn
//               in
//               ( match result with
//               | Ok (res_a, res_b) ->
//                   DList [DList res_a; DList res_b]
//               | Error v ->
//                   v )
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
//   ; { name = fn "List" "getAt" 0
//
//     ; parameters = [Param.make "list" TList; Param.make "index" TInt]
//     ; returnType = TOption
//     ; description =
//         "Returns `Just value` at `index` in `list` if `index` is less than the length of the list. Otherwise returns `Nothing`."
//     ; fn =
//
//           (function
//           | _, [DList l; DInt index] ->
//               List.nth l (Dint.to_int_exn index)
//               |> Option.map (fun a -> DOption (OptJust a))
//               |> Option.value (DOption OptNothing)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = ReplacedBy(fn "" "" 0) }
//   ; { name = fn "List" "getAt" 1
//
//     ; parameters = [Param.make "list" TList; Param.make "index" TInt]
//     ; returnType = TOption
//     ; description =
//         "Returns `Just value` at `index` in `list` if `index` is less than the length of the list otherwise returns `Nothing`."
//     ; fn =
//
//           (function
//           | _, [DList l; DInt index] ->
//               List.nth l (Dint.to_int_exn index)
//               |> Option.map (fun a -> Dval.to_opt_just a)
//               |> Option.value (DOption OptNothing)
//           | args ->
//               incorrectArgs ())
//     ; sqlSpec = NotYetImplementedTODO
//       ; previewable = Pure
//     ; deprecated = NotDeprecated }
    { name = fn "List" "randomElement" 0
      parameters = [ Param.make "list" (TList varA) "" ]
      returnType = TOption varA
      description =
        "Returns {{Just <var randomValue>}}, where <var randomValue> is a randomly selected value in <param list>. Returns {{Nothing}} if <param list> is empty."
      fn =

        (function
        | _, [ DList [] ] -> Value(DOption None)
        | _, [ DList l ] -> Value(l.[Runtime.random.Next l.Length])
        | args -> incorrectArgs ())
      sqlSpec = NotYetImplementedTODO
      previewable = Impure
      deprecated = NotDeprecated } ]
