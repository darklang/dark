open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let list_repeat = Stdlib_util.list_repeat

let fns : Types.fluid_expr fn list =
  [ { prefix_names = ["List::singleton"]
    ; infix_names = []
    ; parameters = [par "val" TAny]
    ; return_type = TList
    ; description = "Returns a one-element list containing the given `val`."
    ; func = InProcess (function _, [v] -> DList [v] | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::head"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TAny
    ; description =
        "Returns the head of a list. Returns null if the empty list is passed."
    ; func =
        InProcess
          (function
          | _, [DList l] ->
            (match List.hd l with Some dv -> dv | None -> DNull)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["List::head_v1"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TOption
    ; description = "Fetches the head of the list and returns an option"
    ; func =
        InProcess
          (function
          | _, [DList l] ->
            ( match List.hd l with
            | Some dv ->
                DOption (OptJust dv)
            | None ->
                DOption OptNothing )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["List::head_v2"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TOption
    ; description =
        "Returns `Just` the head (first value) of a list. Returns `Nothing` if the list is empty."
    ; func =
        InProcess
          (function
          | _, [DList l] ->
            ( match List.hd l with
            | Some dv ->
                Dval.to_opt_just dv
            | None ->
                DOption OptNothing )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::tail"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TOption
    ; description =
        "If the list contains at least one value, returns `Just` a list of every value other than the first. Otherwise, returns `Nothing`."
    ; func =
        (* This matches Elm's implementation, with the added benefit that the error rail
         * means you don't need to handle unwrapping the option
         * unless the passed list is truly empty (which shouldn't happen in most practical uses). *)
        InProcess
          (function
          | _, [DList l] ->
            ( match List.tl l with
            | Some dv ->
                DList dv |> Dval.to_opt_just
            | None ->
                DOption OptNothing )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::empty"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TList
    ; description = "Returns an empty list."
    ; func = InProcess (function _, [] -> DList [] | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::push"]
    ; infix_names = []
    ; parameters = [par "list" TList; par "val" TAny]
    ; return_type = TList
    ; description = "Add element `val` to front of list `list`"
    ; func =
        InProcess
          (* fake cf handled by call *)
          (function
          | _, [DList l; i] -> DList (i :: l) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::pushBack"]
    ; infix_names = []
    ; parameters = [par "list" TList; par "val" TAny]
    ; return_type = TList
    ; description = "Add element `val` to back of list `list`"
    ; func =
        InProcess
          (function _, [DList l; i] -> DList (l @ [i]) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::last"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TAny
    ; description =
        "Returns the last value in `list`. Returns null if the list is empty."
    ; func =
        InProcess
          (function
          | _, [DList []] ->
              DNull
          | _, [DList l] ->
              List.last_exn l
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["List::last_v1"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TOption
    ; description =
        "Returns the last value in `list`, wrapped in an option (`Nothing` if the list is empty)."
    ; func =
        InProcess
          (function
          | _, [DList []] ->
              DOption OptNothing
          | _, [DList l] ->
              DOption (OptJust (List.last_exn l))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["List::last_v2"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TOption
    ; description =
        "Returns the last value in `list`, wrapped in an option (`Nothing` if the list is empty)."
    ; func =
        InProcess
          (function
          | _, [DList []] ->
              DOption OptNothing
          | _, [DList l] ->
              Dval.to_opt_just (List.last_exn l)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::reverse"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TList
    ; description = "Returns a reversed copy of `list`."
    ; func =
        InProcess
          (function _, [DList l] -> DList (List.rev l) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::findFirst"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["val"]]
    ; return_type = TAny
    ; description =
        "Returns the first value of `list` for which `f val` returns `true`. Returns `Nothing` if no such value exists."
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let f (dv : 'expr_type dval) : bool =
                DBool true = Ast.execute_dblock ~state b [dv]
              in
              (match List.find ~f l with None -> DNull | Some dv -> dv)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["List::findFirst_v1"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["val"]]
    ; return_type = TOption
    ; description =
        "Returns the first value of `list` for which `f val` returns `true`. Returns `Nothing` if no such value exists."
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let f (dv : 'expr_type dval) : bool =
                DBool true = Ast.execute_dblock ~state b [dv]
              in
              ( match List.find ~f l with
              | None ->
                  DOption OptNothing
              | Some dv ->
                  DOption (OptJust dv) )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["List::findFirst_v2"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["val"]]
    ; return_type = TOption
    ; description =
        "Returns `Just firstMatch` where `firstMatch` is the first value of the list for which `f` returns `true`. Returns `Nothing` if no such value exists."
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let f (dv : Types.fluid_expr dval) : bool =
                DBool true = Ast.execute_dblock ~state b [dv]
              in
              ( match List.find ~f l with
              | None ->
                  DOption OptNothing
              | Some dv ->
                  Dval.to_opt_just dv )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::contains"]
    ; infix_names = []
    ; parameters = [par "list" TList; par "val" TAny]
    ; return_type = TBool
    ; description = "Returns `true` if `val` is in the list."
    ; func =
        InProcess
          (function
          | _, [DList l; i] ->
              DBool (List.mem ~equal:(equal_dval Types.equal_fluid_expr) l i)
          | args ->
              fail args)
    ; preview_safety =
        Safe
        (* Deprecated in favor of List::member for consistency with Elm's naming *)
    ; deprecated = true }
  ; { prefix_names = ["List::member"]
    ; infix_names = []
    ; parameters = [par "list" TList; par "val" TAny]
    ; return_type = TBool
    ; description = "Returns `true` if `val` is in the list."
    ; func =
        InProcess
          (function
          | _, [DList l; i] ->
              DBool (List.mem ~equal:(equal_dval Types.equal_fluid_expr) l i)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::repeat"]
    ; infix_names = []
    ; parameters = [par "times" TInt; par "val" TAny]
    ; return_type = TList
    ; description =
        "Returns a new list containing `val` repeated `times` times."
    ; func =
        InProcess
          (function
          | _, [DInt t; dv] ->
              DList (List.init (Dint.to_int_exn t) ~f:(fun _ -> dv))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::length"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TInt
    ; description = "Returns the number of values in `list`."
    ; func =
        InProcess
          (function
          | _, [DList l] -> Dval.dint (List.length l) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::range"]
    ; infix_names = []
    ; parameters =
        [ par "lowest" TInt ~d:"First, smallest number in the list"
        ; par "highest" TInt ~d:"Last, largest number in the list" ]
    ; return_type = TList
    ; description =
        "Returns a list of numbers where each element is 1 larger than the previous. You provide the `lowest` and `highest` numbers in the list. If `lowest` is greater than `highest`, returns the empty list."
    ; func =
        InProcess
          (function
          | _, [DInt start; DInt stop] ->
              DList
                ( List.range (Dint.to_int_exn start) (Dint.to_int_exn stop + 1)
                |> List.map ~f:(fun i -> Dval.dint i) )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::fold"]
    ; infix_names = []
    ; parameters = [par "list" TList; par "init" TAny; func ["accum"; "curr"]]
    ; return_type = TAny
    ; description =
        "Folds `list` into a single value, by repeatedly applying `f` to any two pairs."
    ; func =
        InProcess
          (function
          | state, [DList l; init; DBlock b] ->
              (* Fake cf should be propagated by the blocks so we dont need to check *)
              let f (dv1 : 'expr_type dval) (dv2 : 'expr_type dval) :
                  'expr_type dval =
                Ast.execute_dblock ~state b [dv1; dv2]
              in
              List.fold ~f ~init l
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::flatten"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TList
    ; description =
        "Returns a single list containing the values of every list directly in `list` (does not recursively flatten nested lists)."
    ; func =
        InProcess
          (function
          | _, [DList l] ->
              let f a b =
                match (a, b) with
                | DList a, DList b ->
                    DList (List.append a b)
                | _ ->
                    RT.error ~actual:(DList [a; b]) "Flattening non-lists"
              in
              List.fold ~init:(DList []) ~f l
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::uniqueBy"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["val"]]
    ; return_type = TList
    ; description =
        "Returns the passed list, with only unique values, where uniqueness is based on the result of `f`. Only one of each value will be returned, but the order will not be maintained."
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let fn dv = Ast.execute_dblock ~state b [dv] in
              DList
                (List.dedup_and_sort l ~compare:(fun a b ->
                     compare_dval Types.compare_fluid_expr (fn a) (fn b)))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::isEmpty"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TBool
    ; description = "Returns true if `list` has no values."
    ; func =
        InProcess
          (function
          | _, [DList l] -> DBool (List.is_empty l) | args -> fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::sort"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TList
    ; description =
        "Returns a copy of `list` with every value sorted in ascending order. Use this if the values have types Dark knows how to sort.
         Consider `List::sortBy` or `List::sortByComparator` if you need more control over the sorting process."
    ; func =
        InProcess
          (function
          | _, [DList list] ->
              list
              |> List.sort ~compare:(compare_dval Types.compare_fluid_expr)
              |> DList
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::sortBy"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["val"]]
    ; return_type = TList
    ; description =
        {|Returns a copy of `list`, sorted in ascending order, as if each value evaluated to `f val`.
          For example, `List::sortBy ["x","jkl","ab"] \val -> String::length val` returns `[ "x", "ab", "jkl" ]`.
          Consider `List::sort` if the list values can be directly compared, or `List::sortByComparator` if you want more control over the sorting process.|}
    ; func =
        InProcess
          (function
          | state, [DList list; DBlock b] ->
              let fn dv = Ast.execute_dblock ~state b [dv] in
              list
              |> List.sort ~compare:(fun a b ->
                     compare_dval Types.compare_fluid_expr (fn a) (fn b))
              |> DList
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::sortByComparator"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["a"; "b"]]
    ; return_type = TResult
    ; description =
        "Returns a copy of `list`, sorted using `f a b` to compare values `a` and `b`.
        `f` must return `-1` if `a` should appear before `b`, `1` if `a` should appear after `b`, and `0` if the order of `a` and `b` doesn't matter.
        Consider `List::sort` or `List::sortBy` if you don't need this level of control."
    ; func =
        InProcess
          (function
          | state, [DList list; DBlock b] ->
              let fn dv1 dv2 = Ast.execute_dblock ~state b [dv1; dv2] in
              ( try
                  list
                  |> List.sort ~compare:(fun a b ->
                         match fn a b with
                         | DInt i ->
                             (* to_int_exn is just
                              * Int63.to_int_exn; from docs
                              * (https://ocaml.janestreet.com/ocaml-core/latest/doc/base/Base/Int63/index.html),
                              * "The size of Int63 is always 63 bits. On a 64-bit
                              * platform it is just an int (63-bits), and on a
                              * 32-bit platform it is an int64 wrapped to respect
                              * the semantics of 63-bit integers."
                              *
                              * We run these fns in two environments: native
                              * ocaml, in our containers, which are a 64-bit
                              * platform, and jsoo, which is a 32-bit platform.
                              * But you'll only get an _exn there if you manage to
                              * get a DInt constructed that is more than 32 bits.
                              *
                              * Not worrying about that because:
                              * - you'd have to really try to get here with such a
                              *   value, I think (constructing a DInt with a
                              *   >32bit int ...)
                              * - if you do, it'll only affect the editor
                              *   runtime, not bwd execution *)
                             let i = Dint.to_int_exn i in
                             ( match i with
                             | 0 | 1 | -1 ->
                                 i
                             | _ ->
                                 Exception.code
                                   ( "`f` must return one of -1, 0, 1, but returned another int: "
                                     ^ string_of_int i
                                   |> String.substr_replace_all
                                        ~pattern:"\n"
                                        ~with_:"" ) )
                         | nonInt ->
                             Exception.code
                               ( "`f` must return one of -1, 0, 1, but returned non-int: "
                                 ^ Dval.to_developer_repr_v0 nonInt
                               |> String.substr_replace_all
                                    ~pattern:"\n"
                                    ~with_:"" ))
                  |> DList
                  |> ResOk
                  |> DResult
                with Exception.DarkException e ->
                  DResult (ResError (Dval.dstr_of_string_exn e.short)) )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::append"]
    ; infix_names = []
    ; parameters = [par "as" TList; par "bs" TList]
    ; return_type = TList
    ; description =
        "Returns a new list with all values in `as` followed by all values in `bs`, preserving the order."
    ; func =
        InProcess
          (function
          | _, [DList l1; DList l2] ->
              DList (List.append l1 l2) (* no checking for fake cf required *)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::filter"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["val"]]
    ; return_type = TList
    ; description =
        "Return only values in `list` which meet the function's criteria. The function should return true to keep the entry or false to remove it."
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let incomplete = ref false in
              let f (dv : 'expr_type dval) : bool =
                match Ast.execute_dblock ~state b [dv] with
                | DBool b ->
                    b
                | DIncomplete _ ->
                    incomplete := true ;
                    false
                | v ->
                    RT.error "Expecting fn to return bool" ~result:v ~actual:dv
              in
              if !incomplete
              then DIncomplete SourceNone
              else DList (List.filter ~f l)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["List::filter_v1"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["val"]]
    ; return_type = TList
    ; description =
        "Calls `f` on every `val` in `list`, returning a list of only those values for which `f val` returns `true`.
        Preserves the order of values that were not dropped.
        Consider `List::filterMap` if you also want to transform the values."
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let fakecf = ref None in
              let f (dv : 'expr_type dval) : bool =
                let run = !fakecf = None in
                run
                &&
                match Ast.execute_dblock ~state b [dv] with
                | DBool b ->
                    b
                | (DIncomplete _ | DErrorRail _) as dv ->
                    fakecf := Some dv ;
                    false
                | v ->
                    RT.error "Expecting fn to return bool" ~result:v ~actual:dv
              in
              let result = List.filter ~f l in
              (match !fakecf with None -> DList result | Some v -> v)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["List::filter_v2"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["val"]]
    ; return_type = TList
    ; description =
        "Calls `f` on every `val` in `list`, returning a list of only those values for which `f val` returns `true`.
        Preserves the order of values that were not dropped.
        Consider `List::filterMap` if you also want to transform the values."
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let abortReason = ref None in
              let f (dv : 'expr_type dval) : bool =
                !abortReason = None
                &&
                match Ast.execute_dblock ~state b [dv] with
                | DBool b ->
                    b
                | (DIncomplete _ | DErrorRail _ | DError _) as dv ->
                    abortReason := Some dv ;
                    false
                | v ->
                    abortReason :=
                      Some
                        (DError
                           ( SourceNone
                           , "Expected the argument `f` passed to `"
                             ^ state.executing_fnname
                             ^ "` to return `true` or `false` for every value in `list`. However, it returned `"
                             ^ Dval.to_developer_repr_v0 v
                             ^ "` for the input `"
                             ^ Dval.to_developer_repr_v0 dv
                             ^ "`." )) ;
                    false
              in
              let result = List.filter ~f l in
              (match !abortReason with None -> DList result | Some v -> v)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::filterMap"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["val"]]
    ; return_type = TList
    ; description =
        "Calls `f` on every `val` in `list`, returning a new list that drops some values (filter) and transforms others (map).
        If `f val` returns `Nothing`, drops `val` from the list.
        If `f val` returns `Just newValue`, replaces `val` with `newValue`.
        Preserves the order of values that were not dropped.
        This function combines `List::filter` and `List::map`."
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let abortReason = ref None in
              let f (dv : 'expr_type dval) : 'expr_type dval option =
                if !abortReason = None
                then (
                  match Ast.execute_dblock ~state b [dv] with
                  | DOption (OptJust o) ->
                      Some o
                  | DOption OptNothing ->
                      None
                  | (DIncomplete _ | DErrorRail _ | DError _) as dv ->
                      abortReason := Some dv ;
                      None
                  | v ->
                      abortReason :=
                        Some
                          (DError
                             ( SourceNone
                             , "Expected the argument `f` passed to `"
                               ^ state.executing_fnname
                               ^ "` to return `Just` or `Nothing` for every value in `list`. However, it returned `"
                               ^ Dval.to_developer_repr_v0 v
                               ^ "` for the input `"
                               ^ Dval.to_developer_repr_v0 dv
                               ^ "`." )) ;
                      None )
                else None
              in
              let result = List.filter_map ~f l in
              (match !abortReason with None -> DList result | Some v -> v)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::drop"]
    ; infix_names = []
    ; parameters = [par "list" TList; par "count" TInt]
    ; return_type = TList
    ; description = "Drops the first `count` values from `list`."
    ; func =
        InProcess
          (function
          | _, [DList l; DInt c] ->
              DList (List.drop l (Dint.to_int_exn c))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::dropWhile"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["val"]]
    ; return_type = TList
    ; description =
        "Drops the longest prefix of `list` which satisfies the predicate `val`"
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let abortReason = ref None in
              let rec f = function
                | [] ->
                    []
                | dv :: dvs ->
                    if !abortReason = None
                    then (
                      match Ast.execute_dblock ~state b [dv] with
                      | DBool true ->
                          f dvs
                      | DBool false ->
                          dv :: dvs
                      | (DIncomplete _ | DErrorRail _ | DError _) as dv ->
                          abortReason := Some dv ;
                          []
                      | v ->
                          abortReason :=
                            Some
                              (DError
                                 ( SourceNone
                                 , "Expected the argument `f` passed to `"
                                   ^ state.executing_fnname
                                   ^ "` to return a boolean value for every value in `list`. However, it returned `"
                                   ^ Dval.to_developer_repr_v0 v
                                   ^ "` for the input `"
                                   ^ Dval.to_developer_repr_v0 dv
                                   ^ "`." )) ;
                          [] )
                    else []
              in
              let result = f l in
              (match !abortReason with None -> DList result | Some v -> v)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::take"]
    ; infix_names = []
    ; parameters = [par "list" TList; par "count" TInt]
    ; return_type = TList
    ; description = "Drops all but the first `count` values from `list`."
    ; func =
        InProcess
          (function
          | _, [DList l; DInt c] ->
              DList (List.take l (Dint.to_int_exn c))
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::takeWhile"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["val"]]
    ; return_type = TList
    ; description =
        "Return the longest prefix of `list` which satisfies the predicate `val`"
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let abortReason = ref None in
              let rec f = function
                | [] ->
                    []
                | dv :: dvs ->
                    if !abortReason = None
                    then (
                      match Ast.execute_dblock ~state b [dv] with
                      | DBool true ->
                          dv :: f dvs
                      | DBool false ->
                          []
                      | (DIncomplete _ | DErrorRail _ | DError _) as dv ->
                          abortReason := Some dv ;
                          []
                      | v ->
                          abortReason :=
                            Some
                              (DError
                                 ( SourceNone
                                 , "Expected the argument `f` passed to `"
                                   ^ state.executing_fnname
                                   ^ "` to return a boolean value for every value in `list`. However, it returned `"
                                   ^ Dval.to_developer_repr_v0 v
                                   ^ "` for the input `"
                                   ^ Dval.to_developer_repr_v0 dv
                                   ^ "`." )) ;
                          [] )
                    else []
              in
              let result = f l in
              (match !abortReason with None -> DList result | Some v -> v)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::foreach"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["val"]]
    ; return_type = TList
    ; description =
        "Call `f` on every `val` in the list, returning a list of the results of
  those calls"
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let f (dv : 'expr_type dval) : 'expr_type dval =
                Ast.execute_dblock ~state b [dv]
              in
              DList (List.map ~f l)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["List::map"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["val"]]
    ; return_type = TList
    ; description =
        "Calls `f` on every `val` in `list`, returning a list of the results of those calls.
        Consider `List::filterMap` if you also want to drop some of the values."
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let f (dv : 'expr_type dval) : 'expr_type dval =
                Ast.execute_dblock ~state b [dv]
              in
              Dval.to_list (List.map ~f l)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::indexedMap"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["index"; "val"]]
    ; return_type = TList
    ; description =
        "Calls `f` on every `val` and its `index` in `list`, returning a list of the results of those calls.
        Consider `List::map` if you don't need the index."
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let f (idx : int) (dv : 'expr_type dval) : 'expr_type dval =
                Ast.execute_dblock ~state b [Dval.dint idx; dv]
              in
              Dval.to_list (List.mapi ~f l)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::map2shortest"]
    ; infix_names = []
    ; parameters = [par "as" TList; par "bs" TList; func ["a"; "b"]]
    ; return_type = TList
    ; description =
        {|Maps `f` over `as` and `bs` in parallel, calling `f a b` on every pair of values from `as` and `bs`.
        If the lists differ in length, values from the longer list are dropped.
        For example, if `as` is `[1,2]` and `bs` is `["x","y","z"]`, returns `[(f 1 "x"), (f 2 "y")]`.
        Use `List::map2` if you want to enforce equivalent lengths for `as` and `bs`.|}
    ; func =
        InProcess
          (function
          | state, [DList l1; DList l2; DBlock b] ->
              (* We have to do this munging because OCaml's map2 enforces lists of the same length *)
              let len = min (List.length l1) (List.length l2) in
              let l1 = List.take l1 len in
              let l2 = List.take l2 len in
              let f (l1Item : 'expr_type dval) (l2Item : 'expr_type dval) :
                  'expr_type dval =
                Ast.execute_dblock ~state b [l1Item; l2Item]
              in
              Dval.to_list (List.map2_exn ~f l1 l2)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::map2"]
    ; infix_names = []
    ; parameters = [par "as" TList; par "bs" TList; func ["a"; "b"]]
    ; return_type = TOption
    ; description =
        {|If the lists are the same length, returns `Just list` formed by mapping `f` over `as` and `bs` in parallel,
         calling `f a b` on every pair of values from `as` and `bs`.
         For example, if `as` is `[1,2,3]` and `bs` is `["x","y","z"]`, returns `[(f 1 "x"), (f 2 "y"), (f 3 "z")]`.
         If the lists differ in length, returns `Nothing` (consider `List::map2shortest` if you want to drop values from the longer list instead).|}
    ; func =
        InProcess
          (function
          | state, [DList l1; DList l2; DBlock b] ->
              let f (l1Item : 'expr_type dval) (l2Item : 'expr_type dval) :
                  'expr_type dval =
                Ast.execute_dblock ~state b [l1Item; l2Item]
              in
              DOption
                ( match List.map2 ~f l1 l2 with
                | Ok res ->
                    OptJust (Dval.to_list res)
                | Unequal_lengths ->
                    OptNothing )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::zipShortest"]
    ; infix_names = []
    ; parameters = [par "as" TList; par "bs" TList]
    ; return_type = TList
    ; description =
        {|Returns a list of parallel pairs from `as` and `bs`.
        If the lists differ in length, values from the longer list are dropped.
        For example, if `as` is `[1,2]` and `bs` is `["x","y","z"]`, returns `[[1,"x"], [2,"y"]]`.
        Use `List::zip` if you want to enforce equivalent lengths for `as` and `bs`.
        See `List::unzip` if you want to deconstruct the result into `as` and `bs` again.|}
    ; func =
        InProcess
          (function
          | state, [DList l1; DList l2] ->
              (* We have to do this munging because OCaml's map2 enforces lists of the same length *)
              let len = min (List.length l1) (List.length l2) in
              let l1 = List.take l1 len in
              let l2 = List.take l2 len in
              let f (l1Item : 'expr_type dval) (l2Item : 'expr_type dval) :
                  'expr_type dval =
                Dval.to_list [l1Item; l2Item]
              in
              Dval.to_list (List.map2_exn ~f l1 l2)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::zip"]
    ; infix_names = []
    ; parameters = [par "as" TList; par "bs" TList]
    ; return_type = TOption
    ; description =
        {|If the lists have the same length, returns `Just list` formed from parallel pairs in `as` and `bs`.
        For example, if `as` is `[1,2,3]` and `bs` is `["x","y","z"]`, returns `[[1,"x"], [2,"y"], [3,"z"]]`.
        See `List::unzip` if you want to deconstruct `list` into `as` and `bs` again.
        If the lists differ in length, returns `Nothing` (consider `List::zipShortest` if you want to drop values from the longer list instead).|}
    ; func =
        InProcess
          (function
          | state, [DList l1; DList l2] ->
              let f (l1Item : 'expr_type dval) (l2Item : 'expr_type dval) :
                  'expr_type dval =
                Dval.to_list [l1Item; l2Item]
              in
              DOption
                ( match List.map2 ~f l1 l2 with
                | Ok res ->
                    OptJust (Dval.to_list res)
                | Unequal_lengths ->
                    OptNothing )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::unzip"]
    ; infix_names = []
    ; parameters = [par "pairs" TList]
    ; return_type = TList
    ; description =
        {|Given a `pairs` list where each value is a list of two values (such lists are constructed by `List::zip` and `List::zipShortest`), returns a list of two lists,
        one with every first value, and one with every second value. For example, if `pairs` is `[[1,"x"], [2,"y"], [3,"z"]]`, returns `[[1,2,3], ["x","y","z"]]`.|}
    ; func =
        (* We should deprecate this once we have tuples and homogenous lists *)
        InProcess
          (function
          | state, [DList l] ->
              let idx_from_rev_idx (rev_idx : int) (l : 'a list) : int =
                List.length l - 1 - rev_idx
              in
              let fold_fn
                  (rev_idx : int)
                  (acc :
                    ( 'expr_type dval list * 'expr_type dval list
                    , 'expr_type dval
                    (* type error *) )
                    result)
                  (dv : 'expr_type dval) :
                  ( 'expr_type dval list * 'expr_type dval list
                  , 'expr_type dval
                  (* type error *) )
                  result =
                Result.bind acc ~f:(fun (acc_a, acc_b) ->
                    match dv with
                    | DList [a; b] ->
                        Ok (a :: acc_a, b :: acc_b)
                    | (DIncomplete _ | DErrorRail _ | DError _) as dv ->
                        Error dv
                    | v ->
                        let err_details =
                          match v with
                          | DList l ->
                              Printf.sprintf
                                "It has length %i but must have length 2."
                                (List.length l)
                          | non_list ->
                              let tipe =
                                non_list
                                |> Dval.tipe_of
                                |> Dval.tipe_to_developer_repr_v0
                              in
                              Printf.sprintf
                                "It is of type `%s` instead of `List`."
                                tipe
                        in
                        Error
                          (DError
                             ( SourceNone
                             , Printf.sprintf
                                 "Expected every value within the `pairs` argument passed to `%s` to be a list with exactly two values. However, that is not the case for the value at index %i: %s. %s"
                                 state.executing_fnname
                                 (idx_from_rev_idx rev_idx l)
                                 (Dval.to_developer_repr_v0 v)
                                 err_details )))
              in
              let result =
                (* We reverse here so that the [foldi] consing happens in the correct order.
                * It does mean that the index passed by [foldi] counts from the end *)
                l |> List.rev |> List.foldi ~init:(Ok ([], [])) ~f:fold_fn
              in
              ( match result with
              | Ok (res_a, res_b) ->
                  DList [DList res_a; DList res_b]
              | Error v ->
                  v )
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::getAt"]
    ; infix_names = []
    ; parameters = [par "list" TList; par "index" TInt]
    ; return_type = TOption
    ; description =
        "Returns `Just value` at `index` in `list` if `index` is less than the length of the list. Otherwise returns `Nothing`."
    ; func =
        InProcess
          (function
          | _, [DList l; DInt index] ->
              List.nth l (Dint.to_int_exn index)
              |> Option.map ~f:(fun a -> DOption (OptJust a))
              |> Option.value ~default:(DOption OptNothing)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = true }
  ; { prefix_names = ["List::getAt_v1"]
    ; infix_names = []
    ; parameters = [par "list" TList; par "index" TInt]
    ; return_type = TOption
    ; description =
        "Returns `Just value` at `index` in `list` if `index` is less than the length of the list otherwise returns `Nothing`."
    ; func =
        InProcess
          (function
          | _, [DList l; DInt index] ->
              List.nth l (Dint.to_int_exn index)
              |> Option.map ~f:(fun a -> Dval.to_opt_just a)
              |> Option.value ~default:(DOption OptNothing)
          | args ->
              fail args)
    ; preview_safety = Safe
    ; deprecated = false }
  ; { prefix_names = ["List::randomElement"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TOption
    ; description =
        "Returns {{Just <var randomValue>}}, where <var randomValue> is a randomly selected value in <param list>. Returns {{Nothing}} if <param list> is empty."
    ; func =
        InProcess
          (function
          | _, [DList []] ->
              DOption OptNothing
          | _, [DList l] ->
              List.nth l (Random.int (List.length l))
              |> Option.map ~f:Dval.to_opt_just
              |> Option.value_exn
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false } ]
