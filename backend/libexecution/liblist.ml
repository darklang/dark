open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let list_repeat = Util.list_repeat

let fns =
  [ { prefix_names = ["List::head"]
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
    ; preview_execution_safe = true
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
    ; preview_execution_safe = true
    ; deprecated = true }
  ; { prefix_names = ["List::head_v2"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TOption
    ; description =
        "Returns `Just` the head (first item) of a list. Returns `Nothing` if the list is empty."
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
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::empty"]
    ; infix_names = []
    ; parameters = []
    ; return_type = TList
    ; description = "Returns an empty list."
    ; func = InProcess (function _, [] -> DList [] | args -> fail args)
    ; preview_execution_safe = true
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
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::pushBack"]
    ; infix_names = []
    ; parameters = [par "list" TList; par "val" TAny]
    ; return_type = TList
    ; description = "Add element `val` to back of list `list`"
    ; func =
        InProcess
          (function _, [DList l; i] -> DList (l @ [i]) | args -> fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::last"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TAny
    ; description =
        "Returns the last item in a last. Returns null if the list is empty."
    ; func =
        InProcess
          (function
          | _, [DList []] ->
              DNull
          | _, [DList l] ->
              List.last_exn l
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = true }
  ; { prefix_names = ["List::last_v1"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TOption
    ; description = "Returns the last item in the list as an option"
    ; func =
        InProcess
          (function
          | _, [DList []] ->
              DOption OptNothing
          | _, [DList l] ->
              DOption (OptJust (List.last_exn l))
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = true }
  ; { prefix_names = ["List::last_v2"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TOption
    ; description = "Returns the last item in the list as an option"
    ; func =
        InProcess
          (function
          | _, [DList []] ->
              DOption OptNothing
          | _, [DList l] ->
              Dval.to_opt_just (List.last_exn l)
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::reverse"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TList
    ; description = "Reverses `list`"
    ; func =
        InProcess
          (function _, [DList l] -> DList (List.rev l) | args -> fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::findFirst"]
    ; infix_names = []
    ; parameters = [par "l" TList; func ["val"]]
    ; return_type = TAny
    ; description =
        "Find the first element of the list, for which `f` returns true"
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let f (dv : dval) : bool =
                DBool true = Ast.execute_dblock ~state b [dv]
              in
              (match List.find ~f l with None -> DNull | Some dv -> dv)
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = true }
  ; { prefix_names = ["List::findFirst_v1"]
    ; infix_names = []
    ; parameters = [par "l" TList; func ["val"]]
    ; return_type = TOption
    ; description =
        "Find the first element of the list, for which `f` returns true. Returns Nothing if none return true"
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let f (dv : dval) : bool =
                DBool true = Ast.execute_dblock ~state b [dv]
              in
              ( match List.find ~f l with
              | None ->
                  DOption OptNothing
              | Some dv ->
                  DOption (OptJust dv) )
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = true }
  ; { prefix_names = ["List::findFirst_v2"]
    ; infix_names = []
    ; parameters = [par "l" TList; func ["val"]]
    ; return_type = TOption
    ; description =
        "Find the first element of the list, for which `f` returns true. Returns Nothing if none return true"
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let f (dv : dval) : bool =
                DBool true = Ast.execute_dblock ~state b [dv]
              in
              ( match List.find ~f l with
              | None ->
                  DOption OptNothing
              | Some dv ->
                  Dval.to_opt_just dv )
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::contains"]
    ; infix_names = []
    ; parameters = [par "l" TList; par "val" TAny]
    ; return_type = TBool
    ; description = "Returns if the value is in the list"
    ; func =
        InProcess
          (function
          | _, [DList l; i] ->
              DBool (List.mem ~equal:equal_dval l i)
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::repeat"]
    ; infix_names = []
    ; parameters = [par "times" TInt; par "val" TAny]
    ; return_type = TList
    ; description = "Returns a list containing `val` repeated `count` times"
    ; func =
        InProcess
          (function
          | _, [DInt t; dv] ->
              DList (List.init (Dint.to_int_exn t) ~f:(fun _ -> dv))
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::length"]
    ; infix_names = []
    ; parameters = [par "l" TList]
    ; return_type = TInt
    ; description = "Returns the length of the list"
    ; func =
        InProcess
          (function
          | _, [DList l] -> Dval.dint (List.length l) | args -> fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::range"]
    ; infix_names = []
    ; parameters =
        [ par "start" TInt ~d:"First number in the range, will be included"
        ; par "stop" TInt ~d:"Last number in the range, is included" ]
    ; return_type = TList
    ; description =
        "Return a list of increasing integers from `start` to `stop`, inclusive"
    ; func =
        InProcess
          (function
          | _, [DInt start; DInt stop] ->
              DList
                ( List.range (Dint.to_int_exn start) (Dint.to_int_exn stop + 1)
                |> List.map ~f:(fun i -> Dval.dint i) )
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::fold"]
    ; infix_names = []
    ; parameters = [par "l" TList; par "init" TAny; func ["accum"; "curr"]]
    ; return_type = TAny
    ; description =
        "Folds the list into a single value, by repeatedly apply `f` to any two pairs"
    ; func =
        InProcess
          (function
          | state, [DList l; init; DBlock b] ->
              (* Fake cf should be propagated by the blocks so we dont need to check *)
              let f (dv1 : dval) (dv2 : dval) : dval =
                Ast.execute_dblock ~state b [dv1; dv2]
              in
              List.fold ~f ~init l
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::flatten"]
    ; infix_names = []
    ; parameters = [par "l" TList]
    ; return_type = TList
    ; description =
        "Returns a single list containing the elements of all the lists in `l`"
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
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::uniqueBy"]
    ; infix_names = []
    ; parameters = [par "l" TList; func ["val"]]
    ; return_type = TList
    ; description =
        "Returns the passed list, with only unique values, where uniqueness is based on the result of `f`. Only one of each value will be returned, but the order will not be maintained"
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let fn dv = Ast.execute_dblock ~state b [dv] in
              DList
                (List.dedup_and_sort l ~compare:(fun a b ->
                     compare_dval (fn a) (fn b)))
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::isEmpty"]
    ; infix_names = []
    ; parameters = [par "l" TList]
    ; return_type = TBool
    ; description = "Returns true iff. the list `l` is empty"
    ; func =
        InProcess
          (function
          | _, [DList l] -> DBool (List.is_empty l) | args -> fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::sort"]
    ; infix_names = []
    ; parameters = [par "list" TList]
    ; return_type = TList
    ; description = "Returns `list` sorted in ascending order"
    ; func =
        InProcess
          (function
          | _, [DList list] ->
              list |> List.sort ~compare:compare_dval |> DList
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::sortBy"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["val"]]
    ; return_type = TList
    ; description = "Returns `list`, sorted using the results of `f`"
    ; func =
        InProcess
          (function
          | state, [DList list; DBlock b] ->
              let fn dv = Ast.execute_dblock ~state b [dv] in
              list
              |> List.sort ~compare:(fun a b -> compare_dval (fn a) (fn b))
              |> DList
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::sortByComparator"]
    ; infix_names = []
    ; parameters = [par "list" TList; func ["val"]]
    ; return_type = TResult
    ; description =
        "Returns `list`, sorted using a `f`, a lambda taking two args and returning -1, 0, and 1"
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
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::append"]
    ; infix_names = []
    ; parameters = [par "l1" TList; par "l2" TList]
    ; return_type = TList
    ; description = "Returns the combined list of `l1` and `l2`"
    ; func =
        InProcess
          (function
          | _, [DList l1; DList l2] ->
              DList (List.append l1 l2) (* no checking for fake cf required *)
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::filter"]
    ; infix_names = []
    ; parameters = [par "l" TList; func ["val"]]
    ; return_type = TList
    ; description =
        "Return only values in `l` which meet the function's criteria. The function should return true to keep the entry or false to remove it."
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let incomplete = ref false in
              let f (dv : dval) : bool =
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
    ; preview_execution_safe = true
    ; deprecated = true }
  ; { prefix_names = ["List::filter_v1"]
    ; infix_names = []
    ; parameters = [par "l" TList; func ["val"]]
    ; return_type = TList
    ; description =
        "Return only values in `l` which meet the function's criteria"
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let fakecf = ref None in
              let f (dv : dval) : bool =
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
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::drop"]
    ; infix_names = []
    ; parameters = [par "l" TList; par "count" TInt]
    ; return_type = TList
    ; description = "Drops the first `count` items from the list"
    ; func =
        InProcess
          (function
          | _, [DList l; DInt c] ->
              DList (List.drop l (Dint.to_int_exn c))
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::take"]
    ; infix_names = []
    ; parameters = [par "l" TList; par "count" TInt]
    ; return_type = TList
    ; description = "Drops all but the first `count` items from the list"
    ; func =
        InProcess
          (function
          | _, [DList l; DInt c] ->
              DList (List.take l (Dint.to_int_exn c))
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::foreach"]
    ; infix_names = []
    ; parameters = [par "l" TList; func ["val"]]
    ; return_type = TList
    ; description =
        "Call `f` on every item in the list, returning a list of the results of
  those calls"
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let f (dv : dval) : dval = Ast.execute_dblock ~state b [dv] in
              DList (List.map ~f l)
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = true }
  ; { prefix_names = ["List::map"]
    ; infix_names = []
    ; parameters = [par "l" TList; func ["val"]]
    ; return_type = TList
    ; description =
        "Call `f` on every item in the list, returning a list of the results of
  those calls"
    ; func =
        InProcess
          (function
          | state, [DList l; DBlock b] ->
              let f (dv : dval) : dval = Ast.execute_dblock ~state b [dv] in
              Dval.to_list (List.map ~f l)
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false }
  ; { prefix_names = ["List::getAt"]
    ; infix_names = []
    ; parameters = [par "l" TList; par "index" TInt]
    ; return_type = TOption
    ; description =
        "Returns `Just item` at `index` in list `l` if `index` is less than the length of the list otherwise returns `Nothing`"
    ; func =
        InProcess
          (function
          | _, [DList l; DInt index] ->
              List.nth l (Dint.to_int_exn index)
              |> Option.map ~f:(fun a -> DOption (OptJust a))
              |> Option.value ~default:(DOption OptNothing)
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = true }
  ; { prefix_names = ["List::getAt_v1"]
    ; infix_names = []
    ; parameters = [par "l" TList; par "index" TInt]
    ; return_type = TOption
    ; description =
        "Returns `Just item` at `index` in list `l` if `index` is less than the length of the list otherwise returns `Nothing`"
    ; func =
        InProcess
          (function
          | _, [DList l; DInt index] ->
              List.nth l (Dint.to_int_exn index)
              |> Option.map ~f:(fun a -> Dval.to_opt_just a)
              |> Option.value ~default:(DOption OptNothing)
          | args ->
              fail args)
    ; preview_execution_safe = true
    ; deprecated = false } ]
