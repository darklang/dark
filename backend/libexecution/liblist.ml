open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let list_repeat = Util.list_repeat

let fns =
  [ { pns = ["List::head"]
    ; ins = []
    ; p = [par "list" TList]
    ; r = TAny
    ; d =
        "Returns the head of a list. Returns null if the empty list is passed."
    ; f =
        InProcess
          (function
          | _, [DList l] ->
            (match List.hd l with Some dv -> dv | None -> DNull)
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["List::head_v1"]
    ; ins = []
    ; p = [par "list" TList]
    ; r = TOption
    ; d = "Fetches the head of the list and returns an option"
    ; f =
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
    ; ps = true
    ; dep = false }
  ; { pns = ["List::empty"]
    ; ins = []
    ; p = []
    ; r = TList
    ; d = "Returns an empty list."
    ; f = InProcess (function _, [] -> DList [] | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::push"]
    ; ins = []
    ; p = [par "list" TList; par "val" TAny]
    ; r = TList
    ; d = "Add element `val` to front of list `list`"
    ; f =
        InProcess
          (* fake cf handled by call *)
          (function
          | _, [DList l; i] -> DList (i :: l) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::pushBack"]
    ; ins = []
    ; p = [par "list" TList; par "val" TAny]
    ; r = TList
    ; d = "Add element `val` to back of list `list`"
    ; f =
        InProcess
          (function _, [DList l; i] -> DList (l @ [i]) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::last"]
    ; ins = []
    ; p = [par "list" TList]
    ; r = TAny
    ; d = "Returns the last item in a last. Returns null if the list is empty."
    ; f =
        InProcess
          (function
          | _, [DList []] ->
              DNull
          | _, [DList l] ->
              List.last_exn l
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["List::last_v1"]
    ; ins = []
    ; p = [par "list" TList]
    ; r = TOption
    ; d = "Returns the last item in the list as an option"
    ; f =
        InProcess
          (function
          | _, [DList []] ->
              DOption OptNothing
          | _, [DList l] ->
              DOption (OptJust (List.last_exn l))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::reverse"]
    ; ins = []
    ; p = [par "list" TList]
    ; r = TList
    ; d = "Reverses `list`"
    ; f =
        InProcess
          (function _, [DList l] -> DList (List.rev l) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::findFirst"]
    ; ins = []
    ; p = [par "l" TList; func ["val"]]
    ; r = TAny
    ; d = "Find the first element of the list, for which `f` returns true"
    ; f =
        InProcess
          (function
          | _, [DList l; DBlock fn] ->
              let f (dv : dval) : bool = DBool true = fn [dv] in
              (match List.find ~f l with None -> DNull | Some dv -> dv)
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["List::findFirst_v1"]
    ; ins = []
    ; p = [par "l" TList; func ["val"]]
    ; r = TOption
    ; d =
        "Find the first element of the list, for which `f` returns true. Returns Nothing if none return true"
    ; f =
        InProcess
          (function
          | _, [DList l; DBlock fn] ->
              let f (dv : dval) : bool = DBool true = fn [dv] in
              ( match List.find ~f l with
              | None ->
                  DOption OptNothing
              | Some dv ->
                  DOption (OptJust dv) )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::contains"]
    ; ins = []
    ; p = [par "l" TList; par "val" TAny]
    ; r = TBool
    ; d = "Returns if the value is in the list"
    ; f =
        InProcess
          (function
          | _, [DList l; i] ->
              DBool (List.mem ~equal:equal_dval l i)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::repeat"]
    ; ins = []
    ; p = [par "times" TInt; par "val" TAny]
    ; r = TList
    ; d = "Returns a list containing `val` repeated `count` times"
    ; f =
        InProcess
          (function
          | _, [DInt t; dv] ->
              DList (List.init (Dint.to_int_exn t) ~f:(fun _ -> dv))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::length"]
    ; ins = []
    ; p = [par "l" TList]
    ; r = TInt
    ; d = "Returns the length of the list"
    ; f =
        InProcess
          (function
          | _, [DList l] -> Dval.dint (List.length l) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::range"]
    ; ins = []
    ; p =
        [ par "start" TInt ~d:"First number in the range, will be included"
        ; par "stop" TInt ~d:"Last number in the range, is included" ]
    ; r = TList
    ; d =
        "Return a list of increasing integers from `start` to `stop`, inclusive"
    ; f =
        InProcess
          (function
          | _, [DInt start; DInt stop] ->
              DList
                ( List.range (Dint.to_int_exn start) (Dint.to_int_exn stop + 1)
                |> List.map ~f:(fun i -> Dval.dint i) )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::fold"]
    ; ins = []
    ; p = [par "l" TList; par "init" TAny; func ["accum"; "curr"]]
    ; r = TAny
    ; d =
        "Folds the list into a single value, by repeatedly apply `f` to any two pairs"
    ; f =
        InProcess
          (function
          | _, [DList l; init; DBlock fn] ->
              (* Fake cf should be propagated by the blocks so we dont need to check *)
              let f (dv1 : dval) (dv2 : dval) : dval = fn [dv1; dv2] in
              List.fold ~f ~init l
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::flatten"]
    ; ins = []
    ; p = [par "l" TList]
    ; r = TList
    ; d =
        "Returns a single list containing the elements of all the lists in `l`"
    ; f =
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
    ; ps = true
    ; dep = false }
  ; { pns = ["List::uniqueBy"]
    ; ins = []
    ; p = [par "l" TList; func ["val"]]
    ; r = TList
    ; d =
        "Returns the passed list, with only unique values, where uniqueness is based on the result of `f`. Only one of each value will be returned, but the order will not be maintained"
    ; f =
        InProcess
          (function
          | _, [DList l; DBlock fn] ->
              DList
                (List.dedup_and_sort l ~compare:(fun a b ->
                     compare_dval (fn [a]) (fn [b]) ))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::isEmpty"]
    ; ins = []
    ; p = [par "l" TList]
    ; r = TBool
    ; d = "Returns true iff. the list `l` is empty"
    ; f =
        InProcess
          (function
          | _, [DList l] -> DBool (List.is_empty l) | args -> fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::sort"]
    ; ins = []
    ; p = [par "list" TList]
    ; r = TList
    ; d = "Returns `list` sorted in ascending order"
    ; f =
        InProcess
          (function
          | _, [DList list] ->
              list |> List.sort ~compare:compare_dval |> DList
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::sortBy"]
    ; ins = []
    ; p = [par "list" TList; func ["val"]]
    ; r = TList
    ; d = "Returns `list`, sorted using the results of `f`"
    ; f =
        InProcess
          (function
          | _, [DList list; DBlock fn] ->
              list
              |> List.sort ~compare:(fun a b -> compare_dval (fn [a]) (fn [b]))
              |> DList
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::append"]
    ; ins = []
    ; p = [par "l1" TList; par "l2" TList]
    ; r = TList
    ; d = "Returns the combined list of `l1` and `l2`"
    ; f =
        InProcess
          (function
          | _, [DList l1; DList l2] ->
              DList (List.append l1 l2) (* no checking for fake cf required *)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::filter"]
    ; ins = []
    ; p = [par "l" TList; func ["val"]]
    ; r = TList
    ; d =
        "Return only values in `l` which meet the function's criteria. The function should return true to keep the entry or false to remove it."
    ; f =
        InProcess
          (function
          | _, [DList l; DBlock fn] ->
              let incomplete = ref false in
              let f (dv : dval) : bool =
                match fn [dv] with
                | DBool b ->
                    b
                | DIncomplete ->
                    incomplete := true ;
                    false
                | v ->
                    RT.error "Expecting fn to return bool" ~result:v ~actual:dv
              in
              if !incomplete then DIncomplete else DList (List.filter ~f l)
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["List::filter_v1"]
    ; ins = []
    ; p = [par "l" TList; func ["val"]]
    ; r = TList
    ; d = "Return only values in `l` which meet the function's criteria"
    ; f =
        InProcess
          (function
          | _, [DList l; DBlock fn] ->
              let fakecf = ref None in
              let f (dv : dval) : bool =
                let run = !fakecf = None in
                run
                &&
                match fn [dv] with
                | DBool b ->
                    b
                | (DIncomplete | DErrorRail _) as dv ->
                    fakecf := Some dv ;
                    false
                | v ->
                    RT.error "Expecting fn to return bool" ~result:v ~actual:dv
              in
              let result = List.filter ~f l in
              (match !fakecf with None -> DList result | Some v -> v)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::drop"]
    ; ins = []
    ; p = [par "l" TList; par "count" TInt]
    ; r = TList
    ; d = "Drops the first `count` items from the list"
    ; f =
        InProcess
          (function
          | _, [DList l; DInt c] ->
              DList (List.drop l (Dint.to_int_exn c))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::take"]
    ; ins = []
    ; p = [par "l" TList; par "count" TInt]
    ; r = TList
    ; d = "Drops all but the first `count` items from the list"
    ; f =
        InProcess
          (function
          | _, [DList l; DInt c] ->
              DList (List.take l (Dint.to_int_exn c))
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::foreach"]
    ; ins = []
    ; p = [par "l" TList; func ["val"]]
    ; r = TList
    ; d =
        "Call `f` on every item in the list, returning a list of the results of
  those calls"
    ; f =
        InProcess
          (function
          | _, [DList l; DBlock fn] ->
              let f (dv : dval) : dval = fn [dv] in
              DList (List.map ~f l)
          | args ->
              fail args)
    ; ps = true
    ; dep = true }
  ; { pns = ["List::map"]
    ; ins = []
    ; p = [par "l" TList; func ["val"]]
    ; r = TList
    ; d =
        "Call `f` on every item in the list, returning a list of the results of
  those calls"
    ; f =
        InProcess
          (function
          | _, [DList l; DBlock fn] ->
              let f (dv : dval) : dval = fn [dv] in
              Dval.to_list (List.map ~f l)
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; { pns = ["List::getAt"]
    ; ins = []
    ; p = [par "l" TList; par "index" TInt]
    ; r = TOption
    ; d =
        "Returns `Just item` at `index` in list `l` if `index` is less than the length of the list otherwise returns `Nothing`"
    ; f =
        InProcess
          (function
          | _, [DList l; DInt index] ->
              List.nth l (Dint.to_int_exn index)
              |> Option.map ~f:(fun a -> DOption (OptJust a))
              |> Option.value ~default:(DOption OptNothing)
          | args ->
              fail args)
    ; ps = true
    ; dep = false } ]
