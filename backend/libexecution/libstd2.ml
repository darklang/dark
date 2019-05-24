open Core_kernel
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : Lib.shortfn list =
  [ (* ====================================== *)
    (* Dict  *)
    (* ====================================== *)
    { pns = ["Dict::get_v1"]
    ; ins = []
    ; p = [par "dict" TObj; par "key" TStr]
    ; r = TOption
    ; d = "Looks up `key` in object `dict` and returns an option"
    ; f =
        InProcess
          (function
          | _, [DObj o; DStr s] ->
            ( match DvalMap.get o (Unicode_string.to_string s) with
            | Some d ->
                DOption (OptJust d)
            | None ->
                DOption OptNothing )
          | args ->
              fail args)
    ; ps = true
    ; dep = false }
  ; (* ====================================== *)
    (* List *)
    (* ====================================== *)
    { pns = ["List::head_v1"]
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
    ; dep = false } ]
