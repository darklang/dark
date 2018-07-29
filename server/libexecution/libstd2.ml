open Core_kernel

open Lib
open Types.RuntimeT
module RT = Runtime

let fns : Lib.shortfn list = [

  (* ====================================== *)
  (* Dict  *)
  (* ====================================== *)

  { pns = ["Dict::get_v1"]
  ; ins = []
  ; p = [par "dict" TObj; par "key" TStr]
  ; r = TAny
  ; d = "Looks up `key` in object `dict` and returns the value if found, and Error otherwise"
  ; f = InProcess
        (function
          | (_, [DObj o; DStr s]) ->
            (match DvalMap.find o s with
             | Some d -> d
             | None -> DNull)
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  (* ====================================== *)
  (* List *)
  (* ====================================== *)
  { pns = ["List::head_v1"]
  ; ins = []
  ; p = [par "list" TList]
  ; r = TAny
  ; d = ""
  ; f = InProcess
        (function
          | (_, [DList l]) ->
            (match List.hd l with
             | Some dv -> dv
             | None -> DNull)

          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

  { pns = ["List::last_v1"]
  ; ins = []
  ; p = [par "list" TList]
  ; r = TAny
  ; d = ""
  ; f = InProcess
        (function
          | (_, [DList []]) -> DNull
          | (_, [DList l]) -> List.last_exn l
          | args -> fail args)
  ; pr = None
  ; ps = true
  }
  ;

]
