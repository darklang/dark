open Core
open Lib

open Types.RuntimeT
module RT = Runtime


let fns : Lib.shortfn list = [
  { pns = ["emit"]
  ; ins = []
  ; p = [par "Space" TStr; par "Name" TStr; par "Data" TAny]
  ; r = TNull
  ; d = "Emit event `name` in `space`, passing along `data` as a parameter"
  ; f = InProcess
        (function
          | [DStr space; DStr name; data] ->
            Event_queue.enqueue space name data;
            DNull
          | args -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

]
