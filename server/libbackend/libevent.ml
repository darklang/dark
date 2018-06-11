open Core
open Lib

open Types.RuntimeT
module RT = Runtime
module FF = Feature_flag


let fns : Lib.shortfn list = [
  { pns = ["emit"]
  ; ins = []
  ; p = [par "Data" TAny; par "Space" TStr; par "Name" TStr]
  ; r = TAny
  ; d = "Emit event `name` in `space`, passing along `data` as a parameter"
  ; f = InProcess
        (function
          | (state, [data; DStr space; DStr name]) ->
            Event_queue.enqueue state space name data;
            data
          | (_, args) -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

]
