open Core
open Lib

open Types.RuntimeT
module RT = Runtime
module FF = FeatureFlag


let fns : Lib.shortfn list = [
  { pns = ["emit"]
  ; ins = []
  ; p = [par "Space" TStr; par "Name" TStr; par "Data" TAny]
  ; r = TAny
  ; d = "Emit event `name` in `space`, passing along `data` as a parameter"
  ; f = InProcess
        (function
          | (ff, [DStr space; DStr name; data]) ->
            Event_queue.enqueue space name data ff;
            data
          | (_, args) -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

]
