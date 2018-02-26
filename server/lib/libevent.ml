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
            let _ = Log.pp "test" (space, name, data) in
            let scope =
              (match !Event_queue.current_scope with
              | Some sc -> sc
              | None -> Exception.internal "Missing Event_queue.current_scope! Time to ditch global mutable state!")
            in
            Event_queue.enqueue scope space name data;
            DNull
          | args -> fail args)
  ; pr = None
  ; ps = false
  }
  ;

]
