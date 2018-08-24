open Core_kernel
open Libexecution
open Libexecution

open Lib
open Types.RuntimeT
module RT = Runtime


let fns : Lib.shortfn list = [
  { pns = ["emit"]
  ; ins = []
  ; p = [par "Data" TAny; par "Space" TStr; par "Name" TStr]
  ; r = TAny
  ; d = "Emit event `name` in `space`, passing along `data` as a parameter"
  ; f = InProcess
        (function
          | ({ canvas_id; account_id } , [data; DStr space; DStr name]) ->
            (* See Entry.elm for the "_" *)
            Event_queue.enqueue
              ~canvas_id
              ~account_id
              space name "_" data;
            data
          | args -> fail args)
  ; pr = None
  ; ps = false
  ; dep = false
  }
  ;

]
