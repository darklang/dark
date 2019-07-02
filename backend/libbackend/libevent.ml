open Core_kernel
open Libexecution
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : Lib.shortfn list =
  [ { pns = ["emit"]
    ; ins = []
    ; p = [par "Data" TAny; par "Space" TStr; par "Name" TStr]
    ; r = TAny
    ; d = "Emit event `name` in `space`, passing along `data` as a parameter"
    ; f =
        InProcess
          (function
          | {canvas_id; account_id}, [data; DStr space; DStr name] ->
              (* See client/src/Entry.ml for the "_" *)
              let space = Unicode_string.to_string space in
              let name = Unicode_string.to_string name in
              Event_queue.enqueue ~canvas_id ~account_id space name "_" data ;
              data
          | args ->
              fail args)
    ; ps = false
    ; dep = false } ]
