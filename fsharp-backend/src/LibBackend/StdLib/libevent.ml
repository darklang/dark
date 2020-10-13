open Core_kernel
open Libexecution
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : fn list =
  [ { prefix_names = ["emit"]
    ; infix_names = []
    ; parameters = [par "Data" TAny; par "Space" TStr; par "Name" TStr]
    ; return_type = TAny
    ; description =
        "Emit event `name` in `space`, passing along `data` as a parameter"
    ; func =
        InProcess
          (function
          | {canvas_id; account_id; _}, [data; DStr space; DStr name] ->
              (* See client/src/Entry.ml for the "_" *)
              let space = Unicode_string.to_string space in
              let name = Unicode_string.to_string name in
              Event_queue.enqueue ~canvas_id ~account_id space name "_" data ;
              data
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = true }
  ; { prefix_names = ["emit_v1"]
    ; infix_names = []
    ; parameters = [par "event" TAny; par "Name" TStr]
    ; return_type = TAny
    ; description = "Emit a `event` to the `name` worker"
    ; func =
        InProcess
          (function
          | {canvas_id; account_id; _}, [data; DStr name] ->
              (* See client/src/Entry.ml for the "_" *)
              let name = Unicode_string.to_string name in
              Event_queue.enqueue ~canvas_id ~account_id "WORKER" name "_" data ;
              data
          | args ->
              fail args)
    ; preview_safety = Unsafe
    ; deprecated = false } ]
