open Core_kernel
open Libexecution
open Lib
open Types.RuntimeT
module RT = Runtime

let fns : fn list =
  [ { name = fn "" "emit" 0

    ; parameters = [Param.make "Data" TAny; Param.make "Space" TStr; Param.make "Name" TStr]
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
    ; previewable = Impure
    ; deprecated = ReplacedBy(fn "" "" 0) }
  ; { name = fn "" "emit" "1"

    ; parameters = [Param.make "event" TAny; Param.make "Name" TStr]
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
    ; previewable = Impure
    ; deprecated = NotDeprecated } ]
