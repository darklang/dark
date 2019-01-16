open Core_kernel
open Libexecution
open Lib
open Types.RuntimeT
module RT = Runtime

let replacements =
  [ ( "emit"
    , InProcess
        (function
        | {canvas_id; account_id}, [data; DStr space; DStr name] ->
            (* See client/src/Entry.ml for the "_" *)
            let space = Dark_string.to_utf8 space in
            let name = Dark_string.to_utf8 name in
            Event_queue.enqueue ~canvas_id ~account_id space name "_" data ;
            data
        | args ->
            fail args) ) ]
