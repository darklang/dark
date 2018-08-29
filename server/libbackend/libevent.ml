open Core_kernel
open Libexecution

open Lib
open Types.RuntimeT
module RT = Runtime


let replacements = [
  ( "emit"
  , InProcess
      (function
        | ({ canvas_id; account_id } , [data; DStr space; DStr name]) ->
          (* See Entry.elm for the "_" *)
          Event_queue.enqueue
            ~canvas_id
            ~account_id
            space name "_" data;
          data
        | args -> fail args))
]
