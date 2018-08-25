open Core_kernel
open Libexecution

open Types
module RTT = Types.RuntimeT
module TL = Toplevel
module PReq = Parsed_request
module SE = Stored_event

type canvas = Canvas.canvas
type executable_fn_id = (tlid * id * int)

val global_vars : canvas -> RTT.symtable
val default_input_vars : canvas -> RTT.symtable
val initial_input_vars_for_handler : canvas -> RTT.HandlerT.handler -> RTT.symtable list

val initial_input_vars_for_user_fn : canvas -> RTT.user_fn -> RTT.symtable list

val state_for_analysis :
  c : canvas ->
  execution_id : id ->
  exe_fn_ids : id list ->
  tlid ->
  RTT.exec_state


