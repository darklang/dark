open Core_kernel
open Libexecution

open Types
module RTT = Types.RuntimeT
module TL = Toplevel
module PReq = Parsed_request
module SE = Stored_event

type canvas = Canvas.canvas
type executable_fn_id = (tlid * id * int)

val global_vars : canvas -> RTT.input_vars
val initial_input_vars_for_handler : canvas -> RTT.HandlerT.handler -> RTT.input_vars list

val initial_input_vars_for_user_fn : canvas -> RTT.user_fn -> RTT.input_vars list

