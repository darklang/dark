open Core_kernel
open Libexecution

open Types
module RTT = Types.RuntimeT
module TL = Toplevel
module PReq = Parsed_request
module SE = Stored_event

type canvas = Canvas.canvas
type executable_fn_id = (tlid * id * int)

val initial_env : canvas -> RTT.dval_map
val default_env : canvas -> RTT.dval_map
val initial_envs_for_handler : canvas -> RTT.HandlerT.handler -> RTT.dval_map list

val initial_envs_for_user_fn : canvas -> RTT.user_fn -> RTT.dval_map list

val state_for_analysis :
  c : canvas ->
  input_cursor : int ->
  execution_id : id ->
  exe_fn_ids : id list ->
  env: RTT.dval_map ->
  tlid ->
  RTT.exec_state

val state_for_execution :
  c : canvas ->
  execution_id : id ->
  env: RTT.dval_map ->
  tlid ->
  RTT.exec_state

val state_for_enqueue :
  c : canvas ->
  execution_id : id ->
  tlid ->
  RTT.exec_state

