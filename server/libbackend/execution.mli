open Core_kernel
open Libexecution

open Types
module RTT = Types.RuntimeT
module TL = Toplevel
module PReq = Parsed_request
module SE = Stored_event
module FF = Feature_flag

type canvas = Canvas.canvas
type executable_fn_id = (tlid * id * int)

val default_env : canvas -> RTT.dval_map
val initial_envs : canvas -> Handler.handler -> RTT.dval_map list

val state_for_analysis :
  c : canvas ->
  execution_id : int ->
  exe_fn_ids : int list ->
  env: RTT.dval_map ->
  tlid ->
  RTT.exec_state

val state_for_execution :
  c : canvas ->
  execution_id : int ->
  env: RTT.dval_map ->
  tlid ->
  RTT.exec_state

