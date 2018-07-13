open Core_kernel
open Libexecution

val run : Types.id -> (unit, Exception.captured) Result.t
val dequeue_and_evaluate_all : unit -> string
