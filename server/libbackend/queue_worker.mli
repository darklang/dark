open Core_kernel
open Libexecution

val run : unit -> (unit, Exception.captured) Result.t
val dequeue_and_evaluate_all : unit -> string
