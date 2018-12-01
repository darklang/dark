open Core_kernel
open Libexecution

val run : Types.id -> (unit, Exception.captured) Result.t
