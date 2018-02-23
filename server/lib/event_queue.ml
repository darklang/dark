open Core

open Types
open Types.RuntimeT

let enqueue (space: string) (name: string) (data: dval) : unit =
  ()

let dequeue (space: string) (name: string) : dval =
  DIncomplete
