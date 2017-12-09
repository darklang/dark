open Core

open Types.RuntimeT
open Runtime

let par ?(d:string = "") ?(args=[]) ?(opt=false)  name tipe : param =
  { name = name
  ; tipe = tipe
  ; optional = opt
  ; block_args = args
  ; description = d
  }

let func ?(d:string = "") ?(name:string="f") args : param =
  par name TBlock ~args ~d


(* Shorthand *)
type shortfn = { n : string
               ; o : string list
               ; p : param list
               ; r : tipe
               ; f : ccfunc
               ; d : string
               ; pr : (dval list -> int -> dval list) option
               ; pu : bool
               }

let fail (args: dval list) : 'a =
  raise (Runtime.TypeError args)
