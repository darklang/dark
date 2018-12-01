open Core_kernel

val dump : 'a -> string

(* levels *)
type level = [`Off
             | `Inspect | `Fatal | `Error | `Warn | `Info |
              `Success | `Debug | `All ]

val loglevel : level ref

val set_level : level -> unit
val level_to_length : level -> int
val level_to_string : level -> string
val level_to_color : level -> string
val should_log : level -> bool

(* formats *)
type format = [`Stackdriver | `Regular | `Decorated ]

val format : format ref

val set_format : format -> unit
val format_string : level:level -> string -> string

(* initialization *)
val init : level:level -> format:format -> unit -> unit

(* printing *)
val pP : ?data: string
       -> ?params: (string * string) list
       -> ?bt:Caml.Printexc.raw_backtrace
       -> level:level
       -> string
       -> unit

val inspecT : ?f:('a -> string) -> string -> 'a -> unit
val inspect : ?f:('a -> string) -> string -> 'a -> 'a

val debuG :
  ?data: string
  -> ?params: (string * string) list
  -> ?bt:Caml.Printexc.raw_backtrace
  -> string
  -> unit

val infO :
  ?data: string
  -> ?params: (string * string) list
  -> ?bt:Caml.Printexc.raw_backtrace
  -> string
  -> unit

val warN :
  ?data: string
  -> ?params: (string * string) list
  -> ?bt:Caml.Printexc.raw_backtrace
  -> string
  -> unit

val erroR :
  ?data: string
  -> ?params: (string * string) list
  -> ?bt:Caml.Printexc.raw_backtrace
  -> string
  -> unit

val fataL :
  ?data: string
  -> ?params: (string * string) list
  -> ?bt:Caml.Printexc.raw_backtrace
  -> string
  -> unit

val succesS :
  ?data: string
  -> ?params: (string * string) list
  -> ?bt:Caml.Printexc.raw_backtrace
  -> string
  -> unit

val log_exception :
  ?bt:Caml.Printexc.raw_backtrace
  -> ?pp:(exn -> string)
  -> string
  -> string
  -> exn
  -> unit


