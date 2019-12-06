open Core_kernel

val dump : 'a -> string

(* levels *)
type level =
  [ `Off
  | `Inspect
  | `Fatal
  | `Error
  | `Warn
  | `Info
  | `Success
  | `Debug
  | `All ]

val loglevel : level ref

val logkey : (string * Yojson.Safe.t) list Lwt.key ref

val set_level : level -> unit

val level_to_string : level -> string

val string_to_level_opt : string -> level option

val level_to_color : level -> string

val should_log : level -> bool

(* formats *)
type format =
  [ `Json
  | `DecoratedJson ]

val format : format ref

val set_format : format -> unit

(* initialization *)
val init : level:level -> format:format -> unit -> unit

(* printing *)
val pP :
     ?data:string
  -> ?jsonparams:(string * Yojson.Safe.t) list
  -> ?params:(string * string) list
  -> ?bt:Caml.Printexc.raw_backtrace
  -> level:level
  -> string
  -> unit

val inspecT : ?f:('a -> string) -> string -> 'a -> unit

val inspect : ?f:('a -> string) -> string -> 'a -> 'a

val debuG :
     ?data:string
  -> ?jsonparams:(string * Yojson.Safe.t) list
  -> ?params:(string * string) list
  -> ?bt:Caml.Printexc.raw_backtrace
  -> string
  -> unit

val infO :
     ?data:string
  -> ?jsonparams:(string * Yojson.Safe.t) list
  -> ?params:(string * string) list
  -> ?bt:Caml.Printexc.raw_backtrace
  -> string
  -> unit

val warN :
     ?data:string
  -> ?jsonparams:(string * Yojson.Safe.t) list
  -> ?params:(string * string) list
  -> ?bt:Caml.Printexc.raw_backtrace
  -> string
  -> unit

val erroR :
     ?data:string
  -> ?jsonparams:(string * Yojson.Safe.t) list
  -> ?params:(string * string) list
  -> ?bt:Caml.Printexc.raw_backtrace
  -> string
  -> unit

val fataL :
     ?data:string
  -> ?jsonparams:(string * Yojson.Safe.t) list
  -> ?params:(string * string) list
  -> ?bt:Caml.Printexc.raw_backtrace
  -> string
  -> unit

val succesS :
     ?data:string
  -> ?jsonparams:(string * Yojson.Safe.t) list
  -> ?params:(string * string) list
  -> ?bt:Caml.Printexc.raw_backtrace
  -> string
  -> unit

val add_log_annotations : (string * Yojson.Safe.t) list -> (unit -> 'a) -> 'a
