type bufs = string ref * string ref * string ref * string ref * string ref

val new_debug_bufs : unit -> bufs

val debugfn : bufs -> Curl.t -> Curl.curlDebugType -> string -> unit

val log_debug_info : bufs -> string option -> unit
