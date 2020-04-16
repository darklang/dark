open Libexecution.Types.RuntimeT

module HttpclientV0 : sig
  val http_call :
       string
    -> (string * string list) list
    -> Httpclient.verb
    -> (string * string) list
    -> string
    -> string * (string * string) list

  val call :
    string -> Httpclient.verb -> (string * string) list -> string -> string
end

(* These functions contain a bug that despite saying they return
 * the status code, they actually still throw an exception on a non-200
 * response *)
module HttpclientV1 : sig
  val http_call :
       ?raw_bytes:bool
    -> string
    -> (string * string list) list
    -> Httpclient.verb
    -> (string * string) list
    -> string
    -> string * (string * string) list * int

  val call :
       ?raw_bytes:bool
    -> string
    -> Httpclient.verb
    -> (string * string) list
    -> string
    -> string
end

(* These functions _require_ a body in all cases *)
module HttpclientV2 : sig
  val http_call_with_code :
       ?raw_bytes:bool
    -> string
    -> (string * string list) list
    -> Httpclient.verb
    -> (string * string) list
    -> string
    -> string * int * (string * string) list * string

  val http_call :
       ?raw_bytes:bool
    -> string
    -> (string * string list) list
    -> Httpclient.verb
    -> (string * string) list
    -> string
    -> string * (string * string) list * int

  val call :
       ?raw_bytes:bool
    -> string
    -> Httpclient.verb
    -> (string * string) list
    -> string
    -> string
end

(* This module implements the 'old' style of httpclient
 * functions: namely those that either directly
 * throw an exception on a non-2xx error code, or
 * those that simply wrap the exception in a Result *)
module LibhttpclientV0 : sig
  val call : Httpclient.verb -> (expr_dval -> string) -> expr funcimpl

  val call_no_body : Httpclient.verb -> (expr_dval -> string) -> expr funcimpl

  val wrapped_send_request :
       string
    -> Httpclient.verb
    -> (expr dval -> string)
    -> expr_dval
    -> expr_dval
    -> expr_dval
    -> expr_dval

  val wrapped_call : Httpclient.verb -> (expr_dval -> string) -> expr funcimpl

  val wrapped_call_no_body :
    Httpclient.verb -> (expr_dval -> string) -> expr funcimpl
end

(*These functions call the bugged HttpclientV1 impls *)
module LibhttpclientV1 : sig
  val call : Httpclient.verb -> (expr_dval -> string) -> expr funcimpl

  val call_no_body : Httpclient.verb -> (expr_dval -> string) -> expr funcimpl
end

module LibhttpclientV2 : sig
  val send_request :
       string
    -> Httpclient.verb
    -> (expr_dval -> string)
    -> expr_dval
    -> expr_dval
    -> expr_dval
    -> expr_dval

  val call : Httpclient.verb -> (expr_dval -> string) -> expr funcimpl

  val call_no_body : Httpclient.verb -> (expr_dval -> string) -> expr funcimpl
end
