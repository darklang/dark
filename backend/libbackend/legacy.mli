open Libexecution.Types.RuntimeT

(* This module implements the 'old' style of httpclient
 * functions: namely those that either directly
 * throw an exception on a non-2xx error code, or
 * those that simply wrap the exception in a Result *)
module LibhttpclientV0 : sig
  val call : Httpclient.verb -> (dval -> string) -> funcimpl

  val call_no_body : Httpclient.verb -> (dval -> string) -> funcimpl

  val wrapped_send_request :
       string
    -> Httpclient.verb
    -> (dval -> string)
    -> dval
    -> dval
    -> dval
    -> dval

  val wrapped_call : Httpclient.verb -> (dval -> string) -> funcimpl

  val wrapped_call_no_body : Httpclient.verb -> (dval -> string) -> funcimpl
end
