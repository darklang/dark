(* Amazingly, the standard ocaml stuff for a http client is so shit we have to make our own shitty version *)
open Core
module C = Curl

type verb = GET | POST

let call (url: string) (verb: verb) (headers: string list) (body: string) : string =
  let errorbuf = ref "" in
  let responsebuf = ref "" in

  let requestfn int : string =
    body in

  let responsefn str : int =
    responsebuf := str;
    String.length str in

  let (code, error, response) = try
      (* C.global_init C.CURLINIT_GLOBALALL; *)
      let c = C.init () in
      C.set_url c url;
      C.set_errorbuffer c errorbuf;
      C.set_followlocation c true;
      C.set_failonerror c false;
      C.set_writefunction c responsefn;
      C.set_readfunction c requestfn;
      C.set_httpheader c headers;

      C.perform c;

      (* C.cleanup c; *)
      (* C.global_cleanup (); *)
      (C.get_responsecode c, !errorbuf, !responsebuf)
    with
    | Curl.CurlException (_, code, s) ->
      (code, s, !responsebuf) in

  let msg = "url: " ^ url ^ "\ncode: " ^ (string_of_int code) ^ "\nerror: " ^ error ^ "\nresponse: " ^ response in
  if code <> 200 then
    Exception.raise msg;
  (* else *)
    (* print_endline msg; *)

  response
