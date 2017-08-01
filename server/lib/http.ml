(* Amazingly, the standard ocaml stuff for a http client is so shit we have to make our own shitty version *)
open Core
module C = Curl

type verb = GET | POST

let filename_for (url: string) (verb: verb) (body: string) : string =
  let verbs = match verb with
    | GET -> "GET"
    | POST -> "POST" in
  url
  |> String.tr ~target:'/' ~replacement:'_'
  |> String.tr ~target:':' ~replacement:'_'
  |> String.tr ~target:'-' ~replacement:'_'
  |> String.tr ~target:'?' ~replacement:'_'
  |> (^) ("cache/" ^ verbs ^ "_")

let save_call (url: string) (verb: verb) (body: string) (value : string) : unit =
  let filename = filename_for url verb body in
  Util.writefile filename value

let cached_call (url: string) (verb: verb) (body: string) : string option =

  if body <> "" then
    None
  else
    let filename = filename_for url verb body in
    if Sys.file_exists filename <> `Yes then
      None
    else
       if (Unix.stat filename).st_mtime < (Unix.gettimeofday () ) then
         Some (Util.readfile filename)
       else
         None






let http_call (url: string) (verb: verb) (headers: string list) (body: string) : string =
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

let call (url: string) (verb: verb) (headers: string list) (body: string) : string =
  match cached_call url verb body with
  | None -> let result = http_call url verb headers body in
    save_call url verb body result;
    result
  | Some result -> result
