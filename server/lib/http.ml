(* Amazingly, the standard ocaml stuff for a http client is so shit we have to make our own shitty version *)
open Core
module C = Curl

type verb = GET | POST [@@deriving show]
type headers = string list [@@deriving show]

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
      (* (if ((Unix.stat filename).st_mtime +. 300.0) < (Unix.gettimeofday () ) then *)
      (if true then
         Some (Util.readfile2 filename)
       else
         None)

let _ = C.global_init C.CURLINIT_GLOBALALL

let http_call (url: string) (verb: verb) (headers: string list) (body: string) : string =
  let errorbuf = ref "" in
  let responsebuf = Buffer.create 16384 in

  let requestfn int : string =
    body in

  let responsefn str : int =
    Buffer.add_string responsebuf str;
    String.length str in

  let (code, error, response) = try
      let c = C.init () in
      C.set_url c url;
      (* C.set_verbose c true; *)
      C.set_errorbuffer c errorbuf;
      C.set_followlocation c true;
      C.set_failonerror c false;
      C.set_writefunction c responsefn;
      C.set_readfunction c requestfn;
      C.set_httpheader c headers;
      (* C.set_header c true; *)

      C.perform c;

      let response = (C.get_responsecode c, !errorbuf, Buffer.contents responsebuf) in
      C.cleanup c;
      response
    with
    | Curl.CurlException (_, code, s) ->
      (code, s, Buffer.contents responsebuf) in

  let info = [ "url", url
            ; "code", string_of_int code
            ; "error", error
            ; "response", response] in
  if code <> 200 then
    Exception.api ~info ("Bad response code (" ^ (string_of_int code) ^ ") in
call to " ^ url);

  response

let call (url: string) (verb: verb) (headers: string list) (body: string) : string =
  print_endline ("HTTP "
                 ^ (show_verb verb)
                 ^ " ("
                 ^ (body |> String.length |> string_of_int)
                 ^ "): "
                 ^ url);
  match cached_call url verb body with
  | None -> let result = http_call url verb headers body in
    save_call url verb body result;
    result
  | Some result -> result
