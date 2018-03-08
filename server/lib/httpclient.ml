(* Amazingly, the standard ocaml stuff for a http client is so shit we have to make our own shitty version *)
open Core
module C = Curl

type verb = GET | POST | PUT [@@deriving show]
type headers = string list [@@deriving show]

let filename_for (url: string) (verb: verb) (body: string) : string =
  let verbs =
    match verb with
    | GET -> "GET"
    | POST -> "POST"
    | PUT -> "PUT"
  in
  url
  |> String.tr ~target:'/' ~replacement:'_'
  |> String.tr ~target:':' ~replacement:'_'
  |> String.tr ~target:'-' ~replacement:'_'
  |> String.tr ~target:'?' ~replacement:'_'
  |> (^) (Config.cache_dir ^ verbs ^ "_")

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
      Some (Util.readfile filename)

let http_call (url: string) (query_params : (string * string list) list)
    (verb: verb) (headers: (string * string) list) (body: string)
  : (string * (string * string) list) =

  let query_params =
    url
    |> Uri.of_string
    |> Uri.query
    |> List.append query_params
  in
  let url =
    url
    |> Uri.of_string
    |> Uri.with_uri ~query:(Some query_params)
    |> Uri.to_string
  in
  let headers =
    headers
    |> List.map ~f:(fun (k,v) -> k ^ ": " ^ v)
  in
  let errorbuf = ref "" in
  let responsebuf = Buffer.create 16384 in

  (* uploads *)
  let bodybuffer = ref body in
  let putfn (count: int) : string =
    let len = String.length !bodybuffer in
    let this_body = !bodybuffer in
    if count < len
    then (bodybuffer := ""; this_body)
    else
      let result = String.sub ~pos:0 ~len:count this_body in
      let save = String.sub ~pos:count ~len:(len-count) this_body in
      bodybuffer := save;
      result
    in

  let responsefn str : int =
    Buffer.add_string responsebuf str;
    String.length str in

  (* headers *)
  let result_headers = ref [] in
  let headerfn (h: string) : int =
      let split = String.lsplit2 ~on:':' h in
      match split with
      | Some (l, r) ->
          (result_headers := List.cons (l,r) !result_headers;
          String.length h)
      | None ->
          (result_headers := List.cons (h,"") !result_headers;
          String.length h)
  in

  let (code, error, body) =
    try
      let c = C.init () in
      C.set_url c url;
      C.set_verbose c true;
      C.set_errorbuffer c errorbuf;
      C.set_followlocation c true;
      C.set_failonerror c false;
      C.set_writefunction c responsefn;
      C.set_httpheader c headers;
      C.set_headerfunction c headerfn;

      (match verb with
       | PUT ->
         C.set_readfunction c putfn;
         C.set_upload c true
       | POST ->
         C.set_post c true;
         C.set_postfields c body;
         C.set_postfieldsize c (String.length body)
       | GET -> ());

      (* Actually do the request *)
      C.perform c;

      let response =
        (C.get_responsecode c, !errorbuf, Buffer.contents responsebuf)
      in
      C.cleanup c;
      response
    with
    | Curl.CurlException (_, code, s) ->
      (code, s, Buffer.contents responsebuf)
  in
  if code <> 200
  then
    let info = [ "url", url
               ; "code", string_of_int code
               ; "error", error
               ; "response", body]
    in
    Exception.api ~info ("Bad response code (" ^ (string_of_int code) ^ ") in call to " ^ url);
  else
    (body, !result_headers)

let call (url: string) (verb: verb) (headers: (string * string) list) (body: string) : string =
  Log.debuG "HTTP" ((show_verb verb) ^ " (" ^ (body |> String.length |> string_of_int) ^ "): " ^ url) ;
  match cached_call url verb body with
  | None ->
    let (result, headers) = http_call url [] verb headers body in
    save_call url verb body result;
    result
  | Some result -> result

let init () : unit =
  C.global_init C.CURLINIT_GLOBALALL
