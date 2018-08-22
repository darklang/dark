(* Amazingly, the standard ocaml stuff for a http client is so shit we have to make our own shitty version *)
open Core_kernel
open Libexecution
open Libcommon
module C = Curl


type verb = GET | POST | PUT | PATCH | DELETE | HEAD | OPTIONS [@@deriving show]
type headers = string list [@@deriving show]

(* Servers should default to ISO-8859-1 (aka Latin-1) if nothing
 * provided. We ask for UTF-8, but might not get it. If we get
 * ISO-8859-1 we can transcode it using Uutf. Uutf supports more recent
 * unicode than camomile (10, vs 3.2). However, camomile supports many
 * more transcoding formats. So we should default to Uutf, and fallback
 * to camomile if needs be. *)
let recode_latin1 (src:string) =
  let recodebuf = Buffer.create 16384 in
  let rec loop d e =
    match Uutf.decode d with
    | `Uchar _ as u -> ignore (Uutf.encode e u); loop d e
    | `End -> ignore (Uutf.encode e `End)
    | `Malformed _ -> ignore (Uutf.encode e (`Uchar Uutf.u_rep)); loop d e
    | `Await -> assert false
  in
  let d = Uutf.decoder ~encoding:`ISO_8859_1 (`String src) in
  let e = Uutf.encoder `UTF_8 (`Buffer recodebuf) in
  loop d e;
  Buffer.contents recodebuf

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
  (* let bodybuffer = ref body in *)
  (* let putfn (count: int) : string = *)
  (*   let len = String.length !bodybuffer in *)
  (*   let this_body = !bodybuffer in *)
  (*   if count < len *)
  (*   then (bodybuffer := ""; this_body) *)
  (*   else *)
  (*     let result = String.sub ~pos:0 ~len:count this_body in *)
  (*     let save = String.sub ~pos:count ~len:(len-count) this_body in *)
  (*     bodybuffer := save; *)
  (*     result *)
  (*   in *)

  let responsefn str : int =
    Buffer.add_string responsebuf str;
    String.length str in

  (* headers *)
  let result_headers = ref [] in
  let headerfn (h: string) : int =
      (* See comment bout responsebody below before changing this. *)
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
      (* Don't let users curl to e.g. file://; just HTTP and HTTPs. *)
      C.set_protocols c [C.CURLPROTO_HTTP; C.CURLPROTO_HTTPS;];
      (* Seems like redirects can be used to get around the above list... *)
      C.set_redirprotocols c [C.CURLPROTO_HTTP; C.CURLPROTO_HTTPS;];

      (match verb with
       | PUT ->
         C.set_postfields c body;
         C.set_postfieldsize c (String.length body);
         C.set_customrequest c "PUT"
       | POST ->
         C.set_post c true;
         C.set_postfields c body;
         C.set_postfieldsize c (String.length body)
       | PATCH ->
         C.set_postfields c body;
         C.set_postfieldsize c (String.length body);
         C.set_customrequest c "PATCH"
       | DELETE ->
         C.set_followlocation c false;
         C.set_customrequest c "DELETE"
       | OPTIONS ->
         C.set_customrequest c "OPTIONS"
       | HEAD ->
         C.set_nobody c true;
         C.set_customrequest c "HEAD"
       | GET -> ());

      (* Actually do the request *)
      C.perform c;

      (* If we get a redirect back, then we may see the content-type
       * header twice. Fortunately, because we push headers to the front
       * above, and take the first in Util.charset, we get the right
       * answer. Whew. To do this correctly, we'd have to implement our
       * own follow logic which would clear the header ref, which seems
       * straightforward in theory but likely not practice.
       * Alternatively, we could clear the headers ref when we receive a
       * new `ok` header. *)
      let responsebody =
        if (Util.charset !result_headers = `Latin1)
        then recode_latin1 (Buffer.contents responsebuf)
        else Buffer.contents responsebuf
      in

      let response =
        (C.get_responsecode c, !errorbuf, responsebody)
      in
      C.cleanup c;
      response
   with
    | Curl.CurlException (curl_code, code, s) ->
      let info = [ "url", url
                 ; "error", Curl.strerror curl_code
                 ; "curl_code", string_of_int code
                 ; "response", Buffer.contents responsebuf]
      in
      Exception.user ~info ("Bad HTTP request: " ^ Curl.strerror curl_code);
  in
  if code < 200 || code >= 300
  then
    let info = [ "url", url
               ; "code", string_of_int code
               ; "error", error
               ; "response", body]
    in
    Exception.user ~info ("Bad HTTP response (" ^ (string_of_int code) ^ ") in call to " ^ url);
  else
    (body, !result_headers)

let call (url: string) (verb: verb) (headers: (string * string) list) (body: string) : string =
  Log.debuG "HTTP" ~params:[ "verb", show_verb verb
                           ; "url", url
                           ; "body", body |> String.length |> string_of_int
                           ] ;
  let (results, _) = http_call url [] verb headers body  in
  results

let init () : unit =
  C.global_init C.CURLINIT_GLOBALALL
