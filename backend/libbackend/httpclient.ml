(* Amazingly, the standard ocaml stuff for a http client is so shit we have to make our own shitty version *)
open Core_kernel
open Libexecution
open Libcommon
module C = Curl
open Curl_logging

type verb =
  | GET
  | POST
  | PUT
  | PATCH
  | DELETE
  | HEAD
  | OPTIONS
[@@deriving show]

type headers = string list [@@deriving show]

type http_result =
  { body : string
  ; code : int
  ; headers : (string * string) list
  ; error : string }

type curl_error =
  { url : string
  ; error : string
  ; code : int }

(* Given ~regex, return Err if it doesn't match, or list of captures if
 * it does. First elem of the list is the first capture, not the whole
 * match. *)
let string_match ~(regex : string) (str : string) : string list Or_error.t =
  let reg = Re2.create_exn regex in
  str
  |> Re2.find_submatches reg
  |> Result.map ~f:Array.to_list
  |> Result.map ~f:List.tl_exn
  (* skip full match *)
  |> Result.map ~f:(List.map ~f:(Option.value ~default:""))


let charset (headers : (string * string) list) : [> `Latin1 | `Other | `Utf8] =
  let canonicalize s = s |> String.strip |> String.lowercase in
  headers
  |> List.map ~f:(Tuple.T2.map_fst ~f:canonicalize)
  |> List.map ~f:(Tuple.T2.map_snd ~f:canonicalize)
  |> List.filter_map ~f:(function
         | "content-type", v ->
           ( match string_match ~regex:".*;\\s*charset=(.*)$" v with
           | Result.Ok ["utf-8"] ->
               Some `Utf8
           | Result.Ok ["utf8"] ->
               Some `Utf8
           | Result.Ok ["us-ascii"] ->
               Some `Latin1 (* should work *)
           | Result.Ok ["iso-8859-1"] ->
               Some `Latin1
           | Result.Ok ["iso_8859-1"] ->
               Some `Latin1
           | Result.Ok ["latin1"] ->
               Some `Latin1
           | _ ->
               None )
         | _ ->
             None)
  |> List.hd
  |> Option.value ~default:`Other


(* Servers should default to ISO-8859-1 (aka Latin-1) if nothing
 * provided. We ask for UTF-8, but might not get it. If we get
 * ISO-8859-1 we can transcode it using Uutf. Uutf supports more recent
 * unicode than camomile (10, vs 3.2). However, camomile supports many
 * more transcoding formats. So we should default to Uutf, and fallback
 * to camomile if needs be. *)
let recode_latin1 (src : string) =
  let recodebuf = Buffer.create 16384 in
  let rec loop d e =
    match Uutf.decode d with
    | `Uchar _ as u ->
        ignore (Uutf.encode e u) ;
        loop d e
    | `End ->
        ignore (Uutf.encode e `End)
    | `Malformed _ ->
        ignore (Uutf.encode e (`Uchar Uutf.u_rep)) ;
        loop d e
    | `Await ->
        assert false
  in
  let d = Uutf.decoder ~encoding:`ISO_8859_1 (`String src) in
  let e = Uutf.encoder `UTF_8 (`Buffer recodebuf) in
  loop d e ;
  Buffer.contents recodebuf


(* The [body] parameter is optional to force us to actually treat its
 * presence/non-presence correctly between different requests. Naively using
 * the empty string to stand-in for "no body" was a pattern that bubbled up
 * too far and lead to us passing `""` to a function that then JSON encoded
 * the empty string, leading to the string `"\"\""` being passed here. By
 * making this an `option` here, and bubbling this optionality the whole way
 * up the callstack, we hopefully make it clear that a request has an optional
 * body *)
let http_call_with_code
    ?(raw_bytes = false)
    (url : string)
    (query_params : (string * string list) list)
    (verb : verb)
    (headers : (string * string) list)
    (body : string option) : (http_result, curl_error) Result.t =
  try
    let query_params =
      url |> Uri.of_string |> Uri.query |> List.append query_params
    in
    let url =
      url
      |> Uri.of_string
      |> Uri.with_uri ~query:(Some query_params)
      |> Uri.to_string
    in
    let headers = headers |> List.map ~f:(fun (k, v) -> k ^ ": " ^ v) in
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
      Buffer.add_string responsebuf str ;
      String.length str
    in
    (* headers *)
    let result_headers = ref [] in
    let headerfn (h : string) : int =
      (* See comment about responsebody below before changing this. *)
      let split = String.lsplit2 ~on:':' h in
      match split with
      | Some (l, r) ->
          result_headers := List.cons (l, r) !result_headers ;
          String.length h
      | None ->
          result_headers := List.cons (h, "") !result_headers ;
          String.length h
    in
    let debug_bufs = new_debug_bufs () in
    let code, error, body =
      let c = C.init () in
      C.set_url c url ;
      C.set_verbose c true ;
      C.set_debugfunction c (debugfn debug_bufs) ;
      C.set_errorbuffer c errorbuf ;
      C.set_followlocation c true ;
      C.set_failonerror c false ;
      C.set_writefunction c responsefn ;
      C.set_httpheader c headers ;
      C.set_headerfunction c headerfn ;
      C.setopt c (Curl.CURLOPT_TIMEOUT 30) (* timeout is infinite by default *) ;
      (* This tells CURL to send an Accept-Encoding header including all
        * of the encodings it supports *and* tells it to automagically decode
        * responses in those encodings. This works even if someone manually specifies
        * the encoding in the header, as libcurl will still appropriately decode it
        *
        * https://curl.haxx.se/libcurl/c/CURLOPT_ACCEPT_ENCODING.html
        * *)
      if not raw_bytes then C.set_encoding c C.CURL_ENCODING_ANY ;
      (* Don't let users curl to e.g. file://; just HTTP and HTTPs. *)
      C.set_protocols c [C.CURLPROTO_HTTP; C.CURLPROTO_HTTPS] ;
      (* Seems like redirects can be used to get around the above list... *)
      C.set_redirprotocols c [C.CURLPROTO_HTTP; C.CURLPROTO_HTTPS] ;
      C.setopt c (Curl.CURLOPT_PROXY Config.curl_tunnel_url) ;
      ( match verb with
      | PUT ->
        ( match body with
        | Some body ->
            C.set_postfields c body ;
            C.set_postfieldsize c (String.length body) ;
            C.set_customrequest c "PUT"
        | None ->
            C.set_postfields c "" ;
            C.set_postfieldsize c 0 ;
            C.set_customrequest c "PUT" )
      | POST ->
        ( match body with
        | Some body ->
            C.set_post c true ;
            C.set_postfields c body ;
            C.set_postfieldsize c (String.length body)
        | None ->
            C.set_postfields c "" ;
            C.set_postfieldsize c 0 ;
            C.set_customrequest c "POST" )
      | PATCH ->
        ( match body with
        | Some body ->
            C.set_postfields c body ;
            C.set_postfieldsize c (String.length body) ;
            C.set_customrequest c "PATCH"
        | None ->
            C.set_postfields c "" ;
            C.set_postfieldsize c 0 ;
            C.set_customrequest c "PATCH" )
      | DELETE ->
          C.set_followlocation c false ;
          C.set_customrequest c "DELETE"
      | OPTIONS ->
          C.set_customrequest c "OPTIONS"
      | HEAD ->
          C.set_nobody c true ;
          C.set_customrequest c "HEAD"
      | GET ->
          () ) ;
      (* Actually do the request *)
      C.perform c ;
      (* If we get a redirect back, then we may see the content-type
        * header twice. Fortunately, because we push headers to the front
        * above, and take the first in charset, we get the right
        * answer. Whew. To do this correctly, we'd have to implement our
        * own follow logic which would clear the header ref, which seems
        * straightforward in theory but likely not practice.
        * Alternatively, we could clear the headers ref when we receive a
        * new `ok` header. *)
      let responsebody =
        if charset !result_headers = `Latin1
        then recode_latin1 (Buffer.contents responsebuf)
        else Buffer.contents responsebuf
      in
      let response = (C.get_responsecode c, !errorbuf, responsebody) in
      let primaryip = C.get_primaryip c in
      C.cleanup c ;
      log_debug_info debug_bufs (Some primaryip) ;
      response
    in
    let obj = {body; code; headers = !result_headers; error} in
    Ok obj
  with Curl.CurlException (curl_code, code, s) ->
    let info = {url; error = Curl.strerror curl_code; code} in
    Error info


let http_call
    ?(raw_bytes = false)
    (url : string)
    (query_params : (string * string list) list)
    (verb : verb)
    (headers : (string * string) list)
    (body : string option) =
  http_call_with_code ~raw_bytes url query_params verb headers body


let init () : unit = C.global_init C.CURLINIT_GLOBALALL
