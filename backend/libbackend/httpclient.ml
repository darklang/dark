(* Amazingly, the standard ocaml stuff for a http client is so shit we have to make our own shitty version *)
open Core_kernel
open Libexecution
open Libcommon
module C = Curl

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
    (body : string option) : string * int * (string * string) list * string =
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
  let debugbuf_text = ref "" in
  let debugbuf_data_in = ref "" in
  let debugbuf_data_out = ref "" in
  let debugbuf_header_in = ref "" in
  let debugbuf_header_out = ref "" in
  let debugfn (c : Curl.t) (cdt : Curl.curlDebugType) (s : string) : unit =
    (* Types from https://curl.haxx.se/libcurl/c/CURLOPT_DEBUGFUNCTION.html *)
    match cdt with
    | DEBUGTYPE_TEXT ->
        (* "informational text" *)
        debugbuf_text := !debugbuf_text ^ s
    | DEBUGTYPE_DATA_IN ->
        (* response body *)
        debugbuf_data_in := !debugbuf_data_in ^ s
    | DEBUGTYPE_DATA_OUT ->
        (* request body *)
        debugbuf_data_out := !debugbuf_data_out ^ s
    | DEBUGTYPE_HEADER_IN ->
        (* response header *)
        debugbuf_header_in := !debugbuf_header_in ^ s
    | DEBUGTYPE_HEADER_OUT ->
        (* request header *)
        debugbuf_header_out := !debugbuf_header_out ^ s
    | DEBUGTYPE_SSL_DATA_IN | DEBUGTYPE_SSL_DATA_OUT ->
        (* "The data is SSL/TLS (binary) data", says the docs, so not useful to log *)
        ()
    | DEBUGTYPE_END ->
        (* No data in  this type,  it's just "we're done now". I tried calling
         * log_debug_info from here, but it didn't get called, so I'm just doing
         * it by hand at the end of the happy path and in the rescue. *)
        ()
  in
  let log_debug_info
      debugbuf_text
      debugbuf_data_in
      debugbuf_data_out
      debugbuf_header_in
      debugbuf_header_out =
    let header_pair_of_line (line : string) : (string * string) option =
      if String.is_empty line
      then None
      else
        line
        |> String.lsplit2 ~on:':'
        (* If it's None, log and put the whole thing into the header side - this
         * shouldn't happen, but failsafe to avoid losing data *)
        |> Tc.Option.orElseLazy (fun () ->
               Log.erroR
                 "Expected value to be a header separated by a colon, but it was not."
                 ~params:[("line", line)] ;
               Some (line, ""))
        |> Option.map ~f:(fun (header, value) ->
               (header |> String.strip, value |> String.strip))
        |> Option.map ~f:(fun (header, value) ->
               let headers_to_redact = ["authorization"; "set-cookie"] in
               if List.mem
                    ~equal:( = )
                    headers_to_redact
                    (header |> String.lowercase)
               then
                 ( header
                 , Printf.sprintf "[REDACTED: %i bytes]" (value |> String.length)
                 )
               else (header, value))
    in
    let line_and_headers (buffer : string ref) : string * (string * string) list
        =
      (* The first 'header' line of a request or response is not really a
       * 'header' - it'll look like
       * "POST /post?qp1=1 HTTP/1.1"
       * or
       * "HTTP/1.1 200 OK"
       * so we peel it off and handle separately.
       *)
      match !buffer |> String.split_lines with
      | x :: xs ->
          (x, xs |> List.filter_map ~f:header_pair_of_line)
      | [] ->
          ("", [])
    in
    let request_line, request_headers = line_and_headers debugbuf_header_out in
    let response_line, response_headers = line_and_headers debugbuf_header_in in
    Log.infO
      "libcurl"
      ~params:
        (* raw bodies are not useful to log - they may be large, or may contain
         * non-human-readable data (esp binary data, but minimized js, etc can
         * also be this). If we decided it was useful, we could plausibly have a
         * feature-flag-esque construct to turn up/down log verbosity, and make a
         * decision to log the whole thing - for certain requests, or up to a
         * certain size - based on that. *)
        [ ("informational_text", !debugbuf_text)
        ; ("request_line", request_line)
        ; ("response_line", response_line) ]
      ~jsonparams:
        [ ("informational_text_size", `Int (!debugbuf_text |> String.length))
        ; ("response_headers_count", `Int (response_headers |> List.length))
        ; ("request_headers_count", `Int (request_headers |> List.length))
        ; ("response_headers_size", `Int (!debugbuf_header_in |> String.length))
        ; ("request_headers_size", `Int (!debugbuf_header_out |> String.length))
        ; ("response_body_size", `Int (!debugbuf_data_in |> String.length))
        ; ("request_body_size", `Int (!debugbuf_data_out |> String.length))
          (* Honeycomb will not, by default, unpack this into columns - it's a
           * raw string. We don't want HC to auto-unpack it - there's too many
           * possible headers, our schema would be ridiculous.  However, now we
           * have a raw thing to poke at for debugging, and if we wanted to
           * extract known/common headers into a column, we could. *)
        ; ( "request_headers"
          , `Assoc
              (request_headers |> List.map ~f:(fun (k, v) -> (k, `String v))) )
        ; ( "response_headers"
          , `Assoc
              (response_headers |> List.map ~f:(fun (k, v) -> (k, `String v)))
          ) ]
  in
  let code, error, body =
    try
      let c = C.init () in
      C.set_url c url ;
      C.set_verbose c true ;
      C.set_debugfunction c debugfn ;
      C.set_errorbuffer c errorbuf ;
      C.set_followlocation c true ;
      C.set_failonerror c false ;
      C.set_writefunction c responsefn ;
      C.set_httpheader c headers ;
      C.set_headerfunction c headerfn ;
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
      (* If we have the tunnel options, proxy Curl through it with socks ... *)
      Option.value_map
        ~default:()
        ~f:(fun u -> u |> Curl.CURLOPT_PROXY |> C.setopt c)
        Config.curl_tunnel_url ;
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
      C.cleanup c ;
      log_debug_info
        debugbuf_text
        debugbuf_data_in
        debugbuf_data_out
        debugbuf_header_in
        debugbuf_header_out ;
      response
    with Curl.CurlException (curl_code, code, s) ->
      log_debug_info
        debugbuf_text
        debugbuf_data_in
        debugbuf_data_out
        debugbuf_header_in
        debugbuf_header_out ;
      let info =
        [ ("url", url)
        ; ("error", Curl.strerror curl_code)
        ; ("curl_code", string_of_int code)
        ; ("response", Buffer.contents responsebuf) ]
      in
      Exception.code
        ~info
        ("Internal HTTP-stack exception: " ^ Curl.strerror curl_code)
  in
  (body, code, !result_headers, error)


let http_call
    ?(raw_bytes = false)
    (url : string)
    (query_params : (string * string list) list)
    (verb : verb)
    (headers : (string * string) list)
    (body : string option) : string * (string * string) list * int =
  let resp_body, code, resp_headers, _ =
    http_call_with_code ~raw_bytes url query_params verb headers body
  in
  (resp_body, resp_headers, code)


let init () : unit = C.global_init C.CURLINIT_GLOBALALL
