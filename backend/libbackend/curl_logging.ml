open Libcommon
open Core_kernel

type bufs = string ref * string ref * string ref * string ref * string ref

let new_debug_bufs () : bufs = (ref "", ref "", ref "", ref "", ref "")

let debugfn (bufs : bufs) (c : Curl.t) (cdt : Curl.curlDebugType) (s : string) :
    unit =
  let ( debugbuf_text
      , debugbuf_data_in
      , debugbuf_data_out
      , debugbuf_header_in
      , debugbuf_header_out ) =
    bufs
  in
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
      (* No data in  this type, it's just "we're done now". I tried calling
       * log_debug_info from here, but it didn't get called, so I'm just doing
       * it by hand at the end of the happy path and in the rescue. *)
      ()


let log_debug_info (bufs : bufs) =
  let ( debugbuf_text
      , debugbuf_data_in
      , debugbuf_data_out
      , debugbuf_header_in
      , debugbuf_header_out ) =
    bufs
  in
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
  let line_and_headers (buffer : string ref) : string * (string * string) list =
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
        , `Assoc (request_headers |> List.map ~f:(fun (k, v) -> (k, `String v)))
        )
      ; ( "response_headers"
        , `Assoc (response_headers |> List.map ~f:(fun (k, v) -> (k, `String v)))
        ) ]
