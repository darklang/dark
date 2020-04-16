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


type request_or_response =
  | Request
  | Response

let string_of_request_or_response ror : string =
  match ror with Request -> "request" | Response -> "response"


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
             (header |> String.strip |> String.lowercase, value |> String.strip))
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
  (* Query params may contain sensitive data, so remove anything after a ?
   * before logging *)
  let request_line =
    request_line
    |> String.split ~on:'?'
    |> List.hd
    |> Tc.Option.withDefault ~default:""
  in
  let response_line, response_headers = line_and_headers debugbuf_header_in in
  (* Takes a list of headers (k,v pairs) and returns a string of just the header
   * keys, comma-separated, with an additional comma on either end; that way we
   * can filter in honeycomb by header name - for instance,
   * `curl.request_headers contains ,set-cookie,` *)
  let header_names (headers : (string * string) list) : string =
    headers
    |> List.map ~f:fst
    |> String.concat ~sep:","
    |> fun s -> "," ^ s ^ ","
  in
  let extract_headers
      (headers : (string * string) list)
      ~(keys : string list)
      request_or_response : (string * string) list =
    headers
    |> List.filter ~f:(fun (k, _) -> List.mem ~equal:( = ) keys k)
    |> List.map ~f:(fun (k, v) ->
           ( Printf.sprintf
               "curl.%s_%s"
               (string_of_request_or_response request_or_response)
               k
           , v ))
  in
  (* For this and selected_response_headers, I looked at
   * https://en.wikipedia.org/wiki/List_of_HTTP_header_fields *)
  let selected_request_headers =
    extract_headers
      request_headers
      ~keys:
        [ "content-type"
        ; "host"
          (* https://aws.amazon.com/premiumsupport/knowledge-center/s3-request-id-values/ *)
        ; "x-amz-request-id"
        ; "x-amz-id-2"
        ; "x-request-id" ]
      Request
  in
  let selected_response_headers =
    extract_headers
      response_headers
      ~keys:
        [ "content-type"
        ; "server"
          (* https://aws.amazon.com/premiumsupport/knowledge-center/s3-request-id-values/ *)
        ; "x-amz-request-id"
        ; "x-amz-id-2"
        ; "x-request-id" ]
      Response
  in
  Log.infO
    "libcurl"
    ~params:
      (* raw bodies and headers are deliberately not logged - they can be large,
       * and may contain sensitive information *)
      ( [ ("curl.informational_text", !debugbuf_text)
        ; ("curl.request_line", request_line)
        ; ("curl.response_line", response_line)
        ; ("curl.request_headers", request_headers |> header_names)
        ; ("curl.response_headers", response_headers |> header_names) ]
      @ selected_request_headers
      @ selected_response_headers )
    ~jsonparams:
      [ ("curl.informational_text_size", `Int (!debugbuf_text |> String.length))
      ; ("curl.response_body_size", `Int (!debugbuf_data_in |> String.length))
      ; ("curl.request_body_size", `Int (!debugbuf_data_out |> String.length))
      ]
