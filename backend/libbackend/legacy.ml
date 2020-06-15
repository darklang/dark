open Core_kernel
open Libexecution
open Runtime
open Lib
open Types.RuntimeT
open Curl_logging

module HttpclientV0 = struct
  module C = Curl
  open Libcommon

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


  let charset (headers : (string * string) list) : [> `Latin1 | `Other | `Utf8]
      =
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


  let http_call_with_code
      ?(raw_bytes = false)
      (url : string)
      (query_params : (string * string list) list)
      (verb : Httpclient.verb)
      (headers : (string * string) list)
      (body : string) : string * int * (string * string) list * string =
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
      try
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
        C.setopt c (Curl.CURLOPT_PROXY Config.curl_tunnel_url) ;
        ( match verb with
        | PUT ->
            C.set_postfields c body ;
            C.set_postfieldsize c (String.length body) ;
            C.set_customrequest c "PUT"
        | POST ->
            C.set_post c true ;
            C.set_postfields c body ;
            C.set_postfieldsize c (String.length body)
        | PATCH ->
            C.set_postfields c body ;
            C.set_postfieldsize c (String.length body) ;
            C.set_customrequest c "PATCH"
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
      with Curl.CurlException (curl_code, code, s) ->
        log_debug_info debug_bufs None ;
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
      (url : string)
      (query_params : (string * string list) list)
      (verb : Httpclient.verb)
      (headers : (string * string) list)
      (body : string) : string * (string * string) list =
    let resp_body, code, resp_headers, error =
      http_call_with_code url query_params verb headers body
    in
    if code < 200 || code > 299
    then
      let info =
        [ ("url", url)
        ; ("code", string_of_int code)
        ; ("error", error)
        ; ("response", resp_body) ]
      in
      Exception.code
        ~info
        ("Bad HTTP response (" ^ string_of_int code ^ ") in call to " ^ url)
    else (resp_body, resp_headers)


  let call
      (url : string)
      (verb : Httpclient.verb)
      (headers : (string * string) list)
      (body : string) : string =
    let results, _ = http_call url [] verb headers body in
    results
end

module HttpclientV1 = struct
  module C = Curl
  open Libcommon

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


  let charset (headers : (string * string) list) : [> `Latin1 | `Other | `Utf8]
      =
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


  let http_call_with_code
      ?(raw_bytes = false)
      (url : string)
      (query_params : (string * string list) list)
      (verb : Httpclient.verb)
      (headers : (string * string) list)
      (body : string) : string * int * (string * string) list * string =
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
      try
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
        C.setopt c (Curl.CURLOPT_PROXY Config.curl_tunnel_url) ;
        ( match verb with
        | PUT ->
            C.set_postfields c body ;
            C.set_postfieldsize c (String.length body) ;
            C.set_customrequest c "PUT"
        | POST ->
            C.set_post c true ;
            C.set_postfields c body ;
            C.set_postfieldsize c (String.length body)
        | PATCH ->
            C.set_postfields c body ;
            C.set_postfieldsize c (String.length body) ;
            C.set_customrequest c "PATCH"
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
      with Curl.CurlException (curl_code, code, s) ->
        log_debug_info debug_bufs None ;
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
      (verb : Httpclient.verb)
      (headers : (string * string) list)
      (body : string) : string * (string * string) list * int =
    let resp_body, code, resp_headers, error =
      http_call_with_code ~raw_bytes url query_params verb headers body
    in
    if code < 200 || code > 299
    then
      let info =
        [ ("url", url)
        ; ("code", string_of_int code)
        ; ("error", error)
        ; ("response", resp_body) ]
      in
      Exception.code
        ~info
        ("Bad HTTP response (" ^ string_of_int code ^ ") in call to " ^ url)
    else (resp_body, resp_headers, code)


  let call
      ?(raw_bytes = false)
      (url : string)
      (verb : Httpclient.verb)
      (headers : (string * string) list)
      (body : string) : string =
    Libcommon.Log.debuG
      "HTTP"
      ~params:[("verb", Httpclient.show_verb verb); ("url", url)]
      ~jsonparams:[("body", `Int (body |> String.length))] ;
    let results, _, _ = http_call ~raw_bytes url [] verb headers body in
    results
end

module HttpclientV2 = struct
  module C = Curl
  open Libcommon

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


  let charset (headers : (string * string) list) : [> `Latin1 | `Other | `Utf8]
      =
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


  let http_call_with_code
      ?(raw_bytes = false)
      (url : string)
      (query_params : (string * string list) list)
      (verb : Httpclient.verb)
      (headers : (string * string) list)
      (body : string) : string * int * (string * string) list * string =
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
      try
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
        C.setopt c (Curl.CURLOPT_PROXY Config.curl_tunnel_url) ;
        ( match verb with
        | PUT ->
            C.set_postfields c body ;
            C.set_postfieldsize c (String.length body) ;
            C.set_customrequest c "PUT"
        | POST ->
            C.set_post c true ;
            C.set_postfields c body ;
            C.set_postfieldsize c (String.length body)
        | PATCH ->
            C.set_postfields c body ;
            C.set_postfieldsize c (String.length body) ;
            C.set_customrequest c "PATCH"
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
      with Curl.CurlException (curl_code, code, s) ->
        log_debug_info debug_bufs None ;
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
      (verb : Httpclient.verb)
      (headers : (string * string) list)
      (body : string) : string * (string * string) list * int =
    let resp_body, code, resp_headers, _ =
      http_call_with_code ~raw_bytes url query_params verb headers body
    in
    (resp_body, resp_headers, code)


  let call
      ?(raw_bytes = false)
      (url : string)
      (verb : Httpclient.verb)
      (headers : (string * string) list)
      (body : string) : string =
    Log.debuG
      "HTTP"
      ~params:[("verb", Httpclient.show_verb verb); ("url", url)]
      ~jsonparams:[("body", `Int (body |> String.length))] ;
    let results, _, _ = http_call ~raw_bytes url [] verb headers body in
    results
end

module LibhttpclientV0 = struct
  type headers = (string * string) list

  let has_form_header (headers : headers) : bool =
    List.exists headers ~f:(fun (k, v) ->
        String.lowercase k = "content-type"
        && String.lowercase v = "application/x-www-form-urlencoded")


  let has_json_header (headers : headers) : bool =
    List.exists headers ~f:(fun (k, v) ->
        String.lowercase k = "content-type"
        && v
           |> String.lowercase
           |> String.is_substring ~substring:"application/json")


  (* TODO: integrate with dark_request *)
  let send_request
      (uri : string)
      (verb : Httpclient.verb)
      (json_fn : 'expr_type dval -> string)
      (body : 'expr_type dval)
      (query : 'expr_type dval)
      (headers : 'expr_type dval) : Libexecution.Types.fluid_expr dval =
    let query = Dval.dval_to_query query in
    let headers = Dval.to_string_pairs_exn headers in
    let body =
      match body with
      | DObj obj when has_form_header headers ->
          Dval.to_form_encoding body
      | _ ->
          json_fn body
    in
    let result, headers = HttpclientV0.http_call uri query verb headers body in
    let parsed_result =
      if has_form_header headers
      then Dval.of_form_encoding result
      else if has_json_header headers
      then Dval.of_unknown_json_v0 result
      else Dval.dstr_of_string_exn result
    in
    let parsed_headers =
      headers
      |> List.map ~f:(fun (k, v) ->
             (String.strip k, Dval.dstr_of_string_exn (String.strip v)))
      |> List.filter ~f:(fun (k, _) -> String.length k > 0)
      |> DvalMap.from_list
      |> fun dm -> DObj dm
    in
    Dval.to_dobj_exn
      [ ("body", parsed_result)
      ; ("headers", parsed_headers)
      ; ("raw", Dval.dstr_of_string_exn result) ]


  let call verb json_fn =
    InProcess
      (function
      | _, [DStr uri; body; query; headers] ->
          send_request
            (Unicode_string.to_string uri)
            verb
            json_fn
            body
            query
            headers
      | args ->
          fail args)


  (* Some verbs dont have HTTP bodies *)
  let call_no_body verb json_fn =
    InProcess
      (function
      | _, [DStr uri; query; headers] ->
          send_request
            (Unicode_string.to_string uri)
            verb
            json_fn
            (Dval.dstr_of_string_exn "")
            query
            headers
      | args ->
          fail args)


  (* This isn't great, but we throw a lot of exceptions below this point
     * in the callstack and it'd be a lot of churn to rewrite that to propagate Results,
 * especially given it probably needs a rewrite anyway *)
  let wrapped_send_request
      (uri : string)
      (verb : Httpclient.verb)
      (json_fn : 'expr_type dval -> string)
      (body : 'expr_type dval)
      (query : 'expr_type dval)
      (headers : 'expr_type dval) : 'expr_type dval =
    Libcommon.Log.inspecT "uri" uri ;
    Libcommon.Log.inspecT "body" body ;
    Libcommon.Log.inspecT "query" query ;
    Libcommon.Log.inspecT "headers" headers ;
    try DResult (ResOk (send_request uri verb json_fn body query headers)) with
    | Exception.DarkException ed ->
        DResult (ResError (Dval.dstr_of_string_exn ed.short))
    | e ->
        raise e


  let wrapped_call verb json_fn =
    InProcess
      (function
      | _, [DStr uri; body; query; headers] ->
          wrapped_send_request
            (Unicode_string.to_string uri)
            verb
            json_fn
            body
            query
            headers
      | args ->
          fail args)


  (* Some verbs dont have HTTP bodies *)
  let wrapped_call_no_body verb json_fn =
    InProcess
      (function
      | _, [DStr uri; query; headers] ->
          wrapped_send_request
            (Unicode_string.to_string uri)
            verb
            json_fn
            (Dval.dstr_of_string_exn "")
            query
            headers
      | args ->
          fail args)
end

module LibhttpclientV1 = struct
  let params =
    [par "uri" TStr; par "body" TAny; par "query" TObj; par "headers" TObj]


  let params_no_body = [par "uri" TStr; par "query" TObj; par "headers" TObj]

  type headers = (string * string) list

  let has_form_header (headers : headers) : bool =
    List.exists headers ~f:(fun (k, v) ->
        String.lowercase k = "content-type"
        && String.lowercase v = "application/x-www-form-urlencoded")


  let has_json_header (headers : headers) : bool =
    List.exists headers ~f:(fun (k, v) ->
        String.lowercase k = "content-type"
        && v
           |> String.lowercase
           |> String.is_substring ~substring:"application/json")


  let send_request
      (uri : string)
      (verb : Httpclient.verb)
      (json_fn : fluid_dval -> string)
      (body : fluid_dval)
      (query : fluid_dval)
      (headers : fluid_dval) : fluid_dval =
    let query = Dval.dval_to_query query in
    let headers = Dval.to_string_pairs_exn headers in
    let body =
      match body with
      | DObj obj when has_form_header headers ->
          Dval.to_form_encoding body
      | _ ->
          json_fn body
    in
    let result, headers, code =
      HttpclientV1.http_call uri query verb headers body
    in
    let parsed_result =
      if has_form_header headers
      then
        try Dval.of_form_encoding result
        with _ -> Dval.dstr_of_string_exn "form decoding error"
      else if has_json_header headers
      then
        try Dval.of_unknown_json_v0 result
        with _ -> Dval.dstr_of_string_exn "json decoding error"
      else
        try Dval.dstr_of_string_exn result
        with _ -> Dval.dstr_of_string_exn "utf-8 decoding error"
    in
    let parsed_headers =
      headers
      |> List.map ~f:(fun (k, v) ->
             (String.strip k, Dval.dstr_of_string_exn (String.strip v)))
      |> List.filter ~f:(fun (k, _) -> String.length k > 0)
      |> DvalMap.from_list
      |> fun dm -> DObj dm
    in
    let obj =
      Dval.to_dobj_exn
        [ ("body", parsed_result)
        ; ("headers", parsed_headers)
        ; ( "raw"
          , result
            |> Dval.dstr_of_string
            |> Option.value
                 ~default:(Dval.dstr_of_string_exn "utf-8 decoding error") )
        ; ("code", DInt (Dint.of_int code)) ]
    in
    if code >= 200 && code <= 299
    then DResult (ResOk obj)
    else DResult (ResError obj)


  let encode_basic_auth u p =
    let input =
      if Unicode_string.is_substring
           ~substring:(Unicode_string.of_string_exn "-")
           u
      then error "Username cannot contain a colon"
      else
        Unicode_string.append_broken
          (Unicode_string.append_broken u (Unicode_string.of_string_exn ":"))
          p
    in
    let encoded =
      Unicode_string.of_string_exn
        (B64.encode
           ~alphabet:B64.default_alphabet
           ~pad:true
           (Unicode_string.to_string input))
    in
    Unicode_string.append_broken (Unicode_string.of_string_exn "Basic ") encoded


  let call verb json_fn =
    InProcess
      (function
      | _, [DStr uri; body; query; headers] ->
          send_request
            (Unicode_string.to_string uri)
            verb
            json_fn
            body
            query
            headers
      | args ->
          fail args)


  let call_no_body verb json_fn =
    InProcess
      (function
      | _, [DStr uri; query; headers] ->
          send_request
            (Unicode_string.to_string uri)
            verb
            json_fn
            (Dval.dstr_of_string_exn "")
            query
            headers
      | args ->
          fail args)
end

module LibhttpclientV2 = struct
  let params =
    [par "uri" TStr; par "body" TAny; par "query" TObj; par "headers" TObj]


  let params_no_body = [par "uri" TStr; par "query" TObj; par "headers" TObj]

  type headers = (string * string) list

  let has_form_header (headers : headers) : bool =
    List.exists headers ~f:(fun (k, v) ->
        String.lowercase k = "content-type"
        && String.lowercase v = "application/x-www-form-urlencoded")


  let has_json_header (headers : headers) : bool =
    List.exists headers ~f:(fun (k, v) ->
        String.lowercase k = "content-type"
        && v
           |> String.lowercase
           |> String.is_substring ~substring:"application/json")


  let send_request
      (uri : string)
      (verb : Httpclient.verb)
      (json_fn : 'expr_type dval -> string)
      (body : 'expr_type dval)
      (query : 'expr_type dval)
      (headers : 'expr_type dval) : 'expr_type dval =
    let query = Dval.dval_to_query query in
    let headers = Dval.to_string_pairs_exn headers in
    let body =
      match body with
      | DObj obj when has_form_header headers ->
          Dval.to_form_encoding body
      | _ ->
          json_fn body
    in
    let result, headers, code =
      HttpclientV2.http_call uri query verb headers body
    in
    let parsed_result =
      if has_form_header headers
      then
        try Dval.of_form_encoding result
        with _ -> Dval.dstr_of_string_exn "form decoding error"
      else if has_json_header headers
      then
        try Dval.of_unknown_json_v0 result
        with _ -> Dval.dstr_of_string_exn "json decoding error"
      else
        try Dval.dstr_of_string_exn result
        with _ -> Dval.dstr_of_string_exn "utf-8 decoding error"
    in
    let parsed_headers =
      headers
      |> List.map ~f:(fun (k, v) ->
             (String.strip k, Dval.dstr_of_string_exn (String.strip v)))
      |> List.filter ~f:(fun (k, _) -> String.length k > 0)
      |> DvalMap.from_list
      |> fun dm -> DObj dm
    in
    let obj =
      Dval.to_dobj_exn
        [ ("body", parsed_result)
        ; ("headers", parsed_headers)
        ; ( "raw"
          , result
            |> Dval.dstr_of_string
            |> Option.value
                 ~default:(Dval.dstr_of_string_exn "utf-8 decoding error") )
        ; ("code", DInt (Dint.of_int code)) ]
    in
    if code >= 200 && code <= 299
    then DResult (ResOk obj)
    else DResult (ResError obj)


  let encode_basic_auth u p =
    let input =
      if Unicode_string.is_substring
           ~substring:(Unicode_string.of_string_exn "-")
           u
      then error "Username cannot contain a colon"
      else
        Unicode_string.append_broken
          (Unicode_string.append_broken u (Unicode_string.of_string_exn ":"))
          p
    in
    let encoded =
      Unicode_string.of_string_exn
        (B64.encode
           ~alphabet:B64.default_alphabet
           ~pad:true
           (Unicode_string.to_string input))
    in
    Unicode_string.append_broken (Unicode_string.of_string_exn "Basic ") encoded


  let call verb json_fn =
    InProcess
      (function
      | _, [DStr uri; body; query; headers] ->
          send_request
            (Unicode_string.to_string uri)
            verb
            json_fn
            body
            query
            headers
      | args ->
          fail args)


  let call_no_body verb json_fn =
    InProcess
      (function
      | _, [DStr uri; query; headers] ->
          send_request
            (Unicode_string.to_string uri)
            verb
            json_fn
            (Dval.dstr_of_string_exn "")
            query
            headers
      | args ->
          fail args)
end
