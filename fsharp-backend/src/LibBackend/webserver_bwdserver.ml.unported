// This file is being split between ApiServer.fs and BwdServer.fs. I'll delete from it as it's ported.


open Core_kernel
open Libcommon
open Lwt
module Cl = Cohttp_lwt
module Clu = Cohttp_lwt_unix
module SA = Static_assets
module S = Clu.Server
module CRequest = Clu.Request
module CResponse = Clu.Response
module Header = Cohttp.Header
module Cookie = Cohttp.Cookie
module Client = Clu.Client
module A = Account
module C = Canvas
module Exception = Libexecution.Exception
module Util = Libexecution.Util
module Dval = Libexecution.Dval
module DvalMap = Libexecution.Types.RuntimeT.DvalMap
module PReq = Libexecution.Parsed_request
module Types = Libexecution.Types
module Http = Libexecution.Http
module RTT = Types.RuntimeT
module Handler = Libexecution.Handler
module TL = Libexecution.Toplevel
module Prelude = Libexecution.Prelude
module Dbconnection = Libservice.Dbconnection
module Span = Telemetry.Span
module Op = Libserialize.Op

(* ------------------------------- *)
(* utils *)
(* ------------------------------- *)

type timing_header = string * float * string

let shutdown = ref false

let ready = ref false

let get_ip_address (ch : Conduit_lwt_unix.flow) : string =
  match Conduit_lwt_unix.endp_of_flow ch with
  | `TCP (ip, port) ->
      Ipaddr.to_string ip
  | _ ->
      assert false


let request_to_rollbar (body : string) (req : CRequest.t) :
    Libservice.Rollbar.request_data =
  { body
  ; headers = req |> CRequest.headers |> Cohttp.Header.to_list
  ; url = req |> CRequest.uri |> Uri.to_string
  ; http_method = req |> CRequest.meth |> Cohttp.Code.string_of_method }


type response_or_redirect_params =
  | Respond of
      { resp_headers : Header.t
      ; execution_id : Types.id
      ; status : Cohttp.Code.status_code
      ; body : string }
  | Redirect of
      { uri : Uri.t
      ; headers : Header.t option }

let respond_or_redirect (parent : Span.t) (params : response_or_redirect_params)
    =
  match params with
  | Redirect {uri; headers} ->
      Span.set_attr parent "response.status" (`Int 302) ;
      S.respond_redirect ?headers ~uri ()
  | Respond {resp_headers; execution_id; status; body} ->
      let resp_headers =
        Header.add_list
          resp_headers
          [(Libshared.Header.execution_id, Types.string_of_id execution_id)]
      in
      (* add Content-Length if missing, e.g. when function is called directly
       * and not from `respond_or_redirect_empty_body`
       *)
      let resp_headers =
        if Header.get resp_headers "Content-Length" = None
        then
          Header.add
            resp_headers
            "Content-Length"
            (string_of_int (String.length body))
        else resp_headers
      in
      Span.set_attrs
        parent
        [ ("response.status", `Int (Cohttp.Code.code_of_status status))
        ; ("response.content_length", `Int (String.length body)) ] ;
      ( match Header.get resp_headers "content-type" with
      | Some ct ->
          Span.set_attr parent "response.content_type" (`String ct)
      | None ->
          () ) ;
      S.respond_string ~status ~body ~headers:resp_headers ()


let respond_or_redirect_empty_body
    (span : Span.t) (params : response_or_redirect_params) =
  match params with
  | Redirect _ ->
      respond_or_redirect span params
  | Respond r ->
      let headers =
        Header.add
          r.resp_headers
          "Content-Length"
          (string_of_int (String.length r.body))
      in
      respond_or_redirect
        span
        (Respond {r with body = ""; resp_headers = headers})


let respond
    ?(resp_headers = Header.init ())
    ~(execution_id : Types.id)
    (span : Span.t)
    status
    (body : string) =
  respond_or_redirect span (Respond {resp_headers; execution_id; status; body})


type host_route =
  | Canvas of string
  | Static
  | Admin

(* NB: canvas in the DB is a string, not a uuid, because we do routing by canvas
 * name, not canvas_id (see the host_route type above).
 *
 * In addition:
 * - there are other place we use canvas_name as an fk; it's not great, but it's
 *   tech debt we can't solve today (for instance, iirc event queues do this)
 * - the external id will be part of a CNAME target - that is,
 *   some.customdomain.com -> ismith-foo.darkcustomdomain.com. Thus, if you were
 *   able to change your canvas' name (see previous bullet, you currently cannot),
 *   and we used canvas_id, now you'd have a CNAME pointing at the old
 *   canvas_name, but the custom_domains record would point to the new
 *   canvas_name (via JOIN canvases as c ON c.id = canvas_id). So that's not
 *   awesome either!
 *)
let canvas_from_db_opt (host_parts : string list) : host_route option =
  let host = String.concat host_parts ~sep:"." in
  Db.fetch_one_option
    ~name:"get_custom_domain"
    ~subject:host
    "SELECT canvas
       FROM custom_domains WHERE host = $1"
    ~params:[Db.String host]
  (* List.hd_exn because the list in question is a list of fields; we should never
   * get the wrong shape back from a query *)
  |> Option.map ~f:(fun canvas_name -> Canvas (canvas_name |> List.hd_exn))


let should_use_https uri =
  let parts =
    uri |> Uri.host |> Option.value ~default:"" |> fun h -> String.split h '.'
  in
  match parts with
  | ["darklang"; "com"]
  | ["builtwithdark"; "com"]
  | [_; "builtwithdark"; "com"]
  (* Customers - do not remove the marker below *)
  (* ACD-should_use_https-MARKER *)
  | ["hellobirb"; "com"]
  | ["www"; "hellobirb"; "com"] ->
      true
  | parts ->
      (* If we've set up a custom domain, we should force https. If we haven't,
       * and we've fallen all the way through (this is not a known host), then we
       * should not, because it is likely a healthcheck or other k8s endpoint *)
      parts |> canvas_from_db_opt |> Option.is_some


let redirect_to uri =
  let proto = uri |> Uri.scheme |> Option.value ~default:"" in
  (* If it's http and on a domain that can be served with https,
     we want to redirect to the same url but with the scheme
     replaced by "https". *)
  if proto = "http" && should_use_https uri
  then Some "https" |> Uri.with_scheme uri |> Some
  else None


(* Proxies that terminate HTTPs should give us X-Forwarded-Proto: http
   or X-Forwarded-Proto: https.

   Return the URI, adding the scheme to the URI if there is an X-Forwarded-Proto. *)
let with_x_forwarded_proto req =
  let uri = CRequest.uri req in
  match Header.get (CRequest.headers req) "X-Forwarded-Proto" with
  | Some proto ->
      Uri.with_scheme uri (Some proto)
  | None ->
    ( match Uri.scheme uri with
    | Some _ ->
        uri
    | None ->
        Uri.with_scheme uri (Some "http") )


(* Currently we just ensure that the Uri scheme is set *)
let canonicalize_request request =
  let new_uri = with_x_forwarded_proto request in
  let new_req =
    CRequest.make
      ~meth:(CRequest.meth request)
      ~version:(CRequest.version request)
      ~encoding:(CRequest.encoding request)
      ~headers:(CRequest.headers request)
      new_uri
  in
  (* Somewhat unbelievable, but CRequest.make strips the scheme (eg https)
   * from the uri, so we need to add it back in. *)
  {new_req with resource = Uri.to_string new_uri}


(* sanitize both repeated '/' and final '/'.
   "/foo//bar/" -> "/foo/bar"
   but leave "/" [root] untouched *)
let sanitize_uri_path path : string =
  path
  |> (fun str -> Re2.replace_exn (Re2.create_exn "/+") str ~f:(fun _ -> "/"))
  |> fun str -> if str = "/" then str else Util.maybe_chop_suffix "/" str


(* -------------------------------------------- *)
(* handlers for end users *)
(* -------------------------------------------- *)
let cors = ("Access-Control-Allow-Origin", "*")

let infer_cors_header
    (origin : string option) (setting : Canvas.cors_setting option) :
    string option =
  match (origin, setting) with
  (* if there's no explicit canvas setting, allow common localhosts *)
  | Some origin, None
    when let default_origins =
           [ "http://localhost:3000"
           ; "http://localhost:5000"
           ; "http://localhost:8000" ]
         in
         List.mem ~equal:( = ) default_origins origin ->
      Some origin
  (* if there's no explicit canvas setting and no default match, fall back to "*" *)
  | _, None ->
      Some "*"
  (* If there's a "*" in the setting, always use it.
     This is help as a debugging aid since users will always see
     Access-Control-Allow-Origin: * in their browsers, even if the
     request has no Origin. *)
  | _, Some AllOrigins ->
      Some "*"
  (* if there's no supplied origin, don't set the header at all. *)
  | None, _ ->
      None
  (* Return the origin if and only if it's in the setting  *)
  | Some origin, Some (Origins os) when List.mem ~equal:( = ) os origin ->
      Some origin
  (* Otherwise: there was a supplied origin and it's not in the setting.
     return "null" explicitly *)
  | Some _, Some _ ->
      Some "null"


let options_handler ~(execution_id : Types.id) (c : C.canvas) (req : CRequest.t)
    =
  (* When javascript in a browser tries to make an unusual cross-origin
     request (for example, a POST with a weird content-type or something with
     weird headers), the browser first makes an OPTIONS request to the
     server in order to get its permission to make that request. It includes
     "origin", the originating origin, and "access-control-request-headers",
     which is the list of headers the javascript would like to use.

     (Ordinary GETs and some POSTs get handled in result_to_response, above,
     without an OPTIONS).

     Our strategy here is: if it's from an allowed origin (i.e., in the canvas
     cors_setting) to return an Access-Control-Allow-Origin header for that
     origin, to return Access-Control-Allow-Headers with the requested headers,
     and Access-Control-Allow-Methods for all of the methods we think might
     be useful.
  *)
  let req_headers =
    Cohttp.Header.get (CRequest.headers req) "access-control-request-headers"
  in
  let allow_headers = match req_headers with Some h -> h | None -> "*" in
  let resp_headers =
    match
      infer_cors_header
        (Header.get (CRequest.headers req) "Origin")
        c.cors_setting
    with
    | None ->
        []
    | Some origin ->
        [ ( "Access-Control-Allow-Methods"
          , "GET,PUT,POST,DELETE,PATCH,HEAD,OPTIONS" )
        ; ("Access-Control-Allow-Origin", origin)
        ; ("Access-Control-Allow-Headers", allow_headers) ]
  in
  Respond
    { resp_headers = Cohttp.Header.of_list resp_headers
    ; execution_id
    ; status = `OK
    ; body = "" }


let result_to_response
    ~(c : Canvas.canvas ref)
    ~(execution_id : Types.id)
    ~(req : CRequest.t)
    (result : RTT.dval) =
  let maybe_infer_cors headers =
    (* Add the Access-Control-ALlow-Origin, if it doens't exist
       and if infer_cors_header tells us to. *)
    infer_cors_header
      (Header.get (CRequest.headers req) "Origin")
      !c.cors_setting
    |> Option.value_map ~default:headers ~f:(fun cors ->
           Header.add_unless_exists headers "Access-Control-Allow-Origin" cors)
  in
  let maybe_infer_ct value resp_headers =
    let inferred_ct =
      match value with
      | RTT.DObj _ | RTT.DList _ ->
          "application/json; charset=utf-8"
      | _ ->
          "text/plain; charset=utf-8"
    in
    (* Add the content-type, if it doesn't exist *)
    Header.add_unless_exists resp_headers "Content-Type" inferred_ct
  in
  match result with
  | RTT.DIncomplete _ ->
      Respond
        { resp_headers = maybe_infer_cors (Header.init ())
        ; execution_id
        ; status = `Internal_server_error
        ; body =
            "Application error: the executed code was not complete. This error can be resolved by the application author by completing the incomplete code."
        }
  | RTT.DError _ ->
      Respond
        { resp_headers = maybe_infer_cors (Header.init ())
        ; execution_id
        ; status = `Internal_server_error
        ; body =
            "Application error: the executed program was invalid. This problem can be resolved by the application's author by resolving the invalid code (often a type error)."
        }
  | RTT.DResp (Redirect url, value) ->
      Redirect
        { headers = Header.init () |> maybe_infer_cors |> Some
        ; uri = Uri.of_string url }
  | RTT.DResp (Response (code, resp_headers), value) ->
      let resp_headers =
        Header.of_list resp_headers |> maybe_infer_ct value |> maybe_infer_cors
      in
      let body =
        match value with
        | DBytes body ->
            (* If the body is a DBytes, don't re-encode it *)
            body |> RTT.RawBytes.to_string
        | _ ->
            let content_type_prefix =
              Header.get resp_headers "Content-Type"
              |> Option.map ~f:(fun ct -> ct |> String.split ~on:';')
              |> Option.bind ~f:List.hd
            in
            ( match content_type_prefix with
            (* TODO: only pretty print for a webbrowser *)
            | Some "text/plain" | Some "application/xml" ->
                Dval.to_enduser_readable_text_v0 value
            | Some "text/html" ->
                Dval.to_enduser_readable_html_v0 value
            | Some "application/json" | _ ->
                Dval.to_pretty_machine_json_v1 value )
      in
      let status = Cohttp.Code.status_of_code code in
      Respond {resp_headers; execution_id; status; body}
  | _ ->
      let body = Dval.to_pretty_machine_json_v1 result in
      (* for demonstrations sake, let's return 200 Okay when
     * no HTTP response object is returned *)
      let resp_headers =
        Header.init () |> maybe_infer_ct result |> maybe_infer_cors
      in
      Respond {resp_headers; execution_id; status = `OK; body}


let user_page_handler
    ~(execution_id : Types.id)
    ~(canvas : string)
    ~(ip : string)
    ~(uri : Uri.t)
    ~(body : string)
    ~(owner : Uuidm.t)
    ~(canvas_id : Uuidm.t)
    (req : CRequest.t) : response_or_redirect_params =
  let verb = req |> CRequest.meth |> Cohttp.Code.string_of_method in
  let headers = req |> CRequest.headers |> Header.to_list in
  let query = req |> CRequest.uri |> Uri.query in
  let c =
    C.load_http_from_cache
      canvas
      ~owner
      ~canvas_id
      ~verb
      ~path:(sanitize_uri_path (Uri.path uri))
    |> Result.map_error ~f:(String.concat ~sep:", ")
    |> Prelude.Result.ok_or_internal_exception "Canvas loading error"
  in
  let pages =
    !c.handlers
    |> TL.http_handlers
    |> Http.filter_matching_handlers ~path:(sanitize_uri_path (Uri.path uri))
  in
  let trace_id = Util.create_uuid () in
  match pages with
  | [] when String.Caseless.equal verb "OPTIONS" ->
      options_handler ~execution_id !c req
      (* If we have a 404, and path is /favicon.ico, then serve the
             * default dark favicon.ico. Because we're matching on [], this code
             * path won't get run if a user has a /favicon.ico handler (or a /*
             * handler!). *)
  | [] when Uri.path uri = "/favicon.ico" ->
      (* NB: we're sending back a png, not an ico - this is deliberate,
             * favicon.ico can be png, and the png is 685 bytes vs a 4+kb .ico.
             * *)
      let filename = "favicon-32x32.png" in
      let filetype = Magic_mime.lookup filename in
      let file = File.readfile ~root:Webroot "favicon-32x32.png" in
      let resp_headers =
        Cohttp.Header.of_list [cors; ("content-type", filetype)]
      in
      Respond {resp_headers; execution_id; status = `OK; body = file}
  | [] ->
      let fof_timestamp =
        PReq.from_request ~allow_unparseable:true uri headers query body
        |> PReq.to_dval
        |> Stored_event.store_event
             ~trace_id
             ~canvas_id
             ("HTTP", Uri.path uri, verb)
      in
      Stroller.push_new_404
        ~execution_id
        ~canvas_id
        ("HTTP", Uri.path uri, verb, fof_timestamp, trace_id) ;
      let resp_headers = Cohttp.Header.of_list [cors] in
      Respond
        { resp_headers
        ; execution_id
        ; status = `Not_found
        ; body = "404 Not Found: No route matches" }
  | a :: b :: _ ->
      let resp_headers = Cohttp.Header.of_list [cors] in
      Respond
        { resp_headers
        ; execution_id
        ; status = `Internal_server_error
        ; body =
            "500 Internal Server Error: More than one handler for route: "
            ^ Uri.path uri }
  | [page] ->
      let input = PReq.from_request uri headers query body in
      ( match (Handler.module_for page, Handler.modifier_for page) with
      | Some m, Some mo ->
          (* Store the event with the input path not the event name, because we
            * want to be able to
            *    a) use this event if this particular handler changes
            *    b) use the input url params in the analysis for this handler
            *)
          let desc = (m, Uri.path uri, mo) in
          ignore
            (Stored_event.store_event
               ~trace_id
               ~canvas_id
               desc
               (PReq.to_dval input))
      | _ ->
          () ) ;
      let bound =
        Libexecution.Execution.http_route_input_vars page (Uri.path uri)
      in
      let result, touched_tlids =
        Libexecution.Execution.execute_handler
          page
          ~execution_id
          ~account_id:!c.owner
          ~canvas_id
          ~user_fns:(Types.IDMap.data !c.user_functions)
          ~user_tipes:(Types.IDMap.data !c.user_tipes)
          ~package_fns:!c.package_fns
          ~secrets:(Secret.secrets_in_canvas !c.id)
          ~tlid:page.tlid
          ~dbs:(TL.dbs !c.dbs)
          ~input_vars:([("request", PReq.to_dval input)] @ bound)
          ~store_fn_arguments:
            (Stored_function_arguments.store ~canvas_id ~trace_id)
          ~store_fn_result:(Stored_function_result.store ~canvas_id ~trace_id)
      in
      Stroller.push_new_trace_id
        ~execution_id
        ~canvas_id
        trace_id
        (page.tlid :: touched_tlids) ;
      result_to_response ~c ~execution_id ~req result



(* -------------------------------------------- *)
(* The server *)
(* -------------------------------------------- *)

let static_etag_for =
  (* Read the etags.json JSON document here so that it reads
     at load-time, not at each call-time.

     The server gets restarted after we get new etags, so we
     don't need to worry about updates here. *)
  let etags_json =
    File.readfile ~root:Config.Webroot "etags.json" |> Yojson.Basic.from_string
  in
  fun uri ->
    try
      etags_json
      (* Get the JSON field that corresponds to the filename,
         stripped of the leftmost /. *)
      |> Yojson.Basic.Util.member
           (uri |> Uri.path |> String.lstrip ~drop:(( = ) '/'))
      |> Yojson.Basic.Util.to_string
      |> fun x -> [("etag", x)]
    with e -> []


let static_handler uri =
  let fname = S.resolve_file ~docroot:(Config.dir Config.Webroot) ~uri in
  S.respond_file
    ~headers:(Header.of_list (cors :: static_etag_for uri))
    ~fname
    ()


let route_host req =
  match
    req
    |> CRequest.uri
    |> Uri.host
    |> Option.value ~default:""
    |> fun h -> String.split h '.'
  with
  | ["static"; "darklang"; "localhost"]
  | ["static"; "darklang"; "lvh"; "me"]
  | ["static"; "darklang"; "com"]
  | [_; "ngrok"; "io"] ->
      Some Static
  (* Dark canvases *)
  | [a; "builtwithdark"; "com"]
  (* Route *.darkcustomdomain.com same as we do *.builtwithdark.com - it's
   * just another load balancer *)
  | [a; "darkcustomdomain"; "com"]
  | [a; "builtwithdark"; "localhost"]
  | [a; "builtwithdark"; "lvh"; "me"] ->
      Some (Canvas a)
  (* Specific Dark canvas: builtwithdark *)
  | ["builtwithdark"; "localhost"]
  | ["builtwithdark"; "lvh"; "me"]
  | ["builtwithdark"; "com"] ->
      Some (Canvas "builtwithdark")
  (* Customers - do not remove the marker below *)
  (* ACD-route_host-MARKER *)
  | [a; "dabblefox"; "com"] ->
      Some (Canvas ("dabblefox-" ^ a))
  | ["www"; "hellobirb"; "com"] | ["hellobirb"; "com"] ->
      Some (Canvas "pixelkeet")
  (* admin interface + outer site, conditionally *)
  | ["darklang"; "com"]
  | ["darklang"; "localhost"]
  | ["darklang"; "lvh"; "me"]
  | ["dark_dev"; "com"] ->
      Some Admin
  (* No match in the above hard-coded parts; let's try the db (though that may
   * still return None) *)
  | parts ->
      canvas_from_db_opt parts


let db_conn_readiness_check () : string option =
  match Dbconnection.status () with
  | `Healthy ->
      None
  | `Disconnected ->
      Some "Dbconnection.status = `Disconnected"


let stroller_readiness_check () : string option Lwt.t =
  match%lwt Stroller.status () with
  | `Healthy ->
      Lwt.return None
  | `Unconfigured ->
      Lwt.return (Some "Stroller.status = `Unconfigured")
  | `Unhealthy s ->
      Lwt.return (Some ("Stroller.status = `Unhealthy: " ^ s))


let k8s_handler
    (parent : Span.t)
    (req : CRequest.t)
    ~(execution_id : Types.id)
    ~(stopper : unit Lwt.u) : (CResponse.t * Cl.Body.t) Lwt.t =
  let%lwt stroller_readiness_check = stroller_readiness_check () in
  match req |> CRequest.uri |> Uri.path with
  (* For GKE health check *)
  | "/" ->
    ( match Dbconnection.status () with
    | `Healthy ->
        if not !ready
           (* ie. liveness check has found a service with 2 minutes of failing readiness checks *)
        then (
          Log.infO "Liveness check found unready service, returning unavailable" ;
          respond ~execution_id parent `Service_unavailable "Service not ready"
          )
        else respond ~execution_id parent `OK "Hello internal overlord"
    | `Disconnected ->
        respond
          ~execution_id
          parent
          `Service_unavailable
          "Sorry internal overlord" )
  | "/ready" ->
      let checks =
        [db_conn_readiness_check (); stroller_readiness_check]
        |> List.filter_map ~f:(fun x -> x)
      in
      ( match checks with
      | [] ->
          if !ready
          then respond ~execution_id parent `OK "Hello internal overlord"
          else (
            Log.infO "Service ready" ;
            ready := true ;
            respond ~execution_id parent `OK "Hello internal overlord" )
      | _ ->
          Log.erroR
            ("Failed readiness check(s): " ^ String.concat checks ~sep:": ") ;
          respond
            ~execution_id
            parent
            `Service_unavailable
            "Sorry internal overlord" )
  (* For GKE graceful termination *)
  | "/pkill" ->
      if not !shutdown (* note: this is a ref, not a boolean `not` *)
      then (
        shutdown := true ;
        Log.infO
          "shutdown"
          ~data:"Received shutdown request - shutting down"
          ~params:[("execution_id", Types.string_of_id execution_id)] ;
        (* k8s gives us 30 seconds, so ballpark 2s for overhead *)
        Lwt_unix.sleep 28.0
        >>= fun _ ->
        Lwt.wakeup stopper () ;
        respond ~execution_id parent `OK "Terminated" )
      else (
        Log.infO
          "shutdown"
          ~data:"Received redundant shutdown request - already shutting down" ;
        respond ~execution_id parent `OK "Terminated" )
  | _ ->
      respond ~execution_id parent `Not_found ""


let coalesce_head_to_get (req : CRequest.t) : CRequest.t =
  let verb = req |> CRequest.meth in
  match verb with
  (* transform HEAD req method to GET*)
  | `HEAD ->
      CRequest.make (* GET is default value to ?meth argument*)
        ?version:(Some req.version)
        ?encoding:(Some req.encoding)
        ?headers:(Some req.headers)
        (CRequest.uri req)
  | _ ->
      req


let canvas_handler
    ~(execution_id : Types.id)
    ~(canvas : string)
    ~(ip : string)
    ~(uri : Uri.t)
    ~(body : string)
    (parent : Span.t)
    (req : CRequest.t) : (Cohttp.Response.t * Cl.Body.t) Lwt.t =
  let verb = req |> CRequest.meth in
  match Account.for_host canvas with
  | None ->
      respond ~execution_id parent `Not_found "user not found"
  | Some owner ->
      let canvas_id = Serialize.fetch_canvas_id owner canvas in
      (* TODO make sure this resolves before returning *)
      let%lwt resp, body =
        match verb with
        (* transform HEAD req method to GET, discards body in response*)
        | `HEAD ->
            user_page_handler
              ~execution_id
              ~canvas
              ~canvas_id
              ~ip
              ~uri
              ~body
              ~owner
              (coalesce_head_to_get req)
            |> respond_or_redirect_empty_body parent
        | _ ->
            user_page_handler
              ~execution_id
              ~canvas
              ~canvas_id
              ~ip
              ~uri
              ~body
              ~owner
              req
            |> respond_or_redirect parent
      in
      Lwt.async (fun () ->
          Stroller.heapio_track
            ~canvas_id
            ~canvas
            ~execution_id
            ~user_id:owner
            ~event:"canvas_traffic"
            Track
            (`Assoc
              [ ("verb", `String (verb |> Cohttp.Code.string_of_method))
              ; ("path", `String (uri |> Uri.path))
              ; ( "useragent"
                , `String
                    ( req
                    |> CRequest.headers
                    |> fun hs ->
                    Cohttp.Header.get hs Cohttp.Header.user_agent
                    |> Option.value ~default:"" ) )
              ; ("ip", `String ip)
              ; ( "status"
                , `Int
                    ( resp
                    |> Cohttp.Response.status
                    |> Cohttp.Code.code_of_status ) ) ])) ;

      Lwt.return (resp, body)


let callback
    ~k8s_callback
    (parent : Span.t)
    (ip : string)
    (req : CRequest.t)
    (body : string)
    (execution_id : Types.id) : (CResponse.t * Cl.Body.t) Lwt.t =
  let req = canonicalize_request req in
  let uri = CRequest.uri req in
  let handle_error ~(include_internals : bool) (e : exn) =
    try
      let bt = Exception.get_backtrace () in
      let%lwt _ =
        match e with
        | Exception.DarkException e when e.tipe = EndUser ->
            Lwt.return `Disabled
        | _ ->
            Rollbar.report_lwt
              e
              bt
              (Remote (request_to_rollbar body req))
              (Types.show_id execution_id)
      in
      let real_err =
        try
          match e with
          | Pageable.PageableExn (Exception.DarkException e)
          | Exception.DarkException e ->
              e
              |> Exception.exception_data_to_yojson
              |> Yojson.Safe.pretty_to_string
          | Yojson.Json_error msg ->
              "Not a valid JSON value: '" ^ msg ^ "'"
          | _ ->
              "Dark Internal Error: " ^ Exn.to_string e
        with _ -> "UNHANDLED ERROR: real_err"
      in
      let real_err =
        real_err
        (* Commented out because API handlers need to be JSON decoded *)
        (* ^ (Exception.get_backtrace () *)
        (*             |> Exception.backtrace_to_string) *)
      in
      Log.erroR
        real_err
        ~bt
        ~params:[("execution_id", Types.string_of_id execution_id)] ;
      match e with
      | Exception.DarkException e when e.tipe = EndUser ->
          respond ~execution_id parent `Bad_request e.short
      | Exception.DarkException e when e.tipe = DarkClient ->
          respond ~execution_id parent `Bad_request real_err
      | _ ->
          let body =
            if include_internals || Config.show_stacktrace
            then real_err
            else
              "Dark Internal Error: Dark - the service running this application - encountered an error. This problem is a bug in Dark, we're sorry! Our automated systems have noted this error and we are working to resolve it. The author of this application can post in our slack (darkcommunity.slack.com) for more information."
          in
          respond ~execution_id parent `Internal_server_error body
    with e ->
      let bt = Exception.get_backtrace () in
      Rollbar.last_ditch e ~bt "handle_error" (Types.show_id execution_id) ;
      respond ~execution_id parent `Internal_server_error "unhandled error"
  in
  try
    (* first: if this isn't https and should be, redirect *)
    match redirect_to (CRequest.uri req) with
    | Some x ->
        S.respond_redirect ~uri:x ()
    | None ->
      ( match
          ( CRequest.meth req
          , Cohttp.Header.get (CRequest.headers req) "content-type" )
        with
      | `POST, Some s when String.lowercase s = "text/ping" ->
          (* I'm a teapot! We don't support text/ping:
           * https://dev.w3.org/html5/pf-summary/structured-client-side-storage.html#hyperlink-auditing *)
          (* Impl'd here since it doesn't matter which canvas we hit, or if it
           * exists at all *)
          respond ~execution_id parent (Cohttp.Code.status_of_code 418) ""
      | _ ->
        ( match Uri.to_string uri with
        | "/sitemap.xml" | "/favicon.ico" ->
            respond ~execution_id parent `OK ""
        | _ ->
          (* figure out what handler to dispatch to... *)
          ( match route_host req with
          | Some (Canvas canvas) ->
              canvas_handler ~execution_id ~canvas ~ip ~uri ~body parent req
          | Some Static ->
              static_handler uri
          | Some Admin ->
            ( try
                authenticate_then_handle
                  parent
                  ~execution_id
                  (fun ~session ~csrf_token span r ->
                    try
                      admin_handler
                        ~execution_id
                        ~uri
                        ~body
                        ~session
                        ~csrf_token
                        span
                        r
                    with e -> handle_error ~include_internals:true e)
                  req
                  body
              with e -> handle_error ~include_internals:false e )
          | None ->
              k8s_callback req ~execution_id ) ) )
  with e -> handle_error ~include_internals:false e


let server () =
  let stop, stopper = Lwt.wait () in
  let cbwb (conn : S.conn) (req : CRequest.t) (req_body : Cl.Body.t) :
      (CResponse.t * Cl.Body.t) Lwt.t =
    let%lwt body_string = Cl.Body.to_string req_body in
    let execution_id = Util.create_id () in
    let uri = CRequest.uri req in
    (* use the x-forwarded-for ip, falling back to the raw ip in the request *)
    let ip =
      let ch, _ = conn in
      Header.get (CRequest.headers req) "X-forwarded-for"
      |> Option.bind ~f:(fun str -> str |> String.split ~on:';' |> List.hd)
      |> Option.map ~f:String.strip
      |> Option.value ~default:(get_ip_address ch)
    in
    let headers = CRequest.headers req in
    Telemetry.with_root "http.request" (fun span ->
        (* attribute names copied from honeycomb's beeline-go *)
        Span.set_attrs
          span
          [ ("meta.type", `String "http_request")
          ; ("meta.server_version", `String Config.build_hash)
          ; ("execution_id", `String (Types.string_of_id execution_id))
          ; ("request.remote_addr", `String ip)
          ; ( "request.method"
            , `String (req |> CRequest.meth |> Cohttp.Code.string_of_method) )
          ; ("request.url", `String (Uri.to_string uri))
          ; ("request.path", `String (Uri.path uri)) ] ;

        [ ("request.header.user_agent", Cohttp.Header.get headers "user-agent")
        ; ("request.host", Uri.host uri)
        ; ("request.method", Uri.verbatim_query uri) ]
        |> List.iter ~f:(function
               | k, Some v ->
                   Span.set_attr span k (`String v)
               | _ ->
                   ()) ;
        callback
          ~k8s_callback:(k8s_handler span ~stopper)
          span
          ip
          req
          body_string
          execution_id)
  in
  S.create ~stop ~mode:(`TCP (`Port Config.port)) (S.make ~callback:cbwb ())


let run () =
  ignore (Lwt_main.run (Nocrypto_entropy_lwt.initialize () >>= server))
