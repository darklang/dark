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

(* ------------------------------- *)
(* utils *)
(* ------------------------------- *)

type timing_header = string * float * string

let shutdown = ref false

let ready = ref false

let server_timing (times : timing_header list) =
  times
  |> List.map ~f:(fun (name, time, desc) ->
         name
         ^ ";desc=\""
         ^ desc
         ^ "\""
         ^ ";dur="
         ^ (time |> Float.to_string_hum ~decimals:3) )
  |> String.concat ~sep:","
  |> fun x -> [("Server-timing", x)] |> Header.of_list


let time (name : string) (fn : _ -> 'a) : timing_header * 'a =
  let start = Unix.gettimeofday () in
  let result = fn () in
  let finish = Unix.gettimeofday () in
  ((name, (finish -. start) *. 1000.0, name), result)


let get_ip_address ch : string =
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
  | Redirect of {uri : Uri.t; headers : Header.t option}

let respond_or_redirect (params : response_or_redirect_params) =
  match params with
  | Redirect {uri; headers} ->
      S.respond_redirect ?headers ~uri ()
  | Respond {resp_headers; execution_id; status; body} ->
      let resp_headers =
        Header.add
          resp_headers
          "X-Darklang-Execution-ID"
          (Types.string_of_id execution_id)
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
      Log.infO
        "response"
        ~jsonparams:
          [ ("status", `Int (Cohttp.Code.code_of_status status))
          ; ("body_bytes", `Int (String.length body)) ]
        ~params:
          [ ("execution_id", Int63.to_string execution_id)
            (* TODO ismith: maybe a ,-sep list of headers, and then a selection of
        * whitelisted headers? Needs to be flattened. *)
          ; ("headers", Log.dump resp_headers) ] ;
      S.respond_string ~status ~body ~headers:resp_headers ()


let respond_or_redirect_empty_body (params : response_or_redirect_params) =
  match params with
  | Redirect _ ->
      respond_or_redirect params
  | Respond r ->
      let headers =
        Header.add
          r.resp_headers
          "Content-Length"
          (string_of_int (String.length r.body))
      in
      respond_or_redirect (Respond {r with body = ""; resp_headers = headers})


let respond
    ?(resp_headers = Header.init ())
    ~(execution_id : Types.id)
    status
    (body : string) =
  respond_or_redirect (Respond {resp_headers; execution_id; status; body})


let should_use_https uri =
  let parts =
    uri |> Uri.host |> Option.value ~default:"" |> fun h -> String.split h '.'
  in
  match parts with
  | ["darklang"; "com"]
  | ["builtwithdark"; "com"]
  | [_; "builtwithdark"; "com"]
  | ["hellobirb"; "com"]
  | ["www"; "hellobirb"; "com"] ->
      true
  | _ ->
      false


let redirect_to uri =
  let proto = uri |> Uri.scheme |> Option.value ~default:"" in
  (* If it's http and on a domain that can be served with https,
     we want to redirect to the same url but with the scheme
     replaced by "https". *)
  if proto = "http" && should_use_https uri
  then Some "https" |> Uri.with_scheme uri |> Some
  else None


(* there might be some better way to do this... *)
let over_headers (r : CResponse.t) ~(f : Header.t -> Header.t) : CResponse.t =
  CResponse.make
    ~version:(CResponse.version r)
    ~status:(CResponse.status r)
    ~flush:(CResponse.flush r)
    ~encoding:(CResponse.encoding r)
    ~headers:(r |> CResponse.headers |> f)
    ()


let over_headers_promise
    (resp_promise : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t)
    ~(f : Header.t -> Header.t) :
    (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  let%lwt resp, body = resp_promise in
  return (over_headers ~f resp, body)


let wrap_editor_api_headers =
  let headers =
    [ ("Content-type", "application/json; charset=utf-8")
    ; ("X-Darklang-Server-Version", Config.build_hash) ]
  in
  over_headers_promise ~f:(fun h -> Header.add_list h headers)


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
  (* if there's no explicit canvas setting, * is the default. *)
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


let options_handler
    ~(execution_id : Types.id) (c : C.canvas) (req : CRequest.t) =
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
           Header.add_unless_exists headers "Access-Control-Allow-Origin" cors
       )
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
  | RTT.DIncomplete ->
      Respond
        { resp_headers = maybe_infer_cors (Header.init ())
        ; execution_id
        ; status = `Internal_server_error
        ; body =
            "Application error: the executed code was not complete. This error can be resolve by the application author by completing the incomplete code."
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
      let content_type = Header.get resp_headers "Content-Type" in
      let body =
        match content_type with
        (* TODO: only pretty print for a webbrowser *)
        | Some content_type
          when String.is_prefix ~prefix:"text/plain" content_type
               || String.is_prefix ~prefix:"application/xml" content_type ->
            Dval.to_enduser_readable_text_v0 value
        | Some content_type
          when String.is_prefix ~prefix:"text/html" content_type ->
            Dval.to_enduser_readable_html_v0 value
        | Some content_type
          when String.is_prefix ~prefix:"application/json" content_type ->
            Dval.to_pretty_machine_json_v1 value
        | _ ->
          ( match value with
          | DBytes body ->
              body |> RTT.RawBytes.to_string
          | _ ->
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
    (req : CRequest.t) =
  Log.infO "user_page_handler" ;
  let verb = req |> CRequest.meth |> Cohttp.Code.string_of_method in
  let headers = req |> CRequest.headers |> Header.to_list in
  let query = req |> CRequest.uri |> Uri.query in
  let owner = Account.for_host canvas in
  match owner with
  | None ->
      (* Account doesn't exist! Don't store the event or do any loading *)
      let resp_headers = Cohttp.Header.of_list [cors] in
      Respond
        { resp_headers
        ; execution_id
        ; status = `Not_found
        ; body = "404 Not Found: No route matches" }
  | Some owner ->
      let c =
        C.load_http canvas owner ~verb ~path:(sanitize_uri_path (Uri.path uri))
        |> Result.map_error ~f:(String.concat ~sep:", ")
        |> Prelude.Result.ok_or_internal_exception "Canvas loading error"
      in
      let pages =
        !c.handlers
        |> TL.http_handlers
        |> Http.filter_matching_handlers
             ~path:(sanitize_uri_path (Uri.path uri))
      in
      let trace_id = Util.create_uuid () in
      let canvas_id = !c.id in
      ( match pages with
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
              ~tlid:page.tlid
              ~dbs:(TL.dbs !c.dbs)
              ~input_vars:([("request", PReq.to_dval input)] @ bound)
              ~store_fn_arguments:
                (Stored_function_arguments.store ~canvas_id ~trace_id)
              ~store_fn_result:
                (Stored_function_result.store ~canvas_id ~trace_id)
          in
          Stroller.push_new_trace_id
            ~execution_id
            ~canvas_id
            trace_id
            (page.tlid :: touched_tlids) ;
          result_to_response ~c ~execution_id ~req result )


(* -------------------------------------------- *)
(* Admin server *)
(* -------------------------------------------- *)
let static_assets_upload_handler
    ~(execution_id : Types.id) (canvas : string) (username : string) req body :
    (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  try
    let canvas = Canvas.id_for_name canvas in
    try
      let ct =
        match Cohttp.Header.get (CRequest.headers req) "content-type" with
        | Some s ->
            s
        | None ->
            "error"
      in
      (* making branch a request-configurable option requires product work:
        * https://trello.com/c/pAD4uoJc/520-figure-out-branch-feature-for-static-assets
       *)
      let branch = "main" in
      let sa =
        Static_assets.start_static_asset_deploy canvas branch username
      in
      Stroller.push_new_static_deploy ~execution_id ~canvas_id:canvas sa ;
      let deploy_hash = sa.deploy_hash in
      let%lwt stream = Multipart.parse_stream (Lwt_stream.of_list [body]) ct in
      let%lwt upload_results =
        let%lwt parts = Multipart.get_parts stream in
        let files =
          (Multipart.StringMap.filter (fun _ v ->
               match v with `File _ -> true | `String _ -> false ))
            parts
        in
        let files =
          Multipart.StringMap.fold
            (fun _ v acc ->
              List.cons
                ( match v with
                | `File f ->
                    f
                | _ ->
                    Exception.internal "didn't expect a non-`File here" )
                acc )
            files
            ([] : Multipart.file List.t)
        in
        let processfile file =
          let filename = Multipart.file_name file in
          (* file_stream gives us a stream of strings; get a single string out
              of it *)
          let%lwt body =
            Lwt_stream.fold_s
              (fun elt acc -> Lwt.return (acc ^ elt))
              (Multipart.file_stream file)
              ""
          in
          (* Replace DARK_STATIC_ASSETS_BASE_URL with the deployed URL. This
           * will allow users to create SPAs with a sentinel value in them to
           * converts to the absolute url. In React, you would do this with
           * PUBLIC_URL. *)
          let body =
            let filetype = Magic_mime.lookup filename in
            let is_valid_text body =
              body |> Libexecution.Unicode_string.of_string |> Option.is_some
            in
            (* Other mime type prefixes are video, image, audio,
             * chemical, model, x-conference and can be ignored without
             * the expensive conversion check *)
            if String.is_prefix ~prefix:"video" filetype
               || String.is_prefix ~prefix:"image" filetype
               || String.is_prefix ~prefix:"audio" filetype
               || String.is_prefix ~prefix:"chemical" filetype
               || String.is_prefix ~prefix:"model" filetype
               || String.is_prefix ~prefix:"x-conference" filetype
            then body
            else if String.is_prefix ~prefix:"text" filetype
                    || is_valid_text body
                    (* application/ or unknown and valid UTF-8*)
            then
              String.substr_replace_all
                body
                ~pattern:"DARK_STATIC_ASSETS_BASE_URL"
                ~with_:sa.url
            else (* application/* or unknown and _not_ valid UTF-8 *)
              body
          in
          Static_assets.upload_to_bucket filename body canvas deploy_hash
        in
        Lwt.return (files |> List.map ~f:processfile)
      in
      let%lwt _, errors =
        upload_results
        |> Lwt_list.partition_p (fun r ->
               match%lwt r with
               | Ok _ ->
                   Lwt.return true
               | Error _ ->
                   Lwt.return false )
      in
      let deploy =
        Static_assets.finish_static_asset_deploy canvas deploy_hash
      in
      Stroller.push_new_static_deploy ~execution_id ~canvas_id:canvas deploy ;
      match errors with
      | [] ->
          respond
            ~execution_id
            `OK
            ( Yojson.Safe.to_string
                (`Assoc
                  [ ("deploy_hash", `String deploy_hash)
                  ; ( "url"
                    , `String (Static_assets.url canvas deploy_hash `Short) )
                  ; ( "long-url"
                    , `String (Static_assets.url canvas deploy_hash `Long) ) ])
            |> Yojson.Basic.prettify )
      | _ ->
          let err_strs =
            errors
            |> Lwt_list.map_p (fun e ->
                   match%lwt e with
                   | Error (`GcloudAuthError s) ->
                       Lwt.return s
                   | Error (`FailureUploadingStaticAsset s) ->
                       Lwt.return s
                   | Error (`FailureDeletingStaticAsset s) ->
                       Lwt.return s
                   | Ok _ ->
                       Exception.internal
                         "Can't happen, we partition error/ok above." )
          in
          err_strs
          >>= (function
          | err_strs ->
              Log.erroR
                "Failed to deploy static assets to "
                ~params:
                  [ ("canvas", Canvas.name_for_id canvas)
                  ; ("errs", String.concat ~sep:";" err_strs) ] ;
              Static_assets.delete_static_asset_deploy
                canvas
                branch
                username
                deploy_hash ;
              respond
                ~resp_headers:(server_timing []) (* t1; t2; etc *)
                ~execution_id
                `Internal_server_error
                ( Yojson.Safe.to_string
                    (`Assoc
                      [ ( "msg"
                        , `String "We couldn't put this upload in gcloud." )
                      ; ( "execution_id"
                        , `String (Types.string_of_id execution_id) )
                      ; ( "errors"
                        , `List (List.map ~f:(fun s -> `String s) err_strs) )
                      ])
                |> Yojson.Basic.prettify ))
    with e -> raise e
  with _ -> respond ~execution_id `Not_found "Not found"


let admin_add_op_handler ~(execution_id : Types.id) (host : string) body :
    (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  let t1, params =
    time "1-read-api-ops" (fun _ -> Api.to_add_op_rpc_params body)
  in
  let ops = params.ops in
  let tlids = List.filter_map ~f:Op.tlidOf ops in
  let t2, maybe_c =
    time "2-load-saved-ops" (fun _ -> C.load_only_tlids ~tlids host ops)
  in
  match maybe_c with
  | Ok c ->
      let t3, result =
        time "3-to-frontend" (fun _ -> Analysis.to_add_op_rpc_result !c)
      in
      let t4, _ =
        time "4-save-to-disk" (fun _ ->
            (* work out the result before we save it, incase it has a
              stackoverflow or other crashing bug *)
            if Api.causes_any_changes params then C.save_tlids !c tlids else ()
        )
      in
      let t5, strollerMsg =
        (* To make this work with prodclone, we might want to have it specify
         * more ... else people's prodclones will stomp on each other ... *)
        time "5-send-ops-to-stroller" (fun _ ->
            let owner = Account.for_host_exn host in
            let canvas_id = Serialize.fetch_canvas_id owner host in
            if Api.causes_any_changes params
            then (
              let strollerMsg =
                {result; params}
                |> Analysis.add_op_stroller_msg_to_yojson
                |> Yojson.Safe.to_string
              in
              Stroller.push_new_event
                ~execution_id
                ~canvas_id
                ~event:"add_op"
                strollerMsg ;
              Some strollerMsg )
            else None )
      in
      respond
        ~resp_headers:(server_timing [t1; t2; t3; t4; t5])
        ~execution_id
        `OK
        (* if no changes are made, we return an empty string - does this cause a
         * client error? *)
        (Option.value ~default:"" strollerMsg)
  | Error errs ->
      let body = String.concat ~sep:", " errs in
      respond
        ~resp_headers:(server_timing [t1; t2])
        ~execution_id
        `Bad_request
        body


let initial_load
    ~(execution_id : Types.id)
    ~(username : Account.username)
    ~(canvas : string)
    ~(permission : Authorization.permission option)
    body : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  try
    let t1, c =
      time "1-load-saved-ops" (fun _ ->
          C.load_all canvas []
          |> Result.map_error ~f:(String.concat ~sep:", ")
          |> Prelude.Result.ok_or_internal_exception "Failed to load canvas" )
    in
    let t2, unlocked =
      time "2-analyze-unlocked-dbs" (fun _ -> Analysis.unlocked !c)
    in
    let t3, f404s =
      let latest = Time.sub (Time.now ()) (Time.Span.of_day 7.0) in
      time "3-get-404s" (fun _ -> Analysis.get_404s ~since:latest !c)
    in
    let t4, traces =
      time "4-traces" (fun _ ->
          let htraces =
            !c.handlers
            |> TL.handlers
            |> List.map ~f:(fun h ->
                   Analysis.traceids_for_handler !c h
                   |> List.map ~f:(fun traceid -> (h.tlid, traceid)) )
            |> List.concat
          in
          let uftraces =
            !c.user_functions
            |> Types.IDMap.data
            |> List.map ~f:(fun uf ->
                   Analysis.traceids_for_user_fn !c uf
                   |> List.map ~f:(fun traceid -> (uf.tlid, traceid)) )
            |> List.concat
          in
          htraces @ uftraces )
    in
    let t5, assets =
      time "5-static-assets" (fun _ -> SA.all_deploys_in_canvas !c.id)
    in
    let t6, result =
      time "6-to-frontend" (fun _ ->
          Analysis.to_initial_load_rpc_result
            !c
            permission
            f404s
            traces
            unlocked
            assets )
    in
    respond
      ~execution_id
      ~resp_headers:(server_timing [t1; t2; t3; t4; t5; t6])
      `OK
      result
  with e -> Libexecution.Exception.reraise_as_pageable e


let execute_function ~(execution_id : Types.id) (host : string) body :
    (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  let t1, params =
    time "1-read-api-ops" (fun _ -> Api.to_execute_function_rpc_params body)
  in
  let t2, c =
    time "2-load-saved-ops" (fun _ ->
        C.load_with_context ~tlids:[params.tlid] host []
        |> Result.map_error ~f:(String.concat ~sep:", ")
        |> Prelude.Result.ok_or_internal_exception "Failed to load canvas" )
  in
  let t3, (result, tlids) =
    time "3-execute" (fun _ ->
        Analysis.execute_function
          !c
          params.fnname
          ~execution_id
          ~tlid:params.tlid
          ~trace_id:params.trace_id
          ~caller_id:params.caller_id
          ~args:params.args )
  in
  let t4, unlocked =
    time "4-analyze-unlocked-dbs" (fun _ -> Analysis.unlocked !c)
  in
  let t5, response =
    time "5-to-frontend" (fun _ ->
        Analysis.to_execute_function_rpc_result
          (Dval.hash params.args)
          tlids
          unlocked
          result )
  in
  respond
    ~execution_id
    ~resp_headers:(server_timing [t1; t2; t3; t4])
    `OK
    response


let trigger_handler ~(execution_id : Types.id) (host : string) body :
    (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  let t1, params =
    time "1-read-api-params" (fun _ -> Api.to_trigger_handler_rpc_params body)
  in
  let t2, c =
    time "2-load-saved-ops" (fun _ ->
        C.load_with_context ~tlids:[params.tlid] host []
        |> Result.map_error ~f:(String.concat ~sep:", ")
        |> Prelude.Result.ok_or_internal_exception "Failed to load canvas" )
  in
  let t3, touched_tlids =
    time "3-execute" (fun _ ->
        let handler_and_desc =
          Map.find_exn !c.handlers params.tlid
          |> TL.as_handler
          |> Option.bind ~f:(fun h ->
                 Handler.event_desc_for h |> Option.map ~f:(fun d -> (h, d)) )
        in
        match handler_and_desc with
        | None ->
            []
        | Some (handler, desc) ->
            let canvas_id = !c.id in
            let trace_id = params.trace_id in
            let _, touched_tlids =
              Libexecution.Execution.execute_handler
                handler
                ~execution_id
                ~tlid:params.tlid
                ~input_vars:params.input
                ~dbs:(TL.dbs !c.dbs)
                ~user_tipes:(!c.user_tipes |> Map.data)
                ~user_fns:(!c.user_functions |> Map.data)
                ~account_id:!c.owner
                ~canvas_id
                ~store_fn_arguments:
                  (Stored_function_arguments.store ~canvas_id ~trace_id)
                ~store_fn_result:
                  (Stored_function_result.store ~canvas_id ~trace_id)
            in
            touched_tlids )
  in
  let t4, response =
    time "4-to-frontend" (fun _ ->
        Analysis.to_trigger_handler_rpc_result (params.tlid :: touched_tlids)
    )
  in
  respond
    ~execution_id
    ~resp_headers:(server_timing [t1; t2; t3; t4])
    `OK
    response


let get_trace_data ~(execution_id : Types.id) (host : string) (body : string) :
    (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  try
    let t1, params =
      time "1-read-api-tlids" (fun _ -> Api.to_get_trace_data_rpc_params body)
    in
    let tlid = params.tlid in
    let trace_id = params.trace_id in
    let t2, c =
      time "2-load-saved-ops" (fun _ ->
          C.load_only_tlids ~tlids:[params.tlid] host []
          |> Result.map_error ~f:(String.concat ~sep:", ")
          |> Prelude.Result.ok_or_internal_exception "Failed to load canvas" )
    in
    let t3, mht =
      time "3-handler-analyses" (fun _ ->
          !c.handlers
          |> Types.IDMap.data
          |> List.hd
          |> Option.bind ~f:TL.as_handler
          |> Option.map ~f:(fun h -> Analysis.handler_trace !c h trace_id) )
    in
    let t4, mft =
      time "4-user-fn-analyses" (fun _ ->
          !c.user_functions
          |> Types.IDMap.data
          |> List.find ~f:(fun f -> tlid = f.tlid)
          |> Option.map ~f:(fun f -> Analysis.user_fn_trace !c f trace_id) )
    in
    let t5, result =
      time "5-to-frontend" (fun _ ->
          Option.first_some mft mht
          |> Option.map ~f:(Analysis.to_get_trace_data_rpc_result !c) )
    in
    let resp_headers = server_timing [t1; t2; t3; t4; t5] in
    match result with
    | Some str ->
        respond ~execution_id ~resp_headers `OK str
    | None ->
        respond ~execution_id ~resp_headers `Not_found ""
  with e -> raise e


let db_stats ~(execution_id : Types.id) (host : string) (body : string) :
    (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  try
    let t1, params =
      time "1-read-api-tlids" (fun _ -> Api.to_db_stats_rpc_params body)
    in
    let t2, c =
      time "2-load-saved-ops" (fun _ ->
          C.load_all_dbs host []
          |> Result.map_error ~f:(String.concat ~sep:", ")
          |> Prelude.Result.ok_or_internal_exception "Failed to load canvas" )
    in
    let t3, stats =
      time "3-analyze-db-stats" (fun _ -> Analysis.db_stats !c params.tlids)
    in
    let t4, result =
      time "4-to-frontend" (fun _ -> Analysis.to_db_stats_rpc_result stats)
    in
    respond ~execution_id ~resp_headers:(server_timing [t1; t2; t3]) `OK result
  with e -> raise e


let get_unlocked_dbs ~(execution_id : Types.id) (host : string) (body : string)
    : (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  try
    let t1, c =
      time "1-load-saved-ops" (fun _ ->
          C.load_all_dbs host []
          |> Result.map_error ~f:(String.concat ~sep:", ")
          |> Prelude.Result.ok_or_internal_exception "Failed to load canvas" )
    in
    let t2, unlocked =
      time "2-analyze-unlocked-dbs" (fun _ -> Analysis.unlocked !c)
    in
    let t3, result =
      time "3-to-frontend" (fun _ ->
          Analysis.to_get_unlocked_dbs_rpc_result unlocked !c )
    in
    respond ~execution_id ~resp_headers:(server_timing [t1; t2; t3]) `OK result
  with e -> raise e


let delete_404 ~(execution_id : Types.id) (host : string) body :
    (Cohttp.Response.t * Cohttp_lwt__.Body.t) Lwt.t =
  try
    let t1, cid =
      time "1-get-canvas-id" (fun _ ->
          let owner = Account.for_host_exn host in
          Serialize.fetch_canvas_id owner host )
    in
    let t2, p = time "2-to-route-params" (fun _ -> Api.to_route_params body) in
    let t3, _ =
      time "3-delete-404s" (fun _ ->
          Analysis.delete_404s cid p.space p.path p.modifier )
    in
    respond
      ~execution_id
      ~resp_headers:(server_timing [t1; t2; t3])
      `OK
      "{ \"result\" : \"deleted\" } "
  with e -> raise e


let hashed_filename (file : string) (hash : string) : string =
  match List.rev (String.split ~on:'.' file) with
  | [] ->
      Exception.internal "Tried splitting an empty filename key"
  | [_] ->
      Exception.internal "Tried splitting a filename key with no extension"
  | ext :: rest ->
      sprintf "%s-%s.%s" (String.concat ~sep:"." (List.rev rest)) hash ext


let to_assoc_list etags_json : (string * string) list =
  match etags_json with
  | `Assoc alist ->
      let mutated =
        List.filter_map
          ~f:(fun (fst, snd) ->
            match snd with `String inner -> Some (fst, inner) | _ -> None )
          alist
      in
      if List.length mutated <> List.length alist
      then Exception.internal "Some asset in etags.json lacked a hash value."
      else mutated
  | _ ->
      Exception.internal "etags.json must be a top-level object."


let admin_ui_html
    ~(canvas_id : Uuidm.t)
    ~(csrf_token : string)
    ~(local : string option)
    username
    admin =
  let template = File.readfile_lwt ~root:Templates "ui.html" in
  let static_host =
    match local with
    (* TODO: if you want access, we can make this more general *)
    | Some username ->
        "darklang-" ^ username ^ ".ngrok.io"
    | _ ->
        Config.static_host
  in
  let rollbar_js = Config.rollbar_js in
  let hash_static_filenames =
    if local = None then Config.hash_static_filenames else false
  in
  (* TODO: allow APPSUPPORT in here *)
  template
  >|= Util.string_replace "{{ALLFUNCTIONS}}" (Api.functions ~username)
  >|= Util.string_replace
        "{{LIVERELOADJS}}"
        ( if Config.browser_reload_enabled
        then
          "<script type=\"text/javascript\" src=\"//localhost:35729/livereload.js\"> </script>"
        else "" )
  >|= Util.string_replace "{{STATIC}}" static_host
  >|= Util.string_replace "{{IS_ADMIN}}" admin
  >|= Util.string_replace "{{ROLLBARCONFIG}}" rollbar_js
  >|= Util.string_replace "{{PUSHERCONFIG}}" Config.pusher_js
  >|= Util.string_replace "{{USER_CONTENT_HOST}}" Config.user_content_host
  >|= Util.string_replace "{{ENVIRONMENT_NAME}}" Config.env_display_name
  >|= Util.string_replace "{{USERNAME}}" username
  >|= Util.string_replace
        "{{USER_ID}}"
        ( username
        |> Account.id_of_username
        |> Option.value_exn
        |> Uuidm.to_string )
  >|= Util.string_replace "{{CANVAS_ID}}" (Uuidm.to_string canvas_id)
  >|= Util.string_replace
        "{{APPSUPPORT}}"
        (File.readfile ~root:Webroot "appsupport.js")
  >|= Util.string_replace "{{STATIC}}" static_host
  >|= (fun x ->
        if not hash_static_filenames
        then Util.string_replace "{{HASH_REPLACEMENTS}}" "{}" x
        else
          let etags_str = File.readfile ~root:Webroot "etags.json" in
          let etags_json = Yojson.Safe.from_string etags_str in
          let etag_assoc_list =
            to_assoc_list etags_json
            |> List.filter ~f:(fun (file, _) ->
                   not (String.equal "__date" file) )
            |> List.filter (* Only hash our assets, not vendored assets *)
                 ~f:(fun (file, _) ->
                   not (String.is_substring ~substring:"vendor/" file) )
          in
          x
          |> fun instr ->
          etag_assoc_list
          |> List.fold ~init:instr ~f:(fun acc (file, hash) ->
                 (Util.string_replace file (hashed_filename file hash)) acc )
          |> fun instr ->
          Util.string_replace
            "{{HASH_REPLACEMENTS}}"
            ( etag_assoc_list
            |> List.map ~f:(fun (k, v) ->
                   ("/" ^ k, `String ("/" ^ hashed_filename k v)) )
            |> (fun x -> `Assoc x)
            |> Yojson.Safe.to_string )
            instr )
  >|= Util.string_replace "{{CSRF_TOKEN}}" csrf_token
  >|= Util.string_replace "{{BUILD_HASH}}" Config.build_hash


let save_test_handler ~(execution_id : Types.id) host =
  let c = C.load_all host [] in
  match c with
  | Ok c ->
      let filename = C.save_test !c in
      respond ~execution_id `OK ("Saved as: " ^ filename)
  | Error errs ->
      Exception.internal
        ~info:[("errs", String.concat ~sep:", " errs)]
        "Failed to load canvas"


let check_csrf_then_handle ~execution_id ~session handler req =
  if CRequest.meth req = `POST
  then
    let request_token =
      req |> CRequest.headers |> fun h -> Header.get h "X-CSRF-Token"
    in
    if Some (Auth.Session.csrf_token_for session) = request_token
    then handler req
    else respond ~execution_id `Unauthorized "Bad CSRF"
  else handler req


(* Checks for a cookie, prompts for basic auth if there isn't one,
   returns Unauthorized if basic auth doesn't work.

   Importantly this performs no authorization. Just authentication.

   It passes the username and the current CSRF token to the handler.
   Don't check against the CSRF token in the handler; use
   check_csrf_then_handle for that. Only use it to present the token
   to users in HTML.

   Also implements logout (!). *)
let authenticate_then_handle ~(execution_id : Types.id) handler req =
  let path = req |> CRequest.uri |> Uri.path in
  let headers = req |> CRequest.headers in
  let username_header username = ("x-dark-username", username) in
  match%lwt Auth.Session.of_request req with
  | Ok (Some session) ->
      let username = Auth.Session.username_for session in
      let csrf_token = Auth.Session.csrf_token_for session in
      Log.add_log_annotations
        [("username", `String username)]
        (fun _ ->
          if path = "/logout"
          then (
            Auth.Session.clear Auth.Session.backend session ;%lwt
            let headers =
              Header.of_list
                ( username_header username
                :: Auth.Session.clear_hdrs Auth.Session.cookie_key )
            in
            let uri = Uri.of_string ("/a/" ^ Uri.pct_encode username) in
            S.respond_redirect ~headers ~uri () )
          else
            let headers = [username_header username] in
            over_headers_promise
              ~f:(fun h -> Header.add_list h headers)
              (handler ~session ~csrf_token req) )
  | _ ->
    ( match Header.get_authorization headers with
    | Some (`Basic (username, password)) ->
        if Account.authenticate ~username ~password
        then
          Log.add_log_annotations
            [("username", `String username)]
            (fun _ ->
              let%lwt session = Auth.Session.new_for_username username in
              let https_only_cookie =
                req |> CRequest.uri |> should_use_https
              in
              (* For why we use 'darklang.com' and not '.darklang.com', see
               * https://www.mxsasha.eu/blog/2014/03/04/definitive-guide-to-cookie-domains/
               * tl;dr: with a leading-dot was the specified behavior prior to
               * RFC6265 (2011), and in theory is still okay because the leading
               * dot is ignored, but .darklang.localhost doesn't work and
               * darklang.localhost does, so ... no leading dot works better for
               * us. *)
              let domain =
                req
                |> CRequest.headers
                |> fun h ->
                Header.get h "host"
                |> Option.value ~default:"darklang.com"
                (* Host: darklang.localhost:8000 is properly set in-cookie as
                   * "darklang.localhost", the cookie domain doesn't want the
                   * port *)
                |> String.substr_replace_all ~pattern:":8000" ~with_:""
              in
              let headers =
                username_header username
                :: Auth.Session.to_cookie_hdrs
                     ~http_only:true
                     ~secure:https_only_cookie
                     ~domain
                     ~path:"/"
                     Auth.Session.cookie_key
                     session
              in
              let csrf_token = Auth.Session.csrf_token_for session in
              over_headers_promise
                ~f:(fun h -> Header.add_list h headers)
                (handler ~session ~csrf_token req) )
        else respond ~execution_id `Unauthorized "Bad credentials"
    | None ->
        S.respond_need_auth ~auth:(`Basic "dark") ()
    | _ ->
        respond ~execution_id `Unauthorized "Invalid session" )


let admin_ui_handler
    ~(execution_id : Types.id)
    ~(path : string list)
    ~(canvasname : string)
    ~(body : string)
    ~(username : string)
    ~(csrf_token : string)
    ~(admin : string)
    (req : CRequest.t) =
  let verb = req |> CRequest.meth in
  let uri = req |> CRequest.uri in
  let query_param_set name =
    match Uri.get_query_param uri name with
    | Some v when v <> "0" && v <> "false" ->
        true
    | _ ->
        false
  in
  let integration_test =
    query_param_set "integration-test" && Config.allow_test_routes
  in
  let local = Uri.get_query_param uri "localhost-assets" in
  let html_hdrs =
    [ ("Content-type", "text/html; charset=utf-8")
      (* Don't allow any other websites to put this in an iframe;
       this prevents "clickjacking" at tacks.
       https://www.owasp.org/index.php/Clickjacking_Defense_Cheat_Sheet#Content-Security-Policy:_frame-ancestors_Examples
       It would be nice to use CSP to limit where we can load scripts etc from,
       but right now we load from CDNs, <script> tags, etc. So the only thing
       we could do is script-src: 'unsafe-inline', which doesn't offer us
       any additional security. *)
    ; ("Content-security-policy", "frame-ancestors 'none';") ]
  in
  let html_hdrs =
    if local = None
    then html_hdrs
    else ("Access-Control-Allow-Origin", "*") :: html_hdrs
  in
  let html_hdrs = Header.of_list html_hdrs in
  (* this could be more middleware like in the future *if and only if* we
     only make changes in promises .*)
  let when_can_view ~canvas f =
    let auth_domain = Account.auth_domain_for canvas in
    if Authorization.can_view_canvas ~canvas ~username
    then
      match Account.owner ~auth_domain with
      | Some owner ->
          Log.add_log_annotations
            [("canvas", `String canvas)]
            (fun _ -> f (Serialize.fetch_canvas_id owner canvasname))
      | None ->
          respond
            ~execution_id
            `Internal_server_error
            "Dark Internal Error: Dark - the service running this application - encountered an error when loading this canvas. This problem is a bug in Dark, we're sorry! Our automated systems have noted this error and we are working to resolve it. The author of this application can check in our #users channel for more information."
    else respond ~execution_id `Unauthorized "Unauthorized"
  in
  let serve_or_error ~(canvas_id : Uuidm.t) =
    Lwt.try_bind
      (fun _ -> admin_ui_html ~canvas_id ~csrf_token ~local username admin)
      (fun body -> respond ~resp_headers:html_hdrs ~execution_id `OK body)
      (fun e ->
        let bt = Exception.get_backtrace () in
        Rollbar.last_ditch e ~bt "handle_error" (Types.show_id execution_id) ;
        respond
          ~execution_id
          `Internal_server_error
          "Dark Internal Error: Dark - the service running this application - encountered an error. This problem is a bug in Dark, we're sorry! Our automated systems have noted this error and we are working to resolve it. The author of this application can check in our #users channel for more information."
        )
  in
  match (verb, path) with
  | `GET, ["a"; canvas] ->
      when_can_view ~canvas (fun canvas_id ->
          if integration_test then Canvas.load_and_resave_from_test_file canvas ;
          serve_or_error ~canvas_id )
  | _ ->
      respond ~execution_id `Not_found "Not found"


let admin_api_handler
    ~(execution_id : Types.id)
    ~(path : string list)
    ~(body : string)
    ~(username : string)
    (req : CRequest.t) =
  let verb = req |> CRequest.meth in
  (* this could be more middleware like in the future *if and only if* we
     only make changes in promises .*)
  let when_can_edit ~canvas f =
    let p =
      Authorization.permission
        ~auth_domain:(Account.auth_domain_for canvas)
        ~username
    in
    if p = Some Authorization.ReadWrite
    then Log.add_log_annotations [("canvas", `String canvas)] (fun _ -> f p)
    else respond ~execution_id `Unauthorized "Unauthorized"
  in
  let when_can_view ~canvas f =
    let p =
      Authorization.permission
        ~auth_domain:(Account.auth_domain_for canvas)
        ~username
    in
    if p >= Some Authorization.Read
    then Log.add_log_annotations [("canvas", `String canvas)] (fun _ -> f p)
    else respond ~execution_id `Unauthorized "Unauthorized"
  in
  match (verb, path) with
  (* Operational APIs.... maybe these shouldn't be here, but
     they start with /api so they need to be. *)
  | `POST, ["api"; "clear-benchmarking-data"] ->
      Db.delete_benchmarking_data () ;
      respond ~execution_id `OK "Cleared"
  | `POST, ["api"; canvas; "save_test"] when Config.allow_test_routes ->
      save_test_handler ~execution_id canvas
  (* Canvas API *)
  | `POST, ["api"; canvas; "rpc"] (* old name, remove later *)
  | `POST, ["api"; canvas; "add_op"] ->
      when_can_edit ~canvas (fun _ ->
          wrap_editor_api_headers
            (admin_add_op_handler ~execution_id canvas body) )
  | `POST, ["api"; canvas; "initial_load"] ->
      when_can_view ~canvas (fun permission ->
          wrap_editor_api_headers
            (initial_load ~execution_id ~username ~canvas ~permission body) )
  | `POST, ["api"; canvas; "execute_function"] ->
      when_can_edit ~canvas (fun _ ->
          wrap_editor_api_headers (execute_function ~execution_id canvas body)
      )
  | `POST, ["api"; canvas; "trigger_handler"] ->
      when_can_edit ~canvas (fun _ ->
          wrap_editor_api_headers (trigger_handler ~execution_id canvas body)
      )
  | `POST, ["api"; canvas; "get_trace_data"] ->
      when_can_view ~canvas (fun _ ->
          wrap_editor_api_headers (get_trace_data ~execution_id canvas body) )
  | `POST, ["api"; canvas; "get_db_stats"] ->
      when_can_view ~canvas (fun _ ->
          wrap_editor_api_headers (db_stats ~execution_id canvas body) )
  | `POST, ["api"; canvas; "get_unlocked_dbs"] ->
      when_can_view ~canvas (fun _ ->
          wrap_editor_api_headers (get_unlocked_dbs ~execution_id canvas body)
      )
  | `POST, ["api"; canvas; "delete_404"] ->
      when_can_edit ~canvas (fun _ ->
          wrap_editor_api_headers (delete_404 ~execution_id canvas body) )
  | `POST, ["api"; canvas; "static_assets"] ->
      when_can_edit ~canvas (fun _ ->
          wrap_editor_api_headers
            (static_assets_upload_handler
               ~execution_id
               canvas
               username
               req
               body) )
  | _ ->
      respond ~execution_id `Not_found "Not found"


let admin_handler
    ~(execution_id : Types.id)
    ~(uri : Uri.t)
    ~(body : string)
    ~session
    ~(csrf_token : string)
    (req : CRequest.t) =
  let username = Auth.Session.username_for session in
  let admin = Account.is_admin username |> string_of_bool in
  let path =
    uri
    |> Uri.path
    |> String.lstrip ~drop:(( = ) '/')
    |> String.rstrip ~drop:(( = ) '/')
    |> String.split ~on:'/'
  in
  (* routing *)
  match path with
  | "api" :: _ ->
      check_csrf_then_handle
        ~execution_id
        ~session
        (admin_api_handler ~execution_id ~path ~body ~username)
        req
  | "a" :: canvasname :: _ ->
      Log.add_log_annotations
        [("canvas", `String canvasname)]
        (fun _ ->
          admin_ui_handler
            ~execution_id
            ~path
            ~body
            ~username
            ~canvasname
            ~csrf_token
            ~admin
            req )
  | _ ->
      respond ~execution_id `Not_found "Not found"


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


type host_route =
  | Canvas of string
  | Static
  | Admin

let route_host req =
  match
    req
    |> CRequest.uri
    |> Uri.host
    |> Option.value ~default:""
    |> fun h -> String.split h '.'
  with
  | ["static"; "darklang"; "localhost"]
  | ["static"; "darklang"; "com"]
  | [_; "ngrok"; "io"] ->
      Some Static
  (* Dark canvases *)
  | [a; "builtwithdark"; "com"]
  | [a; "builtwithdark"; "localhost"]
  | [a; "darksingleinstance"; "com"] ->
      Some (Canvas a)
  (* Specific Dark canvas: builtwithdark *)
  | ["builtwithdark"; "localhost"] | ["builtwithdark"; "com"] ->
      Some (Canvas "builtwithdark")
  (* Specific Dark canvas: darksingleinstance *)
  | ["darksingleinstance"; "com"] ->
      Some (Canvas "darksingleinstance")
  (* Customers *)
  | [a; "dabblefox"; "com"] ->
      Some (Canvas ("dabblefox-" ^ a))
  | ["www"; "hellobirb"; "com"] | ["hellobirb"; "com"] ->
      Some (Canvas "pixelkeet")
  (* admin interface + outer site, conditionally *)
  | ["darklang"; "com"] | ["darklang"; "localhost"] | ["dark_dev"; "com"] ->
      Some Admin
  (* Not a match... *)
  | _ ->
      None


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


let k8s_handler req ~execution_id ~stopper =
  let%lwt stroller_readiness_check = stroller_readiness_check () in
  match req |> CRequest.uri |> Uri.path with
  (* For GKE health check *)
  | "/" ->
    ( match Dbconnection.status () with
    | `Healthy ->
        if not !ready
           (* ie. liveness check has found a service with 2 minutes of failing readiness checks *)
        then (
          Log.infO
            "Liveness check found unready service, returning unavailable" ;
          respond ~execution_id `Service_unavailable "Service not ready" )
        else respond ~execution_id `OK "Hello internal overlord"
    | `Disconnected ->
        respond ~execution_id `Service_unavailable "Sorry internal overlord" )
  | "/ready" ->
      let checks =
        [db_conn_readiness_check (); stroller_readiness_check]
        |> List.filter_map ~f:(fun x -> x)
      in
      ( match checks with
      | [] ->
          if !ready
          then respond ~execution_id `OK "Hello internal overlord"
          else (
            Log.infO "Service ready" ;
            ready := true ;
            respond ~execution_id `OK "Hello internal overlord" )
      | _ ->
          Log.erroR
            ("Failed readiness check(s): " ^ String.concat checks ~sep:": ") ;
          respond ~execution_id `Service_unavailable "Sorry internal overlord"
      )
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
        respond ~execution_id `OK "Terminated" )
      else (
        Log.infO
          "shutdown"
          ~data:"Received redundant shutdown request - already shutting down" ;
        respond ~execution_id `OK "Terminated" )
  | _ ->
      respond ~execution_id `Not_found ""


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
    (req : CRequest.t) =
  let verb = req |> CRequest.meth in
  match verb with
  (* transform HEAD req method to GET, discards body in response*)
  | `HEAD ->
      user_page_handler
        ~execution_id
        ~canvas
        ~ip
        ~uri
        ~body
        (coalesce_head_to_get req)
      |> respond_or_redirect_empty_body
  | _ ->
      user_page_handler ~execution_id ~canvas ~ip ~uri ~body req
      |> respond_or_redirect


let callback ~k8s_callback ip req body execution_id =
  let req = canonicalize_request req in
  let uri = CRequest.uri req in
  let handle_error ~(include_internals : bool) (e : exn) =
    try
      let bt = Exception.get_backtrace () in
      let%lwt _ =
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
          respond ~execution_id `Bad_request e.short
      | _ ->
          let body =
            if include_internals || Config.show_stacktrace
            then real_err
            else
              "Dark Internal Error: Dark - the service running this application - encountered an error. This problem is a bug in Dark, we're sorry! Our automated systems have noted this error and we are working to resolve it. The author of this application can check in our #users channel for more information."
          in
          respond ~execution_id `Internal_server_error body
    with e ->
      let bt = Exception.get_backtrace () in
      Rollbar.last_ditch e ~bt "handle_error" (Types.show_id execution_id) ;
      respond ~execution_id `Internal_server_error "unhandled error"
  in
  try
    Log.infO
      "request"
      ~params:
        [ ("ip", ip)
        ; ("method", req |> CRequest.meth |> Cohttp.Code.string_of_method)
        ; ("uri", Uri.to_string uri)
        ; ("execution_id", Types.string_of_id execution_id) ] ;
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
          respond ~execution_id (Cohttp.Code.status_of_code 418) ""
      | _ ->
        ( match Uri.to_string uri with
        | "/sitemap.xml" | "/favicon.ico" ->
            respond ~execution_id `OK ""
        | _ ->
          (* figure out what handler to dispatch to... *)
          ( match route_host req with
          | Some (Canvas canvas) ->
              canvas_handler ~execution_id ~canvas ~ip ~uri ~body req
          | Some Static ->
              static_handler uri
          | Some Admin ->
            ( try
                authenticate_then_handle
                  ~execution_id
                  (fun ~session ~csrf_token r ->
                    try
                      admin_handler
                        ~execution_id
                        ~uri
                        ~body
                        ~session
                        ~csrf_token
                        r
                    with e -> handle_error ~include_internals:true e )
                  req
              with e -> handle_error ~include_internals:false e )
          | None ->
              k8s_callback req ~execution_id ) ) )
  with e -> handle_error ~include_internals:false e


let server () =
  let stop, stopper = Lwt.wait () in
  let cbwb conn req req_body =
    let%lwt body_string = Cohttp_lwt__Body.to_string req_body in
    let execution_id = Util.create_id () in
    let request_path = Uri.path_and_query (CRequest.uri req) in
    (* use the x-forwarded-for ip, falling back to the raw ip in the request *)
    let ip =
      let ch, _ = conn in
      Header.get (CRequest.headers req) "X-forwarded-for"
      |> Option.bind ~f:(fun str -> str |> String.split ~on:';' |> List.hd)
      |> Option.map ~f:String.strip
      |> Option.value ~default:(get_ip_address ch)
    in
    Log.add_log_annotations
      [ ("execution_id", `String (Types.string_of_id execution_id))
      ; ("request", `String request_path)
      ; ("method", `String (Cohttp.Code.string_of_method (CRequest.meth req)))
      ; ("host", `String (Uri.host_with_default ~default:"" (CRequest.uri req)))
      ; ("ip", `String ip) ]
      (fun _ ->
        callback
          ~k8s_callback:(k8s_handler ~stopper)
          ip
          req
          body_string
          execution_id )
  in
  S.create ~stop ~mode:(`TCP (`Port Config.port)) (S.make ~callback:cbwb ())


let run () =
  ignore (Lwt_main.run (Nocrypto_entropy_lwt.initialize () >>= server))
