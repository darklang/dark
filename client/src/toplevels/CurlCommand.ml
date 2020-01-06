open Prelude

(* Dark *)
module B = BlankOr
module RT = Runtime
module TL = Toplevel

(* Borrowed from libexecution/dval.mli with some items - DObj, DResult (ResError
 * _), DOption (OptNothing) - moved to the None case *)
let rec to_url_string (dv : dval) : string option =
  match dv with
  | DBlock
  | DIncomplete _
  | DPassword _
  | DObj _
  | DOption OptNothing
  | DResult (ResError _) ->
      None
  | DInt i ->
      Some (string_of_int i)
  | DBool true ->
      Some "true"
  | DBool false ->
      Some "false"
  | DStr s ->
      Some s
  | DFloat f ->
      Some (Tc.Float.toString f)
  | DCharacter c ->
      Some c
  | DNull ->
      Some "null"
  | DDate d ->
      Some d
  | DDB dbname ->
      Some dbname
  | DErrorRail d ->
      to_url_string d
  | DError (_, msg) ->
      Some ("error=" ^ msg)
  | DUuid uuid ->
      Some uuid
  | DResp (_, hdv) ->
      to_url_string hdv
  | DList l ->
      Some
        ( "[ "
        ^ String.join
            ~sep:", "
            (List.filterMap ~f:to_url_string (Array.to_list l))
        ^ " ]" )
  | DOption (OptJust v) ->
      to_url_string v
  | DResult (ResOk v) ->
      to_url_string v
  | DBytes bytes ->
      Some (bytes |> Encoders.base64url_bytes)


let strAsBodyCurl (dv : dval) : string option =
  match dv with
  | DStr s ->
      let body = s |> Util.Regex.replace ~re:(Util.Regex.regex "\n") ~repl:"" in
      Some ("-d '" ^ body ^ "'")
  | _ ->
      None


let objAsHeaderCurl (dv : dval) : string option =
  match dv with
  | DObj o ->
      StrDict.toList o
      (* curl will add content-length automatically, and having it specified
       * explicitly causes weird errors if the user, say, changes the body of
       * the request without changing the value of this header *)
      |> List.filter ~f:(fun (k, _) -> k != "content-length")
      |> List.map ~f:(fun (k, v) ->
             "-H '" ^ k ^ ":" ^ (RT.toRepr v |> RT.stripQuotes) ^ "'")
      |> String.join ~sep:" "
      |> fun s -> Some s
  | _ ->
      None


let curlFromSpec (m : model) (tlid : tlid) : string option =
  TL.get m tlid
  |> Option.andThen ~f:TL.asHandler
  |> Option.andThen ~f:(fun h ->
         let s = h.spec in
         match (s.space, s.name, s.modifier) with
         | F (_, "HTTP"), F (_, path), F (_, meth) ->
             let proto =
               if m.environment = "production" then "https" else "http"
             in
             let route =
               proto ^ "://" ^ m.canvasName ^ "." ^ m.userContentHost ^ path
             in
             ( match meth with
             | "GET" ->
                 Some ("curl " ^ route)
             | _ ->
                 Some
                   ( "curl -X "
                   ^ meth
                   ^ " -H 'Content-Type: application/json' "
                   ^ route ) )
         | _ ->
             None)


(* Constructs curl command from analysis dict.
  headers (which includes cookies),
  fullBody (for both formBody and jsonBody),
  url (which includes queryParams)
*)
let curlFromCurrentTrace (m : model) (tlid : tlid) : string option =
  let wrapInList o =
    o |> Option.andThen ~f:(fun v -> Some [v]) |> Option.withDefault ~default:[]
  in
  let trace =
    Analysis.getSelectedTraceID m tlid
    |> Option.andThen ~f:(Analysis.getTrace m tlid)
  in
  match trace with
  | Some (_, Some td) ->
      StrDict.get ~key:"request" td.input
      |> Option.andThen ~f:(fun obj ->
             match obj with DObj r -> Some r | _ -> None)
      |> Option.andThen ~f:(fun r ->
             match StrDict.get ~key:"url" r with
             | Some (DStr url) ->
                 let headers =
                   StrDict.get ~key:"headers" r
                   |> Option.andThen ~f:objAsHeaderCurl
                   |> wrapInList
                 in
                 let body =
                   StrDict.get ~key:"fullBody" r
                   |> Option.andThen ~f:strAsBodyCurl
                   |> wrapInList
                 in
                 let meth =
                   TL.get m tlid
                   |> Option.andThen ~f:TL.asHandler
                   |> Option.andThen ~f:(fun h -> B.toOption h.spec.modifier)
                   |> Option.andThen ~f:(fun s -> Some ("-X " ^ s))
                   |> wrapInList
                 in
                 ("curl" :: headers) @ body @ meth @ [url]
                 |> String.join ~sep:" "
                 |> Option.some
             | _ ->
                 None)
  | _ ->
      None


let curlFromHttpClientCall (m : model) (tlid : tlid) (id : id) (name : string) :
    string option =
  let traces =
    StrDict.get ~key:(TLID.toString tlid) m.traces
    |> recoverOption
         ~debug:(show_tlid tlid)
         "TLID not found in m.traces in curlFromHttpClientCall"
  in
  let traceId =
    traces
    |> Option.andThen ~f:(fun traces ->
           Analysis.selectedTrace m.tlTraceIDs traces tlid)
    (* We don't recover here b/c it's very possible we don't have an analysis
     * yet *)
    |> fun tid ->
    ( match tid with
    | Some _ ->
        ()
    | None ->
        Js.log "No selectedTrace present for tlid" ) ;
    tid
  in
  let tl =
    TL.get m tlid
    |> recoverOption
         ~debug:(show_tlid tlid)
         "TLID not found in model in curlFromHttpClientCall"
  in
  let args =
    Option.andThen2 tl traceId ~f:(fun tl traceId ->
        Analysis.getArguments m tl id traceId)
    (* TODO this is what fails if we haven't clicked the ast yet; we should fix
     * that, or make it toast instructions? *)
    |> recoverOption
         "Args not found in model in curlFromHttpClientCall"
         ~debug:(show_tlid tlid, Option.map ~f:show_traceID traceId)
  in
  let data =
    args
    |> Option.map ~f:(fun args ->
           let url, body, query, headers =
             match args with
             | [url; body; query; headers] ->
                 (url, Some body, query, headers)
             | [url; query; headers] ->
                 (url, None, query, headers)
             | _ ->
                 recover
                   ~debug:("arg count" ^ string_of_int (List.length args))
                   "args in curlFromHttpClientCall espected 3 or 4, failed"
                   (DNull, None, DNull, DNull)
           in
           let headers =
             objAsHeaderCurl headers |> Option.withDefault ~default:""
           in
           let body =
             strAsBodyCurl (body |> Option.withDefault ~default:DNull)
             |> Option.withDefault ~default:""
           in
           let meth =
             name
             |> Util.Regex.matches ~re:(Util.Regex.regex "HttpClient::([^_]*)")
             |> Option.map ~f:Js.Re.captures
             |> Option.andThen ~f:(fun captures ->
                    captures
                    |> Array.get ~index:1
                    |> Option.andThen ~f:Js.Nullable.toOption)
             |> Option.map ~f:(fun meth -> "-X " ^ meth)
             |> recoverOpt
                  ~debug:name
                  ~default:""
                  "Expected a fn name matching HttpClient::[^_]*"
           in
           let base_url =
             match url with
             | DStr s ->
                 s
             | _ ->
                 recover
                   ~debug:(show_dval url)
                   "Expected url arg to be a DStr"
                   url
                 |> show_dval
           in
           let qps =
             ( match query with
             | DObj map ->
                 map
                 |> StrDict.toList
                 |> List.filterMap ~f:(fun (k, v) ->
                        to_url_string v |> Option.map ~f:(fun v -> k ^ "=" ^ v))
                 |> String.join ~sep:"&"
             | _ ->
                 ignore
                   ( query
                   |> recover
                        ~debug:(show_dval query)
                        "Expected query arg to be a dobj" ) ;
                 "" )
             |> fun s -> if s == "" then "" else "?" ^ s
           in
           let url = "'" ^ base_url ^ qps ^ "'" in
           ["curl"; headers; body; meth; url]
           |> List.filter ~f:(fun s -> s != "")
           |> String.join ~sep:" ")
  in
  data


let makeCommand (m : model) (tlid : tlid) : string option =
  curlFromCurrentTrace m tlid |> Option.orElse (curlFromSpec m tlid)


let copyCurlMod (m : model) (tlid : tlid) (pos : vPos) : modification =
  match makeCommand m tlid with
  | Some data ->
      Native.Clipboard.copyToClipboard data ;
      let modFun m =
        let m1 = Handlers.setHandlerMenu tlid false m in
        {m1 with toast = {toastMessage = Some "Copied!"; toastPos = Some pos}}
      in
      TweakModel modFun
  | None ->
      TweakModel (Handlers.setHandlerMenu tlid false)
