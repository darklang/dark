open Prelude

(* Dark *)
module B = BlankOr
module RT = Runtime
module TL = Toplevel

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
