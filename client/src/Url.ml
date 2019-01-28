open Tc
open Prelude
open Types
module Cmd = Tea.Cmd
module Navigation = Tea.Navigation

let hashUrlParams (params : (string * string) list) : string =
  let merged = List.map ~f:(fun (k, v) -> k ^ "=" ^ v) params in
  "#" ^ String.join ~sep:"&" merged


let urlOf (page : page) (pos : pos) : string =
  let head =
    match page with Toplevels _ -> [] | Fn (tlid, _) -> [("fn", deTLID tlid)]
  in
  let tail = [("x", string_of_int pos.x); ("y", string_of_int pos.y)] in
  hashUrlParams (head @ tail)


let urlFor (page : page) : string =
  let pos =
    (match page with Toplevels pos -> pos | Fn (_, pos) -> pos)
    |> Viewport.toCenteredOn
  in
  urlOf page pos


let navigateTo (page : page) : msg Cmd.t = Navigation.newUrl (urlFor page)

let linkFor (page : page) (class_ : string) (content : msg Html.html list) :
    msg Html.html =
  Html.a [Html.href (urlFor page); Html.class' class_] content


(* When scrolling, there are way too many events to process them through *)
(* the History/location handlers. So instead we process them directly, *)
(* and update the browser url periodically. *)
let maybeUpdateScrollUrl (m : model) : modification =
  let pos =
    match m.currentPage with
    | Toplevels _ ->
        m.canvas.offset
    | Fn (_, _) ->
        m.canvas.fnOffset
  in
  let state = m.urlState in
  if pos <> state.lastPos
  then
    Many
      [ TweakModel (fun m_ -> {m_ with urlState = {lastPos = pos}})
      ; MakeCmd (Navigation.modifyUrl (urlOf m.currentPage pos)) ]
  else NoChange


let parseLocation (loc : Web.Location.location) : page option =
  let unstructured =
    loc.hash
    |> String.dropLeft ~count:1
    |> String.split ~on:"&"
    |> List.map ~f:(String.split ~on:"=")
    |> List.filterMap ~f:(fun arr ->
           match arr with [a; b] -> Some (String.toLower a, b) | _ -> None )
    |> StrDict.fromList
  in
  let center =
    match
      (StrDict.get ~key:"x" unstructured, StrDict.get ~key:"y" unstructured)
    with
    | Some x, Some y ->
      ( match (String.toInt x, String.toInt y) with
      | Ok x, Ok y ->
          Some {x; y}
      | _ ->
          None )
    | _ ->
        None
  in
  let editedFn =
    match StrDict.get ~key:"fn" unstructured with
    | Some sid ->
        Some
          (Fn (TLID sid, Option.withDefault ~default:Defaults.centerPos center))
    | _ ->
        None
  in
  match (center, editedFn) with
  | _, Some _ ->
      editedFn
  | Some pos, _ ->
      Some (Toplevels pos)
  | _ ->
      None


let changeLocation (m : model) (loc : Web.Location.location) : modification =
  let mPage = parseLocation loc in
  match mPage with
  | Some (Fn (id, pos)) ->
    ( match Functions.find m id with
    | None ->
        DisplayError "No function"
    | _ ->
        SetPage (Fn (id, pos)) )
  | Some page ->
      SetPage page
  | _ ->
      NoChange


let parseCanvasName (loc : Web.Location.location) : string =
  match
    loc.pathname
    |> String.dropLeft ~count:1
    (* remove lead "/" *)
    |> String.split ~on:"/"
  with
  | "a" :: canvasName :: _ ->
      canvasName
  | _ ->
      "builtwithdark"


let splitOnEquals (s : string) : (string * bool) option =
  match String.split ~on:"=" s with
  | [] ->
      None
  | [name] ->
      Some (name, true)
  | [name; value] ->
      Some (name, value <> "0" && value <> "false")
  | _ ->
      None


let queryParams : (string * bool) list =
  let search = (Tea_navigation.getLocation ()).search in
  match String.uncons search with
  | Some ('?', rest) ->
      rest
      |> String.toLower
      |> String.split ~on:"&"
      |> List.filterMap ~f:splitOnEquals
  | _ ->
      []


let queryParamSet (name : string) : bool =
  List.find ~f:(fun (k, v) -> if k = name then v else false) queryParams
  |> Option.withDefault ~default:(name, false)
  |> Tuple2.second


let isDebugging = queryParamSet "debugger"

let isIntegrationTest = queryParamSet "integration-test"
