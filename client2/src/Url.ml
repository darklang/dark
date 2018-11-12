open Tea
open! Porting
module Attrs = Html.Attributes
open Prelude
open Types

let hashUrlParams (params : (string * string) list) : string =
  let merged = List.map (fun (k, v) -> k ^ "=" ^ v) params in
  "#" ^ String.join "&" merged

let urlOf (page : page) (pos : pos) : string =
  let head =
    match page with
    | Toplevels _ -> []
    | Fn (tlid, _) -> [("fn", deTLID tlid)]
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

let maybeUpdateScrollUrl (m : model) : modification =
  let pos =
    match m.currentPage with
    | Toplevels _ -> m.canvas.offset
    | Fn (tlid, _) -> m.canvas.fnOffset
  in
  let state = m.urlState in
  if pos <> state.lastPos then
    Many
      [ TweakModel (fun m_ -> {m_ with urlState= {lastPos= pos}})
      ; MakeCmd (Navigation.modifyUrl (urlOf m.currentPage pos)) ]
  else NoChange

let parseLocation (m : model) (loc : Web.Location.location) : page option =
  let unstructured =
    loc.hash |> String.dropLeft 1 |> String.split "&"
    |> List.map (String.split "=")
    |> List.filterMap (fun arr ->
        match arr with [a; b] -> Some (String.toLower a, b) | _ -> None )
    |> StrDict.fromList
  in
  let center =
    match (StrDict.get "x" unstructured, StrDict.get "y" unstructured) with
    | Some x, Some y -> (
      match (String.toInt x, String.toInt y) with
      | Ok x, Ok y -> Some {x; y}
      | _ -> None )
    | _ -> None
  in
  let editedFn =
    match StrDict.get "fn" unstructured with
    | Some sid ->
        Some (Fn (TLID sid, Option.withDefault Defaults.centerPos center))
    | _ -> None
  in
  match (center, editedFn) with
  | _, Some fn -> editedFn
  | Some pos, _ -> Some (Toplevels pos)
  | _ -> None

let changeLocation (m : model) (loc : Web.Location.location) : modification =
  let mPage = parseLocation m loc in
  match mPage with
  | Some (Fn (id, pos)) -> (
    match Functions.find m id with
    | None -> DisplayError "No function"
    | _ -> SetPage (Fn (id, pos)) )
  | Some page -> SetPage page
  | _ -> NoChange

let parseCanvasName (loc : Web.Location.location) : string =
  match loc.pathname |> String.dropLeft 1 |> String.split "/" with
  | "a" :: canvasName :: _ -> canvasName
  | _ -> "builtwithdark"
