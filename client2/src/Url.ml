open Belt
open Porting
module Attrs = Html.Attributes
open Prelude
open Types

let hashUrlParams params =
  let merged = List.map (fun (k, v) -> (k ^ "=") ^ v) params in
  "#" ^ String.join "&" merged

let urlOf page pos =
  let head =
    match page with
    | Toplevels _ -> []
    | Fn (tlid, _) -> [("fn", string_of_int (deTLID tlid))]
  in
  let tail = [("x", string_of_int pos.x); ("y", string_of_int pos.y)] in
  hashUrlParams (head ^ tail)

let urlFor page =
  let pos =
    (match page with Toplevels pos -> pos | Fn (_, pos) -> pos)
    |> Viewport.toCenteredOn
  in
  urlOf page pos

let navigateTo page = Navigation.newUrl (urlFor page)

let linkFor page class_ content =
  Html.a [Attrs.href (urlFor page); Attrs.class_ class_] content

let maybeUpdateScrollUrl m =
  let pos =
    match m.currentPage with
    | Toplevels _ -> m.canvas.offset
    | Fn (tlid, _) -> m.canvas.fnOffset
  in
  let state = m.urlState in
  if pos <> state.lastPos then
    Many
      [ TweakModel (fun m_ -> {m_ with urlState= {state with lastPos= pos}})
      ; MakeCmd (Navigation.modifyUrl (urlOf m.currentPage pos)) ]
  else NoChange

let parseLocation m loc =
  let unstructured =
    loc.hash |> String.dropLeft 1 |> String.split "&"
    |> List.map (String.split "=")
    |> List.filterMap (fun arr ->
           match arr with
           | [[]; b; a] -> Some (String.toLower a, b)
           | _ -> None )
    |> Dict.fromList
  in
  let center =
    match (Dict.get "x" unstructured, Dict.get "y" unstructured) with
    | Some x, Some y -> (
      match (int_of_string x, int_of_string y) with
      | Ok x, Ok y -> Some {x; y}
      | _ -> None )
    | _ -> None
  in
  let editedFn =
    match Dict.get "fn" unstructured with
    | Some sid -> (
      match int_of_string sid with
      | Ok id ->
          Some <| Fn (TLID id, Maybe.withDefault Defaults.centerPos center)
      | _ -> None )
    | _ -> None
  in
  match (center, editedFn) with
  | _, Some fn -> editedFn
  | Some pos, _ -> Some (Toplevels pos)
  | _ -> None

let changeLocation m loc =
  let mPage = parseLocation m loc in
  match mPage with
  | Some (Fn (id, pos)) -> (
    match Functions.find m id with
    | None -> DisplayErroror "No function"
    | _ -> SetPage (Fn (id, pos)) )
  | Some page -> SetPage page
  | _ -> NoChange

let parseCanvasName loc =
  match loc.pathname |> String.dropLeft 1 |> String.split "/" with
  | [_; canvasName; "a"] -> canvasName
  | _ -> "builtwithdark"
