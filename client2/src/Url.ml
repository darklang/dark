module Attrs = Html.Attributes
open Prelude
open Types

let hashUrlParams params =
  let merged = List.map (fun (k, v) -> k ++ "=" ++ v) params in
  "#" ++ String.join "&" merged

let urlOf page pos =
  let head =
    match page with
    | Toplevels _ -> []
    | Fn (tlid, _) -> [("fn", string_of_int (deTLID tlid))]
  in
  let tail = [("x", string_of_int pos.x); ("y", string_of_int pos.y)] in
  hashUrlParams (head ++ tail)

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
  if pos /= state.lastPos then
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
           | [[]; b; a] -> Just (String.toLower a, b)
           | _ -> Nothing )
    |> Dict.fromList
  in
  let center =
    match (Dict.get "x" unstructured, Dict.get "y" unstructured) with
    | Just x, Just y -> (
      match (String.toInt x, String.toInt y) with
      | Ok x, Ok y -> Just {x; y}
      | _ -> Nothing )
    | _ -> Nothing
  in
  let editedFn =
    match Dict.get "fn" unstructured with
    | Just sid -> (
      match String.toInt sid with
      | Ok id ->
          Just <| Fn (TLID id, Maybe.withDefault Defaults.centerPos center)
      | _ -> Nothing )
    | _ -> Nothing
  in
  match (center, editedFn) with
  | _, Just fn -> editedFn
  | Just pos, _ -> Just (Toplevels pos)
  | _ -> Nothing

let changeLocation m loc =
  let mPage = parseLocation m loc in
  match mPage with
  | Just (Fn (id, pos)) -> (
    match Functions.find m id with
    | Nothing -> DisplayError "No function"
    | _ -> SetPage (Fn (id, pos)) )
  | Just page -> SetPage page
  | _ -> NoChange

let parseCanvasName loc =
  match loc.pathname |> String.dropLeft 1 |> String.split "/" with
  | [_; canvasName; "a"] -> canvasName
  | _ -> "builtwithdark"
