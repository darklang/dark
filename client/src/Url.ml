open Tc
open Prelude
open Types
module Cmd = Tea.Cmd
module Navigation = Tea.Navigation
module TL = Toplevel

let hashUrlParams (params : (string * string) list) : string =
  let merged = List.map ~f:(fun (k, v) -> k ^ "=" ^ v) params in
  "#" ^ String.join ~sep:"&" merged


let urlOf (page : page) (mpos : pos option) : string =
  let head =
    match page with
    | Architecture _ ->
        []
    | FocusedFn tlid ->
        [("fn", deTLID tlid)]
    | FocusedHandler tlid ->
        [("handler", deTLID tlid)]
    | FocusedDB tlid ->
        [("db", deTLID tlid)]
  in
  let tail =
    match mpos with
    | Some pos ->
        [("x", string_of_int pos.x); ("y", string_of_int pos.y)]
    | None ->
        []
  in
  hashUrlParams (head @ tail)


let urlFor (page : page) : string =
  let pos =
    match page with
    | Architecture pos ->
        Some {x = pos.x; y = pos.y}
    | _ ->
        None
  in
  urlOf page pos


let navigateTo (page : page) : msg Cmd.t = Navigation.newUrl (urlFor page)

let linkFor (page : page) (class_ : string) (content : msg Html.html list) :
    msg Html.html =
  Html.a [Html.href (urlFor page); Html.class' class_] content


let parseLocation (m : model) (loc : Web.Location.location) : page option =
  let unstructured =
    loc.hash
    |> String.dropLeft ~count:1
    |> String.split ~on:"&"
    |> List.map ~f:(String.split ~on:"=")
    |> List.filterMap ~f:(fun arr ->
           match arr with [a; b] -> Some (String.toLower a, b) | _ -> None )
    |> StrDict.fromList
  in
  let architecture () = Some (Architecture m.canvasProps.lastOffset) in
  let fn () =
    match StrDict.get ~key:"fn" unstructured with
    | Some sid ->
        Some (FocusedFn (TLID sid))
    | _ ->
        None
  in
  let handler () =
    match StrDict.get ~key:"handler" unstructured with
    | Some sid ->
        Some (FocusedHandler (TLID sid))
    | _ ->
        None
  in
  let db () =
    match StrDict.get ~key:"db" unstructured with
    | Some sid ->
        Some (FocusedDB (TLID sid))
    | _ ->
        None
  in
  fn ()
  |> Option.orElse (handler ())
  |> Option.orElse (db ())
  |> Option.orElse (architecture ())


let changeLocation (m : model) (loc : Web.Location.location) : modification =
  let mPage = parseLocation m loc in
  match mPage with
  | Some (FocusedFn id) ->
    ( match Functions.find m id with
    | None ->
        DisplayError "No function with this id"
    | _ ->
        SetPage (FocusedFn id) )
  | Some (FocusedHandler id) ->
    ( match TL.get m id with
    | None ->
        DisplayError "No toplevel with this id"
    | _ ->
        SetPage (FocusedHandler id) )
  | Some (FocusedDB id) ->
    ( match TL.get m id with
    | None ->
        DisplayError "No DB with this id"
    | _ ->
        SetPage (FocusedDB id) )
  | Some (Architecture pos) ->
      SetPage (Architecture pos)
  | None ->
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

let setPage (m : model) (oldPage : page) (newPage : page) : model =
  if oldPage = newPage
  then m
  else
    match newPage with
    | Architecture pos ->
        (* Pan to position *)
        { m with
          currentPage = newPage; canvasProps = {m.canvasProps with offset = pos}
        }
    | FocusedFn _ ->
        { m with
          currentPage = newPage
        ; canvasProps =
            { m.canvasProps with
              lastOffset = m.canvasProps.offset; offset = Defaults.origin }
            (* Stash the offset so that returning to canvas goes to the previous place *)
        ; cursorState = Deselected }
    | FocusedHandler _ | FocusedDB _ ->
        m
