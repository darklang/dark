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
  let pos = match page with Architecture pos -> Some pos | _ -> None in
  urlOf page pos


let navigateTo (page : page) : msg Cmd.t = Navigation.newUrl (urlFor page)

let linkFor (page : page) (class_ : string) (content : msg Html.html list) :
    msg Html.html =
  Html.a [Html.href (urlFor page); Html.class' class_] content


(* When scrolling, there are way too many events to process them through *)
(* the History/location handlers. So instead we process them directly, *)
(* and update the browser url periodically. *)
let maybeUpdateScrollUrl (m : model) : modification =
  match m.currentPage with
  | Architecture _ ->
      let pos = Some m.canvasProps.offset in
      if pos <> m.urlState.lastPos
      then
        Many
          [ TweakModel (fun m -> {m with urlState = {lastPos = pos}})
          ; MakeCmd (Navigation.modifyUrl (urlOf m.currentPage pos)) ]
      else NoChange
  | FocusedDB _ | FocusedHandler _ | FocusedFn _ ->
      (* Dont update the scroll in the as we don't record the scroll in the
       * URL, and the url has already been changed *)
      NoChange


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
  let architecture () =
    match
      (StrDict.get ~key:"x" unstructured, StrDict.get ~key:"y" unstructured)
    with
    | Some x, Some y ->
      ( match (String.toInt x, String.toInt y) with
      | Ok x, Ok y ->
          Some (Architecture {x; y})
      | _ ->
          None )
    | _ ->
        None
  in
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
  architecture ()
  |> Option.orElse (handler ())
  |> Option.orElse (fn ())
  |> Option.orElse (db ())


let changeLocation (m : model) (loc : Web.Location.location) : modification =
  let mPage = parseLocation loc in
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
