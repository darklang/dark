open Prelude
module Cmd = Tea.Cmd
module Navigation = Tea.Navigation
module TL = Toplevel

let hashUrlParams (params : (string * string) list) : string =
  let merged = List.map ~f:(fun (k, v) -> k ^ "=" ^ v) params in
  if merged = []
  then
    (* the space here is important - https://stackoverflow.com/a/49373716/104021 *)
    " "
  else "#" ^ String.join ~sep:"&" merged


let urlFor (page : page) : string =
  let args =
    match page with
    | Architecture ->
        []
    | FocusedPackageManagerFn tlid ->
        [("packageManagerFn", TLID.toString tlid)]
    | FocusedFn (tlid, traceId) ->
      ( match traceId with
      | Some id ->
          [("fn", TLID.toString tlid); ("trace", id)]
      | None ->
          [("fn", TLID.toString tlid)] )
    | FocusedHandler (tlid, traceId, _) ->
      ( match traceId with
      | Some id ->
          [("handler", TLID.toString tlid); ("trace", id)]
      | None ->
          [("handler", TLID.toString tlid)] )
    | FocusedDB (tlid, _) ->
        [("db", TLID.toString tlid)]
    | FocusedType tlid ->
        [("type", TLID.toString tlid)]
    | SettingsModal tab ->
        [("settings", SettingsViewTypes.settingsTabToText tab)]
  in
  hashUrlParams args


let navigateTo (page : page) : msg Cmd.t = Navigation.newUrl (urlFor page)

let updateUrl (page : page) : msg Cmd.t =
  Tea_cmd.call (fun _enqueue ->
      let () = Navigation.pushState (urlFor page) in
      ())


let linkFor (page : page) (class_ : string) (content : msg Html.html list) :
    msg Html.html =
  Html.a [Html.href (urlFor page); Html.class' class_] content


let parseLocation (loc : Web.Location.location) : page option =
  let unstructured =
    loc.hash
    |> String.dropLeft ~count:1
    |> String.split ~on:"&"
    |> List.map ~f:(String.split ~on:"=")
    |> List.filterMap ~f:(fun arr ->
           match arr with [a; b] -> Some (String.toLower a, b) | _ -> None)
    |> StrDict.fromList
  in
  let architecture () = Some Architecture in
  let settingModal () =
    match StrDict.get ~key:"settings" unstructured with
    | Some tab ->
        Some (SettingsModal (SettingsViewTypes.settingsTabFromText tab))
    | _ ->
        None
  in
  let pmfn () =
    match StrDict.get ~key:"packagemanagerfn" unstructured with
    | Some sid ->
        Some (FocusedPackageManagerFn (TLID.fromString sid))
    | _ ->
        None
  in
  let fn () =
    match StrDict.get ~key:"fn" unstructured with
    | Some sid ->
        let trace = StrDict.get ~key:"trace" unstructured in
        Some (FocusedFn (TLID.fromString sid, trace))
    | _ ->
        None
  in
  let handler () =
    match StrDict.get ~key:"handler" unstructured with
    | Some sid ->
        let trace = StrDict.get ~key:"trace" unstructured in
        Some (FocusedHandler (TLID.fromString sid, trace, true))
    | _ ->
        None
  in
  let db () =
    match StrDict.get ~key:"db" unstructured with
    | Some sid ->
        Some (FocusedDB (TLID.fromString sid, true))
    | _ ->
        None
  in
  let tipe () =
    match StrDict.get ~key:"type" unstructured with
    | Some sid ->
        Some (FocusedType (TLID.fromString sid))
    | _ ->
        None
  in
  fn ()
  |> Option.orElse (pmfn ())
  |> Option.orElse (handler ())
  |> Option.orElse (db ())
  |> Option.orElse (tipe ())
  |> Option.orElse (settingModal ())
  |> Option.orElse (architecture ())


let changeLocation (loc : Web.Location.location) : modification =
  let mPage = parseLocation loc in
  Option.map ~f:(fun x -> SetPage x) mPage
  |> Option.withDefault ~default:NoChange


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


let queryParams () : (string * bool) list =
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
  List.find ~f:(fun (k, v) -> if k = name then v else false) (queryParams ())
  |> Option.withDefault ~default:(name, false)
  |> Tuple2.second


let isDebugging () = queryParamSet "debugger"

let isIntegrationTest () = queryParamSet "integration-test"
