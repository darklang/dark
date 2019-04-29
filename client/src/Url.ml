open Tc
open Prelude
open Types
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
    | FocusedFn tlid ->
        [("fn", deTLID tlid)]
    | FocusedHandler tlid ->
        [("handler", deTLID tlid)]
    | FocusedDB tlid ->
        [("db", deTLID tlid)]
    | FocusedType tlid ->
        [("type", deTLID tlid)]
  in
  hashUrlParams args


let navigateTo (page : page) : msg Cmd.t = Navigation.newUrl (urlFor page)

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
           match arr with [a; b] -> Some (String.toLower a, b) | _ -> None )
    |> StrDict.fromList
  in
  let architecture () = Some Architecture in
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
  let tipe () =
    match StrDict.get ~key:"type" unstructured with
    | Some sid ->
        Some (FocusedType (TLID sid))
    | _ ->
        None
  in
  fn ()
  |> Option.orElse (handler ())
  |> Option.orElse (db ())
  |> Option.orElse (tipe ())
  |> Option.orElse (architecture ())


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
  | Some (FocusedType id) ->
    ( match TL.get m id with
    | None ->
        DisplayError "No Type with this id"
    | _ ->
        SetPage (FocusedType id) )
  | Some Architecture ->
      SetPage Architecture
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

let calculatePanOffset (m : model) (tl : toplevel) (page : page) : model =
  let offset =
    let telem = Native.Ext.querySelector (".toplevel.tl-" ^ showTLID tl.id) in
    match telem with
    | Some e ->
        let tsize =
          {w = Native.Ext.clientWidth e; h = Native.Ext.clientHeight e}
        in
        let windowSize = m.canvasProps.viewportSize in
        let sidebarWidth =
          let sidebar = Native.Ext.querySelector "#sidebar-left" in
          match sidebar with Some e -> Native.Ext.clientWidth e | None -> 320
        in
        let outerOffset =
          {m.canvasProps.offset with x = m.canvasProps.offset.x + sidebarWidth}
        in
        if Viewport.isEnclosed (outerOffset, windowSize) (tl.pos, tsize)
        then m.canvasProps.offset
        else Viewport.centerCanvasOn tl m.canvasProps
    | None ->
        m.canvasProps.offset
  in
  let panAnimation = offset <> m.canvasProps.offset in
  let boId =
    let idInToplevel id =
      match TL.find tl id with Some _ -> Some id | None -> None
    in
    match m.cursorState with
    | Selecting (tlid, sid) when tlid = tl.id ->
      (match sid with Some id -> idInToplevel id | None -> None)
    | _ ->
        None
  in
  { m with
    currentPage = page
  ; cursorState = Selecting (tl.id, boId)
  ; canvasProps = {m.canvasProps with offset; panAnimation; lastOffset = None}
  }


let setPage (m : model) (oldPage : page) (newPage : page) : model =
  match (oldPage, newPage) with
  | Architecture, FocusedFn _
  | FocusedHandler _, FocusedFn _
  | FocusedDB _, FocusedFn _
  | Architecture, FocusedType _
  | FocusedHandler _, FocusedType _
  | FocusedDB _, FocusedType _ ->
      (* Going from non-fn/type page to fn/type page.
    * Save the canvas position; set offset to origin
    *)
      { m with
        currentPage = newPage
      ; canvasProps =
          { m.canvasProps with
            lastOffset = Some m.canvasProps.offset; offset = Defaults.origin }
      ; cursorState = Deselected }
  | FocusedFn oldtlid, FocusedFn newtlid
  | FocusedType oldtlid, FocusedFn newtlid
  | FocusedFn oldtlid, FocusedType newtlid
  | FocusedType oldtlid, FocusedType newtlid ->
      (* Going between fn pages
    * Check they are not the same user function;
    * reset offset to origin, just in case user moved around on the fn page
    *)
      if oldtlid == newtlid
      then m
      else
        { m with
          currentPage = newPage
        ; canvasProps = {m.canvasProps with offset = Defaults.origin}
        ; cursorState = Deselected }
  | FocusedFn _, FocusedHandler tlid
  | FocusedFn _, FocusedDB tlid
  | FocusedType _, FocusedHandler tlid
  | FocusedType _, FocusedDB tlid ->
      (* Going from Fn/Type to focused DB/hanlder
    * Jump to position where the toplevel is located
    *)
      let tl = TL.getTL m tlid in
      { m with
        currentPage = newPage
      ; cursorState = Selecting (tlid, None)
      ; canvasProps =
          { m.canvasProps with
            offset = Viewport.toCenteredOn tl.pos; lastOffset = None } }
  | Architecture, FocusedHandler tlid | Architecture, FocusedDB tlid ->
      (* Going from Architecture to focused db/handler
  * Figure out if you can stay where you are or animate pan to toplevel pos
  *)
      let tl = TL.getTL m tlid in
      calculatePanOffset m tl newPage
  | FocusedHandler otlid, FocusedHandler tlid
  | FocusedHandler otlid, FocusedDB tlid
  | FocusedDB otlid, FocusedHandler tlid
  | FocusedDB otlid, FocusedDB tlid ->
      (* Going from focused db/handler to another focused db/handler
  * Check it is a different tl;
  * figure out if you can stay where you are or animate pan to toplevel pos
  *)
      if otlid = tlid
      then m
      else
        let tl = TL.getTL m tlid in
        calculatePanOffset m tl newPage
  | FocusedFn _, Architecture | FocusedType _, Architecture ->
      (* Going from fn back to Architecture
    * Return to the previous position you were on the canvas
    *)
      let offset =
        match m.canvasProps.lastOffset with
        | Some lo ->
            lo
        | None ->
            m.canvasProps.offset
      in
      { m with
        currentPage = newPage
      ; canvasProps = {m.canvasProps with offset; lastOffset = None} }
  | _, Architecture ->
      (* Anything else to Architecture
    * Stay where you are, Deselect
    *)
      {m with currentPage = newPage; cursorState = Deselected}


let shouldUpdateHash (m : model) (tlid : tlid) : msg Tea_cmd.t list =
  let prevTLID = tlidOf m.cursorState in
  let changedFocused =
    match prevTLID with Some tid -> tlid <> tid | None -> false
  in
  let fromArch = m.currentPage = Architecture in
  if fromArch || changedFocused
  then
    let tl = TL.getTL m tlid in
    let page = TL.asPage tl in
    [navigateTo page]
  else []
