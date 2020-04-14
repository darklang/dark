open Prelude
module Cmd = Tea.Cmd
module Navigation = Tea.Navigation
module TL = Toplevel

let tlidOf (page : page) : TLID.t option =
  match page with
  | SettingsModal _ | Architecture ->
      None
  | FocusedFn (tlid, _)
  | FocusedHandler (tlid, _, _)
  | FocusedDB (tlid, _)
  | FocusedGroup (tlid, _)
  | FocusedType tlid ->
      Some tlid


let calculatePanOffset (m : model) (tl : toplevel) (page : page) : model =
  let center =
    match page with
    | FocusedHandler (_, _, center)
    | FocusedDB (_, center)
    | FocusedGroup (_, center) ->
        center
    | _ ->
        false
  in
  let offset =
    if center then Viewport.centerCanvasOn tl else m.canvasProps.offset
  in
  let panAnimation =
    if offset <> m.canvasProps.offset
    then AnimateTransition
    else DontAnimateTransition
  in
  let boId =
    let idInToplevel id =
      match TL.find tl id with Some _ -> Some id | None -> None
    in
    match m.cursorState with
    | Selecting (tlid, sid) when tlid = TL.id tl ->
      (match sid with Some id -> idInToplevel id | None -> None)
    | _ ->
        None
  in
  { m with
    currentPage = page
  ; cursorState = Selecting (TL.id tl, boId)
  ; canvasProps = {m.canvasProps with offset; panAnimation; lastOffset = None}
  }


let updatePageTraceId (oldPage : page) (traceID : traceID) : page =
  match oldPage with
  | FocusedHandler (tlid, _, center) ->
      FocusedHandler (tlid, Some traceID, center)
  | FocusedFn (tlid, _) ->
      FocusedFn (tlid, Some traceID)
  | _ ->
      oldPage


let traceidOf (page : page) : traceID option =
  match page with
  | FocusedDB _
  | FocusedGroup _
  | FocusedType _
  | SettingsModal _
  | Architecture ->
      None
  | FocusedFn (_, traceId) | FocusedHandler (_, traceId, _) ->
      traceId


let setPage (m : model) (oldPage : page) (newPage : page) : model =
  match (oldPage, newPage) with
  | SettingsModal _, FocusedFn (tlid, _)
  | Architecture, FocusedFn (tlid, _)
  | FocusedHandler _, FocusedFn (tlid, _)
  | FocusedDB _, FocusedFn (tlid, _)
  | FocusedGroup _, FocusedFn (tlid, _)
  | SettingsModal _, FocusedType tlid
  | Architecture, FocusedType tlid
  | FocusedHandler _, FocusedType tlid
  | FocusedDB _, FocusedType tlid
  | FocusedGroup _, FocusedType tlid ->
      (* Going from non-fn/type page to fn/type page.
    * Save the canvas position; set offset to origin
    *)
      { m with
        currentPage = newPage
      ; canvasProps =
          { m.canvasProps with
            lastOffset = Some m.canvasProps.offset
          ; offset = Defaults.origin }
      ; cursorState = Selecting (tlid, None) }
  | FocusedFn (oldtlid, otid), FocusedFn (newtlid, tid)
    when oldtlid == newtlid && otid <> tid ->
      {m with currentPage = newPage; cursorState = Selecting (newtlid, None)}
  | FocusedFn (oldtlid, _), FocusedFn (newtlid, _)
  | FocusedType oldtlid, FocusedFn (newtlid, _)
  | FocusedFn (oldtlid, _), FocusedType newtlid
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
        ; cursorState = Selecting (newtlid, None) }
  | FocusedFn _, FocusedHandler (tlid, _, _)
  | FocusedFn _, FocusedDB (tlid, _)
  | FocusedFn _, FocusedGroup (tlid, _)
  | FocusedType _, FocusedHandler (tlid, _, _)
  | FocusedType _, FocusedDB (tlid, _)
  | FocusedType _, FocusedGroup (tlid, _) ->
      (* Going from Fn/Type to focused DB/hanlder
    * Jump to position where the toplevel is located
    *)
      let tl = TL.get m tlid in
      let offset =
        Option.map ~f:(TL.pos >> Viewport.toCenteredOn) tl
        |> recoverOpt "tl not found" ~default:m.canvasProps.offset
      in
      { m with
        currentPage = newPage
      ; cursorState = Selecting (tlid, None)
      ; canvasProps =
          {m.canvasProps with offset; lastOffset = None; minimap = None} }
  | SettingsModal _, FocusedHandler (tlid, _, _)
  | SettingsModal _, FocusedDB (tlid, _)
  | SettingsModal _, FocusedGroup (tlid, _)
  | Architecture, FocusedHandler (tlid, _, _)
  | Architecture, FocusedDB (tlid, _)
  | Architecture, FocusedGroup (tlid, _) ->
      (* Going from Architecture to focused db/handler
  * Figure out if you can stay where you are or animate pan to toplevel pos
  *)
      TL.get m tlid
      |> Option.map ~f:(fun tl -> calculatePanOffset m tl newPage)
      |> recoverOpt "switching to missing tl" ~default:m
  | FocusedHandler (oldtlid, otid, _), FocusedHandler (newtlid, tid, _)
    when oldtlid == newtlid && otid <> tid ->
      {m with currentPage = newPage}
  | FocusedHandler (otlid, _, _), FocusedHandler (tlid, _, _)
  | FocusedHandler (otlid, _, _), FocusedDB (tlid, _)
  | FocusedHandler (otlid, _, _), FocusedGroup (tlid, _)
  | FocusedGroup (otlid, _), FocusedGroup (tlid, _)
  | FocusedGroup (otlid, _), FocusedHandler (tlid, _, _)
  | FocusedGroup (otlid, _), FocusedDB (tlid, _)
  | FocusedDB (otlid, _), FocusedHandler (tlid, _, _)
  | FocusedDB (otlid, _), FocusedDB (tlid, _)
  | FocusedDB (otlid, _), FocusedGroup (tlid, _) ->
      (* Going from focused db/handler to another focused db/handler
  * Check it is a different tl;
  * figure out if you can stay where you are or animate pan to toplevel pos
  *)
      if otlid = tlid
      then m
      else
        TL.get m tlid
        |> Option.map ~f:(fun tl -> calculatePanOffset m tl newPage)
        |> recoverOpt "switching to missing tl" ~default:m
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
      ; canvasProps =
          {m.canvasProps with offset; lastOffset = None; minimap = None} }
  | _, SettingsModal tab ->
      let settingsView =
        SettingsView.update m.settingsView (OpenSettingsView tab)
      in
      {m with settingsView}
  | _, Architecture ->
      (* Anything else to Architecture
    * Stay where you are, Deselect
    *)
      {m with currentPage = newPage; cursorState = Deselected}


let capMinimap (oldPage : page) (newPage : page) : msg Cmd.t list =
  match (oldPage, newPage) with
  | Architecture, FocusedFn _
  | FocusedHandler _, FocusedFn _
  | FocusedDB _, FocusedFn _
  | Architecture, FocusedType _
  | FocusedHandler _, FocusedType _
  | FocusedDB _, FocusedType _ ->
      [Native.OnCaptureView.capture ()]
  | _ ->
      []


(* Go back to Architecture view if user is on the type/fn page
    and then deletes same UserType/UserFunction *)
let maybeChangeFromPage (tlid : TLID.t) (page : page) : modification list =
  match tlidOf page with
  | Some ptlid when ptlid = tlid ->
      [SetPage Architecture]
  | _ ->
      []
