open Prelude
module Cmd = Tea.Cmd
module Navigation = Tea.Navigation
module TL = Toplevel

let tlidOf (page : page) : TLID.t option =
  match page with
  | SettingsModel _ | Architecture ->
      None
  | FocusedFn tlid
  | FocusedHandler (tlid, _)
  | FocusedDB (tlid, _)
  | FocusedGroup (tlid, _)
  | FocusedType tlid ->
      Some tlid


let calculatePanOffset (m : model) (tl : toplevel) (page : page) : model =
  let center =
    match page with
    | FocusedHandler (_, center)
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


let setPage (m : model) (oldPage : page) (newPage : page) : model =
  match (oldPage, newPage) with
  | SettingsModel _, FocusedFn tlid
  | Architecture, FocusedFn tlid
  | FocusedHandler _, FocusedFn tlid
  | FocusedDB _, FocusedFn tlid
  | FocusedGroup _, FocusedFn tlid
  | SettingsModel _, FocusedType tlid
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
        ; cursorState = Selecting (newtlid, None) }
  | FocusedFn _, FocusedHandler (tlid, _)
  | FocusedFn _, FocusedDB (tlid, _)
  | FocusedFn _, FocusedGroup (tlid, _)
  | FocusedType _, FocusedHandler (tlid, _)
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
  | SettingsModel _, FocusedHandler (tlid, _)
  | SettingsModel _, FocusedDB (tlid, _)
  | SettingsModel _, FocusedGroup (tlid, _)
  | Architecture, FocusedHandler (tlid, _)
  | Architecture, FocusedDB (tlid, _)
  | Architecture, FocusedGroup (tlid, _) ->
      (* Going from Architecture to focused db/handler
  * Figure out if you can stay where you are or animate pan to toplevel pos
  *)
      TL.get m tlid
      |> Option.map ~f:(fun tl -> calculatePanOffset m tl newPage)
      |> recoverOpt "switching to missing tl" ~default:m
  | FocusedHandler (otlid, _), FocusedHandler (tlid, _)
  | FocusedHandler (otlid, _), FocusedDB (tlid, _)
  | FocusedHandler (otlid, _), FocusedGroup (tlid, _)
  | FocusedGroup (otlid, _), FocusedGroup (tlid, _)
  | FocusedGroup (otlid, _), FocusedHandler (tlid, _)
  | FocusedGroup (otlid, _), FocusedDB (tlid, _)
  | FocusedDB (otlid, _), FocusedHandler (tlid, _)
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
  | _, SettingsModel _ | _, Architecture ->
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
