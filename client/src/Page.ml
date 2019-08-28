open Tc
open Types
module Cmd = Tea.Cmd
module Navigation = Tea.Navigation
module TL = Toplevel

let tlidOf (page : page) : tlid option =
  match page with
  | Architecture ->
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
    if VariantTesting.variantIsActive m GridLayout
    then
      let tlid = TL.id tl in
      let o = m.canvasProps.offset in
      let e =
        Native.Ext.querySelector (".node .tl-" ^ Prelude.showTLID tlid)
      in
      let dx, dy =
        e
        |> Option.andThen ~f:(fun e ->
               let r = Native.Ext.getBoundingClientRect e in
               let px = Native.Ext.rectLeft r |> int_of_float in
               let py = Native.Ext.rectTop r |> int_of_float in
               Some (px, py) )
        |> Option.withDefault ~default:(0, 0)
      in
      (* magic numbers come from sidebar mode, in _canvas.scss *)
      let x = o.x + dx - 360 in
      let y = o.y + dy - 100 in
      {x; y}
    else if center
    then Viewport.centerCanvasOn tl
    else m.canvasProps.offset
  in
  let panAnimation = offset <> m.canvasProps.offset in
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
  | Architecture, FocusedFn tlid
  | FocusedHandler _, FocusedFn tlid
  | FocusedDB _, FocusedFn tlid
  | FocusedGroup _, FocusedFn tlid
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
            lastOffset = Some m.canvasProps.offset; offset = Defaults.origin }
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
      let tl = TL.getExn m tlid in
      { m with
        currentPage = newPage
      ; cursorState = Selecting (tlid, None)
      ; canvasProps =
          { m.canvasProps with
            offset = Viewport.toCenteredOn (TL.pos tl); lastOffset = None } }
  | Architecture, FocusedHandler (tlid, _)
  | Architecture, FocusedDB (tlid, _)
  | Architecture, FocusedGroup (tlid, _) ->
      (* Going from Architecture to focused db/handler
  * Figure out if you can stay where you are or animate pan to toplevel pos
  *)
      let tl = TL.getExn m tlid in
      calculatePanOffset m tl newPage
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
        let tl = TL.getExn m tlid in
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
