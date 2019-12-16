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


let offsetForGrid (tlid : tlid) (offset : pos) : pos =
  let open Native.Ext in
  let open Native.Window in
  let toplevelSelector = ".node .tl-" ^ Prelude.showTLID tlid in
  querySelector toplevelSelector
  |> Option.andThen ~f:(fun e ->
         let r = getBoundingClient e toplevelSelector in
         let o : Native.rect =
           { id = "canvas"
           ; left = offset.y
           ; top = offset.x
           ; right = offset.x + (viewportWidth - 360)
           ; bottom = offset.y + viewportHeight }
         in
         let topCheck = if o.top < r.top then 1 else 0 in
         let leftCheck = if o.left < r.left then 1 else 0 in
         let rightCheck = if o.right > r.right then 1 else 0 in
         let bottomCheck = if o.bottom > r.bottom then 1 else 0 in
         (* We probably want to check for partial containment, but for the case of Ellen's demo which this vflag is used for, full containment should be alright because unlike listo it does not have huge handlers *)
         if topCheck + leftCheck + rightCheck + bottomCheck != 4
         then Some {x = offset.x + r.left - 360; y = offset.y + r.top - 100}
         else None )
  |> Option.withDefault ~default:offset


let moveToCmd (m : model) (tlid : tlid) : msg Cmd.t =
  TL.get m tlid
  |> Option.map ~f:TL.pos
  |> Option.map ~f:Viewport.moveCanvasToPos
  |> Option.withDefault ~default:Cmd.none


let setPage (m : model) (oldPage : page) (newPage : page) : model * msg Cmd.t =
  match (oldPage, newPage) with
  (* Arch to Special Space *)
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
      let savePos =
        let x, y = Native.Ext.appScrollPos () in
        {x; y}
      in
      ( { m with
          currentPage = newPage
        ; canvasProps =
            { m.canvasProps with
              lastOffset = Some savePos; offset = Defaults.origin }
        ; cursorState = Selecting (tlid, None) }
      , Viewport.moveCanvasTo 0 0 )
  (* Special space to another special space *)
  | FocusedFn oldtlid, FocusedFn newtlid
  | FocusedType oldtlid, FocusedFn newtlid
  | FocusedFn oldtlid, FocusedType newtlid
  | FocusedType oldtlid, FocusedType newtlid ->
      (* Going between fn pages
    * Check they are not the same user function;
    * reset offset to origin, just in case user moved around on the fn page
    *)
      if oldtlid = newtlid
      then (m, Cmd.none)
      else
        ( { m with
            currentPage = newPage
          ; canvasProps = {m.canvasProps with offset = Defaults.origin}
          ; cursorState = Selecting (newtlid, None) }
        , Viewport.moveCanvasTo 0 0 )
  (* Special space to Arch *)
  | FocusedFn _, FocusedHandler (tlid, _)
  | FocusedFn _, FocusedDB (tlid, _)
  | FocusedFn _, FocusedGroup (tlid, _)
  | FocusedType _, FocusedHandler (tlid, _)
  | FocusedType _, FocusedDB (tlid, _)
  | FocusedType _, FocusedGroup (tlid, _) ->
      (* Going from Fn/Type to focused DB/hanlder
    * Jump to position where the toplevel is located
    *)
      let cmd = moveToCmd m tlid in
      ( {m with currentPage = newPage; cursorState = Selecting (tlid, None)}
      , cmd )
  (* Arch to Arch Focused *)
  | Architecture, FocusedHandler (tlid, _)
  | Architecture, FocusedDB (tlid, _)
  | Architecture, FocusedGroup (tlid, _) ->
      (* Going from Architecture to focused db/handler
  * Figure out if you can stay where you are or animate pan to toplevel pos
  *)
      let cmd =
        if Viewport.isToplevelVisible tlid then Cmd.none else moveToCmd m tlid
      in
      ( {m with currentPage = newPage; cursorState = Selecting (tlid, None)}
      , cmd )
  (* Arch Focused to Arch Focused *)
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
      let cmd =
        if Viewport.isToplevelVisible tlid then Cmd.none else moveToCmd m tlid
      in
      if otlid = tlid
      then (m, Cmd.none)
      else
        ( {m with currentPage = newPage; cursorState = Selecting (tlid, None)}
        , cmd )
  (* Special space to Special space *)
  | FocusedFn _, Architecture | FocusedType _, Architecture ->
      (* Going from fn back to Architecture
    * Return to the previous position you were on the canvas
    *)
      let cmd =
        match m.canvasProps.lastOffset with
        | Some lo ->
            Viewport.moveCanvasTo lo.x lo.y
        | None ->
            Cmd.none
      in
      ( { m with
          currentPage = newPage
        ; canvasProps = {m.canvasProps with lastOffset = None; minimap = None}
        }
      , cmd )
  | _, Architecture ->
      (* Anything else to Architecture
    * Stay where you are, Deselect
    *)
      ({m with currentPage = newPage; cursorState = Deselected}, Cmd.none)


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
let maybeChangeFromPage (tlid : tlid) (page : page) : modification list =
  match tlidOf page with
  | Some ptlid when ptlid = tlid ->
      [SetPage Architecture]
  | _ ->
      []
