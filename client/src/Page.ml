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
      m.canvasProps.offset
      (* offsetForGrid (TL.id tl) m.canvasProps.offset (if m.sidebarOpen then 320 else 60) *)
    else if center
    then Viewport.centerCanvasOn tl
    else m.canvasProps.offset
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

let moveToCmd (m : model) (tlid : tlid) : msg Cmd.t =
  TL.get m tlid
  |> Option.andThen ~f:TL.getPos
  |> Option.map ~f:(fun p ->
    let x = match p.x - 200 with nx when nx > 0 -> nx | _ -> 0 in
    let y = match p.y - 100 with ny when ny > 0 -> ny | _ -> 0 in
    Viewport.moveCanvasTo x y
  )
  |> Option.withDefault ~default:Cmd.none

let setPage (m : model) (oldPage : page) (newPage : page) : model * msg Cmd.t =
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
      ({ m with
        currentPage = newPage
      ; canvasProps =
          { m.canvasProps with
            lastOffset = Some m.canvasProps.offset; offset = Defaults.origin }
      ; cursorState = Selecting (tlid, None) }, Cmd.none)
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
        ({ m with
          currentPage = newPage
        ; canvasProps = {m.canvasProps with offset = Defaults.origin}
        ; cursorState = Selecting (newtlid, None) }, Cmd.none)
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
      ({ m with
        currentPage = newPage
      ; cursorState = Selecting (tlid, None)
      }, cmd)
  | Architecture, FocusedHandler (tlid, _)
  | Architecture, FocusedDB (tlid, _)
  | Architecture, FocusedGroup (tlid, _) ->
      (* Going from Architecture to focused db/handler
  * Figure out if you can stay where you are or animate pan to toplevel pos
  *)  
    let cmd = moveToCmd m tlid in
    ({ m with
    currentPage = newPage
  ; cursorState = Selecting (tlid, None)
  }, cmd)
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
  *) (* TODO(alice) if it's witin view don't move *)
    if otlid = tlid then (m, Cmd.none) else ({ m with
    currentPage = newPage
  ; cursorState = Selecting (tlid, None)
  }, moveToCmd m tlid)
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
      ({ m with
        currentPage = newPage
      ; canvasProps =
          {m.canvasProps with offset; lastOffset = None; minimap = None} }, Cmd.none)
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
