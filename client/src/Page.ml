open Tc
open Types
open Prelude
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
  | FocusedType tlid ->
      Some tlid


let calculatePanOffset (m : model) (tl : toplevel) (page : page) : model =
  let center =
    match page with
    | FocusedHandler (_, center) | FocusedDB (_, center) ->
        center
    | _ ->
        false
  in
  let offset =
    if center
    then Viewport.centerCanvasOn tl m.canvasProps
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


let maintainActiveTrace (oldPage : page) (newPage : page) (m : model) : model =
  let validateCallerCallee fnTLID hTLID : bool =
    let fnname =
      match TL.get m fnTLID with
      | Some (TLFunc f) ->
          f.ufMetadata.ufmName
          |> Blank.toMaybe
          |> Option.withDefault ~default:""
      | _ ->
          ""
    in
    TL.get m hTLID
    |> Option.andThen ~f:TL.getAST
    |> Option.map ~f:(AST.allCallsToFn fnname)
    |> fun x -> x <> None && x <> Some []
  in
  let setCursorOrAddTrace m oldtlid newtlid =
    let trace = Analysis.getCurrentTrace m oldtlid in
    match trace with
    | Some (traceID, _) ->
        let newPageHasCurrentTrace =
          Analysis.getTrace m newtlid traceID <> None
        in
        let m =
          if newPageHasCurrentTrace
          then m
          else
            let traces =
              m.traces
              |> StrDict.update ~key:(deTLID newtlid) ~f:(function
                     | Some ts ->
                         Some ((traceID, None) :: ts)
                         (* insert traceID w no data*)
                     | _ ->
                         None )
            in
            {m with traces}
        in
        Analysis.setCursor m newtlid traceID
    | _ ->
        m
  in
  match (oldPage, newPage) with
  | FocusedFn oldtlid, FocusedHandler (newtlid, _)
    when validateCallerCallee oldtlid newtlid ->
      setCursorOrAddTrace m oldtlid newtlid
  | FocusedHandler (oldtlid, _), FocusedFn newtlid
    when validateCallerCallee newtlid oldtlid ->
      setCursorOrAddTrace m oldtlid newtlid
  | _, _ ->
      m


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
      |> maintainActiveTrace oldPage newPage
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
  | FocusedFn _, FocusedHandler (tlid, _)
  | FocusedFn _, FocusedDB (tlid, _)
  | FocusedType _, FocusedHandler (tlid, _)
  | FocusedType _, FocusedDB (tlid, _) ->
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
      |> maintainActiveTrace oldPage newPage
  | Architecture, FocusedHandler (tlid, _) | Architecture, FocusedDB (tlid, _)
    ->
      (* Going from Architecture to focused db/handler
  * Figure out if you can stay where you are or animate pan to toplevel pos
  *)
      let tl = TL.getExn m tlid in
      calculatePanOffset m tl newPage
  | FocusedHandler (otlid, _), FocusedHandler (tlid, _)
  | FocusedHandler (otlid, _), FocusedDB (tlid, _)
  | FocusedDB (otlid, _), FocusedHandler (tlid, _)
  | FocusedDB (otlid, _), FocusedDB (tlid, _) ->
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
