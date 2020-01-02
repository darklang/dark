open Prelude

(* Dark *)
module P = Pointer
module TL = Toplevel

let getCurrentPointer (m : model) : (toplevel * blankOrData) option =
  let myIdOf (m : model) : id option =
    match unwrapCursorState m.cursorState with
    | FluidEntering tlid ->
        let s = m.fluidState in
        TL.get m tlid
        |> Option.andThen ~f:TL.getAST
        |> Option.andThen ~f:(Fluid.getToken s)
        |> Option.map ~f:(fun ti -> FluidToken.tid ti.token)
    | _ ->
        idOf m.cursorState
  in
  match (tlidOf m.cursorState, myIdOf m) with
  | Some tlid, Some id ->
      TL.get m tlid
      |> Option.andThen ~f:(fun tl ->
             Option.map (TL.find tl id) ~f:(fun pd -> (tl, pd)))
  | _ ->
      None


let copy (m : model) : clipboardContents =
  match m.cursorState with
  | Selecting _ | FluidEntering _ ->
    ( match getCurrentPointer m with
    | Some (_, (PParamTipe _ as pd)) ->
        `Json (Encoders.blankOrData pd)
    | Some (_, other) ->
        `Text (Pointer.toContent other)
    | None ->
        `None )
  | _ ->
      `None


let cut (m : model) : clipboardContents * modification =
  match m.cursorState with
  | Selecting _ | FluidEntering _ ->
    ( match getCurrentPointer m with
    | None ->
        (`None, NoChange)
    | Some (tl, pd) ->
        let new_ = Pointer.emptyD (Pointer.typeOf pd) in
        (copy m, TL.replaceMod pd new_ tl) )
  | _ ->
      (`None, NoChange)


let paste (m : model) (data : clipboardContents) : modification =
  match getCurrentPointer m with
  | Some (tl, currentPd) ->
    ( match data with
    | `Json j ->
        let newPd = Decoders.blankOrData j in
        TL.replaceMod currentPd newPd tl
    | `Text t ->
        let newPd = Pointer.strMap currentPd ~f:(fun _ -> t) in
        TL.replaceMod currentPd newPd tl
    | `None ->
        NoChange )
  | None ->
      NoChange


let setData (data : clipboardContents) (e : clipboardEvent) =
  match data with
  | `Text text ->
      e##clipboardData##setData "text/plain" text ;
      e##preventDefault ()
  | `Json json ->
      let data = Json.stringify json in
      e##clipboardData##setData "application/json" data ;
      e##preventDefault ()
  | `None ->
      ()


let getData (e : clipboardEvent) : clipboardContents =
  let json = e##clipboardData##getData "application/json" in
  if json <> ""
  then (
    try `Json (Json.parseOrRaise json)
    with _ ->
      reportError "could not parse clipboard data" json ;
      `None )
  else
    let text = e##clipboardData##getData "text/plain" in
    if text <> "" then `Text text else `None
