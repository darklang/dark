open Tc
open Types
open Prelude

(* Dark *)
module P = Pointer
module TL = Toplevel

type copyData =
  [ `Text of string
  | `Json of Js.Json.t
  | `None ]

let getCurrentPointer (m : model) : (toplevel * pointerData) option =
  let myIdOf (m : model) : id option =
    match unwrapCursorState m.cursorState with
    | FluidEntering tlid ->
        let s = m.fluidState in
        TL.get m tlid
        |> Option.andThen ~f:TL.getAST
        |> Option.map ~f:(Fluid.fromExpr s)
        |> Option.andThen ~f:(Fluid.getToken s)
        |> Option.map ~f:(fun ti -> FluidToken.tid ti.token)
    | _ ->
        idOf m.cursorState
  in
  match (tlidOf m.cursorState, myIdOf m) with
  | Some tlid, Some id ->
      TL.get m tlid
      |> Option.andThen ~f:(fun tl ->
             Option.map (TL.find tl id) ~f:(fun pd -> (tl, pd)) )
  | _ ->
      None


let copy (m : model) : copyData =
  match m.cursorState with
  | Selecting _ | FluidEntering _ ->
    ( match getCurrentPointer m with
    | Some (_, (PExpr _ as pd))
    | Some (_, (PPattern _ as pd))
    | Some (_, (PParamTipe _ as pd)) ->
        `Json (Encoders.pointerData pd)
    | Some (_, other) ->
        Pointer.toContent other
        |> Option.map ~f:(fun text -> `Text text)
        |> Option.withDefault ~default:`None
    | None ->
        `None )
  | _ ->
      `None


let cut (m : model) : copyData * modification =
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


let paste (m : model) (data : copyData) : modification =
  match getCurrentPointer m with
  | Some (tl, currentPd) ->
    ( match data with
    | `Json j ->
        let newPd = Decoders.pointerData j |> TL.clonePointerData in
        TL.replaceMod currentPd newPd tl
    | `Text t ->
        let newPd =
          ( match currentPd with
          | PExpr (Blank id) ->
              (* If we have a blank PExpr and want to map a String into it,
              * we can create a new String literal from it. *)
              let wrapped = "\"" ^ t ^ "\"" in
              PExpr (F (id, Value wrapped))
          | _ ->
              Pointer.strMap currentPd ~f:(fun _ -> t) )
          |> TL.clonePointerData
        in
        TL.replaceMod currentPd newPd tl
    | `None ->
        NoChange )
  | None ->
      NoChange
