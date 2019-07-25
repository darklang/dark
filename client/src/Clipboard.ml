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

let getCurrent (m : model) : (toplevel * pointerData) option =
  let myIdOf (m : model) : id option =
    match unwrapCursorState m.cursorState with
    | FluidEntering tlid ->
        (* TODO: call getTokens once it merges *)
        let s = m.fluidState in
        let neighbours =
          TL.get m tlid
          |> Option.andThen ~f:TL.getAST
          |> Option.map ~f:(Fluid.fromExpr s)
          |> Option.map ~f:(Fluid.toTokens s)
          |> Option.map ~f:(Fluid.getNeighbours ~pos:s.newPos)
        in
        ( match neighbours with
        | Some (Fluid.L (_, ti), _, _) when FluidToken.isTextToken ti.token ->
            Some (FluidToken.tid ti.token)
        | Some (_, Fluid.R (_, ti), _) when FluidToken.isTextToken ti.token ->
            Some (FluidToken.tid ti.token)
        | _ ->
            failwith "no id" )
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
    ( match getCurrent m with
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
    ( match getCurrent m with
    | None ->
        (`None, NoChange)
    | Some (tl, pd) ->
        let new_ = Pointer.emptyD (Pointer.typeOf pd) in
        (copy m, TL.replaceMod pd new_ tl) )
  | _ ->
      (`None, NoChange)


let paste (m : model) (data : copyData) : modification =
  match getCurrent m with
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
