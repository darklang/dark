open Tc
open Types

(* Dark *)
module P = Pointer
module TL = Toplevel

type copyData =
  [ `Text of string
  | `Json of Js.Json.t
  | `None ]

let copy (m : model) : copyData =
  match m.cursorState with
  | Selecting _ ->
    ( match TL.getCurrent m with
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
  | Selecting _ ->
    ( match TL.getCurrent m with
    | None ->
        (`None, NoChange)
    | Some (tl, pd) ->
        let new_ = Pointer.emptyD (Pointer.typeOf pd) in
        (copy m, TL.replaceMod pd new_ tl) )
  | _ ->
      (`None, NoChange)


let paste (m : model) (data : copyData) : modification =
  match TL.getCurrent m with
  | Some (tl, currentPd) ->
    ( match data with
    | `Json j ->
        Debug.loG "pasting json" j ;
        let newPd = Decoders.pointerData j |> TL.clonePointerData in
        TL.replaceMod currentPd newPd tl
    | `Text t ->
        Debug.loG "pasting text" t ;
        let newPd =
          Pointer.strMap currentPd ~f:(fun _ -> t)
          |> TL.clonePointerData
          |> Debug.log "newpd"
        in
        TL.replaceMod currentPd newPd tl |> Debug.log "newmod"
    | `None ->
        NoChange )
  | None ->
      NoChange
