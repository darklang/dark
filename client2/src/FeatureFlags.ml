open Belt
open Tea
open Porting
module B = Blank
module P = Pointer
open Prelude
module TL = Toplevel
open Types

let toFlagged msgId expr =
  match expr with
  | F (id, FeatureFlag (_, _, _, _)) ->
      impossible ("cant convert flagged to flagged", expr)
  | _ -> F (gid (), FeatureFlag (Blank msgId, B.new_ (), expr, B.new_ ()))

let fromFlagged pick expr =
  match expr with
  | F (_, FeatureFlag (_, _, a, b)) -> (
    match pick with PickA -> a | PickB -> b )
  | _ -> impossible ("cant convert flagged to flagged", expr)

let start m =
  match unwrapCursorState m.cursorState with
  | Selecting (tlid, Some id) -> (
      let tl = TL.getTL m tlid in
      let pd = TL.findExn tl id in
      let msgId = gid () in
      let newPd = P.exprmap (toFlagged msgId) pd in
      let newTL = TL.replace pd newPd tl in
      let focus = FocusExact (tl.id, msgId) in
      match newTL.data with
      | TLHandler h -> RPC ([SetHandler (tl.id, tl.pos, h)], focus)
      | TLFunc f -> RPC ([SetFunction f], focus)
      | _ -> NoChange )
  | _ -> NoChange

let end_ m id pick =
  match tlidOf (unwrapCursorState m.cursorState) with
  | None -> NoChange
  | Some tlid -> (
      let tl = TL.getTL m tlid in
      let pd = TL.findExn tl id in
      let newPd = P.exprmap (fromFlagged pick) pd in
      let newTL = TL.replace pd newPd tl in
      let focus = FocusExact (tl.id, P.toID newPd) in
      match newTL.data with
      | TLHandler h -> RPC ([SetHandler (tl.id, tl.pos, h)], focus)
      | TLFunc f -> RPC ([SetFunction f], focus)
      | _ -> NoChange )

let toggle m id isExpanded =
  TweakModel
    (fun m_ ->
      {m_ with featureFlags= Dict.insert (deID id) isExpanded m_.featureFlags}
      )
