open Tc
open Prelude
open Types

(* Dark *)
module B = Blank
module P = Pointer
module TL = Toplevel

let toFlagged (msgId : id) (expr : expr) : expr =
  match expr with
  | F (_, FeatureFlag (_, _, _, _)) ->
      impossible ("cant convert flagged to flagged", expr)
  | _ ->
      F (gid (), FeatureFlag (Blank msgId, B.new_ (), expr, B.new_ ()))


let fromFlagged (pick : pick) (expr : expr) : expr =
  match expr with
  | F (_, FeatureFlag (_, _, a, b)) ->
    (match pick with PickA -> a | PickB -> b)
  | _ ->
      impossible ("cant convert flagged to flagged", expr)


let start (m : model) : modification =
  match unwrapCursorState m.cursorState with
  | Selecting (tlid, Some id) ->
      let tl = TL.getExn m tlid in
      let pd = TL.findExn tl id in
      let msgId = gid () in
      let newPd = P.exprmap (toFlagged msgId) pd in
      let newTL = TL.replace pd newPd tl in
      let focus = FocusExact (tl.id, msgId) in
      ( match newTL.data with
      | TLHandler h ->
          RPC ([SetHandler (tl.id, tl.pos, h)], focus)
      | TLFunc f ->
          RPC ([SetFunction f], focus)
      | _ ->
          NoChange )
  | _ ->
      NoChange


let end_ (m : model) (id : id) (pick : pick) : modification =
  match tlidOf (unwrapCursorState m.cursorState) with
  | None ->
      NoChange
  | Some tlid ->
      let tl = TL.getExn m tlid in
      let pd = TL.findExn tl id in
      let newPd = P.exprmap (fromFlagged pick) pd in
      let newTL = TL.replace pd newPd tl in
      let focus = FocusExact (tl.id, P.toID newPd) in
      ( match newTL.data with
      | TLHandler h ->
          RPC ([SetHandler (tl.id, tl.pos, h)], focus)
      | TLFunc f ->
          RPC ([SetFunction f], focus)
      | _ ->
          NoChange )


let toggle (id : id) (isExpanded : bool) : modification =
  TweakModel
    (fun m_ ->
      { m_ with
        featureFlags =
          StrDict.insert ~key:(deID id) ~value:isExpanded m_.featureFlags } )
