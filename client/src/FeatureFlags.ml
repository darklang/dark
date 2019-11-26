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
      recover "cant convert flagged to flagged" expr expr
  | _ ->
      F (gid (), FeatureFlag (Blank msgId, B.new_ (), expr, B.new_ ()))


let fromFlagged (pick : pick) (expr : expr) : expr =
  match expr with
  | F (_, FeatureFlag (_, _, a, b)) ->
    (match pick with PickA -> a | PickB -> b)
  | _ ->
      recover "cant convert flagged to flagged" expr expr


let wrap (_m : model) (tl : toplevel) (pd : pointerData) : modification =
  let msgId = gid () in
  let newPd = P.exprmap (toFlagged msgId) pd in
  let newTL = TL.replace pd newPd tl in
  let focus = FocusExact (TL.id tl, msgId) in
  match newTL with
  | TLHandler h ->
      RPC ([SetHandler (h.hTLID, h.pos, h)], focus)
  | TLFunc f ->
      RPC ([SetFunction f], focus)
  | _ ->
      NoChange


let start (m : model) : modification =
  match unwrapCursorState m.cursorState with
  | Selecting (tlid, Some id) ->
      let tl = TL.get m tlid in
      let pd = Option.andThen ~f:(fun tl -> TL.find tl id) tl in
      ( match (tl, pd) with
      | Some tl, Some pd ->
          wrap m tl pd
      | _ ->
          recover
            "invalid tl and id for starting feature flag"
            (tl, pd)
            NoChange )
  | _ ->
      NoChange


let end_ (m : model) (id : id) (pick : pick) : modification =
  match
    tlidOf (unwrapCursorState m.cursorState) |> Option.andThen ~f:(TL.get m)
  with
  | None ->
      NoChange
  | Some tl ->
      let pd = TL.find tl id |> AST.recoverPD "FF.end" in
      let newPd = P.exprmap (fromFlagged pick) pd in
      let newTL = TL.replace pd newPd tl in
      let focus = FocusExact (TL.id tl, P.toID newPd) in
      ( match newTL with
      | TLHandler h ->
          RPC ([SetHandler (h.hTLID, h.pos, h)], focus)
      | TLFunc f ->
          RPC ([SetFunction f], focus)
      | _ ->
          recover "ending FF on invalid handler" (tl, id) NoChange )


let toggle (id : id) (isExpanded : bool) : modification =
  TweakModel
    (fun m_ ->
      { m_ with
        featureFlags =
          StrDict.insert ~key:(deID id) ~value:isExpanded m_.featureFlags } )
