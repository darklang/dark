open Tc
open Prelude
open Types

(* Dark *)
module B = Blank
module P = Pointer
module TL = Toplevel
module E = FluidExpression

let toFlagged (expr : E.t) : E.t =
  match expr with
  | EFeatureFlag (_, _, _, _, _) ->
      recover "cant convert flagged to flagged" ~debug:expr expr
  | _ ->
      EFeatureFlag (gid (), "", E.newB (), expr, E.newB ())


let fromFlagged (pick : pick) (expr : E.t) : E.t =
  match expr with
  | EFeatureFlag (_, _, _, a, b) ->
    (match pick with PickA -> a | PickB -> b)
  | _ ->
      recover "cant convert flagged to flagged" ~debug:expr expr


let wrap (_m : model) (_tl : toplevel) (_id : id) : modification =
  (* TODO: needs to be reimplmented in fluid *)
  NoChange


let start (_m : model) : modification =
  (* TODO: needs to be reimplmented in fluid *)
  NoChange


let end_ (_m : model) (_id : id) (_pick : pick) : modification =
  (* TODO: needs to be reimplmented in fluid *)
  NoChange


let toggle (id : id) (isExpanded : bool) : modification =
  TweakModel
    (fun m_ ->
      { m_ with
        featureFlags =
          StrDict.insert ~key:(deID id) ~value:isExpanded m_.featureFlags })
