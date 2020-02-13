open Prelude

(* Dark *)
module B = BlankOr
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


let wrap (_ : model) (tl : toplevel) (id : id) : modification =
  let replacement e : E.t =
    EFeatureFlag (gid (), "flag-name", E.newB (), e, E.newB ())
  in
  TL.getAST tl
  |> Option.map ~f:(E.update ~f:replacement id)
  |> Option.map ~f:(TL.setASTMod tl)
  |> Option.withDefault ~default:NoChange


let unwrap (_ : model) (tl : toplevel) (id : id) : modification =
  let replacement e : E.t =
    match e with
    | E.EFeatureFlag (_id, _name, _cond, default, _enabled) ->
        default
    | expr ->
        recover "tried to remove non-feature flag" expr
  in
  TL.getAST tl
  |> Option.map ~f:(E.update ~f:replacement id)
  |> Option.map ~f:(TL.setASTMod tl)
  |> Option.withDefault ~default:NoChange


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
