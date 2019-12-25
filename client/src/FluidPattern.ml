open Types

type t = Types.fluidPattern

let id (p : t) : id =
  match p with
  | FPVariable (_, id, _)
  | FPConstructor (_, id, _, _)
  | FPInteger (_, id, _)
  | FPBool (_, id, _)
  | FPString (_, id, _)
  | FPFloat (_, id, _, _)
  | FPNull (_, id)
  | FPBlank (_, id) ->
      id
  | FPOldPattern (_, p) ->
      Blank.toID p


let matchID (p : t) : id =
  match p with
  | FPVariable (mid, _, _)
  | FPConstructor (mid, _, _, _)
  | FPInteger (mid, _, _)
  | FPBool (mid, _, _)
  | FPString (mid, _, _)
  | FPFloat (mid, _, _, _)
  | FPNull (mid, _)
  | FPBlank (mid, _) ->
      mid
  | FPOldPattern (mid, _) ->
      mid


let rec toPattern (p : t) : Types.pattern =
  match p with
  | FPVariable (_, id, var) ->
      F (id, PVariable var)
  | FPConstructor (_, id, name, patterns) ->
      F (id, PConstructor (name, Tc.List.map ~f:toPattern patterns))
  | FPInteger (_, id, i) ->
      F (id, PLiteral (FluidUtil.literalToString (`Int i)))
  | FPBool (_, id, b) ->
      F (id, PLiteral (FluidUtil.literalToString (`Bool b)))
  | FPString (_, id, str) ->
      F (id, PLiteral (FluidUtil.literalToString (`String str)))
  | FPFloat (_, id, whole, fraction) ->
      F (id, PLiteral (FluidUtil.literalToString (`Float (whole, fraction))))
  | FPNull (_, id) ->
      F (id, PLiteral (FluidUtil.literalToString `Null))
  | FPBlank (_, id) ->
      Blank id
  | FPOldPattern (_, pattern) ->
      pattern
