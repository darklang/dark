type t = Types.fluidPattern

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
