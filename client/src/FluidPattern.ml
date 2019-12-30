open Types
open Tc
open Prelude

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


let rec fromPattern (mid : id) (p : pattern) : fluidPattern =
  match p with
  | Blank id ->
      FPBlank (mid, id)
  | F (id, np) ->
    ( match np with
    | PVariable name ->
        FPVariable (mid, id, name)
    | PConstructor (name, patterns) ->
        FPConstructor (mid, id, name, List.map ~f:(fromPattern mid) patterns)
    | PLiteral str ->
      ( match FluidUtil.parseString str with
      | `Bool b ->
          FPBool (mid, id, b)
      | `Int i ->
          FPInteger (mid, id, i)
      | `String s ->
          FPString (mid, id, s)
      | `Null ->
          FPNull (mid, id)
      | `Float (whole, fraction) ->
          FPFloat (mid, id, whole, fraction)
      | `Unknown ->
          FPBlank (mid, id) ) )


let rec clone (matchID : id) (p : t) : t =
  match p with
  | FPVariable (_, _, name) ->
      FPVariable (matchID, gid (), name)
  | FPConstructor (_, _, name, patterns) ->
      FPConstructor
        (matchID, gid (), name, List.map ~f:(fun p -> clone matchID p) patterns)
  | FPInteger (_, _, i) ->
      FPInteger (matchID, gid (), i)
  | FPBool (_, _, b) ->
      FPBool (matchID, gid (), b)
  | FPString (_, _, s) ->
      FPString (matchID, gid (), s)
  | FPBlank (_, _) ->
      FPBlank (matchID, gid ())
  | FPNull (_, _) ->
      FPNull (matchID, gid ())
  | FPFloat (_, _, whole, fraction) ->
      FPFloat (matchID, gid (), whole, fraction)


let rec variableNames (p : t) : varName list =
  match p with
  | FPVariable (_, _, name) ->
      [name]
  | FPConstructor (_, _, _, patterns) ->
      patterns |> List.map ~f:variableNames |> List.concat
  | FPInteger _ | FPBool _ | FPString _ | FPBlank _ | FPNull _ | FPFloat _ ->
      []
