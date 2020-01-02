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


let rec variableNames (p : t) : string list =
  match p with
  | FPVariable (_, _, name) ->
      [name]
  | FPConstructor (_, _, _, patterns) ->
      patterns |> List.map ~f:variableNames |> List.concat
  | FPInteger _ | FPBool _ | FPString _ | FPBlank _ | FPNull _ | FPFloat _ ->
      []


let hasVariableNamed (varName : string) (p : fluidPattern) : bool =
  List.member ~value:varName (variableNames p)
