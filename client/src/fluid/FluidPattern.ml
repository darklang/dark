open Prelude

type t = Types.fluidPattern

let id (p : t) : id =
  match p with
  | FPVariable (_, id, _)
  | FPConstructor (_, id, _, _)
  | FPInteger (_, id, _)
  | FPBool (_, id, _)
  | FPString {patternID = id; _}
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
  | FPString {matchID = mid; _}
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
  | FPString {str; _} ->
      FPString {matchID; patternID = gid (); str}
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


let rec findPattern (patID : Types.id) (within : t) : t option =
  match within with
  | FPVariable (_, pid, _)
  | FPInteger (_, pid, _)
  | FPBool (_, pid, _)
  | FPNull (_, pid)
  | FPBlank (_, pid)
  | FPFloat (_, pid, _, _)
  | FPString {matchID = _; patternID = pid; str = _} ->
      if patID = pid then Some within else None
  | FPConstructor (_, pid, _, pats) ->
      if patID = pid
      then Some within
      else List.findMap pats ~f:(fun p -> findPattern patID p)
