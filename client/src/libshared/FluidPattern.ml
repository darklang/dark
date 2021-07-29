open Tc
open Shared

type t =
  (* match id, then pattern id *)
  | FPVariable of id * id * string
  | FPConstructor of id * id * string * t list
  (* TODO: support char *)
  (* Currently we support u62s; we will support s63s. ints in Bucklescript only support 32 bit ints but we want 63 bit int support *)
  | FPInteger of id * id * string
  | FPBool of id * id * bool
  | FPString of
      { matchID : id
      ; patternID : id
      ; str : string }
  | FPFloat of id * id * string * string
  | FPNull of id * id
  | FPBlank of id * id
[@@deriving show {with_path = false}, eq, ord, yojson {optional = true}]

let toID (p : t) : id =
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


let rec ids (p : t) : id list =
  match p with
  | FPConstructor (_, id, _, list) ->
      list |> List.map ~f:ids |> List.concat |> fun l -> id :: l
  | FPVariable _
  | FPInteger _
  | FPBool _
  | FPString _
  | FPFloat _
  | FPNull _
  | FPBlank _ ->
      [toID p]


let toMatchID (p : t) : id =
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


let hasVariableNamed (varName : string) (p : t) : bool =
  List.member ~value:varName (variableNames p)


let rec findPattern (patID : id) (within : t) : t option =
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


let rec preTraversal ~(f : t -> t) (pattern : t) : t =
  let r = preTraversal ~f in
  let pattern = f pattern in
  match pattern with
  | FPVariable _
  | FPInteger _
  | FPBool _
  | FPString _
  | FPBlank _
  | FPNull _
  | FPFloat _ ->
      pattern
  | FPConstructor (matchID, patternID, name, patterns) ->
      FPConstructor
        (matchID, patternID, name, List.map patterns ~f:(fun p -> r p))


let rec postTraversal ~(f : t -> t) (pattern : t) : t =
  let r = postTraversal ~f in
  let result =
    match pattern with
    | FPVariable _
    | FPInteger _
    | FPBool _
    | FPString _
    | FPBlank _
    | FPNull _
    | FPFloat _ ->
        pattern
    | FPConstructor (matchID, patternID, name, patterns) ->
        FPConstructor
          (matchID, patternID, name, List.map patterns ~f:(fun p -> r p))
  in
  f result
