open Tc
open Types

(* Dark *)
module B = Blank

let rec allData (p : fluidPattern) : blankOrData list =
  match p with
  | FPVariable _
  | FPInteger _
  | FPBool _
  | FPString _
  | FPFloat _
  | FPNull _
  | FPBlank _ ->
      [PPattern p]
  | FPConstructor (id, _, name, nested) ->
      PPattern p
      :: PConstructorName (id, name)
      :: (nested |> List.map ~f:allData |> List.concat)


let rec hasVariableNamed (varName : varName) (p : fluidPattern) : bool =
  match p with
  | FPConstructor (_, _, _, args) ->
      List.any ~f:(hasVariableNamed varName) args
  | FPVariable (_, _, name) when name = varName ->
      true
  | _ ->
      false


let rec variableNames (p : pattern) : varName list =
  match p with
  | Blank _ | F (_, PLiteral _) ->
      []
  | F (_, PVariable name) ->
      [name]
  | F (_, PConstructor (_, args)) ->
      args |> List.map ~f:variableNames |> List.concat


let rec extractById (p : pattern) (patternId : id) : pattern option =
  if B.toID p = patternId
  then Some p
  else
    match p with
    | F (_, PConstructor (_, args)) ->
        args
        |> List.find ~f:(fun arg -> extractById arg patternId |> Option.isSome)
    | _ ->
        None
