open Tc
open Types

(* Dark *)
module B = Blank
module P = Pointer

let rec astData (p : fluidPattern) : astData list =
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
      :: (nested |> List.map ~f:astData |> List.concat)


let rec hasVariableNamed (varName : varName) (p : fluidPattern) : bool =
  match p with
  | FPConstructor (_, _, _, args) ->
      List.any ~f:(hasVariableNamed varName) args
  | FPVariable (_, _, name) when name = varName ->
      true
  | _ ->
      false


let rec variableNames (p : fluidPattern) : varName list =
  match p with
  | FPVariable (_, _, name) ->
      [name]
  | FPInteger _ | FPBool _ | FPString _ | FPFloat _ | FPNull _ | FPBlank _ ->
      []
  | FPConstructor (_, _, _, args) ->
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
