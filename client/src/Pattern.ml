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


let rec hasVariableNamed (varName : string) (p : fluidPattern) : bool =
  match p with
  | FPConstructor (_, _, _, args) ->
      List.any ~f:(hasVariableNamed varName) args
  | FPVariable (_, _, name) when name = varName ->
      true
  | _ ->
      false
