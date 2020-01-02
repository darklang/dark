open Tc
open Types

(* Dark *)

let rec hasVariableNamed (varName : string) (p : fluidPattern) : bool =
  match p with
  | FPConstructor (_, _, _, args) ->
      List.any ~f:(hasVariableNamed varName) args
  | FPVariable (_, _, name) when name = varName ->
      true
  | _ ->
      false
