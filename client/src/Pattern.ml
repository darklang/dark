open Tc
open Prelude
open Types

(* Dark *)
module B = Blank
module P = Pointer

let rec allData (p : pattern) : pointerData list =
  match p with
  | Partial _ | Blank _ ->
      [PPattern p]
  | F (_, PLiteral _) ->
      [PPattern p]
  | F (_, PVariable _) ->
      [PPattern p]
  | F (_, PConstructor (_, inner)) ->
      PPattern p :: (inner |> List.map ~f:allData |> List.concat)


let rec replace
    (search : pointerData) (replacement : pointerData) (p : pattern) : pattern
    =
  if P.toID search = B.toID p
  then
    match replacement with
    | PPattern replacement_ ->
        replacement_
    | _ ->
        recoverable ("cannot occur", replacement) p
  else
    match p with
    | F (id, PConstructor (cons, args)) ->
        let replacedArgs = List.map ~f:(replace search replacement) args in
        F (id, PConstructor (cons, replacedArgs))
    | _ ->
        p


let rec hasVariableNamed (name : varName) (p : pattern) : bool =
  match p with
  | F (_, PConstructor (_, args)) ->
      List.any ~f:(hasVariableNamed name) args
  | F (_, PVariable _) ->
      true
  | _ ->
      false


let rec variableNames (p : pattern) : varName list =
  match p with
  | Partial _ | Blank _ | F (_, PLiteral _) ->
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
