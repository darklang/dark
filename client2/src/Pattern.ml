open! Porting
open Prelude
open Types

(* Dark *)
module B = Blank
module P = Pointer

let rec allData (p : pattern) : pointerData list =
  match p with
  | Blank _ -> [PPattern p]
  | F (_, PLiteral _) -> [PPattern p]
  | F (_, PVariable _) -> [PPattern p]
  | F (_, PConstructor (_, inner)) ->
      (PPattern p) :: (inner |> List.map allData |> List.concat)

let rec replace (search : pointerData) (replacement : pointerData) (p : pattern) :
    pattern =
  if P.toID search = B.toID p then
    match replacement with
    | PPattern replacement_ -> replacement_
    | _ -> recoverable ("cannot occur", replacement) p
  else
    match p with
    | F (id, PConstructor (cons, args)) ->
      let replacedArgs = List.map (replace search replacement) args in
      F (id, PConstructor (cons, replacedArgs))
    | _ -> p

let rec extractVariableName (p : pattern) : varName option =
  match p with
  | Blank _ | F (_, PLiteral _) -> None
  | F (_, PVariable name) -> Some name
  | F (_, PConstructor (_, args)) ->
      args
      |> List.map extractVariableName
      |> Option.values
      |> List.head

let rec contains (target : pointerData) (p : pattern) : bool =
  if P.toID target = B.toID p then
    true
  else
    match p with
    | F (_, PConstructor (cons, args)) ->
        List.any (contains target) args
    | _ -> false
