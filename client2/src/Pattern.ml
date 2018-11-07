open! Porting
module B = Blank
module P = Pointer
open Prelude
open Types

let rec allData (p : pattern) : pointerData list =
  match p with
  | Blank _ -> [PPattern p]
  | F (_, PLiteral _) -> [PPattern p]
  | F (_, PVariable _) -> [PPattern p]
  | F (_, PConstructor (_, inner)) ->
      (PPattern p) :: (inner |> List.map allData |> List.concat)
