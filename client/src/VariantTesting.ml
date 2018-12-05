open! Porting
open Types

let variantIsActive (m : model) (vt : variantTest) : bool =
  List.member vt m.tests


let toVariantTest (s : string * bool) : variantTest option =
  match s with
  | _, false -> None
  | test, _ -> (
      match String.toLower test with
      | "arrowmove" -> Some ArrowMove
      | "fluid" -> Some FluidInputModel
      | "stub" -> Some StubVariant
      | _ -> None )

let toCSSClass (vt : variantTest) : string =
  let test = match vt with
      StubVariant -> "stub"
    | FluidInputModel -> "fluid"
    | ArrowMove -> "arrowmove"
    (* _ -> "default" *) (* Please never do this, let the compiler tell you if
                            you missed a variant *)
  in
  test ^ "-variant"

let uniqueTests (xs : variantTest list) : variantTest list =
  List.uniqueBy show_variantTest xs

let splitOnEquals (s : string) : (string * bool) option =
  if String.contains "=" s then
    match String.split "=" s with
    | [] -> None
    | [_] -> None
    | x :: xs -> (
      match xs |> String.join "=" |> String.toLower with
      | "true" -> Some (x, true)
      | "1" -> Some (x, true)
      | "false" -> Some (x, false)
      | _ -> None )
  else None

(* for example: having ?fluid=1 should turn on ArrowMove *)
let rec expandTest (vt : variantTest) : variantTest list =
  match vt with
    FluidInputModel -> FluidInputModel :: ([ArrowMove]
                                           |> List.map expandTest
                                           |> List.flatten)
  | x -> [x]

let parseVariantTestsFromQueryString (s : string) : variantTest list option =
  match String.uncons s with
  | Some ('?', rest) ->
      rest |> String.split "&"
      |> List.filterMap splitOnEquals
      |> List.filterMap toVariantTest
      |> List.map expandTest
      |> List.flatten
      |> uniqueTests
      |> fun x -> Some x
  | _ -> None
