open Tea
open! Porting
open Types

let variantIsActive (m : model) (vt : variantTest) : bool =
  List.member vt m.tests

let toVariantTest (s : string * bool) : variantTest option =
  match s with
  | _, false -> None
  | test, _ -> ( match String.toLower test with _ -> None )

let toCSSClass (vt : variantTest) : string =
  let test = match vt with StubVariant -> "stub" in
  let _ = "comment" in
  test ^ "-variant"

let uniqueTests (xs : variantTest list) : variantTest list =
  xs |> List.uniqueBy (fun x -> match x with StubVariant -> "SV")

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

let parseVariantTestsFromQueryString (s : string) : variantTest list option =
  match String.uncons s with
  | Some ('?', rest) ->
      rest |> String.split "&"
      |> List.filterMap splitOnEquals
      |> List.filterMap toVariantTest
      |> uniqueTests |> Some
  | _ -> None
