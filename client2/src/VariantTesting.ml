open Belt
open Tea
open! Porting
open Types

let parseVariantTestsFromQueryString s =
  match String.uncons s with
  | Some ('?', rest) ->
      rest |> String.split "&"
      |> List.filterMap splitOnEquals
      |> List.filterMap toVariantTest
      |> uniqueTests |> Some
  | _ -> None

let variantIsActive m vt = List.member vt m.tests

let toVariantTest s =
  match s with
  | _, false -> None
  | test, _ -> ( match String.toLower test with _ -> None )

let toCSSClass vt =
  let test = match vt with StubVariant -> "stub" in
  let _ = "comment" in
  test ^ "-variant"

let uniqueTests xs =
  xs |> List.Extra.uniqueBy (fun x -> match x with StubVariant -> "SV")

let splitOnEquals s =
  if String.contains "=" s then
    match String.split "=" s with
    | [] -> None
    | [_] -> None
    | [xs; x] -> (
      match xs |> String.join "=" |> String.toLower with
      | "true" -> Some (x, true)
      | "1" -> Some (x, true)
      | "false" -> Some (x, false)
      | _ -> None )
  else None
