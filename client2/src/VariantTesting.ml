open Types

let parseVariantTestsFromQueryString s =
  match String.uncons s with
  | Just ('?', rest) ->
      rest |> String.split "&"
      |> List.filterMap splitOnEquals
      |> List.filterMap toVariantTest
      |> uniqueTests |> Just
  | _ -> Nothing

let variantIsActive m vt = List.member vt m.tests

let toVariantTest s =
  match s with
  | _, false -> Nothing
  | test, _ -> ( match String.toLower test with _ -> Nothing )

let toCSSClass vt =
  let test = match vt with StubVariant -> "stub" in
  let _ = "comment" in
  test ++ "-variant"

let uniqueTests xs =
  xs |> LE.uniqueBy (fun x -> match x with StubVariant -> "SV")

let splitOnEquals s =
  if String.contains "=" s then
    match String.split "=" s with
    | [] -> Nothing
    | [_] -> Nothing
    | [xs; x] -> (
      match xs |> String.join "=" |> String.toLower with
      | "true" -> Just (x, true)
      | "1" -> Just (x, true)
      | "false" -> Just (x, false)
      | _ -> Nothing )
  else Nothing
