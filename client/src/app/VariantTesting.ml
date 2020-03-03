open Prelude

let variantIsActive (m : model) (vt : variantTest) : bool =
  List.member ~value:vt m.tests


let toVariantTest (s : string * bool) : variantTest option =
  match s with
  | _, false ->
      None
  | test, _ ->
    ( match String.toLower test with
    | "stub" ->
        Some StubVariant
    | "groups" ->
        Some GroupVariant
    | "ff" ->
        Some FeatureFlagVariant
    | "invite" ->
        Some InviteVariant
    | _ ->
        None )


let toCSSClass (vt : variantTest) : string =
  let test =
    match vt with
    | StubVariant ->
        "stub"
    | GroupVariant ->
        "grouping"
    | FeatureFlagVariant ->
        "ff"
    | InviteVariant ->
        "invite"
  in
  test ^ "-variant"


let activeCSSClasses (m : model) : string =
  m.tests |> List.map ~f:toCSSClass |> String.join ~sep:" "


let uniqueTests (xs : variantTest list) : variantTest list =
  List.uniqueBy ~f:show_variantTest xs


let expandTest (vt : variantTest) : variantTest list = match vt with x -> [x]

let enabledVariantTests () : variantTest list =
  Url.queryParams ()
  |> List.filterMap ~f:toVariantTest
  |> List.map ~f:expandTest
  |> List.flatten
  |> uniqueTests


let defaultAutocompleteVisible _m : bool = true
