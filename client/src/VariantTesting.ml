open Tc
open Types

let variantIsActive (m : model) (vt : variantTest) : bool =
  List.member ~value:vt m.tests


let isFluidWithStatus (vts : variantTest list) : bool =
  List.member ~value:FluidVariant vts
  && not (List.member ~value:FluidWithoutStatusVariant vts)


let isFluid (vts : variantTest list) : bool =
  List.member ~value:FluidVariant vts
  || List.member ~value:FluidWithoutStatusVariant vts


let libtwitterAvailable (vts : variantTest list) : bool =
  List.member ~value:LibtwitterVariant vts


let toVariantTest (s : string * bool) : variantTest option =
  match s with
  | _, false ->
      None
  | test, _ ->
    ( match String.toLower test with
    | "stub" ->
        Some StubVariant
    | "fluidv2" ->
        Some FluidVariant
    | "fluid" ->
        Some FluidWithoutStatusVariant
    | "libtwitter" ->
        Some LibtwitterVariant
    | "groups" ->
        Some GroupVariant
    | _ ->
        None )


let toCSSClass (vt : variantTest) : string =
  let test =
    match vt with
    | StubVariant ->
        "stub"
    | FluidVariant ->
        "fluid"
    | FluidWithoutStatusVariant ->
        "fluid"
    | LibtwitterVariant ->
        "libtwitter"
    | GroupVariant ->
        "grouping"
    (* _ -> "default" *)
    (* Please never do this, let the compiler tell you if
     * you missed a variant *)
  in
  test ^ "-variant"


let activeCSSClasses (m : model) : string =
  m.tests |> List.map ~f:toCSSClass |> String.join ~sep:" "


let uniqueTests (xs : variantTest list) : variantTest list =
  List.uniqueBy ~f:show_variantTest xs


let expandTest (vt : variantTest) : variantTest list = match vt with x -> [x]

let enabledVariantTests : variantTest list =
  Url.queryParams
  |> List.filterMap ~f:toVariantTest
  |> List.map ~f:expandTest
  |> List.flatten
  |> uniqueTests


let defaultAutocompleteVisible _m : bool = true
