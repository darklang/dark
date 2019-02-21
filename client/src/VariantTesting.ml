open Tc
open Types

let variantIsActive (m : model) (vt : variantTest) : bool =
  List.member ~value:vt m.tests


let toVariantTest (s : string * bool) : variantTest option =
  match s with
  | _, false ->
      None
  | test, _ ->
    ( match String.toLower test with
    | "fluid" ->
        Some FluidInputModel
    | "stub" ->
        Some StubVariant
    | _ ->
        None )


let toCSSClass (vt : variantTest) : string =
  let test =
    match vt with StubVariant -> "stub" | FluidInputModel -> "fluid"
    (* _ -> "default" *)
    
    (* Please never do this, let the compiler tell you if
     * you missed a variant *)
  in
  test ^ "-variant"


let activeCSSClasses (m : model) : string =
  m.tests |> List.map ~f:toCSSClass |> String.join ~sep:" "


let uniqueTests (xs : variantTest list) : variantTest list =
  List.uniqueBy ~f:show_variantTest xs


let expandTest (vt : variantTest) : variantTest list =
  match vt
  with
  (* eg. | Foo -> Foo :: FluidInputModel, means Foo will turn on fluid also *)
  | x
  -> [x]


let enabledVariantTests : variantTest list =
  Url.queryParams
  |> List.filterMap ~f:toVariantTest
  |> List.map ~f:expandTest
  |> List.flatten
  |> uniqueTests


let defaultAutocompleteVisible m : bool =
  not (variantIsActive m FluidInputModel)
