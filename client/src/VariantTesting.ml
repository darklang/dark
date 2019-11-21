open Tc
open Types

let variantIsActive (m : model) (vt : variantTest) : bool =
  List.member ~value:vt m.tests


(* fluid, but no status box *)
let isFluidForCustomers (vts : variantTest list) : bool =
  List.member ~value:FluidWithoutStatusVariant vts


let isFluidV2 (vts : variantTest list) : bool =
  List.member ~value:FluidVariant vts


let isFluid (vts : variantTest list) : bool =
  List.member ~value:FluidVariant vts
  || List.member ~value:FluidWithoutStatusVariant vts


let libtwitterAvailable (vts : variantTest list) : bool =
  List.member ~value:LibtwitterVariant vts


(* This is temporary to force everyone other than existing active users to use fluid while we decide whether to roll out for everyone. *)
(* To turn off fluid, add the ?fluidv2=0 or ?fluidv2=false *)
let forceFluid (_isAdmin : bool) (username : string) (vts : variantTest list) :
    variantTest list =
  let shouldForceFluid =
    let exemptUsers =
      [ "cordeliamurphy"
      ; "eagon"
      ; "geoffrey"
      ; "hkgumbs"
      ; "jgaskins"
      ; "listo"
      ; "maximfilimonov"
      ; "pmmck"
      ; "renee"
      ; "rockspot"
      ; "stevehind"
      ; "trown"
        (* XXX(JULIAN): The `test` user is here as a hack while we 
         fix integration tests to run in fluid *)
      ; "test" ]
    in
    not (List.member ~value:username exemptUsers)
  in
  if shouldForceFluid
  then
    (* Checking to see if fluid is set to false *)
    (* Checking the url string is not the best way to do this but I dont want to change the existing logic for a temporary thing *)
    let urlString = (Tea_navigation.getLocation ()).search in
    let containsFluid =
      String.contains urlString ~substring:"fluidv2=0"
      || String.contains urlString ~substring:"fluidv2=false"
      || String.contains urlString ~substring:"fluid=false"
      || String.contains urlString ~substring:"fluid=0"
    in
    if isFluid vts || containsFluid
    then vts
    else vts @ [FluidWithoutStatusVariant]
  else vts


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
    | "grid" ->
        Some GridLayout
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
    | GridLayout ->
        "grid-layout"
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
