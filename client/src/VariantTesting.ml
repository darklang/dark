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


let toVariantTest (s : string * bool) (autoFluidEnabled : bool) :
    variantTest option =
  (* We now force dark employees/admins(minus ellen) to use fluid automatically. *)
  match (s, autoFluidEnabled) with
  | (param, true), af ->
    ( match (String.toLower param, af) with
    | "stub", _ ->
        Some StubVariant
    (* If fluid is true for admins, dont use fluid *)
    | "fluidv2", true ->
        None
    (* If fluid is true for everyone other than admins, use fluid *)
    | "fluidv2", _ ->
        Some FluidVariant
    | "fluid", _ ->
        Some FluidWithoutStatusVariant
    | "libtwitter", _ ->
        Some LibtwitterVariant
    | "groups", _ ->
        Some GroupVariant
    | "grid", _ ->
        Some GridLayout
    | _ ->
        None )
  | (param, false), true ->
    ( match String.toLower param with
    (* If fluid is false for admins, show fluid *)
    | "fluidv2" ->
        Some FluidVariant
    | _ ->
        None )
  | (_, false), _ ->
      None


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

(* If user is an admin and NOT ellen then true *)
(* https://trello.com/c/uCDrNYop/1872-force-fluid-on-for-everyone-at-dark-except-ellen-but-allow-to-turn-off *)
let enableAutoFluid (isAdmin : bool) (username : string) : bool =
  if isAdmin && username != "ellen" then true else false


let enabledVariantTests (isAdmin : bool) (username : string) : variantTest list
    =
  let autoFluidEnabled = enableAutoFluid isAdmin username in
  Url.queryParams
  |> List.filterMap ~f:(fun info -> toVariantTest info autoFluidEnabled)
  |> List.map ~f:expandTest
  |> List.flatten
  |> uniqueTests


let defaultAutocompleteVisible _m : bool = true
