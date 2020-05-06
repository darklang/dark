open Prelude

let variantIsActive (m : model) (vt : variantTest) : bool =
  List.member ~value:vt m.tests


let variantIsActive' (vs : variantTest list) (t : variantTest) : bool =
  List.member ~value:t vs


let toVariantTest (s : string) : variantTest option =
  (* names in toVariantTest and nameOf should match *)
  match String.toLower s with
  | "stub" ->
      Some StubVariant
  | "groups" ->
      Some GroupVariant
  | "localhost-assets" ->
      Some NgrokVariant
  | "lpartial" ->
      Some LeftPartialVariant
  | "fnreturn" ->
      Some FnReturnVariant
  | _ ->
      None


let nameOf (vt : variantTest) : string =
  (* names in toVariantTest and nameOf should match *)
  match vt with
  | StubVariant ->
      "stub"
  | GroupVariant ->
      "groups"
  | NgrokVariant ->
      "localhost-assets"
  | LeftPartialVariant ->
      "lpartial"
  | FnReturnVariant ->
      "fnreturn"


let toCSSClass (vt : variantTest) : string = nameOf vt ^ "-variant"

let availableAdminVariants : variantTest list =
  [FnReturnVariant; ForceWelcomeModalVariant; NgrokVariant; GroupVariant]



let activeCSSClasses (m : model) : string =
  m.tests |> List.map ~f:toCSSClass |> String.join ~sep:" "


let enabledVariantTests (isAdmin : bool) : variantTest list =
  (* admins have these enabled by default, but can opt-out via query param *)
  let init = if isAdmin then [FnReturnVariant] else [] in
  Url.queryParams ()
  (* convert a (string * bool) list to a (variantTest * bool) list,
   * ignoring any unknown query params *)
  |> List.filterMap ~f:(fun (k, enabled) ->
         toVariantTest k |> Option.map ~f:(fun vt -> (vt, enabled)))
  (* starting with the defaults above, either add or remove each variantTest *)
  |> List.foldl ~init ~f:(fun (vt, enabled) acc ->
         if enabled then vt :: acc else List.filter ~f:(fun x -> x <> vt) acc)
  |> List.uniqueBy ~f:show_variantTest
