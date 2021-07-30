open Prelude

let variantIsActive (m : model) (vt : variantTest) : bool =
  List.member ~value:vt m.tests


let variantIsActive' (vs : variantTest list) (t : variantTest) : bool =
  List.member ~value:t vs


let toVariantTest (s : string) : variantTest option =
  (* names in toVariantTest and nameOf should match *)
  match String.toLowercase s with
  | "stub" ->
      Some StubVariant
  | "localhost-assets" ->
      Some NgrokVariant
  | "lpartial" ->
      Some LeftPartialVariant
  | _ ->
      None


let nameOf (vt : variantTest) : string =
  (* names in toVariantTest and nameOf should match *)
  match vt with
  | StubVariant ->
      "stub"
  | NgrokVariant ->
      "localhost-assets"
  | LeftPartialVariant ->
      "lpartial"


let toCSSClass (vt : variantTest) : string = nameOf vt ^ "-variant"

let availableAdminVariants : variantTest list = [NgrokVariant]

let activeCSSClasses (m : model) : string =
  m.tests |> List.map ~f:toCSSClass |> String.join ~sep:" "


let enabledVariantTests (isAdmin : bool) : variantTest list =
  (* admins have these enabled by default, but can opt-out via query param *)
  let initial = if isAdmin then [] else [] in
  Url.queryParams ()
  (* convert a (string * bool) list to a (variantTest * bool) list,
   * ignoring any unknown query params *)
  |> List.filterMap ~f:(fun (k, enabled) ->
         toVariantTest k |> Option.map ~f:(fun vt -> (vt, enabled)))
  (* starting with the defaults above, either add or remove each variantTest *)
  |> List.fold ~initial ~f:(fun acc (vt, enabled) ->
         if enabled then vt :: acc else List.filter ~f:(fun x -> x <> vt) acc)
  |> List.uniqueBy ~f:show_variantTest
