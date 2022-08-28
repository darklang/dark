open Prelude

let variantIsActive = (m: AppTypes.model, vt: variantTest): bool => List.member(~value=vt, m.tests)

let variantIsActive' = (vs: list<variantTest>, t: variantTest): bool => List.member(~value=t, vs)

let toVariantTest = (s: string): option<variantTest> =>
  // names in toVariantTest and nameOf should match
  switch String.toLowercase(s) {
  | "stub" => Some(StubVariant)
  | "localhost-assets" => Some(NgrokVariant)
  | "lpartial" => Some(LeftPartialVariant)
  | _ => None
  }

let nameOf = (vt: variantTest): string =>
  // names in toVariantTest and nameOf should match
  switch vt {
  | StubVariant => "stub"
  | NgrokVariant => "localhost-assets"
  | LeftPartialVariant => "lpartial"
  }

let toCSSClass = (vt: variantTest): string => nameOf(vt) ++ "-variant"

let availableAdminVariants: list<variantTest> = list{NgrokVariant}

let activeCSSClasses = (m: AppTypes.model): string =>
  m.tests |> List.map(~f=toCSSClass) |> String.join(~sep=" ")

let enabledVariantTests = (): list<variantTest> => {
  let initial = list{}
  Url.queryParams()
  |> /* convert a (string * bool) list to a (variantTest * bool) list,
   * ignoring any unknown query params */
  List.filterMap(~f=((k, enabled)) => toVariantTest(k) |> Option.map(~f=vt => (vt, enabled)))
  |> // starting with the defaults above, either add or remove each variantTest
  List.fold(~initial, ~f=(acc, (vt, enabled)) =>
    if enabled {
      list{vt, ...acc}
    } else {
      List.filter(~f=x => x != vt, acc)
    }
  )
  |> List.uniqueBy(~f=show_variantTest)
}
