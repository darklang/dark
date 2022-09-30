module State = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {openedCategories: Tc.Set.String.t}

  let encode = (s: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{("openedCategories", strSet(s.openedCategories))})
  }

  let decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    {
      openedCategories: withDefault(Tc.Set.String.empty, field("openedCategories", strSet), j),
    }
  }

  let default = {openedCategories: Tc.Set.String.empty}
}
@ppx.deriving(show({with_path: false}))
type rec msg =
  | ResetSidebar
  | MarkCategoryOpen(bool, string)
