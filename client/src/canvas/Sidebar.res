module Mode = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    | DetailedMode
    | AbridgedMode

  let encode = (s: t): Js.Json.t => {
    open Json_encode_extended
    switch s {
    | DetailedMode => variant("DetailedMode", list{})
    | AbridgedMode => variant("AbridgedMode", list{})
    }
  }

  let decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    j |> variants(list{
      ("DetailedMode", variant0(DetailedMode)),
      ("AbridgedMode", variant0(AbridgedMode)),
    })
  }
}

module State = {
  @ppx.deriving(show({with_path: false}))
  type rec t = {
    mode: Mode.t,
    openedCategories: Tc.Set.String.t,
  }

  let encode = (s: t): Js.Json.t => {
    open Json_encode_extended
    object_(list{("mode", Mode.encode(s.mode)), ("openedCategories", strSet(s.openedCategories))})
  }

  let decode = (j: Js.Json.t): t => {
    open Json_decode_extended
    {
      mode: field("mode", Mode.decode, j),
      openedCategories: withDefault(Tc.Set.String.empty, field("openedCategories", strSet), j),
    }
  }

  let default = {mode: DetailedMode, openedCategories: Tc.Set.String.empty}
}
@ppx.deriving(show({with_path: false}))
type rec msg =
  | ToggleSidebarMode
  | ResetSidebar
  | MarkCategoryOpen(bool, string)
