open Prelude
module Cmd = Tea.Cmd
module Navigation = Tea.Navigation
module TL = Toplevel

let hashUrlParams = (params: list<(string, string)>): string => {
  let merged = List.map(~f=((k, v)) => k ++ ("=" ++ v), params)
  if merged == list{} {
    // the space here is important - https://stackoverflow.com/a/49373716/104021
    " "
  } else {
    "#" ++ String.join(~sep="&", merged)
  }
}

let urlFor = (page: page): string => {
  let args = switch page {
  | Architecture => list{}
  | FocusedPackageManagerFn(tlid) => list{("packageManagerFn", TLID.toString(tlid))}
  | FocusedFn(tlid, traceId) =>
    switch traceId {
    | Some(id) => list{("fn", TLID.toString(tlid)), ("trace", id)}
    | None => list{("fn", TLID.toString(tlid))}
    }
  | FocusedHandler(tlid, traceId, _) =>
    switch traceId {
    | Some(id) => list{("handler", TLID.toString(tlid)), ("trace", id)}
    | None => list{("handler", TLID.toString(tlid))}
    }
  | FocusedDB(tlid, _) => list{("db", TLID.toString(tlid))}
  | FocusedType(tlid) => list{("type", TLID.toString(tlid))}
  | SettingsModal(tab) => list{("settings", SettingsViewTypes.settingsTabToText(tab))}
  }

  hashUrlParams(args)
}

let navigateTo = (page: page): Cmd.t<msg> => Navigation.newUrl(urlFor(page))

let updateUrl = (page: page): Cmd.t<msg> =>
  Tea_cmd.call(_enqueue => {
    let () = Navigation.pushState(urlFor(page))
  })

let linkFor = (page: page, class_: string, content: list<Html.html<msg>>): Html.html<msg> =>
  Html.a(list{Html.href(urlFor(page)), Html.class'(class_)}, content)

let parseLocation = (loc: Web.Location.location): option<page> => {
  let unstructured =
    loc.hash
    |> String.dropLeft(~count=1)
    |> String.split(~on="&")
    |> List.map(~f=String.split(~on="="))
    |> List.filterMap(~f=arr =>
      switch arr {
      | list{a, b} => Some(String.toLowercase(a), b)
      | _ => None
      }
    )
    |> Map.String.fromList

  let architecture = () => Some(Architecture)
  let settingModal = () =>
    switch Map.get(~key="settings", unstructured) {
    | Some(tab) => Some(SettingsModal(SettingsViewTypes.settingsTabFromText(tab)))
    | _ => None
    }

  let pmfn = () =>
    switch Map.get(~key="packagemanagerfn", unstructured) {
    | Some(sid) => Some(FocusedPackageManagerFn(TLID.fromString(sid)))
    | _ => None
    }

  let fn = () =>
    switch Map.get(~key="fn", unstructured) {
    | Some(sid) =>
      let trace = Map.get(~key="trace", unstructured)
      Some(FocusedFn(TLID.fromString(sid), trace))
    | _ => None
    }

  let handler = () =>
    switch Map.get(~key="handler", unstructured) {
    | Some(sid) =>
      let trace = Map.get(~key="trace", unstructured)
      Some(FocusedHandler(TLID.fromString(sid), trace, true))
    | _ => None
    }

  let db = () =>
    switch Map.get(~key="db", unstructured) {
    | Some(sid) => Some(FocusedDB(TLID.fromString(sid), true))
    | _ => None
    }

  let tipe = () =>
    switch Map.get(~key="type", unstructured) {
    | Some(sid) => Some(FocusedType(TLID.fromString(sid)))
    | _ => None
    }

  fn()
  |> Option.orElse(pmfn())
  |> Option.orElse(handler())
  |> Option.orElse(db())
  |> Option.orElse(tipe())
  |> Option.orElse(settingModal())
  |> Option.orElse(architecture())
}

let changeLocation = (loc: Web.Location.location): modification => {
  let mPage = parseLocation(loc)
  Option.map(~f=x => SetPage(x), mPage) |> Option.unwrap(~default=NoChange)
}

let splitOnEquals = (s: string): option<(string, bool)> =>
  switch String.split(~on="=", s) {
  | list{} => None
  | list{name} => Some(name, true)
  | list{name, value} => Some(name, value != "0" && value != "false")
  | _ => None
  }

let queryParams = (): list<(string, bool)> => {
  let search = Tea_navigation.getLocation().search
  switch String.uncons(search) {
  | Some('?', rest) =>
    rest |> String.toLowercase |> String.split(~on="&") |> List.filterMap(~f=splitOnEquals)
  | _ => list{}
  }
}

let queryParamSet = (name: string): bool => List.find(~f=((k, v)) =>
    if k == name {
      v
    } else {
      false
    }
  , queryParams()) |> Option.unwrap(~default=(name, false)) |> Tuple2.second

let isDebugging = () => queryParamSet("debugger")

let isIntegrationTest = () => queryParamSet("integration-test")
