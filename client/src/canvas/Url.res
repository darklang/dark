open Prelude

module Html = Tea.Html
module Attrs = Tea.Html.Attributes
module Cmd = Tea.Cmd
module Navigation = Tea.Navigation

module TL = Toplevel
module Page = AppTypes.Page
module Mod = AppTypes.Modification
type page = Page.t

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
  | SettingsModal(tab) => list{("settings", Settings.Tab.toText(tab))}
  }

  hashUrlParams(args)
}

let navigateTo = (page: page): AppTypes.cmd => Navigation.newUrl(urlFor(page))

let updateUrl = (page: page): AppTypes.cmd =>
  Tea_cmd.call(_enqueue => {
    let () = Navigation.pushState(urlFor(page))
  })

let linkFor = (page: page, class: string, content: list<Html.html<AppTypes.msg>>): Html.html<
  AppTypes.msg,
> => Html.a(list{Attrs.href(urlFor(page)), Attrs.class(class)}, content)

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

  let architecture = () => Some(Page.Architecture)
  let settingModal = () =>
    switch Map.get(~key="settings", unstructured) {
    | Some(tab) => Some(Page.SettingsModal(Settings.Tab.parse(tab)))
    | _ => None
    }

  let getTLID = (key: string): option<TLID.t> =>
    Map.get(~key, unstructured) |> Option.andThen(~f=TLID.fromString)

  let trace = Map.get(~key="trace", unstructured)

  let pmfn = () =>
    getTLID("packagemanagerfn")->Option.map(~f=tlid => Page.FocusedPackageManagerFn(tlid))
  let fn = () => getTLID("fn")->Option.map(~f=tlid => Page.FocusedFn(tlid, trace))
  let handler = () =>
    getTLID("handler")->Option.map(~f=tlid => Page.FocusedHandler(tlid, trace, true))
  let db = () => getTLID("db")->Option.map(~f=tlid => Page.FocusedDB(tlid, true))
  let typ = () => getTLID("type")->Option.map(~f=tlid => Page.FocusedType(tlid))

  fn()
  |> Option.orElse(pmfn())
  |> Option.orElse(handler())
  |> Option.orElse(db())
  |> Option.orElse(typ())
  |> Option.orElse(settingModal())
  |> Option.orElse(architecture())
}

let changeLocation = (loc: Web.Location.location): AppTypes.modification => {
  let mPage = parseLocation(loc)
  Option.map(~f=x => Mod.SetPage(x), mPage) |> Option.unwrap(~default=Mod.NoChange)
}

let splitOnEquals = (s: string): option<(string, bool)> =>
  switch String.split(~on="=", s) {
  | list{} => None
  | list{name} => Some(name, true)
  | list{name, value} => Some(name, value != "0" && value != "false")
  | _ => None
  }

let queryParams = (): list<(string, bool)> => {
  let search = Tea.Navigation.getLocation().search
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
