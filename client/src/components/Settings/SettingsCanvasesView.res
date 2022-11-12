open Tc

module Html = Tea.Html
module Attrs = Tea.Html.Attributes
module Utils = SettingsUtils

module T = SettingsCanvases

module C = SettingsViewComponents

let title = "Canvases"

let view = (settings: T.t): list<Html.html<'msg>> => {
  let canvasLink = c => {
    let url = "/a/" ++ c
    Html.li(~unique=c, list{Attrs.class(%twc("list-none px-2 py-[5px]"))}, list{Html.a(list{Attrs.href(url),Attrs.class(%twc("no-underline text-white1 cursor-pointer hover:text-purple"))}, list{Html.text(url)})})
  }

  let canvases = if List.length(settings.canvasList) > 0 {
    List.map(settings.canvasList, ~f=canvasLink) |> Html.ul(list{})
  } else {
    Html.p(list{}, list{Html.text("No other personal canvases")})
  }

  let canvasView = list{
    Html.p(list{}, list{Html.text("Personal canvases:")}),
    C.listView(list{canvases}),
    Html.p(list{}, list{Html.text("Create a new canvas by navigating to the URL")}),
  }

  let orgs = List.map(settings.orgCanvasList, ~f=canvasLink) |> Html.ul(list{})
  let orgView = if List.length(settings.orgCanvasList) > 0 {
    list{
      Html.p(list{}, list{Html.text("Shared canvases:")}),
      C.listView(list{orgs})
    }
  } else {
    list{Vdom.noNode}
  }

  Belt.List.concat(orgView, canvasView)
}
