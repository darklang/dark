open Tc

module Html = Tea_html_extended

module Utils = SettingsUtils

module T = SettingsCanvasesState

let title = "Canvases"

let view = (settings: T.t): list<Html.html<'msg>> => {
  let canvasLink = c => {
    let url = "/a/" ++ c
    Html.li(~unique=c, list{}, list{Html.a(list{Html.href(url)}, list{Html.text(url)})})
  }

  let canvases = if List.length(settings.canvasList) > 0 {
    List.map(settings.canvasList, ~f=canvasLink) |> Html.ul(list{})
  } else {
    Html.p(list{}, list{Html.text("No other personal canvases")})
  }

  let canvasView = list{
    Html.p(list{Html.class'("canvas-list-title")}, list{Html.text("Personal canvases:")}),
    Html.div(list{Html.class'("canvas-list")}, list{canvases}),
    Html.p(list{}, list{Html.text("Create a new canvas by navigating to the URL")}),
  }

  let orgs = List.map(settings.orgCanvasList, ~f=canvasLink) |> Html.ul(list{})
  let orgView = if List.length(settings.orgCanvasList) > 0 {
    list{
      Html.p(list{Html.class'("canvas-list-title")}, list{Html.text("Shared canvases:")}),
      Html.div(list{Html.class'("canvas-list")}, list{orgs}),
    }
  } else {
    list{Vdom.noNode}
  }

  Belt.List.concat(orgView, canvasView)
}
