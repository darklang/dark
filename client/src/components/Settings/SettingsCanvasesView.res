open Tc

module Html = Tea.Html
module Attrs = Tea.Html.Attributes
module Utils = SettingsUtils
module Events = Tea.Html.Events

let tw = Attrs.class
let tw2 = (c1, c2) => Attrs.class(`${c1} ${c2}`)

module T = SettingsCanvases

module C = SettingsViewComponents

let title = "Canvases"

let view = (settings: T.t): list<Html.html<'msg>> => {
  let canvasLink = c => {
    let url = "/a/" ++ c
    Html.li(
      ~unique=c,
      list{tw(%twc("list-none px-2 py-1"))},
      list{
        Html.a(
          list{
            Attrs.href(url),
            tw(%twc("no-underline text-white1 cursor-pointer hover:text-purple")),
          },
          list{Html.text(url)},
        ),
      },
    )
  }

  let canvases = if List.length(settings.canvasList) > 0 {
    List.map(settings.canvasList, ~f=canvasLink) |> Html.ul(list{})
  } else {
    Html.p(list{tw(%twc("font-text text-lg"))}, list{Html.text("No other personal canvases")})
  }

  let canvasView = list{
    Html.p(list{tw(%twc("font-text text-lg"))}, list{Html.text("Personal canvases:")}),
    C.listView(list{canvases}),
  }

  let createCanvas = {
    let field = Html.div(
      list{},
      list{
        C.input(
          ~loadStatus=LoadStatus.Success(""),
          ~style="ml-2.5 w-80",
          ~attrs=list{
            Attrs.placeholder("canvas-name"),
            Attrs.spellcheck(false),
            Events.onInput(str => AppTypes.Msg.SettingsMsg(Settings.CanvasesMsg(T.Update(str)))),
            Attrs.value(settings.newCanvasName.value),
          },
          "",
        ),
        C.errorSpan(settings.newCanvasName.error |> Option.unwrap(~default="")),
      },
    )

    let newCanvasUrl = `https://darklang.com/a/${settings.username}-${settings.newCanvasName.value}`

    let button = {
      let sharedStyle = %twc(
        "ml-3 rounded px-2.5 py-0.5 bg-grey2 hover:bg-grey1 text-white1 text-md font-semibold font-text no-underline text-white1 cursor-pointer"
      )
      if (
        Js.String.length(settings.newCanvasName.value) == 0 ||
          Js.Option.isSome(settings.newCanvasName.error)
      ) {
        Html.a(
          list{tw2(sharedStyle, %twc("bg-black3 text-grey9 hover:bg-black3"))},
          list{Html.text("Create")},
        )
      } else {
        Html.a(list{Attrs.href(newCanvasUrl), tw(sharedStyle)}, list{Html.text("Create")})
      }
    }

    let row = Html.div(
      list{tw(%twc("flex items-baseline mt-4 font-text text-lg tracking-wider"))},
      list{Html.text(`Create a new canvas: ${settings.username}-`), field, button},
    )

    Html.div(list{}, list{row})
  }

  let orgs = List.map(settings.orgCanvasList, ~f=canvasLink) |> Html.ul(list{})
  let orgView = if List.length(settings.orgCanvasList) > 0 {
    list{
      Html.p(list{tw(%twc("font-text text-lg"))}, list{Html.text("Shared canvases:")}),
      C.listView(list{orgs}),
    }
  } else {
    list{Vdom.noNode}
  }

  Belt.List.concatMany([orgView, canvasView, list{createCanvas}])
}
