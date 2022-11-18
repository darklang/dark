open Tc

module Html = Tea.Html
module Attrs = Tea.Html.Attributes
module Utils = SettingsUtils
module Events = Tea.Html.Events

let tw = Attrs.class

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
    C.listView(list{canvases}),}

  let createCanvas =  {
    let field = Html.div(
      list{},
      list{
        C.input(
          ~loadStatus=LoadStatus.Success(""),
          ~style="ml-1 w-80",
          ~attrs=list{
            Attrs.placeholder("canvas-name"),
            Attrs.spellcheck(false),
            Events.onInput(str => AppTypes.Msg.SettingsMsg(
              Settings.CanvasesMsg(T.Update(str)),
              )),
            Attrs.value(settings.newCanvasName.value),
          },
          ""
        ),
        C.errorSpan(settings.newCanvasName.error |> Option.unwrap(~default="")),
      },
    )

    let newCanvasUrl= `https://darklang.com/a/${settings.username}-${settings.newCanvasName.value}`

    let button = {
      if Js.String.length(settings.newCanvasName.value) == 0 || Js.Option.isSome(settings.newCanvasName.error){
        Html.a(list{tw(%twc("ml-3 rounded h-8 px-2.5 pt-2 pb-0 bg-black3 text-grey9 text-base font-semibold no-underline text-white1"))},list{Html.text("Create")})
      } else{
      Html.a(list{Attrs.href(newCanvasUrl),tw(%twc("ml-3 rounded h-8 px-2.5 pt-2 pb-0 bg-grey2 hover:bg-grey1 text-white1 text-base font-semibold no-underline text-white1 cursor-pointer"))},list{Html.text("Create")})
      }
    }


    let row = Html.div(
        list{tw(%twc("flex items-baseline mt-4"))},
        list{Html.text(`Create a new canvas: ${settings.username}-`),field, button})

    Html.div(list{}, list{row})
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

  Belt.List.concatMany([orgView, canvasView,list{createCanvas}])
}
