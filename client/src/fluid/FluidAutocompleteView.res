open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs

module Msg = AppTypes.Msg

type data = FluidTypes.AutoComplete.data
type state = FluidTypes.AutoComplete.t

let tw = Attrs.class

let viewAutocompleteItemTypes = ({item, validity}: data): Html.html<AppTypes.msg> => {
  let (args, rt) = FluidAutocomplete.asTypeStrings(item)
  let html = {
    let returnTypeHtml = {
      let returnTypeClass = switch validity {
      | FACItemInvalidReturnType(_) => %twc("text-red1")
      | _ => ""
      }

      list{Html.span(list{Attrs.class(returnTypeClass)}, list{Html.text(rt)})}
    }

    let argsHtml = switch args {
    | list{} => list{}
    | list{arg0, ...rest} =>
      let arg0Class = switch validity {
      | FACItemInvalidPipedArg(_) => %twc("text-red1")
      | _ => ""
      }

      let args = list{
        Html.span(list{Attrs.class(arg0Class)}, list{Html.text(arg0)}),
        ...List.map(~f=Html.text, rest),
      }

      args
      |> List.intersperse(~sep=Html.text(", "))
      |> (args => Belt.List.concatMany([list{Html.text("(")}, list{Html.span(list{Attrs.class(%twc("inline-block align-top overflow-hidden max-w-[25ch] text-ellipsis whitespace-nowrap"))},args)}, list{Html.text(") -> ")}]))
    }

    Belt.List.concat(argsHtml, returnTypeHtml)
  }
  Html.span(list{tw(%twc("float-right ml-8 lowercase text-[13.33px] font-code h-4"))}, html)
}

let view = (ac: state): Html.html<AppTypes.msg> => {
  let index = ac.index |> Option.unwrap(~default=-1)
  let autocompleteList = List.mapWithIndex(ac.completions, ~f=(i, {item, validity}) => {
    let class' = if validity == FACItemValid {
      "valid"
    } else {
      %twc("text-grey3")
    }
    let highlighted = index == i
    let classHighlighted =
      switch (highlighted, validity) {
      | (true, FACItemValid) => %twc("bg-highlight-color text-white2")
      | (true, _) => %twc("bg-highlight-color text-[#11262e]")
      | (false, _) => ""
    }

    let name = FluidAutocomplete.asName(item)
    let fnDisplayName = FluidUtil.fnDisplayName(name)
    let versionDisplayName = FluidUtil.versionDisplayName(name)
    let versionView = if String.length(versionDisplayName) > 0 {
      Html.span(list{tw(%twc("text-grey2 align-bottom"))}, list{Html.text(versionDisplayName)})
    } else {
      Vdom.noNode
    }

    let types = viewAutocompleteItemTypes({item: item, validity: validity})
    Html.li(
      ~unique=name,
      list{
        Attrs.classList(list{
          (%twc("list-none py-0 px-2 h-4"),true),
          ("autocomplete-item", true),
          (classHighlighted, highlighted),
          (class', true),
        }),
        EventListeners.nothingMouseEvent("mouseup"),
        ViewEntry.defaultPasteHandler,
        EventListeners.nothingMouseEvent("mousedown"),
        EventListeners.eventNoPropagation(~key="ac-" ++ name, "click", _ => Msg.FluidMsg(
          FluidAutocompleteClick(item),
        )),
        EventListeners.eventBoth(~key="ac-mousemove" ++ name, "mousemove", _ => Msg.FluidMsg(
          FluidUpdateDropdownIndex(i),
        )),
      },
      list{Html.text(fnDisplayName), versionView, types},
    )
  })
  Html.div(list{Attrs.id("fluid-dropdown"), tw(%twc("cursor-pointer bg-black3 text-white3 z-[1000] absolute -left-2 min-w-max w-full max-h-[6.25rem] overflow-y-scroll p-0 border border-solid border-black1 border-t-0 shadow-[1px_1px_1px_black] rounded-b-sm"))}, list{Html.ul(list{tw(%twc("m-0 text-white2 pr-1"))}, autocompleteList)})
}
