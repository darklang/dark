open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs

module Msg = AppTypes.Msg

type data = FluidTypes.AutoComplete.data
type state = FluidTypes.AutoComplete.t

let viewAutocompleteItemTypes = ({item, validity}: data): Html.html<AppTypes.msg> => {
  let (args, rt) = FluidAutocomplete.asTypeStrings(item)
  let html = {
    let returnTypeHtml = {
      let returnTypeClass = switch validity {
      | FACItemInvalidReturnType(_) => "invalid-culprit"
      | _ => ""
      }

      list{Html.span(list{Attrs.class(returnTypeClass)}, list{Html.text(rt)})}
    }

    let argsHtml = switch args {
    | list{} => list{}
    | list{arg0, ...rest} =>
      let arg0Class = switch validity {
      | FACItemInvalidPipedArg(_) => "invalid-culprit"
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

  Html.span(list{Attrs.class("types")}, html)
}

let view = (ac: state): Html.html<AppTypes.msg> => {
  let index = ac.index |> Option.unwrap(~default=-1)
  let autocompleteList = List.mapWithIndex(ac.completions, ~f=(i, {item, validity}) => {
    let class' = if validity == FACItemValid {
      "valid"
    } else {
      "invalid"
    }
    let highlighted = index == i
    let name = FluidAutocomplete.asName(item)
    let fnDisplayName = FluidUtil.fnDisplayName(name)
    let versionDisplayName = FluidUtil.versionDisplayName(name)
    let versionView = if String.length(versionDisplayName) > 0 {
      Html.span(list{Attrs.class("version")}, list{Html.text(versionDisplayName)})
    } else {
      Vdom.noNode
    }

    let types = viewAutocompleteItemTypes({item: item, validity: validity})
    Html.li(
      ~unique=name,
      list{
        Attrs.classList(list{
          ("autocomplete-item", true),
          ("fluid-selected", highlighted),
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

  Html.div(list{Attrs.id("fluid-dropdown")}, list{Html.ul(list{}, autocompleteList)})
}
