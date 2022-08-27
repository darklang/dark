module Html = Tea.Html
module Events = Tea.Html.Events
module Attrs = Tea.Html.Attributes

let tw = Attrs.class // tailwind

// -------------------
// Tooltips
// -------------------

module Tooltip = {
  // A tooltip, providing styling for just the outer tooltip. Includes the hover
  // mechanism for the hoverable node. The body should be styled by the provider
  let add = (node: Html.html<'msg>, body: Html.html<'msg>): Html.html<'msg> => {
    // A "top" tooltip, based on https://www.kindacode.com/article/tailwind-css-how-to-create-tooltips/
    Html.span(
      list{tw("mt-20 group relative duration-300")},
      list{
        node,
        Html.span(
          list{
            tw(
              "absolute hidden group-hover:flex -left-5 -top-2 -translate-y-full w-48 px-2 py-1 bg-gray-700 rounded-lg after:content-[''] after:absolute after:left-1/2 after:top-[100%] after:-translate-x-1/2 after:border-8 after:border-x-transparent after:border-b-transparent after:border-t-gray-700",
            ),
          },
          list{body},
        ),
      },
    )
  }

  // Returns a text node with approproate styling for a tooltip body
  let text = (text: string): Html.html<'msg> => {
    Html.span(list{tw("text-sm text-center text-white text-left")}, list{Html.text(text)})
  }
}

module InfoIcon = {
  // Show an information icon: a small "i" that you can hover to see the passed information
  let generic = (body: Html.html<'msg>): Html.html<'msg> =>
    Icons.fontAwesome(~tw="text-sm px-1 text-[#484848]", "info-circle")->Tooltip.add(body)

  // Show an information icon: a small "i" that you can hover to see the passed information
  let text = (text: string): Html.html<'msg> => generic(Tooltip.text(text))
}

// -------------------
// Interactive components
// -------------------

let toggleButton = (msgAttr: Vdom.property<'msg>, enabled: bool): Html.html<'msg> => {
  // https://tailwindui.com/components/application-ui/forms/toggles#component-92732eaa2a1e1af9d23939f08cabd44f

  let (enabledPosition, enabledColor) = if enabled {
    ("translate-x-5", "bg-indigo-600")
  } else {
    ("translate-x-0", "bg-grey-200")
  }

  Html.button(
    list{
      tw(
        `${enabledColor} relative inline-flex flex-shrink-0 h-6 w-11 border-2 border-transparent rounded-full cursor-pointer transition-colors ease-in-out duration-200 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500`,
      ),
      msgAttr,
      Attrs.role("switch"),
      Attrs.ariaChecked(false),
    },
    list{
      Html.span(list{tw("sr-only")}, list{}),
      Html.span(
        list{
          tw(
            `${enabledPosition} mt-0.5 pointer-events-none inline-block h-5 w-5 rounded-full bg-white shadow transform ring-0 transition ease-in-out duration-200`,
          ),
          Attrs.ariaHidden(true),
        },
        list{},
      ),
    },
  )
}

let button = (
  msgAttr: Vdom.property<'msg>,
  saveStatus: SaveStatus.t,
  contents: list<Html.html<'msg>>,
): Html.html<'msg> => {
  let savingSpinner = switch saveStatus {
  | Saving => Html.i(list{Attrs.class("fa fa-spinner text-[#e8e8e8] px-1")}, list{})
  | Saved => Html.i(list{Attrs.class("fa fa-check text-[#a1b56c] px-1")}, list{})
  | NotSaving => Vdom.noNode
  }

  Html.button(
    list{
      tw(
        "rounded h-9 px-2.5 bg-[#585858] hover:bg-[#484848] text-[#d8d8d8] cursor-pointer text-xl font-bold align-top",
      ),
      msgAttr,
    },
    list{savingSpinner, ...contents},
  )
}

// -------------------
// Content components
// -------------------

let sectionHeading = (text: string, info: option<Html.html<'msg>>): Html.html<'msg> => {
  let info = info->Tc.Option.map(~f=InfoIcon.generic)->Tc.Option.unwrap(~default=Html.noNode)

  Html.span(list{tw("font-bold text-xl mt-3")}, list{Html.text(text), info})
}

let errorSpan = (error: string): Html.html<'msg> => {
  Html.span(
    list{},
    list{
      Html.p(
        // TODO: use tailwind
        list{Attrs.class("error-text")},
        list{Html.text(error)},
      ),
    },
  )
}

let settingRow = (
  ~info: option<string>,
  ~error: option<string>,
  caption: string,
  contents: list<Html.html<'msg>>,
): Html.html<'msg> => {
  let infoText: Html.html<'msg> =
    info->Tc.Option.map(~f=InfoIcon.text)->Tc.Option.unwrap(~default=Html.noNode)
  let error: Html.html<'msg> =
    error->Tc.Option.map(~f=errorSpan)->Tc.Option.unwrap(~default=Html.noNode)
  Html.div(
    list{},
    list{
      Html.div(
        list{tw("flex items-center justify-between h-9")},
        list{
          Html.span(list{tw("flex-1")}, list{Html.text(caption), infoText}),
          Html.span(list{tw("flex-3")}, contents),
        },
      ),
      error,
    },
  )
}
