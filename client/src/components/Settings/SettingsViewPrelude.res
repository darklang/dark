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
  let text = (text: string): Html.html<'msg> =>
    ViewUtils.fontAwesome(~tw="text-slate-400", "info-circle")->Tooltip.add(Tooltip.text(text))

  // Show an information icon: a small "i" that you can hover to see the passed information
  let generic = (body: Html.html<'msg>): Html.html<'msg> =>
    ViewUtils.fontAwesome(~tw="text-slate-400", "info-circle")->Tooltip.add(body)
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

// -------------------
// Content components
// -------------------

let sectionHeading = (text: string, info: option<Html.html<'msg>>) => {
  let info = info->Tc.Option.map(~f=InfoIcon.generic)->Tc.Option.unwrap(~default=Html.noNode)

  Html.span(list{tw("font-bold text-xl mt-3")}, list{Html.text(text), info})
}

let settingRow = (caption: string, info: option<string>, button: Html.html<'msg>): Html.html<
  'msg,
> => {
  let infoText: Html.html<'msg> =
    info->Tc.Option.map(~f=InfoIcon.text)->Tc.Option.unwrap(~default=Html.noNode)
  Html.div(
    list{tw("flex items-center justify-between")},
    list{
      Html.span(list{tw("flex-1")}, list{Html.text(caption), infoText}),
      Html.span(list{tw("flex-1")}, list{button}),
    },
  )
}
