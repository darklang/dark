module Html = Tea.Html
module Events = Tea.Html.Events
module Attrs = Tea.Html.Attributes

let tw = Attrs.class // tailwind

let sectionHeading = (text: string) =>
  Html.span(list{tw("font-bold text-xl mt-3")}, list{Html.text(text)})

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
