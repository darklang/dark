module Html = Tea.Html
module Events = Tea.Html.Events
module Attrs = Tea.Html.Attributes

let tailwind = Attrs.class // tailwind

// -------------------
// Tooltips
// -------------------

module Tooltip = {
  // A tooltip, providing styling for just the outer tooltip. Includes the hover
  // mechanism for the hoverable node. The body should be styled by the provider
  let add = (node: Html.html<'msg>, body: Html.html<'msg>): Html.html<'msg> => {
    // A "top" tooltip, based on https://www.kindacode.com/article/tailwind-css-how-to-create-tooltips/
    Html.span(
      list{tailwind("mt-20 group relative duration-300")},
      list{
        node,
        Html.span(
          list{
            tailwind(
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
    Html.span(list{tailwind("text-sm text-center text-white text-left")}, list{Html.text(text)})
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
    ("translate-x-6 mr-1", "bg-indigo-600")
  } else {
    ("translate-x-0 ml-1", "bg-[#585858]")
  }

  Html.button(
    list{
      tailwind(
        `${enabledColor} relative inline-flex flex-shrink-0 h-6 w-11 border-2 border-transparent rounded-full cursor-pointer transition-colors ease-in-out duration-200 focus:outline-none`,
      ),
      msgAttr,
      Attrs.role("switch"),
      Attrs.ariaChecked(false),
    },
    list{
      Html.span(list{tailwind("sr-only")}, list{}),
      Html.span(
        list{
          tailwind(
            `${enabledPosition} mt-1 pointer-events-none inline-block h-4 w-4 rounded-full bg-white shadow transform ring-0 transition ease-in-out duration-200`,
          ),
          Attrs.ariaHidden(true),
        },
        list{},
      ),
    },
  )
}

let button = (
  ~tw="",
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
      tailwind(
        `rounded-lg h-9 px-2.5 bg-[#585858] hover:bg-[#484848] text-[#d8d8d8] cursor-pointer text-large font-bold align-top ${tw}`,
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

  Html.span(list{tailwind("font-bold text-xl mt-5")}, list{Html.text(text), info})
}

let sectionIntroText = contents =>
  Html.p(list{tailwind("mx-2 mt-1 mb-3 text-sm text-[#b8b8b8]")}, contents)

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

let input = (
  ~tw="",
  ~loadStatus: LoadStatus.t<'v>,
  ~attrs: list<Attrs.property<'msg>>,
  value: string,
) => {
  let loadingAttrs = switch loadStatus {
  | LoadStatus.Loading => list{Attrs.disabled(true)}
  | LoadStatus.Success(_)
  | LoadStatus.Error => list{Attrs.noProp}
  }
  let loadingSpinner = switch loadStatus {
  | LoadStatus.Loading => Html.i(list{Attrs.class("fa fa-spinner -ml-5 text-[#e8e8e8]")}, list{})
  | LoadStatus.Success(_)
  | LoadStatus.Error => Vdom.noNode
  }
  Html.span(
    list{},
    list{
      Html.input'(
        list{
          // TODO: move colors into theme
          Attrs.class(`${tw} rounded-sm px-2 h-9 bg-[#383838] text-[#d8d8d8] caret-[#b8b8b8]`),
          Attrs.value(value),
          ...List.concat(list{loadingAttrs, attrs}),
        },
        list{},
      ),
      loadingSpinner,
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
        list{tailwind("mt-1 flex items-center justify-between h-9")},
        list{
          Html.span(list{tailwind("flex-2")}, list{Html.text(caption), infoText}),
          Html.span(list{tailwind("flex-3")}, contents),
        },
      ),
      error,
    },
  )
}
