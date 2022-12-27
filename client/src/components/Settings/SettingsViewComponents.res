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
      list{tw(%twc("mt-20 group relative duration-300"))},
      list{
        node,
        Html.span(
          list{
            tw(
              %twc(
                "absolute hidden group-hover:flex -left-5 -top-2 -translate-y-full w-64 px-4 py-4 bg-grey1 rounded-lg after:content-[''] after:absolute after:left-1/2 after:top-[100%] after:-translate-x-1/2 after:border-8 after:border-x-transparent after:border-b-transparent after:border-t-grey1"
              ),
            ),
          },
          list{body},
        ),
      },
    )
  }

  // Returns a text node with appropriate styling for a tooltip body
  let text = (text: string): Html.html<'msg> => {
    Html.span(list{tw(%twc("text-base font-text text-left text-white3"))}, list{Html.text(text)})
  }

    let addTooltipErrRail = (node: Html.html<'msg>, body: Html.html<'msg>): Html.html<'msg> => {
    // A "top" tooltip, based on https://www.kindacode.com/article/tailwind-css-how-to-create-tooltips/
    Html.span(
      list{tw(%twc("mt-20 group relative duration-300"))},
      list{
        node,
        Html.span(
          list{
            tw(
              %twc(
                "absolute hidden group-hover:flex -left-5 top-0 -translate-y-full w-64 px-4 py-2.5 bg-black3 rounded-lg after:content-[''] after:absolute after:left-1/2 after:top-[100%] after:-translate-x-1/2 after:border-8 after:border-x-transparent after:border-b-transparent after:border-t-grey1"
              ),
            ),
          },
          list{body},
        ),
      },
    )
  }
}

module InfoIcon = {
  // Show an information icon: a small "i" that you can hover to see the passed information
  let generic = (body: Html.html<'msg>): Html.html<'msg> =>
    Icons.fontAwesome(~style=%twc("text-sm pl-1 text-grey1"), "info-circle")->Tooltip.add(body)

  let genericErrRail = (body: Html.html<'msg>): Html.html<'msg> =>
    Icons.fontAwesome(~style=%twc("text-xs pl-1 text-grey1"), "info-circle")->Tooltip.addTooltipErrRail(body)

  // Show an information icon: a small "i" that you can hover to see the passed information
  let text = (text: string): Html.html<'msg> => generic(Tooltip.text(text))
}

// -------------------
// Interactive components
// -------------------

let toggleButton = (msgAttr: Vdom.property<'msg>, enabled: bool): Html.html<'msg> => {
  // https://tailwindui.com/components/application-ui/forms/toggles#component-92732eaa2a1e1af9d23939f08cabd44f

  let (enabledPosition, enabledColor) = if enabled {
    (%twc("translate-x-6 mr-1"), %twc("bg-purple"))
  } else {
    (%twc("translate-x-0 ml-1"), %twc("bg-grey2"))
  }

  Html.button(
    list{
      Attrs.classes([
        enabledColor,
        %twc(
          "relative inline-flex flex-shrink-0 h-6 w-11 border-2 border-transparent rounded-full cursor-pointer transition-colors ease-in-out duration-200 focus:outline-none"
        ),
      ]),
      msgAttr,
      Attrs.role("switch"),
      Attrs.ariaChecked(false),
    },
    list{
      Html.span(list{tw(%twc("sr-only"))}, list{}),
      Html.span(
        list{
          Attrs.classes([
            enabledPosition,
            %twc(
              "mt-1 pointer-events-none inline-block h-4 w-4 rounded-full bg-white3 shadow transform ring-0 transition ease-in-out duration-200"
            ),
          ]),
          Attrs.ariaHidden(true),
        },
        list{},
      ),
    },
  )
}

let button = (
  ~style="",
  msgAttr: Vdom.property<'msg>,
  saveStatus: SaveStatus.t,
  contents: list<Html.html<'msg>>,
): Html.html<'msg> => {
  let savingSpinner = switch saveStatus {
  | Saving => Icons.fontAwesome(~style=%twc("text-white2 pr-1"), "spinner")
  | Saved => Icons.fontAwesome(~style=%twc("text-green pr-1"), "check")
  | NotSaving => Vdom.noNode
  }
  let savingAttrs = switch saveStatus {
  | Saving => list{Attrs.disabled(true)}
  | Saved => list{Vdom.noProp}
  | NotSaving => list{Vdom.noProp}
  }

  Html.button(
    list{
      Attrs.classes([
        style,
        %twc(
          "rounded-lg h-7 px-2.5 bg-grey2 hover:bg-grey1 text-white1 cursor-pointer text-base font-bold align-top"
        ),
      ]),
      msgAttr,
      ...savingAttrs,
    },
    list{savingSpinner, ...contents},
  )
}

let submitBtn = (
  ~style="",
  loadingState: Vdom.property<'msg>,
  msgAttr: Vdom.property<'msg>,
  contents: list<Html.html<'msg>>,
): Html.html<'msg> => {
  Html.button(
    list{
      Attrs.classes([
        style,
        %twc(
          "flex items-center justify-center w-auto rounded py-1 px-3 ml-4 cursor-pointer text-md text-white1 bg-grey2 hover:bg-grey1"
        ),
      ]),
      loadingState,
      msgAttr,
    },
    contents,
  )
}

// -------------------
// Content components
// -------------------

let sectionHeading = (text: string, info: option<Html.html<'msg>>): Html.html<'msg> => {
  let info = info->Tc.Option.map(~f=InfoIcon.generic)->Tc.Option.unwrap(~default=Html.noNode)

  Html.span(
    list{tw(%twc("font-bold font-heading text-2xl mt-4 mb-1"))},
    list{Html.text(text), info},
  )
}

let sectionIntroText = contents =>
  Html.p(list{tw(%twc("font-heading mx-2 mt-1 mb-3 text-base text-grey8"))}, contents)

let errorSpan = (error: string): Html.html<'msg> => {
  Html.span(
    list{},
    list{Html.p(list{tw(%twc("font-text text-red h-6 m-0"))}, list{Html.text(error)})},
  )
}

let input = (
  ~style="",
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
  | LoadStatus.Loading => Icons.fontAwesome(~style=%twc("-ml-5 text-white2"), "spinner")
  | LoadStatus.Success(_)
  | LoadStatus.Error => Vdom.noNode
  }
  Html.span(
    list{},
    list{
      Html.input(
        list{
          Attrs.classes([style, %twc("rounded-sm px-2 h-7 bg-black3 text-white1 caret-grey8")]),
          Attrs.value(value),
          ...List.concat(list{loadingAttrs, attrs}),
        },
        list{},
      ),
      loadingSpinner,
    },
  )
}

let docErrorRailTooltip = (
  ~info: Vdom.t<PrettyDocs.msg>,
  ~error: option<string>,
  caption: string,
  contents: list<Html.html<'msg>>,
): Html.html<'msg> => {
  let infoText: Html.html<'msg> =
    InfoIcon.genericErrRail(info)
  let error: Html.html<'msg> =
    error->Tc.Option.map(~f=errorSpan)->Tc.Option.unwrap(~default=Html.noNode)
  Html.div(
    list{},
    list{
      Html.div(
        list{tw(%twc("flex items-center"))},
        list{
          Html.span(list{tw(%twc("font-text text-grey5"))}, contents),
          Html.span(list{tw(%twc("text-md font-text"))}, list{Html.text(caption), infoText}),
        },
      ),
      error,
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
        list{tw(%twc("flex items-center justify-between h-7"))},
        list{
          Html.span(list{tw(%twc("text-md font-text"))}, list{Html.text(caption), infoText}),
          Html.span(list{}, contents),
        },
      ),
      error,
    },
  )
}

let listView = listContent =>
  Html.div(
    list{
      tw(
        %twc(
          "min-h-[35px] max-h-[200px] overflow-y-auto overflow-x-hidden w-full bg-grey1 rounded-md scrollbar-thin scrollbar-thumb-black3 scrollbar-track-grey2 hover:scrollbar-thumb-black2 scrollbar-track-rounded-lg"
        ),
      ),
    },
    listContent,
  )
