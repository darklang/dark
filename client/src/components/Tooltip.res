module Html = Tea.Html
module Attrs = Tea.Html.Attributes

let tw = Attrs.class // tailwind


module Tooltip = {
  // A tooltip, providing styling for just the outer tooltip. Includes the hover
  // mechanism for the hoverable node. The body and the position should be styled by the provider
  let add = (~style="", node: Html.html<'msg>, body: Html.html<'msg>): Html.html<'msg> => {
    // A tooltip, based on https://www.kindacode.com/article/tailwind-css-how-to-create-tooltips/
    Html.span(
      list{tw(%twc("group relative duration-300"))},
      list{
        node,
        Html.span(
          list{
            Attrs.classes([ style,
              %twc(
                "absolute hidden group-hover:flex -translate-y-full w-72 px-4 py-3 bg-grey1 rounded-lg"
              )])

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

}

module InfoIcon = {
  // Show an information icon: a small "i" that you can hover to see the passed information
  let generic = (~style="", body: Html.html<'msg>): Html.html<'msg> =>
    Icons.fontAwesome(~style=%twc("text-sm pl-1 text-grey1"), "info-circle")->Tooltip.add(body, ~style)

  // Show an information icon: a small "i" that you can hover to see the passed information
  let text = (text: string): Html.html<'msg> => generic(Tooltip.text(text))
}


let tooltip = (
  ~style="",
  ~info: Vdom.t<PrettyDocs.msg>,
  ~error: option<string>,
  caption: string,
  contents: list<Html.html<'msg>>,
): Html.html<'msg> => {
  let infoText: Html.html<'msg> =
    InfoIcon.generic(~style, info)
  let error: Html.html<'msg> =
    error->Tc.Option.map(~f=SettingsViewComponents.errorSpan)->Tc.Option.unwrap(~default=Html.noNode)
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