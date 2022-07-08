open Prelude

// Dark
module TL = Toplevel
module P = Pointer
module TD = TLIDDict

let viewBrowserMessage: Html.html<msg> = Html.div(
  list{Html.class'("warning")},
  list{
    Html.p(
      list{Html.class'("title")},
      list{
        Html.text(
          "Unfortunately we only support Dark on desktop Chrome right now. Between browser different input models, differences in scripting and rendering performance, and differing web platform support, we don't have the capacity to support other browsers at the moment. We hope to support Firefox, Safari, and mobile use once we've really nailed the experience on Chrome. Thanks for understanding!",
        ),
      },
    ),
    Html.p(
      list{Html.class'("title")},
      list{
        Html.text("A "),
        Html.a(
          list{Html.href("http://darklang.com/desktop-client"), Html.target("_blank")},
          list{Html.text("desktop client")},
        ),
        Html.text(
          " is available as well. It is still in the beta phase, so the experience may be slightly different than in Chrome.",
        ),
      },
    ),
  },
)

let unsupportedBrowser = (~show: bool): Html.html<msg> =>
  if show {
    Html.div(
      list{
        Html.class'("modal-overlay"),
        Html.id("unsupportedBrowser"),
        ViewUtils.nothingMouseEvent("mousedown"),
        ViewUtils.nothingMouseEvent("mouseup"),
      },
      list{Html.div(list{Html.classList(list{("modal", true)})}, list{viewBrowserMessage})},
    )
  } else {
    Vdom.noNode
  }
