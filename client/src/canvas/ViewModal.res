// open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs

// Dark
module TL = Toplevel
module P = Pointer
module TD = TLID.Dict

let viewBrowserMessage: Html.html<AppTypes.msg> = Html.div(
  list{Attrs.class'("warning")},
  list{
    Html.p(
      list{Attrs.class'("title")},
      list{
        Html.text(
          "Unfortunately we only support Dark on desktop Chrome right now. Between browser different input models, differences in scripting and rendering performance, and differing web platform support, we don't have the capacity to support other browsers at the moment. We hope to support Firefox, Safari, and mobile use once we've really nailed the experience on Chrome. Thanks for understanding!",
        ),
      },
    ),
    Html.p(
      list{Attrs.class'("title")},
      list{
        Html.text("A "),
        Html.a(
          list{Attrs.href("http://darklang.com/desktop-client"), Attrs.target("_blank")},
          list{Html.text("desktop client")},
        ),
        Html.text(
          " is available as well. It is still in the beta phase, so the experience may be slightly different than in Chrome.",
        ),
      },
    ),
  },
)

let unsupportedBrowser = (~show: bool): Html.html<AppTypes.msg> =>
  if show {
    Html.div(
      list{
        Attrs.class'("modal-overlay"),
        Attrs.id("unsupportedBrowser"),
        ViewUtils.nothingMouseEvent("mousedown"),
        ViewUtils.nothingMouseEvent("mouseup"),
      },
      list{Html.div(list{Attrs.classList(list{("modal", true)})}, list{viewBrowserMessage})},
    )
  } else {
    Vdom.noNode
  }
