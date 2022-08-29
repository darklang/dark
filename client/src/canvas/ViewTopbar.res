// open Prelude

module Html = Tea.Html
module Attrs = Tea.Attrs

module Msg = AppTypes.Msg

let msgLink = (~key: string, content: Html.html<AppTypes.msg>, handler: AppTypes.msg): Html.html<
  AppTypes.msg,
> => {
  let event = EventListeners.eventNeither(~key, "mouseup", _ => handler)
  Html.a(list{event, Attrs.class'("")}, list{content})
}

let html = (_m: AppTypes.model) =>
  /* If you need to add a topbar, the steps are:
   * - set the default in Defaults.ml
   * - edit the text and links below
   * - change the name of the key in serializedEditor, in encoders.ml
   *   and decoders.ml. Otherwise, the user's old "showTopbar" setting
   *   will be used.
   */
  if false /* m.showTopbar */ {
    let url = {
      let qp = ""
      let loc = {...Tea.Navigation.getLocation(), search: qp}
      loc.protocol ++ ("//" ++ (loc.host ++ (loc.pathname ++ (loc.search ++ loc.hash))))
    }

    list{
      Html.div(
        list{Attrs.styles(list{}), Attrs.classList(list{("topbar", true)})},
        list{
          Html.a(
            list{
              Attrs.href(url),
              EventListeners.eventNoPropagation(~key="toggle-topbar", "mouseup", _ => Msg.IgnoreMsg(
                "topbar",
              )),
            },
            list{Html.text("Fill in message here")},
          ),
          msgLink(~key="hide-topbar", Html.text("(hide)"), HideTopbar),
        },
      ),
    }
  } else {
    list{}
  }
