module Html = Tea.Html
module Attrs = Tea.Html.Attributes

let fontAwesome = (~tw="", name: string): Html.html<'msg> =>
  Html.i(list{Attrs.class'(`fa fa-${name} ${tw}`)}, list{})

let darkIcon = (name: string): Html.html<'msg> =>
  Html.i(list{Attrs.class'("di di-" ++ name)}, list{})
