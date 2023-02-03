module Html = Tea.Html
module Attrs = Tea.Html.Attributes

let fontAwesome = (~style="", name: string): Html.html<'msg> =>
  Html.i(list{Attrs.class(`fa fa-${name} ${style}`)}, list{})

let fontAwesomeBrands = (~style="", name: string): Html.html<'msg> =>
  Html.i(list{Attrs.class(`fab fa-${name} font-brands ${style}`)}, list{})

let darkIcon = (name: string): Html.html<'msg> =>
  Html.i(list{Attrs.class("di di-" ++ name)}, list{})
