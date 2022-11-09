// Extensions for Tea functionality
// Others should be upstreamed

module Result = {
  include Tea_result
}

module Cmd = {
  include Tea_cmd
}

module Sub = Tea_sub
module App = Tea_app
module Debug = Tea_debug
module Svg = Tea_svg
module Task = Tea_task
module Json = Tea_json
module Navigation = Tea_navigation
module Random = Tea_random
module AnimationFrame = Tea_animationframe
module Mouse = Tea_mouse
module Ex = Tea_ex

module Http = {
  include Tea_http
}

module Html = {
  include (
    Tea_html: module type of Tea_html
    // Modules that we're extending
      with module Attributes := Tea_html.Attributes
      and module Events := Tea_html.Events
  )

  type html<'a> = Vdom.t<'a>
  let input = Tea_html.input'

  let noNode = Vdom.noNode

  // Override so we can add our own attributes/properties
  module Attributes = {
    include Tea_html.Attributes

    type property<'a> = Vdom.property<'a>

    // Standard properties that are not in rescript-tea
    // TODO: upstream
    let classes = (classes: array<string>) => class(classes->Js.Array2.joinWith(" "))
  }
  module Attrs = Attributes
  module Events = Tea_html.Events
}

module Events = Html.Events
module Attrs = Html.Attrs

module Time = {
  include Tea_time

  // TODO: upstream
  // Setting the key by associating it with interval,
  // will lead to rewrites of setInterval functions,
  // if two or more share the same interval.
  let every = (~key, interval, tagger) => {
    open Vdom
    let enableCall = callbacks => {
      let id = Js.Global.setIntervalFloat(
        () => Js.Date.now() |> tagger |> callbacks.enqueue,
        interval,
      )

      () => Js.Global.clearInterval(id)
    }

    Tea_sub.registration(key, enableCall)
  }
}
