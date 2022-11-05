// Extensions for Tea functionality
// pp functions all deriving(show) to work
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
module Program = Tea_program
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
  // Html and Html2 are the same with a different organization. We use Html2, but we
  // were using both, so for consistency let's rename it
  include (
    Tea_html2: module type of Tea_html2
    // Modules that we're extending
      with module Attributes := Tea_html2.Attributes
      and module Events := Tea_html2.Events
  )

  type html<'a> = Vdom.t<'a>

  let noNode = Vdom.noNode
  let input = input'

  // Override so we can add our own attributes/properties
  module Attributes = {
    include Tea_html2.Attributes

    type property<'a> = Vdom.property<'a>

    // Standard properties that are not in rescript-tea
    // TODO: upstream
    let class = class'
    let classes = (classes: array<string>) => class(classes->Js.Array2.joinWith(" "))
    let role = (name: string) => Vdom.prop("role", name)
    let ariaChecked = (v: bool) => Vdom.prop("aria-checked", string_of_bool(v))
    let ariaHidden = (v: bool) => Vdom.prop("aria-hidden", string_of_bool(v))
    let spellcheck = (b: bool) => Vdom.attribute("", "spellcheck", string_of_bool(b))
  }
  module Attrs = Attributes

  module Events = {
    include Tea_html2.Events

    // TODO: upstream
    let onWithOptions = (~key="", eventName, options: Tea_html.options, decoder) =>
      Tea_html2.Events.onCB(eventName, key, event => {
        if options.stopPropagation {
          Webapi.Dom.Event.stopPropagation(Obj.magic(event))
        }
        if options.preventDefault {
          Webapi.Dom.Event.preventDefault(Obj.magic(event))
        }
        event |> Tea_json.Decoder.decodeEvent(decoder) |> Tea_result.result_to_option
      })
  }
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
      let id = Web.Window.setInterval(() => Web.Date.now() |> tagger |> callbacks.enqueue, interval)

      () => Web.Window.clearTimeout(id)
    }

    Tea_sub.registration(key, enableCall)
  }
}
