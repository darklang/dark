module Result = {
  include Tea_result
  let pp = (
    okValueFormatter: (Format.formatter, 'okValue) => unit,
    errValueFormatter: (Format.formatter, 'errValue) => unit,
    fmt: Format.formatter,
    value: t<'okValue, 'errValue>,
  ) => {
    switch value {
    | Ok(value) =>
      Format.pp_print_string(fmt, "Ok")
      okValueFormatter(fmt, value)
    | Error(value) =>
      Format.pp_print_string(fmt, "Error")
      errValueFormatter(fmt, value)
    }
  }
}

module Cmd = Tea_cmd
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
  let pp_error = (
    _valueFormatter: (Format.formatter, 'v) => unit,
    fmt: Format.formatter,
    _value: error<'v>,
  ) => {
    Format.pp_print_string(fmt, "Tea.Http.error")
    // valueFormatter(fmt, value)
  }
}

module Html = {
  include Tea_html2

  // TODO: upstream
  let onWithOptions = (~key="", eventName, options: Tea_html.options, decoder) =>
    Tea_html.onCB(eventName, key, event => {
      if options.stopPropagation {
        event["stopPropagation"]() |> ignore
      }
      if options.preventDefault {
        event["preventDefault"]() |> ignore
      }
      event |> Tea_json.Decoder.decodeEvent(decoder) |> Tea_result.result_to_option
    })

  type html<'a> = Vdom.t<'a>

  type property<'a> = Vdom.property<'a>

  let noNode = Vdom.noNode
}

module Attrs = Tea_html2.Attributes
module Events = Tea_html2.Events

module Time = {
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
