include Tea.Html

// TODO(ian): push to fork + upstream
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
