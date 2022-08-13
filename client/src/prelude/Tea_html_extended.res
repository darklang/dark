include Tea.Html

// TODO: push to fork + upstream
// WHAT can we remove that comment? I think it's not useful at this piont.
// WHAT is this? like .on(), but typed? `options` isn't an intuitive name here
//  (maybe even misleading - I thought it was referring to Option<>s)
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
