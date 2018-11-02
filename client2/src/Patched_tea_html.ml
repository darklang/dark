open Tea

(* TODO(ian): push to fork + upstream *)
let onWithOptions ?(key="") eventName (options: Tea_html.options) decoder =
  Tea_html.onCB eventName key (fun event ->
    if options.stopPropagation then event##stopPropagation () |> ignore;
    if options.preventDefault then event##preventDefault () |> ignore;
    event
    |> Tea_json.Decoder.decodeEvent decoder
    |> Tea_result.result_to_option
  )

let every interval tagger key =
  let open Vdom in
  let enableCall callbacks =
  let id = (Web.Window.setInterval (fun () -> callbacks.enqueue (tagger (Web.Date.now ())) ) interval) in
  fun () ->
    Web.Window.clearTimeout id
  in Tea_sub.registration key enableCall
