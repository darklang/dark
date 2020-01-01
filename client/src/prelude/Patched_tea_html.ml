(* TODO(ian): push to fork + upstream *)
let onWithOptions ?(key = "") eventName (options : Tea_html.options) decoder =
  Tea_html.onCB eventName key (fun event ->
      if options.stopPropagation then event##stopPropagation () |> ignore ;
      if options.preventDefault then event##preventDefault () |> ignore ;
      event
      |> Tea_json.Decoder.decodeEvent decoder
      |> Tea_result.result_to_option)
