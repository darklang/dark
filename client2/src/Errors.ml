open Helpers

module DisplayClientError = struct
  let decode =
    let open Tea.Json.Decoder in
    map (fun msg -> msg)
      (field "detail" string)
  let listen ?(key="") tagger =
    Helpers.registerGlobal "displayError" key tagger decode
end