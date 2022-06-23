open Prelude

// Dark
module P = Pointer
module TL = Toplevel

let setData = ((text, json): clipboardContents, e: clipboardEvent) => {
  e["clipboardData"]["setData"]("text/plain", text)
  Option.map(json, ~f=data => {
    let data = Json.stringify(data)
    e["clipboardData"]["setData"]("application/json", data)
  }) |> ignore
  e["preventDefault"]()
}

let getData = (e: clipboardEvent): clipboardContents => {
  let json = {
    let json = e["clipboardData"]["getData"]("application/json")
    if json == "" {
      None
    } else {
      try Some(Json.parseOrRaise(json)) catch {
      | _ =>
        reportError("could not parse clipboard data", json)
        None
      }
    }
  }

  (e["clipboardData"]["getData"]("text/plain"), json)
}
