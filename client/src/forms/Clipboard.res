open Prelude

// Dark
module P = Pointer
module TL = Toplevel

let setData = ((text, json): clipboardContents, e: Webapi.Dom.ClipboardEvent.t) => {
  let set = (format: string, v: string) =>
    e->Webapi.Dom.ClipboardEvent.clipboardData->Webapi.Dom.DataTransfer.setData(~format, v)
  set("text/plain", text)
  Option.map(json, ~f=data => {
    set("application/json", Json.stringify(data))
  }) |> ignore
  Webapi.Dom.ClipboardEvent.preventDefault(e)
}

let getData = (e: Webapi.Dom.ClipboardEvent.t): clipboardContents => {
  let json = {
    let json =
      e
      ->Webapi.Dom.ClipboardEvent.clipboardData
      ->Webapi.Dom.DataTransfer.getData("application/json")
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
  let text = {
    e->Webapi.Dom.ClipboardEvent.clipboardData->Webapi.Dom.DataTransfer.getData("text/plain")
  }
  (text, json)
}
