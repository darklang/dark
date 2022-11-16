// open Tc

let title = "Canvases"

module Utils = SettingsUtils

@ppx.deriving(show)
type rec t = {
  canvasList: list<string>,
  username: string,
  orgs: list<string>,
  orgCanvasList: list<string>, // This is org canvases, not orgs themselves
  newCanvasName: Utils.formField,
  canvasName: string,
}

@ppx.deriving(show)
type rec msg =
 Update(string)

let default = {canvasList: list{}, username: "", orgs: list{}, orgCanvasList: list{}, newCanvasName: Utils.defaultFormField, canvasName: "",}

let toSaved = _ => Js.Json.null

let setInfo = (
  _state: t,
  canvasList: list<string>,
  username: string,
  orgs: list<string>,
  orgCanvasList: list<string>,
): t => {
  ..._state,
  canvasList: canvasList,
  username: username,
  orgs: orgs,
  orgCanvasList: orgCanvasList,
}

let update = (state: t, msg: msg):(t) =>
  switch msg {
    | Update(value) => ({...state, newCanvasName: {value: value, error: None}, canvasName:value})
  }

