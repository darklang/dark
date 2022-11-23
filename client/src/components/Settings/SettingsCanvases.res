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
}

@ppx.deriving(show)
type rec msg =
 Update(string)

let default = {canvasList: list{}, username: "", orgs: list{}, orgCanvasList: list{}, newCanvasName: Utils.defaultFormField,}

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

let validateCanvasName = (newCanvasName: string): option<string> =>{
    if !Js.Re.test_(%re("/^[A-Za-z0-9-]*$/"), newCanvasName){
      Some("Invalid Canvas Name")
    }else{
        None
    }
}


let update = (state: t, msg: msg):(t) =>
  switch msg {
    | Update(value) =>
      let isInvalid= validateCanvasName(value)
      {...state, newCanvasName: {value: value, error: isInvalid}}
  }

