// open Tc

let title = "Canvases"

@ppx.deriving(show)
type rec t = {
  canvasList: list<string>,
  username: string,
  orgs: list<string>,
  orgCanvasList: list<string>, // This is org canvases, not orgs themselves
}

@ppx.deriving(show)
type rec msg = unit

let default = {canvasList: list{}, username: "", orgs: list{}, orgCanvasList: list{}}

let setInfo = (
  _state: t,
  canvasList: list<string>,
  username: string,
  orgs: list<string>,
  orgCanvasList: list<string>,
): t => {
  canvasList: canvasList,
  username: username,
  orgs: orgs,
  orgCanvasList: orgCanvasList,
}
