// open Tc

// WHAT write description

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

// WHAT 'create' (or simply defining the object at the usage sit) seems more idiomatic
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

// WHAT if it's not appropriate to include _msg here, what is appropriate?
// the only usage passes in a `msg`, seemingly to fit in with other 'update' fns like this.
let update = (state: t, _msg) => state
