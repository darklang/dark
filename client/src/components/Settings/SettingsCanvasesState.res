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

// No extra data so just reuse `t`
@ppx.deriving(show)
type rec initData = t

let default = {canvasList: list{}, username: "", orgs: list{}, orgCanvasList: list{}}

let init = (d: initData) => d
