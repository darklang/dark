open Prelude

// Dark
module B = BlankOr
module P = Pointer
module TD = TLID.Dict

type model = AppTypes.model

let fromList = (handlers: list<PT.Handler.t>): TLID.Dict.t<PT.Handler.t> =>
  handlers |> List.map(~f=(h: PT.Handler.t) => (h.tlid, h)) |> TLID.Dict.fromList

let upsert = (m: model, h: PT.Handler.t): model => {
  ...m,
  handlers: Map.add(~key=h.tlid, ~value=h, m.handlers),
}

let update = (m: model, ~tlid: TLID.t, ~f: PT.Handler.t => PT.Handler.t): model => {
  ...m,
  handlers: Map.updateIfPresent(~key=tlid, ~f, m.handlers),
}

let remove = (m: model, h: PT.Handler.t): model => {
  ...m,
  handlers: Map.remove(~key=h.tlid, m.handlers),
}

let getWorkerSchedule = (m: model, h: PT.Handler.t): option<string> =>
  switch h.spec {
  | Worker(name, _) => Map.get(~key=name, m.workerSchedules)
  | _ => None
  }
