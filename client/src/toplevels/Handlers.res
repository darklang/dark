open Prelude

// Dark
module B = BlankOr
module P = Pointer
module TD = TLIDDict

let fromList = (handlers: list<handler>): TLIDDict.t<handler> =>
  handlers |> List.map(~f=h => (h.hTLID, h)) |> TLIDDict.fromList

let upsert = (m: model, h: handler): model => {
  ...m,
  handlers: Map.add(~key=h.hTLID, ~value=h, m.handlers),
}

let update = (m: model, ~tlid: TLID.t, ~f: handler => handler): model => {
  ...m,
  handlers: Map.updateIfPresent(~key=tlid, ~f, m.handlers),
}

let remove = (m: model, h: handler): model => {...m, handlers: Map.remove(~key=h.hTLID, m.handlers)}

let getWorkerSchedule = (m: model, h: handler): option<string> =>
  switch h.spec.name {
  | F(_, name) => Map.get(~key=name, m.workerSchedules)
  | Blank(_) => None
  }
