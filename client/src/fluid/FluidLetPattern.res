open Tc

type t = ProgramTypes.LetPattern.t
type id = ID.t

let gid = Prelude.gid

let toID = (p: t): id =>
  switch p {
  | LPVariable(id, _) => id
  }

let ids = (p: t): list<id> => list{toID(p)}

let clone = (p: t): t =>
  switch p {
  | LPVariable(_, name) => LPVariable(gid(), name)
  }

let variableNames = (p: t): list<string> =>
  switch p {
  | LPVariable(_, name) => list{name}
  }

let hasVariableNamed = (varName: string, p: t): bool =>
  List.member(~value=varName, variableNames(p))

let isBlank = (pat: t) =>
  switch pat {
  | _ => false
  }

let findLetPattern = (patID: id, within: t): option<t> =>
  switch within {
  | LPVariable(pid, _) =>
    if patID == pid {
      Some(within)
    } else {
      None
    }
  }

let preTraversal = (~f: t => t, lp: t): t => {
  let lp = f(lp)
  switch lp {
  | LPVariable(_) => lp
  }
}

let postTraversal = (~f: t => t, lp: t): t => {
  let result = switch lp {
  | LPVariable(_) => lp
  }

  f(result)
}
