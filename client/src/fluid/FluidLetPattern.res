open Tc

type t = ProgramTypes.LetPattern.t
type id = ID.t

let gid = Prelude.gid

let toID = (p: t): id =>
  switch p {
  | LPVariable(id, _)
  | LPTuple(id, _, _, _) => id
  }

let ids = (p: t): list<id> =>
  list{toID(p)}

let clone = (p: t): t =>
  switch p {
  | LPVariable(_, name) => LPVariable(gid(), name)
  | LPTuple(_, first, second, theRest) =>
    LPTuple(gid(), first, second, List.map(~f=p => p, theRest))
  }

let variableNames = (p: t): list<string> =>
  switch p {
  | LPVariable(_, name) => list{name}
  | LPTuple(_, first, second, theRest) =>
    list{first, second, ...theRest}
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
  | LPTuple(pid, _first, _second, _theRest) =>
    if patID == pid {
      Some(within)
    } else {
      None
    }
  }

let preTraversal = (~f: t => t, mp: t): t => {
  let mp = f(mp)
  switch mp {
  | LPVariable(_) => mp
  | LPTuple(patternID, first, second, theRest) =>
    LPTuple(patternID, first, second, List.map(theRest, ~f=p => p))
  }
}

let postTraversal = (~f: t => t, mp: t): t => {
  let result = switch mp {
  | LPVariable(_) => mp
  | LPTuple(patternID, first, second, theRest) =>
    LPTuple(patternID, first, second, List.map(theRest, ~f=p => p))
  }

  f(result)
}

