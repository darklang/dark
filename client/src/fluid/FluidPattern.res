open Tc

type t = ProgramTypes.Pattern.t
type id = ID.t

let gid = Prelude.gid

let toID = (p: t): id =>
  switch p {
  | PVariable(_, id, _)
  | PConstructor(_, id, _, _)
  | PInteger(_, id, _)
  | PBool(_, id, _)
  | PString({patternID: id, _})
  | PFloat(_, id, _, _, _)
  | PNull(_, id)
  | PBlank(_, id) => id
  }

let rec ids = (p: t): list<id> =>
  switch p {
  | PConstructor(_, id, _, list) =>
    list |> List.map(~f=ids) |> List.flatten |> (l => list{id, ...l})
  | PVariable(_)
  | PInteger(_)
  | PBool(_)
  | PString(_)
  | PFloat(_)
  | PNull(_)
  | PBlank(_) => list{toID(p)}
  }

let toMatchID = (p: t): id =>
  switch p {
  | PVariable(mid, _, _)
  | PConstructor(mid, _, _, _)
  | PInteger(mid, _, _)
  | PBool(mid, _, _)
  | PString({matchID: mid, _})
  | PFloat(mid, _, _, _, _)
  | PNull(mid, _)
  | PBlank(mid, _) => mid
  }

let rec clone = (matchID: id, p: t): t =>
  switch p {
  | PVariable(_, _, name) => PVariable(matchID, gid(), name)
  | PConstructor(_, _, name, patterns) =>
    PConstructor(matchID, gid(), name, List.map(~f=p => clone(matchID, p), patterns))
  | PInteger(_, _, i) => PInteger(matchID, gid(), i)
  | PBool(_, _, b) => PBool(matchID, gid(), b)
  | PString({str, _}) => PString({matchID: matchID, patternID: gid(), str: str})
  | PBlank(_, _) => PBlank(matchID, gid())
  | PNull(_, _) => PNull(matchID, gid())
  | PFloat(_, _, sign, whole, fraction) => PFloat(matchID, gid(), sign, whole, fraction)
  }

let rec variableNames = (p: t): list<string> =>
  switch p {
  | PVariable(_, _, name) => list{name}
  | PConstructor(_, _, _, patterns) => patterns |> List.map(~f=variableNames) |> List.flatten
  | PInteger(_) | PBool(_) | PString(_) | PBlank(_) | PNull(_) | PFloat(_) => list{}
  }

let hasVariableNamed = (varName: string, p: t): bool =>
  List.member(~value=varName, variableNames(p))

let rec findPattern = (patID: id, within: t): option<t> =>
  switch within {
  | PVariable(_, pid, _)
  | PInteger(_, pid, _)
  | PBool(_, pid, _)
  | PNull(_, pid)
  | PBlank(_, pid)
  | PFloat(_, pid, _, _, _)
  | PString({matchID: _, patternID: pid, str: _}) =>
    if patID == pid {
      Some(within)
    } else {
      None
    }
  | PConstructor(_, pid, _, pats) =>
    if patID == pid {
      Some(within)
    } else {
      List.findMap(pats, ~f=p => findPattern(patID, p))
    }
  }

let rec preTraversal = (~f: t => t, pattern: t): t => {
  let r = preTraversal(~f)
  let pattern = f(pattern)
  switch pattern {
  | PVariable(_)
  | PInteger(_)
  | PBool(_)
  | PString(_)
  | PBlank(_)
  | PNull(_)
  | PFloat(_) => pattern
  | PConstructor(matchID, patternID, name, patterns) =>
    PConstructor(matchID, patternID, name, List.map(patterns, ~f=p => r(p)))
  }
}

let rec postTraversal = (~f: t => t, pattern: t): t => {
  let r = postTraversal(~f)
  let result = switch pattern {
  | PVariable(_)
  | PInteger(_)
  | PBool(_)
  | PString(_)
  | PBlank(_)
  | PNull(_)
  | PFloat(_) => pattern
  | PConstructor(matchID, patternID, name, patterns) =>
    PConstructor(matchID, patternID, name, List.map(patterns, ~f=p => r(p)))
  }

  f(result)
}
