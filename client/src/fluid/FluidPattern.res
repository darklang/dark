open Tc

type t = ProgramTypes.Pattern.t
type id = ID.t

let gid = Prelude.gid

let toID = (p: t): id =>
  switch p {
  | PVariable(id, _)
  | PConstructor(id, _, _)
  | PInteger(id, _)
  | PBool(id, _)
  | PString(id, _)
  | PCharacter(id, _)
  | PFloat(id, _, _, _)
  | PNull(id)
  | PBlank(id) => id
  }

let rec ids = (p: t): list<id> =>
  switch p {
  | PConstructor(id, _, list) => list |> List.map(~f=ids) |> List.flatten |> (l => list{id, ...l})
  | PVariable(_)
  | PInteger(_)
  | PBool(_)
  | PString(_)
  | PCharacter(_)
  | PFloat(_)
  | PNull(_)
  | PBlank(_) => list{toID(p)}
  }

let rec clone = (p: t): t =>
  switch p {
  | PVariable(_, name) => PVariable(gid(), name)
  | PConstructor(_, name, patterns) =>
    PConstructor(gid(), name, List.map(~f=p => clone(p), patterns))
  | PInteger(_, i) => PInteger(gid(), i)
  | PBool(_, b) => PBool(gid(), b)
  | PString(_, str) => PString(gid(), str)
  | PCharacter(_, str) => PCharacter(gid(), str)
  | PBlank(_) => PBlank(gid())
  | PNull(_) => PNull(gid())
  | PFloat(_, sign, whole, fraction) => PFloat(gid(), sign, whole, fraction)
  }

let rec variableNames = (p: t): list<string> =>
  switch p {
  | PVariable(_, name) => list{name}
  | PConstructor(_, _, patterns) => patterns |> List.map(~f=variableNames) |> List.flatten
  | PInteger(_) | PBool(_) | PString(_) | PCharacter(_) | PBlank(_) | PNull(_) | PFloat(_) => list{}
  }

let hasVariableNamed = (varName: string, p: t): bool =>
  List.member(~value=varName, variableNames(p))

let rec findPattern = (patID: id, within: t): option<t> =>
  switch within {
  | PVariable(pid, _)
  | PInteger(pid, _)
  | PBool(pid, _)
  | PNull(pid)
  | PBlank(pid)
  | PFloat(pid, _, _, _)
  | PCharacter(pid, _)
  | PString(pid, _) =>
    if patID == pid {
      Some(within)
    } else {
      None
    }
  | PConstructor(pid, _, pats) =>
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
  | PCharacter(_)
  | PBlank(_)
  | PNull(_)
  | PFloat(_) => pattern
  | PConstructor(patternID, name, patterns) =>
    PConstructor(patternID, name, List.map(patterns, ~f=p => r(p)))
  }
}

let rec postTraversal = (~f: t => t, pattern: t): t => {
  let r = postTraversal(~f)
  let result = switch pattern {
  | PVariable(_)
  | PInteger(_)
  | PBool(_)
  | PString(_)
  | PCharacter(_)
  | PBlank(_)
  | PNull(_)
  | PFloat(_) => pattern
  | PConstructor(patternID, name, patterns) =>
    PConstructor(patternID, name, List.map(patterns, ~f=p => r(p)))
  }

  f(result)
}
