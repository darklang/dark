open Tc

type t = ProgramTypes.MatchPattern.t
type id = ID.t

let gid = Prelude.gid

let toID = (p: t): id =>
  switch p {
  | MPVariable(id, _)
  | MPConstructor(id, _, _)
  | MPInteger(id, _)
  | MPBool(id, _)
  | MPString(id, _)
  | MPCharacter(id, _)
  | MPFloat(id, _, _, _)
  | MPNull(id)
  | MPBlank(id)
  | MPTuple(id, _, _, _) => id
  }

let rec ids = (p: t): list<id> =>
  switch p {
  | MPConstructor(id, _, list) => list |> List.map(~f=ids) |> List.flatten |> (l => list{id, ...l})

  | MPTuple(id, first, second, theRest) =>
    list{first, second, ...theRest} |> List.map(~f=ids) |> List.flatten |> (l => list{id, ...l})

  | MPVariable(_)
  | MPInteger(_)
  | MPBool(_)
  | MPString(_)
  | MPCharacter(_)
  | MPFloat(_)
  | MPNull(_)
  | MPBlank(_) => list{toID(p)}
  }

let rec clone = (p: t): t =>
  switch p {
  | MPVariable(_, name) => MPVariable(gid(), name)
  | MPConstructor(_, name, patterns) =>
    MPConstructor(gid(), name, List.map(~f=p => clone(p), patterns))
  | MPInteger(_, i) => MPInteger(gid(), i)
  | MPBool(_, b) => MPBool(gid(), b)
  | MPString(_, str) => MPString(gid(), str)
  | MPCharacter(_, str) => MPCharacter(gid(), str)
  | MPBlank(_) => MPBlank(gid())
  | MPNull(_) => MPNull(gid())
  | MPFloat(_, sign, whole, fraction) => MPFloat(gid(), sign, whole, fraction)
  | MPTuple(_, first, second, theRest) =>
    MPTuple(gid(), clone(first), clone(second), List.map(~f=p => clone(p), theRest))
  }

let rec variableNames = (p: t): list<string> =>
  switch p {
  | MPVariable(_, name) => list{name}
  | MPConstructor(_, _, patterns) => patterns |> List.map(~f=variableNames) |> List.flatten
  | MPTuple(_, first, second, theRest) =>
    list{first, second, ...theRest} |> List.map(~f=variableNames) |> List.flatten
  | MPInteger(_)
  | MPBool(_)
  | MPString(_)
  | MPCharacter(_)
  | MPBlank(_)
  | MPNull(_)
  | MPFloat(_) => list{}
  }

let hasVariableNamed = (varName: string, p: t): bool =>
  List.member(~value=varName, variableNames(p))

let isPatternBlank = (pat: t) =>
  switch pat {
  | MPBlank(_) => true
  | _ => false
  }

let rec findPattern = (patID: id, within: t): option<t> =>
  switch within {
  | MPVariable(pid, _)
  | MPInteger(pid, _)
  | MPBool(pid, _)
  | MPNull(pid)
  | MPBlank(pid)
  | MPFloat(pid, _, _, _)
  | MPCharacter(pid, _)
  | MPString(pid, _) =>
    if patID == pid {
      Some(within)
    } else {
      None
    }
  | MPConstructor(pid, _, pats) =>
    if patID == pid {
      Some(within)
    } else {
      List.findMap(pats, ~f=p => findPattern(patID, p))
    }
  | MPTuple(pid, first, second, theRest) =>
    if patID == pid {
      Some(within)
    } else {
      list{first, second, ...theRest} |> List.findMap(~f=p => findPattern(patID, p))
    }
  }

let rec preTraversal = (~f: t => t, pattern: t): t => {
  let r = preTraversal(~f)
  let pattern = f(pattern)
  switch pattern {
  | MPVariable(_)
  | MPInteger(_)
  | MPBool(_)
  | MPString(_)
  | MPCharacter(_)
  | MPBlank(_)
  | MPNull(_)
  | MPFloat(_) => pattern
  | MPConstructor(patternID, name, patterns) =>
    MPConstructor(patternID, name, List.map(patterns, ~f=p => r(p)))
  | MPTuple(patternID, first, second, theRest) =>
    MPTuple(patternID, r(first), r(second), List.map(theRest, ~f=p => r(p)))
  }
}

let rec postTraversal = (~f: t => t, pattern: t): t => {
  let r = postTraversal(~f)
  let result = switch pattern {
  | MPVariable(_)
  | MPInteger(_)
  | MPBool(_)
  | MPString(_)
  | MPCharacter(_)
  | MPBlank(_)
  | MPNull(_)
  | MPFloat(_) => pattern
  | MPConstructor(patternID, name, patterns) =>
    MPConstructor(patternID, name, List.map(patterns, ~f=p => r(p)))
  | MPTuple(patternID, first, second, theRest) =>
    MPTuple(patternID, r(first), r(second), List.map(theRest, ~f=p => r(p)))
  }

  f(result)
}

let recurseDeprecated = (~f: t => t, pattern: t): t =>
  switch pattern {
  | MPVariable(_)
  | MPInteger(_)
  | MPBool(_)
  | MPString(_)
  | MPCharacter(_)
  | MPBlank(_)
  | MPNull(_)
  | MPFloat(_) => pattern
  | MPConstructor(patternID, name, patterns) =>
    MPConstructor(patternID, name, List.map(~f, patterns))
  | MPTuple(patternID, first, second, theRest) =>
    MPTuple(patternID, f(first), f(second), List.map(~f, theRest))
  }
