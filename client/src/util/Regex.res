// RegEx utility functions

let regex = (~flags="g", s): Js.Re.t => Js.Re.fromStringWithFlags(~flags, s)

let contains = (~re: Js.Re.t, s: string): bool => Js.Re.test_(re, s)

let replace = (~re: Js.Re.t, ~repl: string, str: string) => Js.String.replaceByRe(re, repl, str)

/* WARNING: Js.Re.result contains an array, consisting of the whole match
 * followed by any substring matches. It does _not_ return every possible
 * match; for that, you need to call Js.Re.exec_ until it returns None */
let matches = (~re: Js.Re.t, s: string): option<Js.Re.result> => Js.Re.exec_(re, s)

let exactly = (~re: string, s: string): bool => contains(~re=regex("^" ++ (re ++ "$")), s)

/* Returns a list of capture groups if the string is matched by the expression.
  The list head is the whole match, and tail contains all the matched capture groups.
  If nothing is matched then it returns an empty list
 */
let captures = (~re: Js.Re.t, s: string): list<string> =>
  switch Js.Re.exec_(re, s) {
  | Some(m) =>
    Js.Re.captures(m)
    |> Tc.Array.toList
    |> Tc.List.filterMap(~f=group => Js.Nullable.toOption(group))
  | None => list{}
  }
