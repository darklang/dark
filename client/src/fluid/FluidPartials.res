open Prelude

let parseStringExpr = (str: string): option<ProgramTypes.Expr.t> => {
  if String.startsWith(~prefix="\"", str) && String.endsWith(~suffix="\"", str) {
    let newStr = String.slice(~from=1, ~to_=-1, str)
    Some(EString(gid(), newStr))
  } else {
    None
  }
}

let parseIntExpr = (str: string): option<ProgramTypes.Expr.t> => {
  if Js.Re.test_(%re("/^-?[0-9]+$/"), str) {
    switch Int64.of_string_opt(str) {
    | Some(int) => Some(EInteger(gid(), int))
    | None => None
    }
  } else {
    None
  }
}

let parseFloatExpr = (str: string): option<ProgramTypes.Expr.t> => {
  if Regex.exactly(~re="-?[0-9]+\\.[0-9]+", str) {
    switch String.split(~on=".", str) {
    | list{whole, fraction} =>
      let (sign, whole) = ProgramTypes.Sign.split(whole)
      Some(EFloat(gid(), sign, whole, fraction))
    | _ => recover("invalid float passed to regex", ~debug=str, None)
    }
  } else {
    None
  }
}

let parseNumberExpr = (str: string): option<ProgramTypes.Expr.t> => {
  parseIntExpr(str) |> Option.orElseLazy(() => parseFloatExpr(str))
}

let parseExpr = (str: string): option<ProgramTypes.Expr.t> => {
  parseStringExpr(str)
  |> Option.orElseLazy(() => parseIntExpr(str))
  |> Option.orElseLazy(() => parseFloatExpr(str))
}
