// The types that the user sees. For all type definitions, see ProgramTypes.fs

module FQFnName = {
  @ppx.deriving(show({with_path: false}))
  type rec stdlibFnName = {module_: string, function: string, version: int}

  @ppx.deriving(show({with_path: false}))
  type rec infixStdlibFnName = {module_: option<string>, function: string}

  @ppx.deriving(show({with_path: false})) type rec userFnName = string

  @ppx.deriving(show({with_path: false}))
  type rec packageFnName = {
    owner: string,
    package: string,
    module_: string,
    function: string,
    version: int,
  }

  @ppx.deriving(show({with_path: false}))
  type rec t =
    | User(userFnName)
    | Stdlib(stdlibFnName)
    | Package(packageFnName)
}

@ppx.deriving(show({with_path: false}))
type rec sign =
  | Positive
  | Negative

module Sign = {
  let toString = (sign: sign): string =>
    switch sign {
    | Positive => ""
    | Negative => "-"
    }
  // Split the string into a sign and a string (removes the sign if present and )
  let split = (whole: string): (sign, string) => {
    if Tc.String.startsWith(~prefix="-", whole) {
      (Negative, Tc.String.dropLeft(~count=1, whole))
    } else if Tc.String.startsWith(~prefix="+", whole) {
      (Positive, Tc.String.dropLeft(~count=1, whole))
    } else {
      (Positive, whole)
    }
  }
  let combine = (sign: sign, whole: string): string => {
    toString(sign) ++ whole
  }
}

module Pattern = {
  @ppx.deriving(show({with_path: false}))
  type rec t =
    // match id, then pattern id
    | PVariable(ID.t, string)
    | PConstructor(ID.t, string, list<t>)
    // TODO: support char
    | PInteger(ID.t, int64)
    | PBool(ID.t, bool)
    | PString(ID.t, string)
    | PFloat(ID.t, sign, string, string)
    | PNull(ID.t)
    | PBlank(ID.t)
}

module Expr = {
  @ppx.deriving(show({with_path: false}))
  type rec sendToRail =
    | Rail
    | NoRail

  @ppx.deriving(show({with_path: false}))
  type rec t =
    | EInteger(ID.t, int64)
    | EBool(ID.t, bool)
    | EString(ID.t, string)
    | EFloat(ID.t, sign, string, string)
    | ENull(ID.t)
    | EBlank(ID.t)
    | ELet(ID.t, string, t, t)
    | EIf(ID.t, t, t, t)
    | EBinOp(ID.t, string, t, t, sendToRail)
    | ELambda(ID.t, list<(ID.t, string)>, t)
    | EFieldAccess(ID.t, t, string)
    | EVariable(ID.t, string)
    | EFnCall(ID.t, string, list<t>, sendToRail)
    | EPartial(ID.t, string, t)
    | ERightPartial(ID.t, string, t)
    | ELeftPartial(ID.t, string, t)
    | EList(ID.t, list<t>)
    | ETuple(ID.t, t, t, list<t>)
    | ERecord(ID.t, list<(string, t)>)
    | EPipe(ID.t, t, t, list<t>)
    | EConstructor(ID.t, string, list<t>)
    | EMatch(ID.t, t, list<(Pattern.t, t)>)
    | EPipeTarget(ID.t)
    | EFeatureFlag(ID.t, string, t, t, t)
}

module AST = {
  @ppx.deriving(show({with_path: false}))
  type rec t = Root(Expr.t)
}
